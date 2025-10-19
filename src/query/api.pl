/**
 * High-Level Query API
 *
 * User-friendly interface for common transit queries
 */

:- module(api, [
    load_gtfs/1,
    find_route/5,
    next_departures/4,
    trips_between/4,
    accessible_route/4,
    fastest_route/4,
    least_transfers/4,
    isochrone/4,
    trip_planner/3
]).

:- use_module('../core/gtfs_loader').
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').
:- use_module('../routing/route_planner').
:- use_module('../routing/constraints').
:- use_module('../routing/transfers').
:- use_module('../multimodal/walking').

/**
 * load_gtfs(+DataDirectory)
 * Convenience wrapper for loading GTFS data
 */
load_gtfs(DataDir) :-
    load_gtfs_data(DataDir).

/**
 * find_route(+From, +To, +When, -Routes, +Options)
 * Main route finding interface
 *
 * From/To can be:
 *   - stop_id('123')
 *   - stop_name('Downtown Station')
 *   - location(Lat, Lon)
 *
 * When can be:
 *   - time(H, M, S)
 *   - now
 *
 * Options:
 *   - max_transfers(N)
 *   - wheelchair_accessible(true/false)
 *   - optimize(time/transfers/balanced)
 *   - date(YYYYMMDD)
 */
find_route(From, To, When, Routes, Options) :-
    resolve_stop(From, FromStopId),
    resolve_stop(To, ToStopId),
    resolve_time(When, DepartureTime),

    find_routes(FromStopId, ToStopId, DepartureTime, AllRoutes, Options),

    % Apply optimization preference
    option(optimize(OptMode), Options, balanced),
    optimize_routes(AllRoutes, OptMode, Routes).

/**
 * next_departures(+StopId, +Count, +AfterTime, -Departures)
 * Gets next N departures from a stop
 */
next_departures(StopId, Count, AfterTime, Departures) :-
    findall(
        departure(RouteShortName, RouteLongName, Headsign, DepTime, TripId),
        (
            stop_time(TripId, StopId, _, DepTime, _, _),
            time_diff(AfterTime, DepTime, Diff),
            Diff >= 0,
            trip(TripId, RouteId, _, Headsign, _),
            route(RouteId, RouteShortName, RouteLongName, _, _, _)
        ),
        AllDepartures
    ),

    % Sort by departure time and take first Count
    sort_by_time(AllDepartures, Sorted),
    take_first(Count, Sorted, Departures).

/**
 * trips_between(+FromStop, +ToStop, +TimeWindow, -Trips)
 * Finds all direct trips between two stops in a time window
 */
trips_between(FromStop, ToStop, time_window(Start, End), Trips) :-
    findall(
        trip_option(TripId, RouteId, RouteName, DepTime, ArrTime, Duration),
        (
            next_trip_segment(FromStop, ToStop, Start, _,
                            segment(TripId, RouteId, _, _, DepTime, ArrTime, _)),
            time_between(DepTime, Start, End),
            route(RouteId, RouteName, _, _, _, _),
            time_diff(DepTime, ArrTime, Duration)
        ),
        Trips
    ).

/**
 * accessible_route(+From, +To, +When, -Route)
 * Finds wheelchair accessible route
 */
accessible_route(From, To, When, Route) :-
    Options = [wheelchair_accessible(true), max_transfers(2)],
    find_route(From, To, When, [Route|_], Options).

/**
 * fastest_route(+From, +To, +When, -Route)
 * Finds route with minimum travel time
 */
fastest_route(From, To, When, Route) :-
    Options = [optimize(time), max_transfers(3)],
    find_route(From, To, When, [Route|_], Options).

/**
 * least_transfers(+From, +To, +When, -Route)
 * Finds route with minimum transfers
 */
least_transfers(From, To, When, Route) :-
    Options = [optimize(transfers), max_transfers(2)],
    find_route(From, To, When, [Route|_], Options).

/**
 * isochrone(+FromStop, +DepartureTime, +MaxMinutes, -ReachableStops)
 * Generates isochrone - all stops reachable within time limit
 */
isochrone(FromStop, DepartureTime, MaxMinutes, ReachableStops) :-
    reachable_stops(FromStop, DepartureTime, MaxMinutes, ReachableStops).

/**
 * trip_planner(+TripSpec, -Plan, +Options)
 * Advanced trip planning with multiple destinations
 *
 * TripSpec: [location(Origin), visit(Dest1), visit(Dest2), ..., return(Origin)]
 */
trip_planner([Start|Destinations], Plan, Options) :-
    plan_multi_destination_trip(Start, Destinations, Plan, Options).

plan_multi_destination_trip(_Origin, [], [], _Options).
plan_multi_destination_trip(CurrentLoc, [visit(NextLoc)|Rest], [Segment|RestPlan], Options) :-
    option(departure_time(DepTime), Options, time(9, 0, 0)),
    find_route(CurrentLoc, NextLoc, DepTime, [Segment|_], Options),

    % Calculate arrival time for next leg
    Segment = route_solution(_Time, _Transfers, Segments),
    last(Segments, segment(_, _, _, _, _, ArrTime, _)),
    time_add(ArrTime, 30, NextDepTime),  % 30 min at destination

    % Plan rest of trip
    NewOptions = [departure_time(NextDepTime)|Options],
    plan_multi_destination_trip(NextLoc, Rest, RestPlan, NewOptions).

/**
 * Helper predicates
 */

% Resolve stop from various input formats
resolve_stop(stop_id(Id), Id) :- !.
resolve_stop(stop_name(Name), Id) :-
    !,
    stop(Id, Name, _, _, _, _, _).
resolve_stop(location(Lat, Lon), Id) :-
    !,
    % Find nearest stop
    findall(
        dist(Distance, StopId),
        (
            stop(StopId, _, StopLat, StopLon, _, _, _),
            haversine_distance(Lat, Lon, StopLat, StopLon, Distance)
        ),
        Distances
    ),
    keysort(Distances, [dist(_, Id)|_]).

% Resolve time from various formats
resolve_time(now, Time) :-
    !,
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(time, DateTime, Time).
resolve_time(Time, Time).

% Optimize routes based on preference
optimize_routes(Routes, time, Optimized) :-
    minimize_time(Routes, BestRoute),
    Optimized = [BestRoute].
optimize_routes(Routes, transfers, Optimized) :-
    minimize_transfers(Routes, BestRoute),
    Optimized = [BestRoute].
optimize_routes(Routes, balanced, Optimized) :-
    pareto_optimal_routes(Routes, [time, transfers], Optimized).

% Sort departures by time
sort_by_time(Departures, Sorted) :-
    map_list_to_pairs(extract_time, Departures, Paired),
    keysort(Paired, SortedPairs),
    pairs_values(SortedPairs, Sorted).

extract_time(departure(_, _, _, Time, _), TimeMinutes) :-
    gtfs_time_to_minutes(Time, TimeMinutes).

% Take first N elements
take_first(0, _, []) :- !.
take_first(_, [], []) :- !.
take_first(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_first(N1, T, Rest).

% Option handling
option(Option, Options, Default) :-
    (member(Option, Options) -> true ; Option =.. [_|[Default]]).
