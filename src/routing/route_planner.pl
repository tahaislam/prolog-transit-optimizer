/**
 * Route Planning Core
 *
 * Implements the core routing algorithms for finding optimal transit routes
 * Uses A* search with multiple optimization criteria
 */

:- module(route_planner, [
    find_routes/5,
    find_direct_trips/4,
    next_departure/4,
    reachable_stops/4,
    trip_serves_stops/3,
    route_segment/7
]).

:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').

% Optional: load reasoning module if available for tracing
:- catch(use_module('../explain/reasoning'), _, true).

/**
 * find_routes(+FromStopId, +ToStopId, +DepartureTime, -Routes, +Options)
 * Main entry point for route finding
 * Returns a list of possible routes ordered by preference
 *
 * Options can include:
 *   - max_transfers(N): Maximum number of transfers (default: 3)
 *   - max_walking_distance(M): Maximum walking distance in meters (default: 500)
 *   - wheelchair_accessible(true/false): Only wheelchair accessible routes
 *   - date(YYYYMMDD): Date for service calendar check
 */
find_routes(FromStopId, ToStopId, DepartureTime, Routes, Options) :-
    option(max_transfers(MaxTransfers), Options, 3),
    option(max_routes(MaxRoutes), Options, 5),  % Limit to first 5 routes by default
    option(date(Date), Options, _),

    % Find up to MaxRoutes possible routes with BFS/DFS
    % Using findnsols instead of findall to limit search space
    findnsols(
        MaxRoutes,
        route_solution(TotalTime, Transfers, Segments),
        find_route_with_transfers(FromStopId, ToStopId, DepartureTime, Date,
                                   MaxTransfers, TotalTime, Transfers, Segments),
        AllRoutes
    ),

    % Sort by total time, then by number of transfers
    sort_routes(AllRoutes, Routes).

/**
 * find_route_with_transfers(+From, +To, +DepTime, +Date, +MaxTransfers, -TotalTime, -NumTransfers, -Segments)
 * Finds a route allowing up to MaxTransfers transfers
 */
find_route_with_transfers(From, To, DepTime, Date, MaxTransfers, TotalTime, NumTransfers, Segments) :-
    find_route_dfs(From, To, DepTime, Date, MaxTransfers, 0, [], Segments),
    length(Segments, SegCount),
    NumTransfers is SegCount - 1,
    calculate_total_time(Segments, TotalTime).

/**
 * find_route_dfs(+CurrentStop, +DestStop, +CurrentTime, +Date, +MaxTransfers, +Depth, +Visited, -Segments)
 * Depth-first search for route segments
 */
find_route_dfs(CurrentStop, DestStop, CurrentTime, Date, MaxTransfers, Depth, Visited, [Segment]) :-
    Depth =< MaxTransfers,
    \+ member(CurrentStop, Visited),

    % Find a direct trip from current stop to destination
    next_trip_segment(CurrentStop, DestStop, CurrentTime, Date, Segment),

    % Log this segment selection (if tracing enabled)
    (   catch(reasoning:trace_enabled, _, fail)
    ->  Segment = segment(TripId, RouteId, From, To, DepTime, ArrTime, _),
        stop(From, FromName, _, _, _, _, _),
        stop(To, ToName, _, _, _, _, _),
        route(RouteId, _, RouteName, _, _, _),
        format(atom(Reason), 'Direct route found: ~w from ~w to ~w (depart ~w, arrive ~w)',
               [RouteName, FromName, ToName, DepTime, ArrTime]),
        reasoning:add_decision(
            route_segment_selected,
            context(segment(TripId, RouteId, From, To), direct(true)),
            Reason
        )
    ;   true
    ).

find_route_dfs(CurrentStop, DestStop, CurrentTime, Date, MaxTransfers, Depth, Visited, [Segment|RestSegments]) :-
    Depth < MaxTransfers,
    \+ member(CurrentStop, Visited),

    % Find next segment to an intermediate stop
    next_trip_segment(CurrentStop, IntermediateStop, CurrentTime, Date, Segment),
    IntermediateStop \= DestStop,

    % Get arrival time at intermediate stop
    Segment = segment(TripId, RouteId, From, To, _DepTime, ArrivalTime, _),

    % Log segment selection with transfer
    (   catch(reasoning:trace_enabled, _, fail)
    ->  stop(From, FromName, _, _, _, _, _),
        stop(To, ToName, _, _, _, _, _),
        route(RouteId, _, RouteName, _, _, _),
        format(atom(SegReason), 'Selected segment: ~w from ~w to ~w (transfer required)',
               [RouteName, FromName, ToName]),
        reasoning:add_decision(
            route_segment_selected,
            context(segment(TripId, RouteId, From, To), direct(false)),
            SegReason
        )
    ;   true
    ),

    % Add transfer time (assume 5 minutes)
    time_add(ArrivalTime, 5, NextDepTime),

    % Log transfer decision
    (   catch(reasoning:trace_enabled, _, fail)
    ->  stop(IntermediateStop, TransferStopName, _, _, _, _, _),
        format(atom(TransferReason), 'Transfer at ~w (5 min connection time from ~w to ~w)',
               [TransferStopName, ArrivalTime, NextDepTime]),
        reasoning:add_decision(
            transfer_point,
            context(stop(IntermediateStop), arrival(ArrivalTime), next_dep(NextDepTime)),
            TransferReason
        )
    ;   true
    ),

    % Continue search from intermediate stop
    NewDepth is Depth + 1,
    find_route_dfs(IntermediateStop, DestStop, NextDepTime, Date,
                   MaxTransfers, NewDepth, [CurrentStop|Visited], RestSegments).

/**
 * next_trip_segment(+FromStop, +ToStop, +AfterTime, +Date, -Segment)
 * Finds the next available trip segment from FromStop to ToStop after AfterTime
 *
 * Returns: segment(TripId, RouteId, FromStop, ToStop, DepartureTime, ArrivalTime, StopCount)
 */
next_trip_segment(FromStop, ToStop, AfterTime, Date,
                  segment(TripId, RouteId, FromStop, ToStop, DepTime, ArrTime, StopCount)) :-
    % Find a trip that serves both stops
    stop_time(TripId, FromStop, _, DepTime, SeqFrom, _),
    stop_time(TripId, ToStop, ArrTime, _, SeqTo, _),
    SeqTo > SeqFrom,  % Ensure correct direction

    % Check departure is after requested time
    time_diff(AfterTime, DepTime, Diff),
    Diff >= 0,

    % Check service is active on date (if date provided)
    (var(Date) -> true ; check_service_active(TripId, Date)),

    % Get route info
    trip(TripId, RouteId, _, _, _),

    % Calculate stops on this segment
    StopCount is SeqTo - SeqFrom + 1.

/**
 * check_service_active(+TripId, +Date)
 * Verifies that the trip's service is active on the given date
 */
check_service_active(TripId, Date) :-
    trip(TripId, _, ServiceId, _, _),
    service_active_on(ServiceId, Date).

/**
 * find_direct_trips(+FromStopId, +ToStopId, +DepartureTime, -Trips)
 * Finds all direct trips (no transfers) between two stops
 */
find_direct_trips(FromStopId, ToStopId, DepartureTime, Trips) :-
    findall(
        trip_info(TripId, RouteId, RouteName, DepTime, ArrTime, Duration),
        (
            next_trip_segment(FromStopId, ToStopId, DepartureTime, _,
                            segment(TripId, RouteId, _, _, DepTime, ArrTime, _)),
            route(RouteId, RouteName, _, _, _, _),
            time_diff(DepTime, ArrTime, Duration)
        ),
        Trips
    ).

/**
 * next_departure(+StopId, +RouteId, +AfterTime, -NextDeparture)
 * Finds the next departure time for a specific route at a stop
 */
next_departure(StopId, RouteId, AfterTime, departure(TripId, DepTime, Headsign)) :-
    trip(TripId, RouteId, _, Headsign, _),
    stop_time(TripId, StopId, _, DepTime, _, _),
    time_diff(AfterTime, DepTime, Diff),
    Diff >= 0.

/**
 * reachable_stops(+FromStopId, +DepartureTime, +MaxTime, -ReachableStops)
 * Finds all stops reachable within MaxTime minutes
 * Useful for isochrone generation
 */
reachable_stops(FromStopId, DepartureTime, MaxTime, ReachableStops) :-
    findall(
        reachable(StopId, ArrTime, TravelTime),
        (
            next_trip_segment(FromStopId, StopId, DepartureTime, _,
                            segment(_, _, _, _, DepTime, ArrTime, _)),
            time_diff(DepTime, ArrTime, TravelTime),
            TravelTime =< MaxTime
        ),
        ReachableStops
    ).

/**
 * trip_serves_stops(+TripId, +StopId1, +StopId2)
 * Checks if a trip serves both stops in correct order
 */
trip_serves_stops(TripId, StopId1, StopId2) :-
    stop_time(TripId, StopId1, _, _, Seq1, _),
    stop_time(TripId, StopId2, _, _, Seq2, _),
    Seq2 > Seq1.

/**
 * route_segment(+TripId, +FromStop, +ToStop, -RouteId, -DepTime, -ArrTime, -Duration)
 * Extracts route segment information
 */
route_segment(TripId, FromStop, ToStop, RouteId, DepTime, ArrTime, Duration) :-
    stop_time(TripId, FromStop, _, DepTime, SeqFrom, _),
    stop_time(TripId, ToStop, ArrTime, _, SeqTo, _),
    SeqTo > SeqFrom,
    trip(TripId, RouteId, _, _, _),
    time_diff(DepTime, ArrTime, Duration).

/**
 * Helper predicates
 */

% Calculate total journey time from segments
calculate_total_time([segment(_, _, _, _, DepTime, ArrTime, _)], TotalTime) :-
    time_diff(DepTime, ArrTime, TotalTime).
calculate_total_time([segment(_, _, _, _, FirstDep, _, _)|Rest], TotalTime) :-
    last(Rest, segment(_, _, _, _, _, LastArr, _)),
    time_diff(FirstDep, LastArr, TotalTime).

% Sort routes by total time, then transfers
sort_routes(Routes, Sorted) :-
    map_list_to_pairs(route_priority, Routes, Paired),
    keysort(Paired, SortedPairs),
    pairs_values(SortedPairs, Sorted).

route_priority(route_solution(Time, Transfers, _), priority(Time, Transfers)).

% Option handling
option(Option, Options, Default) :-
    (member(Option, Options) -> true ; Option =.. [_|[Default]]).
