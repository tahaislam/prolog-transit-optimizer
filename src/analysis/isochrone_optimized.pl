/**
 * Optimized Isochrone Analysis Module
 *
 * PRACTICAL version with geographic filtering
 * (Original isochrone.pl is too slow for large networks)
 *
 * Key optimization: Only test stops within reasonable geographic radius
 */

:- module(isochrone_optimized, [
    isochrone_fast/4,
    compare_isochrones_fast/4,
    nearby_stops_geographic/3
]).

:- use_module('../routing/route_planner_astar').
:- use_module('../core/gtfs_schema').
:- use_module('../routing/transfers', [haversine_distance/5]).

/**
 * isochrone_fast(+Origin, +MaxMinutes, +Options, -Isochrone)
 *
 * OPTIMIZED: Only tests stops within geographic radius
 * Much faster than full network search
 */
isochrone_fast(Origin, MaxMinutes, Options, Isochrone) :-
    % Extract options
    option(time(StartTime), Options, time(9,0,0)),
    option(date(Date), Options, _),
    option(max_radius_km(MaxRadius), Options, 10),  % Default: 10km radius

    % Get origin coordinates
    stop(Origin, _, OriginLat, OriginLon, _, _, _),

    % Find candidate stops within geographic radius
    writeln('Finding candidate stops within geographic radius...'),
    nearby_stops_geographic(Origin, MaxRadius, CandidateStops),
    length(CandidateStops, CandidateCount),
    format('Testing ~w stops within ~w km~n', [CandidateCount, MaxRadius]),

    % Find reachable destinations
    writeln('Finding reachable destinations...'),
    findall(
        reachable(Dest, TravelTime, Transfers, Route),
        (
            member(nearby(Dest, _, _), CandidateStops),
            Dest \= Origin,

            % Try to find route
            find_route_astar(Origin, Dest, StartTime, Route, [date(Date)]),
            Route = route_solution(TravelTime, Transfers, _),

            % Within time limit
            TravelTime =< MaxMinutes * 60
        ),
        ReachableStops
    ),

    length(ReachableStops, ReachableCount),
    format('Found ~w reachable destinations~n', [ReachableCount]),

    % Group into time bands
    group_by_time_bands(ReachableStops, TimeBands),

    % Calculate statistics
    calculate_stats(ReachableStops, Stats),

    Isochrone = isochrone(
        origin(Origin),
        max_time(MaxMinutes),
        reachable(ReachableStops),
        time_bands(TimeBands),
        stats(Stats)
    ).

/**
 * nearby_stops_geographic(+Origin, +RadiusKm, -NearbyStops)
 *
 * Find all stops within geographic radius
 * MUCH faster than testing all stops in network
 */
nearby_stops_geographic(Origin, RadiusKm, NearbyStops) :-
    stop(Origin, _, OriginLat, OriginLon, _, _, _),

    findall(
        nearby(StopId, StopName, Distance),
        (
            stop(StopId, StopName, StopLat, StopLon, _, _, _),
            StopId \= Origin,
            haversine_distance(OriginLat, OriginLon, StopLat, StopLon, Distance),
            Distance =< RadiusKm
        ),
        NearbyStops
    ).

/**
 * compare_isochrones_fast(+Origin1, +Origin2, +MaxMinutes, -Comparison)
 *
 * Optimized version of isochrone comparison
 */
compare_isochrones_fast(Origin1, Origin2, MaxMinutes, Comparison) :-
    Options = [max_radius_km(10)],

    writeln('Generating isochrone for Origin 1...'),
    isochrone_fast(Origin1, MaxMinutes, Options, Iso1),

    writeln('Generating isochrone for Origin 2...'),
    isochrone_fast(Origin2, MaxMinutes, Options, Iso2),

    % Extract stats
    Iso1 = isochrone(_, _, reachable(_), _, stats(Stats1)),
    Iso2 = isochrone(_, _, reachable(_), _, stats(Stats2)),

    Stats1 = stats(total_destinations(Count1), _, avg_transfers(Trans1), avg_travel_time(Time1)),
    Stats2 = stats(total_destinations(Count2), _, avg_transfers(Trans2), avg_travel_time(Time2)),

    % Calculate ratios
    (   Count1 > 0 -> AccessRatio is Count2 / Count1 ; AccessRatio = 0 ),
    (   Trans1 > 0 -> TransferRatio is Trans2 / Trans1 ; TransferRatio = 1 ),
    (   Time1 > 0 -> TimeRatio is Time2 / Time1 ; TimeRatio = 1 ),

    % Calculate equity score
    equity_score_calc([AccessRatio, TransferRatio, TimeRatio], EquityScore),

    % Determine status
    (   EquityScore >= 0.9 -> Status = 'roughly equal'
    ;   EquityScore >= 0.7 -> Status = 'moderately underserved'
    ;   Status = 'significantly underserved'
    ),

    Comparison = comparison(
        origin1(Origin1, Count1),
        origin2(Origin2, Count2),
        access_ratio(AccessRatio),
        equity_score(EquityScore),
        status(Status)
    ).

% Helper predicates (same as original)
group_by_time_bands(ReachableStops, TimeBands) :-
    findall(
        band(MinTime, MaxTime, Stops),
        (
            member(BandMin, [0, 10, 20, 30, 40, 50]),
            BandMax is BandMin + 10,
            findall(
                reachable(Dest, TravelTime, Transfers, Route),
                (
                    member(reachable(Dest, TravelTime, Transfers, Route), ReachableStops),
                    TravelMinutes is TravelTime / 60,
                    TravelMinutes >= BandMin,
                    TravelMinutes < BandMax
                ),
                Stops
            ),
            Stops \= [],
            MinTime = BandMin,
            MaxTime = BandMax
        ),
        TimeBands
    ).

calculate_stats(ReachableStops, Stats) :-
    length(ReachableStops, TotalCount),

    findall(T, member(reachable(_, _, T, _), ReachableStops), Transfers),
    (   Transfers = [] -> AvgTransfers = 0
    ;   sum_list(Transfers, TransferSum),
        AvgTransfers is TransferSum / TotalCount
    ),

    findall(1, member(reachable(_, _, 0, _), ReachableStops), DirectList),
    length(DirectList, DirectCount),

    findall(Time, member(reachable(_, Time, _, _), ReachableStops), Times),
    (   Times = [] -> AvgTime = 0
    ;   sum_list(Times, TimeSum),
        AvgTime is TimeSum / TotalCount
    ),

    Stats = stats(
        total_destinations(TotalCount),
        direct_connections(DirectCount),
        avg_transfers(AvgTransfers),
        avg_travel_time(AvgTime)
    ).

equity_score_calc(Ratios, Score) :-
    maplist(normalize_ratio, Ratios, Normalized),
    geometric_mean(Normalized, Score).

normalize_ratio(Ratio, Normalized) :-
    (   Ratio =< 1 -> Normalized = Ratio
    ;   Normalized is 1 / Ratio
    ).

geometric_mean([], 1).
geometric_mean(List, Mean) :-
    List \= [],
    multiply_list(List, Product),
    length(List, N),
    Mean is Product ** (1 / N).

multiply_list([], 1).
multiply_list([H|T], Product) :-
    multiply_list(T, RestProduct),
    Product is H * RestProduct.

option(Option, Options, Default) :-
    (   member(Option, Options) -> true
    ;   Option =.. [_Name, Default]
    ).
