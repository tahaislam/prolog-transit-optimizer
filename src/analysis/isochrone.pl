/**
 * Isochrone Analysis Module
 *
 * Generate and analyze isochrones (areas reachable within time limit)
 * FOUNDATION for all 3 applications:
 * - Equity Analyzer: Compare neighborhoods
 * - Scenario Engine: Before/after analysis
 * - First/Last Mile: Coverage gaps
 */

:- module(isochrone, [
    isochrone/4,
    compare_isochrones/4,
    equity_score/3,
    time_band_analysis/3,
    is_subway_stop/1
]).

:- use_module('../routing/route_planner_astar').
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').

% Optional: Load station_bundles if available (for subway stop detection)
:- catch(use_module('station_bundles', [get_bundle_for_stop/2]), _, true).

/**
 * isochrone(+Origin, +MaxMinutes, +Options, -Isochrone)
 *
 * Generate isochrone showing all destinations reachable within MaxMinutes
 *
 * Options:
 *   - time(Time): Start time (default: time(9,0,0))
 *   - date(Date): Service date
 *   - mode(Mode): transit, walk, all (default: transit)
 *   - subway_only(Bool): Only route to subway stops (default: true)
 *
 * Isochrone structure:
 *   isochrone(Origin, MaxMinutes, ReachableStops, TimeBands, Stats)
 *
 * Performance Note:
 *   With subway_only(true): ~160 destinations, <1s per route, 100% success
 *   With subway_only(false): ~9,000 destinations, 1-60s per route, 20% success
 *   Default is subway_only(true) for practical performance.
 */
isochrone(Origin, MaxMinutes, Options, Isochrone) :-
    % Extract options
    option(time(StartTime), Options, time(9,0,0)),
    option(date(Date), Options, _),
    option(subway_only(SubwayOnly), Options, true),  % Default to subway-only

    % Find all reachable destinations
    findall(
        reachable(Dest, TravelTime, Transfers, Route),
        (
            % Try to reach each potential destination
            stop(Dest, _, _, _, _, _, _),
            Dest \= Origin,

            % Filter to subway stops if requested (PERFORMANCE OPTIMIZATION)
            (   SubwayOnly = true
            ->  is_subway_stop(Dest)
            ;   true
            ),

            % Find route using A*
            find_route_astar(Origin, Dest, StartTime, Route, [date(Date)]),
            Route = route_solution(TravelTime, Transfers, _),

            % Within time limit
            TravelTime =< MaxMinutes * 60
        ),
        ReachableStops
    ),

    % Group into time bands (0-10, 10-20, 20-30, 30-40, 40-50, 50-60 min)
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
 * group_by_time_bands(+ReachableStops, -TimeBands)
 *
 * Group stops into 10-minute time bands
 */
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
            Stops \= [],  % Only include non-empty bands
            MinTime = BandMin,
            MaxTime = BandMax
        ),
        TimeBands
    ).

/**
 * calculate_stats(+ReachableStops, -Stats)
 *
 * Calculate statistics about reachable destinations
 */
calculate_stats(ReachableStops, Stats) :-
    length(ReachableStops, TotalCount),

    % Count destinations by transfer count
    findall(T, member(reachable(_, _, T, _), ReachableStops), Transfers),
    (   Transfers = []
    ->  AvgTransfers = 0
    ;   sum_list(Transfers, TransferSum),
        AvgTransfers is TransferSum / TotalCount
    ),

    % Count zero-transfer destinations
    findall(1, member(reachable(_, _, 0, _), ReachableStops), DirectList),
    length(DirectList, DirectCount),

    % Calculate average travel time
    findall(Time, member(reachable(_, Time, _, _), ReachableStops), Times),
    (   Times = []
    ->  AvgTime = 0
    ;   sum_list(Times, TimeSum),
        AvgTime is TimeSum / TotalCount
    ),

    Stats = stats(
        total_destinations(TotalCount),
        direct_connections(DirectCount),
        avg_transfers(AvgTransfers),
        avg_travel_time(AvgTime)
    ).

/**
 * compare_isochrones(+Origin1, +Origin2, +MaxMinutes, -Comparison)
 *
 * Compare accessibility between two origins (EQUITY ANALYSIS!)
 *
 * This is THE core function for transit equity analysis
 */
compare_isochrones(Origin1, Origin2, MaxMinutes, Comparison) :-
    % Generate isochrones for both origins
    isochrone(Origin1, MaxMinutes, [], Iso1),
    isochrone(Origin2, MaxMinutes, [], Iso2),

    % Extract data
    Iso1 = isochrone(_, _, reachable(_Reach1), _, stats(Stats1)),
    Iso2 = isochrone(_, _, reachable(_Reach2), _, stats(Stats2)),

    % Extract counts
    Stats1 = stats(total_destinations(Count1), _, avg_transfers(Trans1), avg_travel_time(Time1)),
    Stats2 = stats(total_destinations(Count2), _, avg_transfers(Trans2), avg_travel_time(Time2)),

    % Calculate ratios
    (   Count1 > 0
    ->  AccessRatio is Count2 / Count1
    ;   AccessRatio = 0
    ),

    (   Trans1 > 0
    ->  TransferRatio is Trans2 / Trans1
    ;   TransferRatio = 1
    ),

    (   Time1 > 0
    ->  TimeRatio is Time2 / Time1
    ;   TimeRatio = 1
    ),

    % Calculate equity score (1.0 = equal, <1.0 = Origin2 disadvantaged)
    equity_score_calc([AccessRatio, TransferRatio, TimeRatio], EquityScore),

    % Determine status
    (   EquityScore >= 0.9
    ->  Status = 'roughly equal'
    ;   EquityScore >= 0.7
    ->  Status = 'moderately underserved'
    ;   Status = 'significantly underserved'
    ),

    Comparison = comparison(
        origin1(Origin1, Count1),
        origin2(Origin2, Count2),
        access_ratio(AccessRatio),
        transfer_ratio(TransferRatio),
        time_ratio(TimeRatio),
        equity_score(EquityScore),
        status(Status)
    ).

/**
 * equity_score(+Origin1, +Origin2, -Score)
 *
 * Calculate overall equity score between two origins
 * Score ranges from 0 (completely unequal) to 1 (perfectly equal)
 */
equity_score(Origin1, Origin2, Score) :-
    compare_isochrones(Origin1, Origin2, 30, Comparison),
    Comparison = comparison(_, _, _, _, _, equity_score(Score), _).

/**
 * equity_score_calc(+Ratios, -Score)
 *
 * Calculate equity score from list of ratios
 * Uses geometric mean of (min(ratio, 1/ratio)) for each metric
 */
equity_score_calc(Ratios, Score) :-
    maplist(normalize_ratio, Ratios, Normalized),
    geometric_mean(Normalized, Score).

normalize_ratio(Ratio, Normalized) :-
    (   Ratio =< 1
    ->  Normalized = Ratio
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

/**
 * time_band_analysis(+Origin, +MaxMinutes, -Analysis)
 *
 * Analyze how many destinations are reachable in each time band
 * Useful for visualizing accessibility decay
 */
time_band_analysis(Origin, MaxMinutes, Analysis) :-
    isochrone(Origin, MaxMinutes, [], Iso),
    Iso = isochrone(_, _, _, time_bands(TimeBands), _),

    % Count stops in each band
    findall(
        band_stats(MinTime, MaxTime, Count),
        (
            member(band(MinTime, MaxTime, Stops), TimeBands),
            length(Stops, Count)
        ),
        Analysis
    ).

/**
 * option/3 - Extract option or use default
 */
option(Option, Options, Default) :-
    (   member(Option, Options)
    ->  true
    ;   Option =.. [_Name, Default]
    ).

/**
 * is_subway_stop(+StopId) is semidet.
 *
 * Check if a stop is a subway station (vs surface bus/streetcar stop)
 *
 * Uses station_bundles module if available, otherwise falls back to
 * name-based detection (stops with "Platform" in the name).
 *
 * Performance Impact:
 *   This predicate enables subway-only filtering, reducing isochrone
 *   computation from 9,000+ destinations to ~160 destinations, with
 *   100x faster performance and 100% success rate.
 */
is_subway_stop(StopId) :-
    % Try bundle-based detection first (most accurate)
    catch(
        get_bundle_for_stop(StopId, _),
        _,
        % Fallback to name-based detection
        (   stop(StopId, StopName, _, _, _, _, _),
            atom_string(StopName, NameStr),
            (   sub_string(NameStr, _, _, _, "Platform")
            ;   sub_string(NameStr, _, _, _, " Station -")
            )
        )
    ).
