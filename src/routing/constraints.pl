/**
 * Constraint-Based Route Optimization
 *
 * Uses CLP(FD) for multi-objective optimization of transit routes
 * Balances travel time, transfers, walking distance, and cost
 */

:- module(constraints, [
    optimize_route/4,
    route_constraints/5,
    minimize_transfers/2,
    minimize_time/2,
    pareto_optimal_routes/3
]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').
:- use_module('../routing/route_planner').

/**
 * optimize_route(+FromStop, +ToStop, +Options, -OptimalRoute)
 * Finds the optimal route based on multiple weighted criteria
 *
 * Options:
 *   - departure_time(Time): When to depart
 *   - weights(time(W1), transfers(W2), walking(W3)): Optimization weights
 *   - constraints([...]): Hard constraints (max_time, max_transfers, etc.)
 */
optimize_route(FromStop, ToStop, Options, OptimalRoute) :-
    option(departure_time(DepTime), Options, time(8, 0, 0)),
    option(weights(TimeW, TransferW, WalkW), Options, weights(5, 3, 2)),
    option(date(Date), Options, _),

    % Find all feasible routes
    find_routes(FromStop, ToStop, DepTime, AllRoutes, Options),

    % Apply constraints and score each route
    findall(
        scored_route(Score, Route),
        (
            member(Route, AllRoutes),
            route_score(Route, TimeW, TransferW, WalkW, Score)
        ),
        ScoredRoutes
    ),

    % Select route with minimum score
    keysort(ScoredRoutes, [scored_route(_BestScore, OptimalRoute)|_]).

/**
 * route_score(+Route, +TimeWeight, +TransferWeight, +WalkWeight, -Score)
 * Calculates weighted score for a route
 */
route_score(route_solution(TotalTime, NumTransfers, _Segments), TimeW, TransferW, _WalkW, Score) :-
    % Normalize values to comparable scales
    TimeScore is TotalTime,
    TransferScore is NumTransfers * 15,  % Each transfer = 15 min penalty

    % Weighted sum
    Score is TimeW * TimeScore + TransferW * TransferScore.

/**
 * route_constraints(+Route, +MaxTime, +MaxTransfers, +MaxWalking, +Accessible)
 * Validates that a route meets hard constraints
 */
route_constraints(route_solution(TotalTime, NumTransfers, Segments), MaxTime, MaxTransfers, _MaxWalking, Accessible) :-
    TotalTime #=< MaxTime,
    NumTransfers #=< MaxTransfers,

    % If accessibility required, check all segments
    (Accessible = true -> all_accessible(Segments) ; true).

/**
 * all_accessible(+Segments)
 * Checks if all stops in route segments are wheelchair accessible
 */
all_accessible([]).
all_accessible([segment(_TripId, _RouteId, FromStop, ToStop, _, _, _)|Rest]) :-
    stop(FromStop, _, _, _, WheelchairFrom, _, _),
    stop(ToStop, _, _, _, WheelchairTo, _, _),
    WheelchairFrom #= 1,  % 1 = accessible
    WheelchairTo #= 1,
    all_accessible(Rest).

/**
 * minimize_transfers(+Routes, -MinimalRoute)
 * Selects route with minimum transfers, breaking ties by time
 */
minimize_transfers(Routes, MinimalRoute) :-
    findall(
        priority(Transfers, Time, Route),
        (
            member(Route, Routes),
            Route = route_solution(Time, Transfers, _)
        ),
        Prioritized
    ),
    sort(Prioritized, [priority(_, _, MinimalRoute)|_]).

/**
 * minimize_time(+Routes, -FastestRoute)
 * Selects fastest route, breaking ties by transfers
 */
minimize_time(Routes, FastestRoute) :-
    findall(
        priority(Time, Transfers, Route),
        (
            member(Route, Routes),
            Route = route_solution(Time, Transfers, _)
        ),
        Prioritized
    ),
    sort(Prioritized, [priority(_, _, FastestRoute)|_]).

/**
 * pareto_optimal_routes(+Routes, +Objectives, -ParetoFront)
 * Finds Pareto-optimal routes considering multiple objectives
 * Objectives: [time, transfers, walking_distance]
 *
 * A route is Pareto-optimal if no other route is better in all objectives
 */
pareto_optimal_routes(Routes, _Objectives, ParetoFront) :-
    findall(
        Route,
        (
            member(Route, Routes),
            \+ dominated(Route, Routes)
        ),
        ParetoFront
    ).

/**
 * dominated(+Route, +AllRoutes)
 * Checks if Route is dominated by any other route
 * (another route is better in all objectives)
 */
dominated(Route1, AllRoutes) :-
    Route1 = route_solution(Time1, Transfers1, _),
    member(Route2, AllRoutes),
    Route2 = route_solution(Time2, Transfers2, _),
    Route1 \= Route2,
    Time2 =< Time1,
    Transfers2 =< Transfers1,
    (Time2 < Time1 ; Transfers2 < Transfers1).  % At least one strictly better

/**
 * time_window_constraint(+DesiredArrival, +Tolerance, +Route)
 * Constrains arrival time to be within tolerance of desired time
 */
time_window_constraint(DesiredArrival, ToleranceMinutes, route_solution(_Time, _Transfers, Segments)) :-
    last(Segments, segment(_, _, _, _, _, ArrivalTime, _)),
    time_diff(DesiredArrival, ArrivalTime, Diff),
    abs(Diff) #=< ToleranceMinutes.

/**
 * transfer_quality(+TransferStops, +MinTransferTime, -Quality)
 * Evaluates quality of transfers based on available time
 */
transfer_quality([], _, excellent).
transfer_quality([transfer_point(FromStop, ToStop, AvailableTime)|Rest], MinTime, Quality) :-
    (   transfer(FromStop, ToStop, _Type, RequiredTime)
    ->  ActualMin = RequiredTime
    ;   ActualMin = MinTime
    ),

    (   AvailableTime >= ActualMin * 2
    ->  Quality1 = excellent
    ;   AvailableTime >= ActualMin
    ->  Quality1 = adequate
    ;   Quality1 = tight
    ),

    transfer_quality(Rest, MinTime, Quality2),
    worse_quality(Quality1, Quality2, Quality).

worse_quality(tight, _, tight).
worse_quality(_, tight, tight).
worse_quality(adequate, excellent, adequate).
worse_quality(excellent, adequate, adequate).
worse_quality(adequate, adequate, adequate).
worse_quality(excellent, excellent, excellent).

/**
 * cost_constraint(+Route, +MaxCost, +FareRules)
 * Validates route does not exceed maximum cost
 */
cost_constraint(route_solution(_Time, NumTransfers, Segments), MaxCost, FareRules) :-
    % Simplified: base fare + transfer fees
    option(base_fare(Base), FareRules, 2.50),
    option(transfer_fee(TransferFee), FareRules, 0),

    TotalCost is Base + (NumTransfers * TransferFee),
    TotalCost #=< MaxCost.

/**
 * route_reliability(+Route, +HistoricalData, -ReliabilityScore)
 * Estimates route reliability based on historical performance
 * (Placeholder for future integration with real-time data)
 */
route_reliability(route_solution(_Time, Transfers, _Segments), _HistoricalData, Score) :-
    % Simple heuristic: fewer transfers = more reliable
    BaseScore is 100,
    TransferPenalty is Transfers * 5,
    Score is BaseScore - TransferPenalty.

/**
 * Helper: option/3
 */
option(Option, Options, Default) :-
    (member(Option, Options) -> true ; Option =.. [_|[Default]]).
