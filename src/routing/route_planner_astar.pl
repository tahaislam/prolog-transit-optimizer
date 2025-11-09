/**
 * A* Route Planning Algorithm
 *
 * Efficient route finding using A* search with geographic heuristic
 * Much faster than DFS for large transit networks
 */

:- module(route_planner_astar, [
    find_route_astar/5,
    find_routes_astar/5
]).

:- use_module(library(assoc)).
:- use_module(library(heaps)).
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').

% Optional: load reasoning module if available
:- catch(use_module('../explain/reasoning'), _, true).

/**
 * find_route_astar(+From, +To, +StartTime, -Route, +Options)
 *
 * Main entry point for A* route finding
 *
 * Options:
 *   - max_transfers(N): Maximum transfers allowed (default: 3)
 *   - date(Date): Service date
 */
find_route_astar(From, To, StartTime, Route, Options) :-
    option(max_transfers(MaxTransfers), Options, 3),
    option(date(Date), Options, _),

    % Run A* search
    astar_search(From, To, StartTime, Date, MaxTransfers, RawSegments),

    % Merge consecutive segments on the same trip
    merge_segments(RawSegments, Segments),

    % Build route from segments
    Route = route_solution(TotalTime, NumTransfers, Segments),
    count_actual_transfers(Segments, NumTransfers),
    calculate_total_time(Segments, TotalTime).

/**
 * find_routes_astar(+From, +To, +StartTime, -Routes, +Options)
 *
 * Find multiple routes using A*
 */
find_routes_astar(From, To, StartTime, Routes, Options) :-
    option(max_routes(MaxRoutes), Options, 5),

    % Find up to MaxRoutes different routes
    findnsols(
        MaxRoutes,
        Route,
        find_route_astar(From, To, StartTime, Route, Options),
        Routes
    ).

/**
 * astar_search(+From, +To, +StartTime, +Date, +MaxTransfers, -Segments)
 *
 * Core A* search algorithm
 *
 * Uses:
 * - Open set: Priority heap of nodes to explore
 * - Closed set: Association list of explored nodes
 * - Heuristic: Geographic distance to goal
 * - Cost: Actual travel time
 */
astar_search(From, To, StartTime, Date, MaxTransfers, Segments) :-
    % Get goal coordinates for heuristic
    stop(To, _, ToLat, ToLon, _, _, _),

    % Initialize open set with start node
    % Node format: node(Stop, Time, Transfers, Path, GCost)
    initial_node(From, StartTime, Node),
    heuristic(From, ToLat, ToLon, H),
    F is H,  % Initial f = h (g = 0)

    empty_heap(EmptyHeap),
    add_to_heap(EmptyHeap, F, Node, OpenSet),

    % Initialize closed set
    empty_assoc(ClosedSet),

    % Run search
    astar_loop(OpenSet, ClosedSet, To, ToLat, ToLon, Date, MaxTransfers, Segments).

/**
 * initial_node(+Stop, +Time, -Node)
 */
initial_node(Stop, Time, node(Stop, Time, 0, [], 0)).

/**
 * astar_loop(+OpenSet, +ClosedSet, +Goal, +GoalLat, +GoalLon, +Date, +MaxTransfers, -Segments)
 *
 * Main A* search loop
 */
astar_loop(OpenSet, ClosedSet, Goal, GoalLat, GoalLon, Date, MaxTransfers, Segments) :-
    % Get node with lowest f-score
    get_from_heap(OpenSet, _F, Current, RestOpen),

    Current = node(CurrentStop, CurrentTime, Transfers, Path, _GCost),

    % Check if we reached the goal
    (   CurrentStop = Goal
    ->  % Goal reached! Return path
        reverse(Path, Segments)
    ;   % Not at goal, continue search
        % Add current to closed set
        put_assoc(CurrentStop, ClosedSet, true, NewClosed),

        % Explore neighbors
        findall(
            neighbor(NextStop, Segment, ArrTime, NewTransfers),
            (
                % Find trips from current stop
                next_trip_segment_bounded(CurrentStop, NextStop, CurrentTime, Date, Segment),
                NextStop \= CurrentStop,
                Segment = segment(_, _, _, _, _, ArrTime, _),

                % Calculate new transfer count
                (   Path = []  % First segment
                ->  NewTransfers = Transfers
                ;   Path = [LastSeg|_],
                    LastSeg = segment(LastTrip, _, _, _, _, _, _),
                    Segment = segment(ThisTrip, _, _, _, _, _, _),
                    (   LastTrip = ThisTrip
                    ->  NewTransfers = Transfers  % Same trip, no transfer
                    ;   NewTransfers is Transfers + 1  % Different trip, transfer
                    )
                ),

                % Check transfer limit
                NewTransfers =< MaxTransfers,

                % Not in closed set
                \+ get_assoc(NextStop, NewClosed, _)
            ),
            Neighbors
        ),

        % Process all neighbors
        process_neighbors(Neighbors, Current, RestOpen, NewClosed, GoalLat, GoalLon, Date, MaxTransfers, NewOpen),

        % Continue search
        astar_loop(NewOpen, NewClosed, Goal, GoalLat, GoalLon, Date, MaxTransfers, Segments)
    ).

/**
 * next_trip_segment_bounded(+From, -To, +Time, +Date, -Segment)
 *
 * Find trip segments from From to To, with time constraints
 * Limits to trips departing within 1 hour to bound search space
 * CRITICAL: Limits to next few stops only (max 10 stops ahead) to prevent explosion
 */
next_trip_segment_bounded(From, To, Time, Date, Segment) :-
    stop_time(TripId, From, _, DepTime, SeqFrom, _),
    time_diff(Time, DepTime, Diff),
    Diff >= 0,
    Diff =< 3600,  % Only consider trips within 1 hour

    stop_time(TripId, To, ArrTime, _, SeqTo, _),
    SeqTo > SeqFrom,
    SeqTo =< SeqFrom + 10,  % CRITICAL: Limit to next 10 stops to prevent explosion

    % Check service if date provided
    (var(Date) -> true ; check_service_active(TripId, Date)),

    trip(TripId, RouteId, _, _, _),

    StopCount is SeqTo - SeqFrom + 1,
    Segment = segment(TripId, RouteId, From, To, DepTime, ArrTime, StopCount).

/**
 * check_service_active(+TripId, +Date)
 */
check_service_active(TripId, Date) :-
    trip(TripId, _, ServiceId, _, _),
    service_active_on(ServiceId, Date).

/**
 * process_neighbors(+Neighbors, +Current, +OpenSet, +ClosedSet, +GoalLat, +GoalLon, +Date, +MaxTransfers, -NewOpenSet)
 */
process_neighbors([], _Current, OpenSet, _Closed, _GoalLat, _GoalLon, _Date, _MaxTransfers, OpenSet).

process_neighbors([neighbor(NextStop, Segment, ArrTime, NewTransfers)|Rest], Current, OpenSet, Closed, GoalLat, GoalLon, Date, MaxTransfers, FinalOpen) :-
    Current = node(_CurrentStop, _CurrentTime, _Transfers, Path, GCost),

    % Calculate new g-cost (actual cost so far)
    Segment = segment(_, _, _, _, DepTime, ArrTime, _),
    time_diff(DepTime, ArrTime, SegmentTime),
    NewGCost is GCost + SegmentTime,

    % Calculate heuristic to goal
    heuristic(NextStop, GoalLat, GoalLon, H),

    % Calculate f-score
    F is NewGCost + H,

    % Create new node
    NewPath = [Segment|Path],
    NewNode = node(NextStop, ArrTime, NewTransfers, NewPath, NewGCost),

    % Add to open set
    add_to_heap(OpenSet, F, NewNode, UpdatedOpen),

    % Process remaining neighbors
    process_neighbors(Rest, Current, UpdatedOpen, Closed, GoalLat, GoalLon, Date, MaxTransfers, FinalOpen).

/**
 * heuristic(+Stop, +GoalLat, +GoalLon, -H)
 *
 * Heuristic function: Estimated time to goal based on geographic distance
 * Assumes average transit speed of 20 km/h
 */
heuristic(Stop, GoalLat, GoalLon, H) :-
    stop(Stop, _, StopLat, StopLon, _, _, _),
    haversine_distance(StopLat, StopLon, GoalLat, GoalLon, DistanceKm),

    % Estimate time: distance / average_speed
    % Average transit speed ~20 km/h = 0.33 km/min = 3 min/km
    H is DistanceKm * 3 * 60.  % Convert to seconds

/**
 * haversine_distance(+Lat1, +Lon1, +Lat2, +Lon2, -DistanceKm)
 *
 * Calculate great-circle distance between two points
 */
haversine_distance(Lat1, Lon1, Lat2, Lon2, DistanceKm) :-
    % Convert to radians
    Lat1Rad is Lat1 * pi / 180,
    Lon1Rad is Lon1 * pi / 180,
    Lat2Rad is Lat2 * pi / 180,
    Lon2Rad is Lon2 * pi / 180,

    % Haversine formula
    DLat is Lat2Rad - Lat1Rad,
    DLon is Lon2Rad - Lon1Rad,

    A is sin(DLat/2) * sin(DLat/2) +
         cos(Lat1Rad) * cos(Lat2Rad) *
         sin(DLon/2) * sin(DLon/2),

    C is 2 * atan2(sqrt(A), sqrt(1-A)),

    % Earth radius in km
    R is 6371,
    DistanceKm is R * C.

/**
 * calculate_total_time(+Segments, -TotalTime)
 */
calculate_total_time([segment(_, _, _, _, DepTime, ArrTime, _)], TotalTime) :-
    time_diff(DepTime, ArrTime, TotalTime).

calculate_total_time([segment(_, _, _, _, FirstDep, _, _)|Rest], TotalTime) :-
    last(Rest, segment(_, _, _, _, _, LastArr, _)),
    time_diff(FirstDep, LastArr, TotalTime).

/**
 * merge_segments(+RawSegments, -MergedSegments)
 *
 * Merge consecutive segments on the same trip into one segment
 * This makes the route clearer and more intuitive
 *
 * Example:
 *   [segment(Trip1, Route1, A, B, ...), segment(Trip1, Route1, B, C, ...)]
 *   → [segment(Trip1, Route1, A, C, ...)]
 */
merge_segments([], []).
merge_segments([Seg], [Seg]).  % Single segment, nothing to merge
merge_segments([Seg1, Seg2|Rest], Merged) :-
    Seg1 = segment(Trip1, Route1, From1, _To1, Dep1, _Arr1, Count1),
    Seg2 = segment(Trip2, Route2, _From2, To2, _Dep2, Arr2, Count2),

    (   Trip1 = Trip2, Route1 = Route2  % Same trip and route
    ->  % Merge these two segments
        NewCount is Count1 + Count2 - 1,  % -1 because To1 = From2 (overlap)
        MergedSeg = segment(Trip1, Route1, From1, To2, Dep1, Arr2, NewCount),
        merge_segments([MergedSeg|Rest], Merged)
    ;   % Different trips, keep first and continue
        merge_segments([Seg2|Rest], RestMerged),
        Merged = [Seg1|RestMerged]
    ).

/**
 * count_actual_transfers(+Segments, -NumTransfers)
 *
 * Count actual transfers (when trip ID changes between consecutive segments)
 */
count_actual_transfers([], 0).
count_actual_transfers([_], 0).  % Single segment = no transfers
count_actual_transfers([segment(Trip1, _, _, _, _, _, _), segment(Trip2, _, _, _, _, _, _)|Rest], NumTransfers) :-
    count_actual_transfers([segment(Trip2, _, _, _, _, _, _)|Rest], RestTransfers),
    (   Trip1 = Trip2
    ->  NumTransfers = RestTransfers  % Same trip, no transfer
    ;   NumTransfers is RestTransfers + 1  % Different trip, count transfer
    ).

/**
 * option/3 - Extract option or use default
 */
option(Option, Options, Default) :-
    (   member(Option, Options)
    ->  true
    ;   Option =.. [_Name, Default]
    ).
