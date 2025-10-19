/**
 * Transfer Management
 *
 * Handles transfers between routes, including walking transfers,
 * timed transfers, and transfer quality assessment
 */

:- module(transfers, [
    find_transfer/4,
    walking_transfer/3,
    timed_transfer/4,
    transfer_time/3,
    valid_transfer/5,
    nearby_stops/3
]).

:- use_module(library(clpfd)).
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').

/**
 * find_transfer(+FromStop, +ToStop, +ArrivalTime, -TransferInfo)
 * Finds available transfer options between stops
 */
find_transfer(FromStop, ToStop, ArrivalTime, TransferInfo) :-
    % First check for explicit transfer in GTFS data
    (   transfer(FromStop, ToStop, TransferType, MinTime)
    ->  TransferInfo = explicit_transfer(TransferType, MinTime, NextDepTime),
        time_add(ArrivalTime, MinTime, NextDepTime)
    ;   % Otherwise, check if walking transfer is possible
        walking_transfer(FromStop, ToStop, WalkTime),
        TransferInfo = walking_transfer(WalkTime, NextDepTime),
        time_add(ArrivalTime, WalkTime, NextDepTime)
    ).

/**
 * walking_transfer(+FromStop, +ToStop, -WalkingMinutes)
 * Calculates walking time between two stops based on distance
 * Uses haversine formula for geographic distance
 */
walking_transfer(FromStop, ToStop, WalkingMinutes) :-
    stop(FromStop, _, Lat1, Lon1, _, _, _),
    stop(ToStop, _, Lat2, Lon2, _, _, _),

    % Calculate distance in meters
    haversine_distance(Lat1, Lon1, Lat2, Lon2, DistanceMeters),

    % Walking speed: 1.4 m/s (5 km/h) + 1 minute overhead
    WalkingSeconds is DistanceMeters / 1.4,
    WalkingMinutes is ceiling(WalkingSeconds / 60) + 1,

    % Only allow reasonable walking distances (max 10 minutes / ~800m)
    WalkingMinutes =< 10.

/**
 * haversine_distance(+Lat1, +Lon1, +Lat2, +Lon2, -DistanceMeters)
 * Calculates great-circle distance between two points
 */
haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance) :-
    % Convert to radians
    Lat1Rad is Lat1 * pi / 180,
    Lat2Rad is Lat2 * pi / 180,
    DLat is (Lat2 - Lat1) * pi / 180,
    DLon is (Lon2 - Lon1) * pi / 180,

    % Haversine formula
    A is sin(DLat/2) * sin(DLat/2) +
         cos(Lat1Rad) * cos(Lat2Rad) *
         sin(DLon/2) * sin(DLon/2),
    C is 2 * atan2(sqrt(A), sqrt(1-A)),

    % Earth radius in meters
    R = 6371000,
    Distance is R * C.

/**
 * timed_transfer(+FromStop, +ToStop, +ArrivalTime, -TransferInfo)
 * Finds timed transfers where connecting vehicle waits
 */
timed_transfer(FromStop, ToStop, ArrivalTime, timed(WaitTime, GuaranteedDeparture)) :-
    transfer(FromStop, ToStop, 1, MinTime),  % Type 1 = timed transfer
    time_add(ArrivalTime, MinTime, GuaranteedDeparture),
    time_diff(ArrivalTime, GuaranteedDeparture, WaitTime).

/**
 * transfer_time(+FromStop, +ToStop, -Minutes)
 * Determines required transfer time between stops
 */
transfer_time(FromStop, ToStop, Minutes) :-
    % Check for explicit transfer time
    (   transfer(FromStop, ToStop, _Type, MinTime)
    ->  Minutes = MinTime
    ;   % Use walking time as fallback
        walking_transfer(FromStop, ToStop, Minutes)
    ;   % Default transfer time
        Minutes = 5
    ).

/**
 * valid_transfer(+FromStop, +ToStop, +ArrivalTime, +DepartureTime, +Options)
 * Validates that a transfer is feasible given timing constraints
 */
valid_transfer(FromStop, ToStop, ArrivalTime, DepartureTime, Options) :-
    option(min_transfer_time(MinTime), Options, 3),

    % Calculate available transfer time
    time_diff(ArrivalTime, DepartureTime, AvailableTime),

    % Check if enough time for transfer
    transfer_time(FromStop, ToStop, RequiredTime),
    AvailableTime >= max(RequiredTime, MinTime),

    % Check accessibility if required
    (   option(wheelchair_accessible(true), Options)
    ->  accessible_transfer(FromStop, ToStop)
    ;   true
    ).

/**
 * accessible_transfer(+FromStop, +ToStop)
 * Checks if transfer is wheelchair accessible
 */
accessible_transfer(FromStop, ToStop) :-
    stop(FromStop, _, _, _, Wheelchair1, _, _),
    stop(ToStop, _, _, _, Wheelchair2, _, _),
    Wheelchair1 = 1,  % Accessible
    Wheelchair2 = 1.

/**
 * nearby_stops(+StopId, +MaxDistanceMeters, -NearbyStops)
 * Finds all stops within walking distance
 */
nearby_stops(StopId, MaxDistance, NearbyStops) :-
    stop(StopId, _, Lat, Lon, _, _, _),
    findall(
        nearby(NearbyId, Name, Distance),
        (
            stop(NearbyId, Name, NearbyLat, NearbyLon, _, _, _),
            NearbyId \= StopId,
            haversine_distance(Lat, Lon, NearbyLat, NearbyLon, Distance),
            Distance =< MaxDistance
        ),
        NearbyStops
    ).

/**
 * transfer_penalty(+TransferType, -PenaltyMinutes)
 * Assigns time penalty for different transfer types
 */
transfer_penalty(recommended, 2).     % Minimal penalty
transfer_penalty(timed, 0).           % No penalty, guaranteed connection
transfer_penalty(minimum_time, 5).    % Moderate penalty for uncertainty
transfer_penalty(walking, 3).         % Penalty for walking

/**
 * transfer_quality_score(+FromStop, +ToStop, +AvailableTime, -Score)
 * Scores transfer quality (0-100, higher is better)
 */
transfer_quality_score(FromStop, ToStop, AvailableTime, Score) :-
    transfer_time(FromStop, ToStop, RequiredTime),

    % Calculate buffer ratio
    BufferRatio is AvailableTime / RequiredTime,

    % Score based on buffer
    (   BufferRatio >= 2.0
    ->  Score = 100        % Plenty of time
    ;   BufferRatio >= 1.5
    ->  Score = 80         % Comfortable
    ;   BufferRatio >= 1.2
    ->  Score = 60         % Adequate
    ;   BufferRatio >= 1.0
    ->  Score = 40         % Tight
    ;   Score = 0          % Impossible
    ).

/**
 * same_platform_transfer(+FromStop, +ToStop)
 * Checks if transfer can happen at same platform/station
 */
same_platform_transfer(FromStop, ToStop) :-
    stop(FromStop, _, _, _, _, _, ParentStation),
    stop(ToStop, _, _, _, _, _, ParentStation),
    ParentStation \= ''.

/**
 * cross_platform_transfer(+FromStop, +ToStop)
 * Checks if transfer requires crossing platforms but in same station
 */
cross_platform_transfer(FromStop, ToStop) :-
    stop(FromStop, _, _, _, _, _, ParentStation),
    stop(ToStop, _, _, _, _, _, ParentStation),
    ParentStation \= '',
    FromStop \= ToStop.

/**
 * Helper: option/3
 */
option(Option, Options, Default) :-
    (member(Option, Options) -> true ; Option =.. [_|[Default]]).
