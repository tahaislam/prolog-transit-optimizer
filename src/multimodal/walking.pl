/**
 * Walking Route Integration
 *
 * Handles walking segments in multi-modal routes
 * Can be extended to integrate with OpenStreetMap for accurate walking paths
 */

:- module(walking, [
    walking_route/4,
    walking_accessible/2,
    first_last_mile/4,
    walking_distance/3,
    walking_segment/5
]).

:- use_module(library(clpfd)).
:- use_module('../core/gtfs_schema').
:- use_module('../routing/transfers').

/**
 * walking_route(+FromLat, +FromLon, +ToLat, +ToLon, -WalkingInfo)
 * Generates a walking route between two coordinates
 */
walking_route(FromLat, FromLon, ToLat, ToLon,
              walking_segment(Distance, Duration, DirectRoute)) :-
    haversine_distance(FromLat, FromLon, ToLat, ToLon, Distance),

    % Walking speed: 1.4 m/s (5 km/h)
    Duration is ceiling(Distance / 1.4 / 60) + 1,  % Convert to minutes

    % For now, direct route (can be extended with OSM)
    DirectRoute = direct.

/**
 * walking_accessible(+FromStop, +ToStop)
 * Checks if walking route between stops is accessible
 * (placeholder for OSM integration to check sidewalks, crossings, etc.)
 */
walking_accessible(FromStop, ToStop) :-
    stop(FromStop, _, Lat1, Lon1, WheelchairFrom, _, _),
    stop(ToStop, _, Lat2, Lon2, WheelchairTo, _, _),

    % Both stops must be accessible
    WheelchairFrom = 1,
    WheelchairTo = 1,

    % Walking distance must be reasonable
    haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance),
    Distance =< 400.  % Max 400m for accessible walking

/**
 * first_last_mile(+AddressLocation, +TransitStop, +Mode, -Segment)
 * Handles first/last mile connections to transit
 * Mode can be: walking, cycling, scooter, ride_hailing
 */
first_last_mile(location(Lat, Lon), StopId, walking, Segment) :-
    stop(StopId, Name, StopLat, StopLon, _, _, _),
    haversine_distance(Lat, Lon, StopLat, StopLon, Distance),
    Distance =< 1200,  % Max 1.2km / 15 min walk
    Duration is ceiling(Distance / 1.4 / 60) + 1,
    Segment = walking_segment(location(Lat, Lon), stop(StopId, Name), Distance, Duration).

first_last_mile(location(Lat, Lon), StopId, cycling, Segment) :-
    stop(StopId, Name, StopLat, StopLon, _, _, _),
    haversine_distance(Lat, Lon, StopLat, StopLon, Distance),
    Distance =< 3000,  % Max 3km / 12 min cycle
    % Cycling speed: 4.2 m/s (15 km/h)
    Duration is ceiling(Distance / 4.2 / 60) + 1,
    Segment = cycling_segment(location(Lat, Lon), stop(StopId, Name), Distance, Duration).

/**
 * walking_distance(+StopId1, +StopId2, -Meters)
 * Calculates walking distance between two stops
 */
walking_distance(StopId1, StopId2, Meters) :-
    stop(StopId1, _, Lat1, Lon1, _, _, _),
    stop(StopId2, _, Lat2, Lon2, _, _, _),
    haversine_distance(Lat1, Lon1, Lat2, Lon2, Meters).

/**
 * walking_segment(+Origin, +Destination, -Distance, -Duration, -Path)
 * Creates a walking segment with path information
 * Path is placeholder for future OSM integration
 */
walking_segment(Origin, Destination, Distance, Duration, Path) :-
    (   Origin = location(Lat1, Lon1)
    ;   Origin = stop(StopId1, _),
        stop(StopId1, _, Lat1, Lon1, _, _, _)
    ),

    (   Destination = location(Lat2, Lon2)
    ;   Destination = stop(StopId2, _),
        stop(StopId2, _, Lat2, Lon2, _, _, _)
    ),

    haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance),
    Duration is ceiling(Distance / 1.4 / 60) + 1,

    % Placeholder path - can be replaced with OSM routing
    Path = direct_line(point(Lat1, Lon1), point(Lat2, Lon2)).

/**
 * nearby_accessible_stops(+Location, +MaxDistance, -Stops)
 * Finds nearby stops accessible by walking
 */
nearby_accessible_stops(location(Lat, Lon), MaxDistance, AccessibleStops) :-
    findall(
        accessible_stop(StopId, Name, Distance, WalkTime),
        (
            stop(StopId, Name, StopLat, StopLon, _, _, _),
            haversine_distance(Lat, Lon, StopLat, StopLon, Distance),
            Distance =< MaxDistance,
            WalkTime is ceiling(Distance / 1.4 / 60) + 1
        ),
        AccessibleStops
    ).

/**
 * walk_score(+Location, -Score)
 * Calculates walk score based on nearby transit accessibility
 * Score: 0-100 (higher is better)
 */
walk_score(location(Lat, Lon), Score) :-
    % Count stops within walking distance
    findall(1, (
        stop(_, _, StopLat, StopLon, _, _, _),
        haversine_distance(Lat, Lon, StopLat, StopLon, Distance),
        Distance =< 800  % 10 minute walk
    ), Stops),
    length(Stops, NumStops),

    % Calculate score (capped at 100)
    RawScore is NumStops * 10,
    Score is min(RawScore, 100).

/**
 * OSM Integration placeholders
 * These can be implemented when integrating with OpenStreetMap data
 */

% osm_walking_route(+FromLat, +FromLon, +ToLat, +ToLon, -Path)
% Returns actual walking path from OSM

% osm_elevation_profile(+Path, -ElevationGain)
% Calculates elevation gain for accessibility considerations

% osm_sidewalk_coverage(+Path, -Coverage)
% Checks percentage of path with sidewalks

% osm_crossing_safety(+Path, -SafetyScore)
% Evaluates safety of pedestrian crossings

/**
 * multi_modal_segment(+Origin, +Destination, +Modes, -BestSegment)
 * Finds best first/last mile option from available modes
 */
multi_modal_segment(Origin, Destination, Modes, BestSegment) :-
    findall(
        segment(Mode, Duration, Segment),
        (
            member(Mode, Modes),
            first_last_mile(Origin, Destination, Mode, Segment),
            Segment =.. [_, _, _, _, Duration]
        ),
        Segments
    ),

    % Select fastest mode
    keysort(Segments, [segment(_BestMode, _BestDuration, BestSegment)|_]).
