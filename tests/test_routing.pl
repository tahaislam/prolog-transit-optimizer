/**
 * Unit Tests for Transit Routing
 *
 * Run with: ?- run_tests.
 */

:- begin_tests(routing).

:- use_module('../src/core/gtfs_schema').
:- use_module('../src/core/time_utils').
:- use_module('../src/routing/route_planner').
:- use_module('../src/routing/transfers').

% Setup test data
:- dynamic test_data_loaded/0.

setup_test_data :-
    test_data_loaded, !.
setup_test_data :-
    % Create sample stops
    assertz(stop('stop_1', 'Stop 1', 45.5231, -122.6765, 1, 0, '')),
    assertz(stop('stop_2', 'Stop 2', 45.5289, -122.6814, 1, 0, '')),
    assertz(stop('stop_3', 'Stop 3', 45.5320, -122.6890, 0, 0, '')),

    % Create sample route
    assertz(route('route_1', '10', 'Main Street Line', 3, 'FF0000', 'agency_1')),

    % Create sample trip
    assertz(trip('trip_1', 'route_1', 'weekday', 'Downtown', 0)),

    % Create sample stop times
    assertz(stop_time('trip_1', 'stop_1', time(9, 0, 0), time(9, 0, 0), 1, 0)),
    assertz(stop_time('trip_1', 'stop_2', time(9, 15, 0), time(9, 15, 0), 2, 0)),
    assertz(stop_time('trip_1', 'stop_3', time(9, 30, 0), time(9, 30, 0), 3, 0)),

    % Create calendar
    assertz(calendar('weekday', 1, 1, 1, 1, 1, 0, 0)),

    assertz(test_data_loaded).

:- setup_test_data.

%% Time utilities tests

test(parse_time) :-
    parse_gtfs_time('09:15:30', time(9, 15, 30)).

test(parse_time_past_midnight) :-
    parse_gtfs_time('25:30:00', time(25, 30, 0)).

test(time_to_minutes) :-
    gtfs_time_to_minutes(time(9, 15, 0), 555).

test(minutes_to_time) :-
    minutes_to_gtfs_time(555, time(9, 15, 0)).

test(time_diff) :-
    time_diff(time(9, 0, 0), time(9, 30, 0), 30).

test(time_add) :-
    time_add(time(9, 0, 0), 30, time(9, 30, 0)).

%% Transfer tests

test(haversine_distance) :-
    % Distance between two Portland stops (approximately)
    haversine_distance(45.5231, -122.6765, 45.5289, -122.6814, Distance),
    Distance > 600,
    Distance < 800.

test(walking_transfer_possible) :-
    setup_test_data,
    walking_transfer('stop_1', 'stop_2', Minutes),
    Minutes > 0,
    Minutes =< 10.

test(walking_transfer_not_too_far) :-
    setup_test_data,
    % These stops should be within walking distance
    walking_transfer('stop_1', 'stop_2', _).

test(transfer_time) :-
    setup_test_data,
    transfer_time('stop_1', 'stop_2', Minutes),
    Minutes > 0.

%% Route planning tests

test(trip_serves_stops) :-
    setup_test_data,
    trip_serves_stops('trip_1', 'stop_1', 'stop_2').

test(trip_serves_stops_order) :-
    setup_test_data,
    trip_serves_stops('trip_1', 'stop_1', 'stop_3').

test(trip_serves_stops_wrong_order) :-
    setup_test_data,
    \+ trip_serves_stops('trip_1', 'stop_3', 'stop_1').

test(next_trip_segment) :-
    setup_test_data,
    next_trip_segment('stop_1', 'stop_2', time(8, 30, 0), _, Segment),
    Segment = segment('trip_1', 'route_1', 'stop_1', 'stop_2',
                      time(9, 0, 0), time(9, 15, 0), 2).

test(route_segment) :-
    setup_test_data,
    route_segment('trip_1', 'stop_1', 'stop_2', RouteId, _, _, Duration),
    RouteId = 'route_1',
    Duration = 15.

%% Schema validation tests

test(valid_stop) :-
    setup_test_data,
    valid_stop('stop_1').

test(invalid_stop) :-
    setup_test_data,
    \+ valid_stop('nonexistent_stop').

test(stop_coordinates) :-
    setup_test_data,
    stop_coordinates('stop_1', Lat, Lon),
    Lat =:= 45.5231,
    Lon =:= -122.6765.

test(trip_route) :-
    setup_test_data,
    trip_route('trip_1', 'route_1').

%% Constraint tests

test(wheelchair_accessible_stop) :-
    setup_test_data,
    stop('stop_1', _, _, _, Wheelchair, _, _),
    Wheelchair = 1.

test(wheelchair_not_accessible_stop) :-
    setup_test_data,
    stop('stop_3', _, _, _, Wheelchair, _, _),
    Wheelchair = 0.

%% Helper tests

test(route_type_name) :-
    route_type_name(3, 'Bus').

test(route_type_rail) :-
    route_type_name(2, 'Rail').

test(wheelchair_boarding_accessible) :-
    wheelchair_boarding(1, accessible).

test(transfer_type_timed) :-
    transfer_type(1, timed).

:- end_tests(routing).

% Convenience predicate to run all tests
run_all_tests :-
    run_tests.

% Run tests on load (optional, comment out if not desired)
% :- run_tests.
