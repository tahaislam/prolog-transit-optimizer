/**
 * Example Queries
 *
 * Demonstrates how to use the transit optimizer system
 * Load this file after loading GTFS data to try example queries
 */

:- module(example_queries, [
    demo_basic_routing/0,
    demo_advanced_routing/0,
    demo_accessibility/0,
    demo_transfers/0,
    demo_isochrone/0,
    demo_multi_destination/0
]).

:- use_module('../src/query/api').
:- use_module('../src/core/gtfs_schema').
:- use_module('../src/core/time_utils').

/**
 * demo_basic_routing
 * Demonstrates basic route finding between two stops
 */
demo_basic_routing :-
    writeln('=== DEMO: Basic Routing ==='),
    writeln(''),

    % Find a route departing at 9:00 AM
    writeln('Finding route from Stop A to Stop B, departing at 9:00 AM...'),
    writeln(''),

    % Example query (replace with actual stop IDs from your data)
    % find_route(
    %     stop_id('STOP_A'),
    %     stop_id('STOP_B'),
    %     time(9, 0, 0),
    %     Routes,
    %     [max_transfers(2)]
    % ),

    writeln('Example query:'),
    writeln('  find_route('),
    writeln('      stop_id(\'STOP_A\'),'),
    writeln('      stop_id(\'STOP_B\'),'),
    writeln('      time(9, 0, 0),'),
    writeln('      Routes,'),
    writeln('      [max_transfers(2)]'),
    writeln('  ).'),
    writeln('').

/**
 * demo_advanced_routing
 * Demonstrates optimization preferences
 */
demo_advanced_routing :-
    writeln('=== DEMO: Advanced Routing with Optimization ==='),
    writeln(''),

    writeln('1. Fastest route (minimize travel time):'),
    writeln('   fastest_route(stop_id(\'A\'), stop_id(\'B\'), time(9,0,0), Route).'),
    writeln(''),

    writeln('2. Least transfers (minimize number of transfers):'),
    writeln('   least_transfers(stop_id(\'A\'), stop_id(\'B\'), time(9,0,0), Route).'),
    writeln(''),

    writeln('3. Balanced optimization (Pareto optimal):'),
    writeln('   find_route(stop_id(\'A\'), stop_id(\'B\'), time(9,0,0), Routes,'),
    writeln('              [optimize(balanced)]).'),
    writeln('').

/**
 * demo_accessibility
 * Demonstrates wheelchair accessible routing
 */
demo_accessibility :-
    writeln('=== DEMO: Wheelchair Accessible Routing ==='),
    writeln(''),

    writeln('Finding wheelchair accessible route:'),
    writeln('  accessible_route('),
    writeln('      stop_id(\'DOWNTOWN\'),'),
    writeln('      stop_id(\'AIRPORT\'),'),
    writeln('      time(10, 30, 0),'),
    writeln('      Route'),
    writeln('  ).'),
    writeln(''),

    writeln('Or with explicit options:'),
    writeln('  find_route(stop_id(\'A\'), stop_id(\'B\'), time(10,30,0), Routes,'),
    writeln('             [wheelchair_accessible(true), max_transfers(1)]).'),
    writeln('').

/**
 * demo_transfers
 * Demonstrates transfer analysis
 */
demo_transfers :-
    writeln('=== DEMO: Transfer Analysis ==='),
    writeln(''),

    writeln('Find nearby stops for transfers:'),
    writeln('  nearby_stops(stop_id(\'CENTRAL_STATION\'), 400, NearbyStops).'),
    writeln(''),

    writeln('Calculate walking time between stops:'),
    writeln('  walking_transfer(stop_id(\'STOP_1\'), stop_id(\'STOP_2\'), Minutes).'),
    writeln(''),

    writeln('Check if transfer is feasible:'),
    writeln('  valid_transfer('),
    writeln('      stop_id(\'FROM\'),'),
    writeln('      stop_id(\'TO\'),'),
    writeln('      time(9, 15, 0),  % arrival time'),
    writeln('      time(9, 25, 0),  % next departure'),
    writeln('      [min_transfer_time(5)]'),
    writeln('  ).'),
    writeln('').

/**
 * demo_isochrone
 * Demonstrates isochrone generation (reachability analysis)
 */
demo_isochrone :-
    writeln('=== DEMO: Isochrone Generation ==='),
    writeln(''),

    writeln('Find all stops reachable within 30 minutes:'),
    writeln('  isochrone('),
    writeln('      stop_id(\'DOWNTOWN\'),'),
    writeln('      time(9, 0, 0),'),
    writeln('      30,  % max minutes'),
    writeln('      ReachableStops'),
    writeln('  ).'),
    writeln(''),

    writeln('This is useful for:'),
    writeln('  - Visualizing transit coverage'),
    writeln('  - Comparing service levels'),
    writeln('  - Analyzing equity of transit access'),
    writeln('').

/**
 * demo_multi_destination
 * Demonstrates multi-destination trip planning
 */
demo_multi_destination :-
    writeln('=== DEMO: Multi-Destination Trip Planning ==='),
    writeln(''),

    writeln('Plan a trip visiting multiple destinations:'),
    writeln('  trip_planner('),
    writeln('      ['),
    writeln('          location(42.3601, -71.0589),  % Start: Boston'),
    writeln('          visit(stop_id(\'MUSEUM\')),'),
    writeln('          visit(stop_id(\'RESTAURANT\')),'),
    writeln('          visit(stop_id(\'THEATER\')),'),
    writeln('          return(location(42.3601, -71.0589))'),
    writeln('      ],'),
    writeln('      Plan,'),
    writeln('      [departure_time(time(9, 0, 0))]'),
    writeln('  ).'),
    writeln('').

/**
 * demo_next_departures
 * Demonstrates real-time departure board
 */
demo_next_departures :-
    writeln('=== DEMO: Next Departures (Departure Board) ==='),
    writeln(''),

    writeln('Get next 5 departures from a stop:'),
    writeln('  next_departures('),
    writeln('      stop_id(\'CENTRAL_STATION\'),'),
    writeln('      5,  % count'),
    writeln('      time(14, 30, 0),  % after this time'),
    writeln('      Departures'),
    writeln('  ).'),
    writeln('').

/**
 * demo_location_based
 * Demonstrates location-based queries
 */
demo_location_based :-
    writeln('=== DEMO: Location-Based Routing ==='),
    writeln(''),

    writeln('Route from coordinates to a stop:'),
    writeln('  find_route('),
    writeln('      location(42.3601, -71.0589),  % Current location'),
    writeln('      stop_id(\'AIRPORT\'),'),
    writeln('      now,'),
    writeln('      Routes,'),
    writeln('      []'),
    writeln('  ).'),
    writeln(''),

    writeln('System will automatically:'),
    writeln('  1. Find nearest stop to your location'),
    writeln('  2. Calculate walking time to that stop'),
    writeln('  3. Plan transit route from there'),
    writeln('').

/**
 * demo_constraint_optimization
 * Demonstrates CLP-based optimization
 */
demo_constraint_optimization :-
    writeln('=== DEMO: Constraint-Based Optimization ==='),
    writeln(''),

    writeln('Optimize with custom weights:'),
    writeln('  optimize_route('),
    writeln('      stop_id(\'A\'),'),
    writeln('      stop_id(\'B\'),'),
    writeln('      ['),
    writeln('          departure_time(time(9, 0, 0)),'),
    writeln('          weights(5, 3, 2),  % time, transfers, walking'),
    writeln('          constraints([max_time(60), max_transfers(2)])'),
    writeln('      ],'),
    writeln('      OptimalRoute'),
    writeln('  ).'),
    writeln('').

/**
 * run_all_demos
 * Runs all demonstration examples
 */
run_all_demos :-
    demo_basic_routing,
    demo_advanced_routing,
    demo_accessibility,
    demo_transfers,
    demo_isochrone,
    demo_multi_destination,
    demo_next_departures,
    demo_location_based,
    demo_constraint_optimization,

    writeln('=== All Demos Complete ==='),
    writeln(''),
    writeln('To run actual queries:'),
    writeln('  1. Load GTFS data: load_gtfs(\'data/gtfs\').'),
    writeln('  2. Replace example stop IDs with real ones from your data'),
    writeln('  3. Run queries from the examples above'),
    writeln('').

/**
 * show_data_stats
 * Displays loaded GTFS data statistics
 */
show_data_stats :-
    writeln('=== GTFS Data Statistics ==='),
    writeln(''),

    aggregate_all(count, stop(_, _, _, _, _, _, _), StopCount),
    aggregate_all(count, route(_, _, _, _, _, _), RouteCount),
    aggregate_all(count, trip(_, _, _, _, _), TripCount),

    format('Stops:  ~w~n', [StopCount]),
    format('Routes: ~w~n', [RouteCount]),
    format('Trips:  ~w~n', [TripCount]),
    writeln('').

/**
 * sample_route_display(+Route)
 * Pretty-prints a route solution
 */
sample_route_display(route_solution(TotalTime, NumTransfers, Segments)) :-
    format('~n=== Route Found ===~n', []),
    format('Total Time: ~w minutes~n', [TotalTime]),
    format('Transfers: ~w~n~n', [NumTransfers]),

    writeln('Journey:'),
    display_segments(Segments, 1).

display_segments([], _).
display_segments([segment(TripId, RouteId, FromStop, ToStop, DepTime, ArrTime, StopCount)|Rest], Num) :-
    route(RouteId, ShortName, LongName, _, _, _),
    stop(FromStop, FromName, _, _, _, _, _),
    stop(ToStop, ToName, _, _, _, _, _),
    format_time_display(DepTime, DepDisplay),
    format_time_display(ArrTime, ArrDisplay),

    format('~n~w. Take ~w (~w)~n', [Num, ShortName, LongName]),
    format('   Board at: ~w (~w)~n', [FromName, DepDisplay]),
    format('   Alight at: ~w (~w)~n', [ToName, ArrDisplay]),
    format('   Stops: ~w~n', [StopCount]),

    Num1 is Num + 1,
    display_segments(Rest, Num1).
