# Quick Start Guide

Get started with the Prolog Transit Optimizer in 5 minutes!

## Prerequisites

Install SWI-Prolog (version 8.0 or higher recommended):

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install swi-prolog

# macOS
brew install swi-prolog

# Verify installation
swipl --version
```

## Step 1: Download GTFS Data

I recommend starting with **Portland TriMet** for its clean, well-documented data:

```bash
cd data/gtfs

# Download Portland TriMet GTFS
wget https://developer.trimet.org/schedule/gtfs.zip

# Extract
unzip gtfs.zip

# You should now have: stops.txt, routes.txt, trips.txt, stop_times.txt, calendar.txt, etc.
```

**Alternative datasets:**
- Ann Arbor: https://www.theride.org/about/plans-reports/data-resources
- Madison: https://www.cityofmadison.com/metro/business/information-for-developers
- Browse 1000+ feeds: https://database.mobilitydata.org/

## Step 2: Load the System

```bash
cd /home/islam/Workspace/prolog-transit-optimizer
swipl
```

In the Prolog console:

```prolog
% Load the main API
?- [src/query/api].

% Load GTFS data
?- load_gtfs('data/gtfs').

% This will take 10-30 seconds depending on dataset size
% You should see output showing progress and statistics
```

## Step 3: Explore Your Data

```prolog
% List some stops
?- stop(StopId, Name, Lat, Lon, _, _, _), write(Name), nl, fail.

% List routes
?- route(RouteId, ShortName, LongName, Type, _, _),
   format('~w: ~w~n', [ShortName, LongName]), fail.

% Count stops
?- aggregate_all(count, stop(_, _, _, _, _, _, _), Count),
   format('Total stops: ~w~n', [Count]).
```

## Step 4: Run Example Queries

### Find a Route Between Two Stops

```prolog
% First, find stop IDs from your data
?- stop(StopId, Name, _, _, _, _, _),
   sub_atom(Name, _, _, _, 'Downtown'),
   format('~w: ~w~n', [StopId, Name]).

% Then plan a route (replace with actual stop IDs)
?- find_route(
    stop_id('STOP_ID_1'),
    stop_id('STOP_ID_2'),
    time(9, 0, 0),
    Routes,
    [max_transfers(2)]
).
```

### Next Departures (Departure Board)

```prolog
% Get next 5 departures from a stop at 2:30 PM
?- next_departures(
    'YOUR_STOP_ID',
    5,
    time(14, 30, 0),
    Departures
).
```

### Accessible Route

```prolog
% Find wheelchair accessible route
?- accessible_route(
    stop_id('STOP_A'),
    stop_id('STOP_B'),
    time(10, 0, 0),
    Route
).
```

### Isochrone (Reachability)

```prolog
% Find all stops reachable within 30 minutes
?- isochrone(
    'DOWNTOWN_STOP',
    time(9, 0, 0),
    30,
    ReachableStops
).
```

## Step 5: Explore Advanced Features

Load example queries:

```prolog
?- [examples/example_queries].
?- run_all_demos.
```

This will show you examples of:
- Multi-objective optimization
- Transfer analysis
- Location-based routing
- Multi-destination trip planning
- Constraint-based route finding

## Troubleshooting

### "ERROR: Undefined procedure" when loading

Make sure you're in the project root directory:
```bash
cd /home/islam/Workspace/prolog-transit-optimizer
swipl
```

### CSV parsing errors

Your GTFS files might have extra columns. The loader is designed to handle standard GTFS format. Check that your files match the [GTFS specification](https://gtfs.org/schedule/reference/).

### No routes found

1. Check that stop IDs are correct (they're case-sensitive)
2. Verify the calendar.txt has services active
3. Try a later departure time (some routes don't run early morning)

```prolog
% Debug: Check if stops exist
?- stop('YOUR_STOP_ID', Name, _, _, _, _, _).

% Debug: Check for trips
?- trip(TripId, _, _, _, _), !.

% Debug: Check stop times
?- stop_time(_, 'YOUR_STOP_ID', _, DepTime, _, _).
```

## Next Steps

1. **Customize optimization weights**: Edit [src/routing/constraints.pl](src/routing/constraints.pl)
2. **Add OSM integration**: Follow guide in [src/multimodal/osm_integration.pl](src/multimodal/osm_integration.pl)
3. **Build a web interface**: Use SWI-Prolog's HTTP server library
4. **Add real-time data**: Integrate GTFS-Realtime feeds
5. **Visualize routes**: Export to GeoJSON for mapping

## Learning Resources

- **Prolog Tutorial**: https://www.swi-prolog.org/pldoc/man?section=quickstart
- **CLP(FD) Guide**: https://www.swi-prolog.org/man/clpfd.html
- **GTFS Reference**: https://gtfs.org/schedule/reference/
- **Transit Routing Algorithms**: [Connection Scan Algorithm](https://arxiv.org/abs/1703.05997)

## Example Session

```prolog
% Complete example session
swipl

?- [src/query/api].
true.

?- load_gtfs('data/gtfs').
Loading GTFS data from: data/gtfs
[1/7] Loading agencies...
  Loaded 1 agencies
[2/7] Loading stops...
  Loaded 1043 stops
[3/7] Loading routes...
  Loaded 17 routes
...
✓ GTFS data loaded successfully!

?- stop(StopId, Name, _, _, _, _, _),
   sub_atom(Name, _, _, _, 'Library'), !,
   format('Found: ~w (~w)~n', [Name, StopId]).
Found: Central Library (stop_123)
StopId = stop_123,
Name = 'Central Library'.

?- find_route(stop_id('stop_123'), stop_id('stop_456'),
              time(9, 0, 0), [Route|_], []).
Route = route_solution(23, 0, [segment(...)]).

% Success! You've found a route with 0 transfers taking 23 minutes
```

## Getting Help

- Check [README.md](README.md) for architecture overview
- Review [examples/example_queries.pl](examples/example_queries.pl) for query patterns
- Read source code comments for detailed documentation
- File issues on GitHub for bugs or questions

Happy routing! 🚌🚇🚶
