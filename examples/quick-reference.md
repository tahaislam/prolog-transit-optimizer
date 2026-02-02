# Quick Commands Reference

Common commands for working with the Prolog Transit Optimizer.

---

## 🚀 Preparation Steps

### 1. Start SWI-Prolog with Sufficient Memory

For MMT dataset (smaller):
```bash
swipl --stack_limit=2g
```

For TTC/Portland TriMet (larger datasets):
```bash
swipl --stack_limit=4g
```

For very large datasets or complex queries:
```bash
swipl --stack_limit=8g
```

### 2. Load the System

```prolog
% Load main API
?- consult('src/query/api.pl').

% Load search helpers (for case-insensitive searches)
?- consult('src/query/search_helpers.pl').

% Load reasoning/tracing (for explainability)
?- consult('src/explain/reasoning.pl').
```

### 3. Load GTFS Data

```prolog
% MMT dataset (small - fast to load)
?- load_gtfs('data/gtfs/mmt').

% Toronto TTC (large - ~2-3 minutes)
?- load_gtfs('data/gtfs/toronto-ttc').

% Portland TriMet (very large - ~5 minutes)
?- load_gtfs('data/gtfs/Portland-TriMet').
```

---

## 🔍 Search Commands

### Find Stops by Name (Case-Insensitive)

**Interactive:**
```prolog
?- list_stops_containing('station').
?- list_stops_containing('union').
?- list_stops_containing('downtown').
```

**One-line command (from terminal):**
```bash
swipl --stack_limit=4g -g "
    consult('src/query/api.pl'),
    consult('src/query/search_helpers.pl'),
    load_gtfs('data/gtfs/toronto-ttc'),
    list_stops_containing('station'),
    halt.
"
```

**Get results as a list:**
```prolog
?- findall(
    stop(StopId, Name),
    find_stop_case_insensitive('station', stop(StopId, Name, _, _, _, _, _)),
    Stops
).
```

**Count matching stops:**
```prolog
?- findall(1,
    find_stop_case_insensitive('station', _),
    L),
   length(L, Count),
   format('Found ~w stops~n', [Count]).
```

### Find Routes by Name

```prolog
?- list_routes_containing('subway').
?- list_routes_containing('express').
?- list_routes_containing('bloor').
```

### Find Nearby Stops

Find stops within 500 meters of a location:
```prolog
?- nearby_stops(43.6452, -79.3806, 500, Stops).
```

---

## 🚌 Routing Commands

### Find a Route Between Two Stops

**Basic routing:**
```prolog
?- find_route('StopId1', 'StopId2', time(9,0,0), Routes, []).
```

**With constraints:**
```prolog
% Max 1 transfer
?- find_route('StopId1', 'StopId2', time(9,0,0), Routes, [max_transfers(1)]).

% Wheelchair accessible only
?- find_route('StopId1', 'StopId2', time(9,0,0), Routes, [wheelchair_accessible(true)]).

% Multiple constraints
?- find_route('StopId1', 'StopId2', time(9,0,0), Routes,
              [max_transfers(2), wheelchair_accessible(true)]).
```

### Find Route with Reasoning Trace

```prolog
?- find_route_with_trace('StopId1', 'StopId2', time(9,0,0), [], Route, Trace).

% Examine the trace
?- length(Trace, Len).
?- member(Decision, Trace), writeln(Decision).
```

---

## 📊 Data Exploration Commands

### View Dataset Statistics

The statistics are shown automatically when loading, or you can query:

```prolog
% Count stops
?- findall(1, stop(_, _, _, _, _, _, _), L), length(L, Count).

% Count routes
?- findall(1, route(_, _, _, _, _, _), L), length(L, Count).

% Count trips
?- findall(1, trip(_, _, _, _, _), L), length(L, Count).
```

### Explore Stop Details

```prolog
% Get all information about a specific stop
?- stop('9227', Name, Lat, Lon, Wheelchair, LocType, Parent).

% Find all stops at a location (parent station)
?- stop(StopId, Name, _, _, _, _, '9227').
```

### Explore Route Details

```prolog
% Get route information
?- route(RouteId, AgencyId, ShortName, LongName, Type, Color).

% Find all trips on a route
?- trip(TripId, 'RouteId', _, _, _).
```

---

## 🧪 Testing Commands

### Test Reasoning Trace Collection

```bash
swipl --stack_limit=2g test_tracing_final.pl
```

### Test Search Helpers

```bash
swipl --stack_limit=2g test_trace_simple.pl
```

### Manual Testing in REPL

```prolog
% Test trace collection
?- init_trace, enable_tracing.
?- add_decision(test, context(foo), 'Testing').
?- get_trace(T), writeln(T).
?- clear_trace.
```

---

## 💡 Common Use Cases

### Use Case 1: Find All Subway Stations in Toronto

```bash
swipl --stack_limit=4g -g "
    consult('src/query/api.pl'),
    consult('src/query/search_helpers.pl'),
    load_gtfs('data/gtfs/toronto-ttc'),
    list_stops_containing('station'),
    halt.
"
```

### Use Case 2: Find Wheelchair Accessible Stops

```prolog
?- stop(StopId, Name, Lat, Lon, 1, _, _),
   format('♿ ~w: ~w~n', [StopId, Name]),
   fail.
```

### Use Case 3: Find All Routes Serving a Stop

```prolog
?- stop_time(TripId, 'StopId', _, _, _, _),
   trip(TripId, RouteId, _, _, _),
   route(RouteId, _, ShortName, LongName, _, _),
   format('~w: ~w~n', [ShortName, LongName]),
   fail.
```

### Use Case 4: Find Next Departures from a Stop

```prolog
?- next_departures('StopId', 5, time(9,0,0), Departures).
```

### Use Case 5: Find Direct Trips Between Two Stops

```prolog
?- find_direct_trips('FromStopId', 'ToStopId', time(9,0,0), Trips).
```

---

## 🔧 Useful Prolog Commands

### Getting Help

```prolog
% List all predicates in a module
?- listing(api:).

% Get help on a specific predicate
?- help(load_gtfs/1).

% Trace execution (debugging)
?- trace.
?- find_route(...).
?- notrace.
```

### Working with Results

```prolog
% Collect all results
?- findall(X, predicate(X), Results).

% Count results
?- findall(1, predicate(_), L), length(L, Count).

% First N results
?- findall(X, predicate(X), All), take_first(5, All, First5).

% Sort results
?- findall(X, predicate(X), Unsorted), sort(Unsorted, Sorted).
```

### File Operations

```prolog
% Reload a file after changes
?- make.

% Or reload specific file
?- consult('src/query/api.pl').

% Exit Prolog
?- halt.
```

---

## 📝 Example Session

Complete example session from start to finish:

```bash
# 1. Start Prolog with enough memory
swipl --stack_limit=4g

# 2. Load the system
?- consult('src/query/api.pl').
?- consult('src/query/search_helpers.pl').

# 3. Load TTC data
?- load_gtfs('data/gtfs/toronto-ttc').

# 4. Find Union Station
?- list_stops_containing('union').

# 5. Get Union Station ID (from output: 9227)
# 6. Find Finch Station
?- list_stops_containing('finch station').

# 7. Get Finch Station ID (from output: 14203)
# 8. Find route between them
?- find_route('9227', '14203', time(9,0,0), Routes, []).

# 9. Exit
?- halt.
```

---

## 🎯 One-Liner Commands (Copy & Paste)

### Search for Stops

```bash
# Find stops with "station" (TTC)
swipl --stack_limit=4g -g "consult('src/query/api.pl'), consult('src/query/search_helpers.pl'), load_gtfs('data/gtfs/toronto-ttc'), list_stops_containing('station'), halt."

# Find stops with "union" (TTC)
swipl --stack_limit=4g -g "consult('src/query/api.pl'), consult('src/query/search_helpers.pl'), load_gtfs('data/gtfs/toronto-ttc'), list_stops_containing('union'), halt."

# Count stops with "station" (TTC)
swipl --stack_limit=4g -g "consult('src/query/api.pl'), consult('src/query/search_helpers.pl'), load_gtfs('data/gtfs/toronto-ttc'), findall(1, find_stop_case_insensitive('station', _), L), length(L, C), format('Found ~w stops~n', [C]), halt."
```

### Load and Test

```bash
# Quick test with MMT
swipl --stack_limit=2g test_tracing_final.pl

# Load TTC and show stats
swipl --stack_limit=4g -g "consult('src/query/api.pl'), load_gtfs('data/gtfs/toronto-ttc'), halt."
```

---

## ⚠️ Troubleshooting

### Out of Stack Error

**Problem:** `ERROR: Stack limit exceeded`

**Solution:** Increase stack limit
```bash
swipl --stack_limit=8g
```

### Module Not Found

**Problem:** `ERROR: ... existence error`

**Solution:** Make sure you're in the project root directory
```bash
cd /path/to/prolog-transit-optimizer
swipl
```

### Data Not Loading

**Problem:** No GTFS files found

**Solution:** Check data directory exists
```bash
ls data/gtfs/toronto-ttc/
# Should show: stops.txt, routes.txt, trips.txt, stop_times.txt, etc.
```

### Slow Queries

**Problem:** Query takes too long

**Solutions:**
- Use more specific queries (smaller result sets)
- Increase stack limit
- Use indexed fields (stop_id, route_id, trip_id)
- Limit results with `findall/3` and `take_first/3`

---

## 📚 Related Documentation

- [TESTING-GUIDE.md](TESTING-GUIDE.md) - How to test the system
- [USING-SEARCH-HELPERS.md](USING-SEARCH-HELPERS.md) - Detailed search examples
- [README.md](README.md) - Full project documentation
- [session_notes/phase1-explainability-implementation.md](session_notes/phase1-explainability-implementation.md) - Implementation details

---

## 🔗 Quick Links

### Load Different Datasets

```prolog
% Small dataset (fast, for testing)
?- load_gtfs('data/gtfs/mmt').

% Toronto TTC (medium-large)
?- load_gtfs('data/gtfs/toronto-ttc').

% Portland TriMet (large)
?- load_gtfs('data/gtfs/Portland-TriMet').
```

### Common Search Patterns

```prolog
% Case-insensitive stop search
?- find_stop_case_insensitive('SearchTerm', StopInfo).

% Case-insensitive route search
?- find_route_case_insensitive('SearchTerm', RouteInfo).

% Geographic search (nearby stops)
?- nearby_stops(Lat, Lon, RadiusMeters, Stops).

% Pretty-print results
?- list_stops_containing('SearchTerm').
?- list_routes_containing('SearchTerm').
```

---

**Pro Tip:** Bookmark this file for quick reference! All commands are tested and ready to use.

**Last Updated:** November 2, 2025
