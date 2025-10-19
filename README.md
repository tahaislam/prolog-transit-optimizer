# Prolog Transit Route Optimizer

A multi-constraint transit route optimizer built in Prolog that combines GTFS transit data with multi-modal routing capabilities. The system uses constraint logic programming (CLP) to find optimal routes considering schedules, transfers, accessibility, walking distance, and travel time.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Features

- **Multi-Objective Optimization**: Balance travel time, number of transfers, and walking distance
- **Accessibility Support**: Filter routes for wheelchair accessibility
- **Transfer Management**: Intelligent transfer handling with walking time calculations
- **Schedule-Aware Routing**: Uses actual transit schedules from GTFS data
- **Multi-Modal Integration**: Combine walking, cycling, and transit
- **Isochrone Generation**: Analyze transit accessibility and coverage
- **Flexible Querying**: Find fastest route, minimize transfers, or custom optimization

## Quick Start

### Prerequisites

- SWI-Prolog 8.0 or higher ([download here](https://www.swi-prolog.org/Download.html))
- GTFS data from your local transit agency

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/prolog-transit-optimizer.git
cd prolog-transit-optimizer

# Download GTFS data (example: Portland TriMet)
cd data/gtfs
wget https://developer.trimet.org/schedule/gtfs.zip
unzip gtfs.zip
cd ../..
```

### Basic Usage

Start SWI-Prolog and load the system:

```prolog
% Start Prolog
swipl

% Load the API
?- [src/query/api].

% Load GTFS data
?- load_gtfs('data/gtfs').
```

Find a route between two stops:

```prolog
% Find route with up to 2 transfers
?- find_route(
    stop_id('12345'),
    stop_id('67890'),
    time(9, 0, 0),
    Routes,
    [max_transfers(2)]
).
```

For detailed examples, see [QUICKSTART.md](QUICKSTART.md).

## Example Queries

### Fastest Route

```prolog
?- fastest_route(
    stop_id('downtown_station'),
    stop_id('airport'),
    time(10, 30, 0),
    Route
).
```

### Wheelchair Accessible Route

```prolog
?- accessible_route(
    stop_id('central_library'),
    stop_id('university'),
    time(14, 0, 0),
    Route
).
```

### Next Departures

```prolog
% Get next 5 departures from a stop
?- next_departures(
    stop_id('main_street'),
    5,
    time(8, 45, 0),
    Departures
).
```

### Isochrone (Reachability Analysis)

```prolog
% Find all stops reachable within 30 minutes
?- isochrone(
    stop_id('city_center'),
    time(9, 0, 0),
    30,
    ReachableStops
).
```

### Location-Based Routing

```prolog
% Route from GPS coordinates
?- find_route(
    location(45.5231, -122.6765),
    stop_id('convention_center'),
    now,
    Routes,
    []
).
```

## Architecture

```
src/
├── core/              # GTFS data loading and time utilities
│   ├── gtfs_schema.pl
│   ├── gtfs_loader.pl
│   └── time_utils.pl
├── routing/           # Route planning and optimization
│   ├── route_planner.pl
│   ├── constraints.pl
│   └── transfers.pl
├── multimodal/        # Walking and OSM integration
│   ├── walking.pl
│   └── osm_integration.pl
└── query/             # High-level API
    └── api.pl
```

### Core Modules

- **gtfs_loader**: Parses GTFS CSV files and loads data into Prolog predicates
- **route_planner**: Implements depth-first search routing with transfer limits
- **constraints**: CLP(FD)-based multi-objective optimization
- **transfers**: Manages transfers with walking time calculations using haversine formula
- **api**: User-friendly high-level query interface

## Query API Reference

### Main Predicates

#### `load_gtfs(+DataDirectory)`
Loads all GTFS data from a directory.

#### `find_route(+From, +To, +When, -Routes, +Options)`
Finds routes between two locations.

**Parameters:**
- `From/To`: `stop_id(Id)`, `stop_name(Name)`, or `location(Lat, Lon)`
- `When`: `time(H, M, S)` or `now`
- `Routes`: List of route solutions
- `Options`: List of options (see below)

**Options:**
- `max_transfers(N)`: Maximum number of transfers (default: 3)
- `wheelchair_accessible(true/false)`: Require accessible routes
- `optimize(time/transfers/balanced)`: Optimization preference
- `date(YYYYMMDD)`: Service date for calendar checking

#### `fastest_route(+From, +To, +When, -Route)`
Finds the route with minimum travel time.

#### `least_transfers(+From, +To, +When, -Route)`
Finds the route with fewest transfers.

#### `accessible_route(+From, +To, +When, -Route)`
Finds a wheelchair accessible route.

#### `next_departures(+StopId, +Count, +AfterTime, -Departures)`
Gets the next N departures from a stop.

#### `isochrone(+FromStop, +DepartureTime, +MaxMinutes, -ReachableStops)`
Finds all stops reachable within a time limit.

## GTFS Data Sources

The system works with any transit agency that provides GTFS data. Recommended starting datasets:

### Mid-Sized Cities (Recommended for Learning)

- **Portland, OR (TriMet)**: https://developer.trimet.org/GTFS.shtml
- **Ann Arbor, MI (TheRide)**: https://www.theride.org/about/plans-reports/data-resources
- **Madison, WI (Metro Transit)**: https://www.cityofmadison.com/metro/business/information-for-developers

### Finding GTFS Feeds

Browse 1,000+ transit agencies worldwide at:
- **MobilityData Catalog**: https://database.mobilitydata.org/
- **TransitFeeds**: https://transitfeeds.com/

## Advanced Features

### Constraint-Based Optimization

Use custom weights for multi-objective optimization:

```prolog
?- optimize_route(
    stop_id('A'),
    stop_id('B'),
    [
        departure_time(time(9, 0, 0)),
        weights(5, 3, 2),  % time, transfers, walking
        constraints([max_time(60), max_transfers(2)])
    ],
    OptimalRoute
).
```

### Multi-Destination Trip Planning

Plan trips with multiple stops:

```prolog
?- trip_planner(
    [
        location(42.3601, -71.0589),
        visit(stop_id('museum')),
        visit(stop_id('restaurant')),
        return(location(42.3601, -71.0589))
    ],
    Plan,
    [departure_time(time(9, 0, 0))]
).
```

### Transfer Analysis

```prolog
% Find nearby stops for transfers
?- nearby_stops(stop_id('central_station'), 400, NearbyStops).

% Calculate walking time between stops
?- walking_transfer(stop_id('stop_a'), stop_id('stop_b'), Minutes).

% Validate transfer feasibility
?- valid_transfer(
    stop_id('from_stop'),
    stop_id('to_stop'),
    time(9, 15, 0),  % arrival time
    time(9, 25, 0),  % next departure
    [min_transfer_time(5)]
).
```

## Data Structures

### Route Solution

```prolog
route_solution(TotalTime, NumTransfers, Segments)
```

Where each segment is:
```prolog
segment(TripId, RouteId, FromStop, ToStop, DepartureTime, ArrivalTime, StopCount)
```

### Stop Information

```prolog
stop(StopId, Name, Latitude, Longitude, WheelchairBoarding, LocationType, ParentStation)
```

### Route Information

```prolog
route(RouteId, ShortName, LongName, Type, Color, AgencyId)
```

## Testing

Run the unit tests:

```prolog
?- [tests/test_routing].
?- run_tests.
```

## Performance Considerations

- **Indexing**: Key predicates are hash-indexed for O(1) lookup
- **Memory**: All GTFS data is loaded into memory for fast access
- **Large Datasets**: For very large transit systems (>10,000 stops), consider:
  - Limiting search depth
  - Using external database (PostgreSQL)
  - Implementing tabling for memoization

## Future Enhancements

Potential areas for expansion:

- **Real-Time Data**: Integrate GTFS-Realtime for live vehicle positions
- **OpenStreetMap**: Add detailed walking/cycling routes (see [src/multimodal/osm_integration.pl](src/multimodal/osm_integration.pl))
- **Web Interface**: Build REST API using SWI-Prolog HTTP server
- **Visualization**: Export routes to GeoJSON for mapping
- **Fare Calculation**: Add fare zones and pricing
- **A* Search**: Replace DFS with A* for better performance

## Contributing

Contributions are welcome! Areas that need work:

1. OSM integration for accurate walking routes
2. Real-time data support (GTFS-Realtime)
3. Performance optimization for large datasets
4. Additional optimization algorithms (A*, Connection Scan)
5. Web interface development

## Technical Background

### Why Prolog?

This project leverages Prolog's unique strengths:

- **Declarative Queries**: Express routing constraints naturally
- **Backtracking**: Explore multiple route alternatives automatically
- **Unification**: Pattern matching for schedule and stop queries
- **CLP(FD)**: Constraint logic programming for optimization
- **Graph Processing**: Natural fit for transit network traversal

### Algorithms Used

- **Route Finding**: Depth-first search with transfer limits
- **Optimization**: CLP(FD) constraint satisfaction
- **Transfer Calculation**: Haversine formula for geographic distance
- **Time Handling**: GTFS time format with past-midnight support

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- GTFS specification: https://gtfs.org/
- SWI-Prolog: https://www.swi-prolog.org/
- Transit agencies providing open data

## Support

For questions, issues, or feature requests:
- Open an issue on GitHub
- Check [QUICKSTART.md](QUICKSTART.md) for common problems
- Review [examples/example_queries.pl](examples/example_queries.pl) for usage patterns

---

Built with SWI-Prolog • GTFS • CLP(FD)
