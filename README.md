# Prolog Transit Route Optimizer

![Language](https://img.shields.io/badge/language-Prolog-blue)
![License](https://img.shields.io/badge/license-MIT-yellow)
![Status](https://img.shields.io/badge/status-research%20prototype-orange)
![Stars](https://img.shields.io/github/stars/tahaislam/prolog-transit-optimizer?style=social)

A research-oriented transit routing prototype built in **Prolog** to explore how far declarative, logic-based approaches can be pushed on real-world, schedule-driven transportation data.

This project uses **GTFS (General Transit Feed Specification)** data to model transit networks and implements time-dependent routing using A* search. Its primary goal is not to compete with production planners, but to **stress-test logic programming on a data-heavy, algorithmically demanding domain** and document where it succeeds — and where it breaks down.

## Project Motivation

Most transit routing systems are built using imperative languages and specialized algorithms designed to handle large, time-indexed datasets efficiently.

This project intentionally takes a different path.

The objective was to understand:

- How naturally GTFS data maps to Prolog facts and rules
- Whether declarative constraints can express real transit routing logic cleanly
- How far generic search algorithms (A*) scale under dense, multimodal transit networks
- Where the **tool–problem mismatch** becomes fundamental rather than optimizable

The result is a working prototype that performs well on **small to medium networks** (e.g., subway-scale systems) and clearly demonstrates the limits of this approach on large surface networks.

## What This Project Is — and Is Not

### ✅ This project **is**:
- A practical learning vehicle for Prolog and logic programming
- A reference GTFS loader implemented declaratively
- A working example of time-dependent A* routing in Prolog
- A documented exploration of performance boundaries
- A useful sandbox for experimentation and refactoring

### ❌ This project **is not**:
- A production-grade transit planner
- A replacement for tools like OpenTripPlanner
- Optimized for very large transit systems with dense surface networks

That distinction is intentional.

## Key Capabilities

- **Declarative GTFS Modeling**
- **Time-Dependent Routing**
- **A\* Search Implementation**
- **Multi-Constraint Queries**
- **Station Bundling & Caching**
- **Reachability Analysis (Isochrones)**

## Performance Characteristics (Important)

This project performs well under **limited branching factors** such as subway or rail-only networks.

Performance degrades rapidly when applied to **large surface transit networks** with thousands of stops and high-frequency services. This behavior is documented and intentional.

## Getting Started

### Prerequisites

- SWI-Prolog 8.0+
- GTFS dataset from a transit agency

### Installation

Clone the repository and place your GTFS feed under `data/gtfs/`.

```bash
git clone https://github.com/tahaislam/prolog-transit-optimizer.git
cd prolog-transit-optimizer
```

### Loading the System

Start SWI-Prolog and load the main module:

```prolog
% Start Prolog
swipl

% Load the API
?- [src/query/api].

% Load GTFS data
?- load_gtfs('data/gtfs').
```

### Example Query

Find a route from stop 'FROM_STOP_ID' to 'TO_STOP_ID' departing at 9:00 AM:

```prolog
?- find_route(
    stop_id('FROM_STOP_ID'),
    stop_id('TO_STOP_ID'),
    time(9, 0, 0),
    Routes,
    [max_transfers(2)]
).
```

## Recommended GTFS Feeds (for Learning)

For best results, start with mid-sized systems:

* Portland, OR (TriMet)
* Ann Arbor, MI (TheRide)
* Madison, WI (Metro Transit)

Large metropolitan bus networks are intentionally challenging for this architecture.

## Architecture Overview

```graphql
src/
├── core/
├── routing/
├── multimodal/
├── query/
└── tests/
```

**Core Modules**

* **gtfs_loader.pl** — Parses GTFS CSV files into Prolog facts
* **route_planner_astar.pl** — Time-dependent A* search
* **constraints.pl** — Multi-objective constraint handling
* **transfers.pl** — Transfer feasibility and walking logic
* **api.pl** — User-facing query interface

## Query API (Selected)

`load_gtfs(+Directory)`

Loads GTFS data into memory.

`find_route(+From, +To, +When, -Routes, +Options)`

Finds candidate routes between two locations.

**Options include:**

* `max_transfers(N)`
* `wheelchair_accessible(true/false)`
* `optimize(time | transfers | balanced)`
* `date(YYYYMMDD)`

`fastest_route/4`

Returns the minimum-time route.

`isochrone/4`

Computes stops reachable within a given time budget.

## Data Model (Simplified)

```Prolog
stop(StopId, Name, Lat, Lon, Wheelchair, LocationType, ParentStation).
route(RouteId, ShortName, LongName, Type, Color, Agency).
stop_time(TripId, StopId, Arrival, Departure, Sequence).
```

## Lessons Encoded in the Codebase

This repository intentionally preserves:

* A clean declarative GTFS representation
* A working but non-scaling routing approach
* Multiple attempted mitigations (caching, bundling, constraints)

Together, they document **where declarative elegance gives way to algorithmic reality**.

## When to Use This Project

Use this repository if you want to:
- Learn Prolog through a real, non-trivial domain
- Understand transit routing mechanics conceptually
- Experiment with declarative search and constraints
- Study why specialized algorithms like RAPTOR exist

Do **not** use it as a production routing backend.

## License

MIT License. See [LICENSE](LICENSE) file for details.

## Acknowledgments

- GTFS Specification — https://gtfs.org/
- SWI-Prolog — https://www.swi-prolog.org/
- Transit agencies providing open GTFS data