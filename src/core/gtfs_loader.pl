/**
 * GTFS Data Loader
 *
 * Loads GTFS data from CSV files into Prolog dynamic predicates
 * Supports all core GTFS files: stops, routes, trips, stop_times, calendar, transfers
 */

:- module(gtfs_loader, [
    load_gtfs_data/1,
    load_gtfs_file/2,
    load_stops/1,
    load_routes/1,
    load_trips/1,
    load_stop_times/1,
    load_calendar/1,
    load_transfers/1,
    load_agency/1,
    gtfs_stats/0
]).

:- use_module(library(csv)).
:- use_module(library(apply)).
:- use_module('../core/gtfs_schema').
:- use_module('../core/time_utils').

/**
 * load_gtfs_data(+DataDir)
 * Loads all GTFS files from a directory
 */
load_gtfs_data(DataDir) :-
    format('~nLoading GTFS data from: ~w~n', [DataDir]),
    clear_all_gtfs_data,

    % Load files in dependency order
    format('~n[1/7] Loading agencies...~n', []),
    atomic_list_concat([DataDir, '/agency.txt'], AgencyFile),
    catch(load_agency(AgencyFile), E,
          (format('  Warning: Could not load agency.txt: ~w~n', [E]), true)),

    format('[2/7] Loading stops...~n', []),
    atomic_list_concat([DataDir, '/stops.txt'], StopsFile),
    load_stops(StopsFile),

    format('[3/7] Loading routes...~n', []),
    atomic_list_concat([DataDir, '/routes.txt'], RoutesFile),
    load_routes(RoutesFile),

    format('[4/7] Loading trips...~n', []),
    atomic_list_concat([DataDir, '/trips.txt'], TripsFile),
    load_trips(TripsFile),

    format('[5/7] Loading stop times...~n', []),
    atomic_list_concat([DataDir, '/stop_times.txt'], StopTimesFile),
    load_stop_times(StopTimesFile),

    format('[6/7] Loading calendar...~n', []),
    atomic_list_concat([DataDir, '/calendar.txt'], CalendarFile),
    catch(load_calendar(CalendarFile), E2,
          (format('  Warning: Could not load calendar.txt: ~w~n', [E2]), true)),

    format('[7/7] Loading transfers...~n', []),
    atomic_list_concat([DataDir, '/transfers.txt'], TransfersFile),
    catch(load_transfers(TransfersFile), E3,
          (format('  Warning: Could not load transfers.txt: ~w~n', [E3]), true)),

    format('~n✓ GTFS data loaded successfully!~n', []),
    gtfs_stats.

/**
 * load_stops(+File)
 * Loads stops.txt
 * Format: stop_id, stop_name, stop_lat, stop_lon, wheelchair_boarding, location_type, parent_station
 */
load_stops(File) :-
    csv_read_file(File, Rows, [functor(stop_row), arity(9)]),
    maplist(process_stop_row, Rows),
    aggregate_all(count, stop(_, _, _, _, _, _, _), Count),
    format('  Loaded ~w stops~n', [Count]).

process_stop_row(stop_row(_StopId, _StopCode, StopName, _StopDesc, Lat, Lon, _ZoneId, _StopUrl, WheelchairStr)) :-
    stop_row(_StopId, _StopCode, StopName, _StopDesc, Lat, Lon, _ZoneId, _StopUrl, WheelchairStr) =
        stop_row(StopId, _, Name, _, LatAtom, LonAtom, _, _, WheelchairAtom),

    % Parse coordinates
    (atom_number(LatAtom, LatNum) -> true ; LatNum = 0.0),
    (atom_number(LonAtom, LonNum) -> true ; LonNum = 0.0),

    % Parse wheelchair boarding (default to 0 if empty)
    (WheelchairAtom = '' -> Wheelchair = 0
    ; atom_number(WheelchairAtom, Wheelchair) -> true
    ; Wheelchair = 0),

    % For simplicity, set location_type and parent_station to defaults
    LocationType = 0,
    ParentStation = '',

    assertz(stop(StopId, Name, LatNum, LonNum, Wheelchair, LocationType, ParentStation)).

/**
 * load_routes(+File)
 * Loads routes.txt
 * Format: route_id, route_short_name, route_long_name, route_type, route_color, agency_id
 */
load_routes(File) :-
    csv_read_file(File, Rows, [functor(route_row), arity(8)]),
    maplist(process_route_row, Rows),
    aggregate_all(count, route(_, _, _, _, _, _), Count),
    format('  Loaded ~w routes~n', [Count]).

process_route_row(route_row(RouteId, AgencyId, ShortName, LongName, _Desc, TypeAtom, _Url, ColorAtom)) :-
    (atom_number(TypeAtom, Type) -> true ; Type = 3),  % Default to bus
    (ColorAtom = '' -> Color = 'FFFFFF' ; Color = ColorAtom),
    (AgencyId = '' -> Agency = 'default' ; Agency = AgencyId),
    assertz(route(RouteId, ShortName, LongName, Type, Color, Agency)).

/**
 * load_trips(+File)
 * Loads trips.txt
 * Format: trip_id, route_id, service_id, trip_headsign, direction_id
 */
load_trips(File) :-
    csv_read_file(File, Rows, [functor(trip_row), arity(7)]),
    maplist(process_trip_row, Rows),
    aggregate_all(count, trip(_, _, _, _, _), Count),
    format('  Loaded ~w trips~n', [Count]).

process_trip_row(trip_row(RouteId, ServiceId, TripId, Headsign, _ShortName, DirectionAtom, _BlockId)) :-
    (atom_number(DirectionAtom, Direction) -> true ; Direction = 0),
    assertz(trip(TripId, RouteId, ServiceId, Headsign, Direction)).

/**
 * load_stop_times(+File)
 * Loads stop_times.txt
 * Format: trip_id, stop_id, arrival_time, departure_time, stop_sequence, pickup_type
 */
load_stop_times(File) :-
    csv_read_file(File, Rows, [functor(stop_time_row), arity(9)]),
    maplist(process_stop_time_row, Rows),
    aggregate_all(count, stop_time(_, _, _, _, _, _), Count),
    format('  Loaded ~w stop times~n', [Count]).

process_stop_time_row(stop_time_row(TripId, ArrivalStr, DepartureStr, StopId, SeqAtom, _Headsign, PickupAtom, _DropOff, _ShapeDist)) :-
    parse_gtfs_time(ArrivalStr, ArrivalTime),
    parse_gtfs_time(DepartureStr, DepartureTime),
    (atom_number(SeqAtom, Sequence) -> true ; Sequence = 0),
    (atom_number(PickupAtom, Pickup) -> true ; Pickup = 0),
    assertz(stop_time(TripId, StopId, ArrivalTime, DepartureTime, Sequence, Pickup)).

/**
 * load_calendar(+File)
 * Loads calendar.txt
 * Format: service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday
 */
load_calendar(File) :-
    csv_read_file(File, Rows, [functor(calendar_row), arity(10)]),
    maplist(process_calendar_row, Rows),
    aggregate_all(count, calendar(_, _, _, _, _, _, _, _), Count),
    format('  Loaded ~w calendar entries~n', [Count]).

process_calendar_row(calendar_row(ServiceId, MonAtom, TueAtom, WedAtom, ThuAtom, FriAtom, SatAtom, SunAtom, _Start, _End)) :-
    atom_number(MonAtom, Mon),
    atom_number(TueAtom, Tue),
    atom_number(WedAtom, Wed),
    atom_number(ThuAtom, Thu),
    atom_number(FriAtom, Fri),
    atom_number(SatAtom, Sat),
    atom_number(SunAtom, Sun),
    assertz(calendar(ServiceId, Mon, Tue, Wed, Thu, Fri, Sat, Sun)).

/**
 * load_transfers(+File)
 * Loads transfers.txt
 * Format: from_stop_id, to_stop_id, transfer_type, min_transfer_time
 */
load_transfers(File) :-
    csv_read_file(File, Rows, [functor(transfer_row), arity(4)]),
    maplist(process_transfer_row, Rows),
    aggregate_all(count, transfer(_, _, _, _), Count),
    format('  Loaded ~w transfers~n', [Count]).

process_transfer_row(transfer_row(FromStop, ToStop, TypeAtom, MinTimeAtom)) :-
    (atom_number(TypeAtom, Type) -> true ; Type = 0),
    (atom_number(MinTimeAtom, MinTime) -> true ; MinTime = 0),
    assertz(transfer(FromStop, ToStop, Type, MinTime)).

/**
 * load_agency(+File)
 * Loads agency.txt
 */
load_agency(File) :-
    csv_read_file(File, Rows, [functor(agency_row), arity(6)]),
    maplist(process_agency_row, Rows),
    aggregate_all(count, agency(_, _, _, _), Count),
    format('  Loaded ~w agencies~n', [Count]).

process_agency_row(agency_row(AgencyId, Name, Url, Timezone, _Lang, _Phone)) :-
    assertz(agency(AgencyId, Name, Url, Timezone)).

/**
 * gtfs_stats
 * Prints statistics about loaded GTFS data
 */
gtfs_stats :-
    aggregate_all(count, stop(_, _, _, _, _, _, _), StopCount),
    aggregate_all(count, route(_, _, _, _, _, _), RouteCount),
    aggregate_all(count, trip(_, _, _, _, _), TripCount),
    aggregate_all(count, stop_time(_, _, _, _, _, _), StopTimeCount),
    aggregate_all(count, calendar(_, _, _, _, _, _, _, _), CalendarCount),
    aggregate_all(count, transfer(_, _, _, _), TransferCount),

    format('~n=== GTFS Data Statistics ===~n', []),
    format('Stops:       ~w~n', [StopCount]),
    format('Routes:      ~w~n', [RouteCount]),
    format('Trips:       ~w~n', [TripCount]),
    format('Stop Times:  ~w~n', [StopTimeCount]),
    format('Calendars:   ~w~n', [CalendarCount]),
    format('Transfers:   ~w~n', [TransferCount]),
    format('============================~n~n', []).

/**
 * load_gtfs_file(+Type, +File)
 * Generic loader that dispatches to specific file loaders
 */
load_gtfs_file(stops, File) :- load_stops(File).
load_gtfs_file(routes, File) :- load_routes(File).
load_gtfs_file(trips, File) :- load_trips(File).
load_gtfs_file(stop_times, File) :- load_stop_times(File).
load_gtfs_file(calendar, File) :- load_calendar(File).
load_gtfs_file(transfers, File) :- load_transfers(File).
load_gtfs_file(agency, File) :- load_agency(File).
