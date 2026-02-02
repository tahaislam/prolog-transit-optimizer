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
    catch(load_transfers(TransfersFile), _E3,
          (format('  Note: transfers.txt not provided (optional)~n', []), true)),

    format('~n✓ GTFS data loaded successfully!~n', []),
    gtfs_stats.

/**
 * load_stops(+File)
 * Loads stops.txt
 * Format: stop_id, stop_name, stop_lat, stop_lon, wheelchair_boarding, location_type, parent_station
 */
load_stops(File) :-
    csv_read_file(File, [Header|Rows], [convert(false)]),
    maplist(process_stop_row(Header), Rows),
    aggregate_all(count, stop(_, _, _, _, _, _, _), Count),
    format('  Loaded ~w stops~n', [Count]).

process_stop_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],

    % Find column indices
    nth0(StopIdIdx, ColumnNames, stop_id),
    nth0(NameIdx, ColumnNames, stop_name),
    nth0(LatIdx, ColumnNames, stop_lat),
    nth0(LonIdx, ColumnNames, stop_lon),

    % Get values
    nth0(StopIdIdx, Values, StopId),
    nth0(NameIdx, Values, Name),
    nth0(LatIdx, Values, LatAtom),
    nth0(LonIdx, Values, LonAtom),

    % Optional fields with defaults
    (nth0(WheelchairIdx, ColumnNames, wheelchair_boarding),
     nth0(WheelchairIdx, Values, WheelchairAtom) -> true ; WheelchairAtom = ''),
    (nth0(LocationIdx, ColumnNames, location_type),
     nth0(LocationIdx, Values, LocationAtom) -> true ; LocationAtom = ''),
    (nth0(ParentIdx, ColumnNames, parent_station),
     nth0(ParentIdx, Values, ParentAtom) -> true ; ParentAtom = ''),

    % Parse coordinates (may already be numbers from CSV)
    (number(LatAtom) -> LatNum = LatAtom
    ; atom_number(LatAtom, LatNum) -> true
    ; LatNum = 0.0),
    (number(LonAtom) -> LonNum = LonAtom
    ; atom_number(LonAtom, LonNum) -> true
    ; LonNum = 0.0),

    % Parse wheelchair boarding (default to 0 if empty)
    (number(WheelchairAtom) -> Wheelchair = WheelchairAtom
    ; WheelchairAtom = '' -> Wheelchair = 0
    ; atom_number(WheelchairAtom, Wheelchair) -> true
    ; Wheelchair = 0),

    % Parse location type (default to 0 if empty)
    (number(LocationAtom) -> LocationType = LocationAtom
    ; LocationAtom = '' -> LocationType = 0
    ; atom_number(LocationAtom, LocationType) -> true
    ; LocationType = 0),

    % Parent station (default to empty)
    (ParentAtom = '' -> ParentStation = '' ; ParentStation = ParentAtom),

    assertz(stop(StopId, Name, LatNum, LonNum, Wheelchair, LocationType, ParentStation)).

/**
 * load_routes(+File)
 * Loads routes.txt
 * Format: route_id, route_short_name, route_long_name, route_type, route_color, agency_id
 */
load_routes(File) :-
    csv_read_file(File, [Header|Rows], [convert(false)]),
    maplist(process_route_row(Header), Rows),
    aggregate_all(count, route(_, _, _, _, _, _), Count),
    format('  Loaded ~w routes~n', [Count]).

process_route_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],

    % Find column indices
    nth0(RouteIdIdx, ColumnNames, route_id),
    nth0(ShortNameIdx, ColumnNames, route_short_name),
    nth0(LongNameIdx, ColumnNames, route_long_name),
    nth0(TypeIdx, ColumnNames, route_type),

    % Get values
    nth0(RouteIdIdx, Values, RouteId),
    nth0(ShortNameIdx, Values, ShortName),
    nth0(LongNameIdx, Values, LongName),
    nth0(TypeIdx, Values, TypeVal),

    % Optional fields
    (nth0(ColorIdx, ColumnNames, route_color),
     nth0(ColorIdx, Values, ColorVal),
     ColorVal \= '' -> Color = ColorVal ; Color = 'FFFFFF'),
    (nth0(AgencyIdx, ColumnNames, agency_id),
     nth0(AgencyIdx, Values, AgencyVal),
     AgencyVal \= '' -> Agency = AgencyVal ; Agency = 'default'),

    % Parse type
    (number(TypeVal) -> Type = TypeVal
    ; atom_number(TypeVal, Type) -> true
    ; Type = 3),  % Default to bus

    assertz(route(RouteId, ShortName, LongName, Type, Color, Agency)).

/**
 * load_trips(+File)
 * Loads trips.txt
 * Format: trip_id, route_id, service_id, trip_headsign, direction_id
 */
load_trips(File) :-
    csv_read_file(File, [Header|Rows], [convert(false)]),
    maplist(process_trip_row(Header), Rows),
    aggregate_all(count, trip(_, _, _, _, _), Count),
    format('  Loaded ~w trips~n', [Count]).

process_trip_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],

    % Find column indices
    nth0(RouteIdIdx, ColumnNames, route_id),
    nth0(ServiceIdIdx, ColumnNames, service_id),
    nth0(TripIdIdx, ColumnNames, trip_id),

    % Get values
    nth0(RouteIdIdx, Values, RouteId),
    nth0(ServiceIdIdx, Values, ServiceId),
    nth0(TripIdIdx, Values, TripId),

    % Optional fields
    (nth0(HeadsignIdx, ColumnNames, trip_headsign),
     nth0(HeadsignIdx, Values, HeadsignVal),
     HeadsignVal \= '' -> Headsign = HeadsignVal ; Headsign = ''),
    (nth0(DirectionIdx, ColumnNames, direction_id),
     nth0(DirectionIdx, Values, DirectionVal) -> true ; DirectionVal = ''),

    % Parse direction
    (number(DirectionVal) -> Direction = DirectionVal
    ; atom_number(DirectionVal, Direction) -> true
    ; Direction = 0),

    assertz(trip(TripId, RouteId, ServiceId, Headsign, Direction)).

/**
 * load_stop_times(+File)
 * Loads stop_times.txt
 * Format: trip_id, stop_id, arrival_time, departure_time, stop_sequence, pickup_type
 * Note: Uses standard csv_read_file with convert(false) for efficiency
 */
load_stop_times(File) :-
    format('    Reading stop times (large file, this may take 1-2 minutes)...~n', []),
    csv_read_file(File, [Header|Rows], [convert(false)]),
    length(Rows, RowCount),
    format('    Processing ~w rows...~n', [RowCount]),
    process_stop_times_safe(Header, Rows, 0, RowCount),
    aggregate_all(count, stop_time(_, _, _, _, _, _), Count),
    format('  Loaded ~w stop times~n', [Count]).

process_stop_times_safe(_, [], _, _).
process_stop_times_safe(Header, [Row|Rest], N, Total) :-
    (   catch(process_stop_time_row(Header, Row), Error, (
            format('ERROR at row ~w: ~w~n', [N, Error]),
            format('Row data: ~w~n', [Row]),
            fail
        ))
    ->  true
    ;   format('FAILED at row ~w: ~w~n', [N, Row]),
        fail
    ),
    N1 is N + 1,
    (N1 mod 100000 =:= 0 -> format('      Progress: ~w/~w (~w%)~n', [N1, Total, floor(N1*100/Total)]) ; true),
    process_stop_times_safe(Header, Rest, N1, Total).

process_stop_time_row_vals(ColumnNames, Row) :-
    Row =.. [row|Values],

    % Find column indices
    nth0(TripIdIdx, ColumnNames, trip_id),
    nth0(ArrivalIdx, ColumnNames, arrival_time),
    nth0(DepartureIdx, ColumnNames, departure_time),
    nth0(StopIdIdx, ColumnNames, stop_id),
    nth0(SeqIdx, ColumnNames, stop_sequence),

    % Get values
    nth0(TripIdIdx, Values, TripId),
    nth0(ArrivalIdx, Values, ArrivalStr),
    nth0(DepartureIdx, Values, DepartureStr),
    nth0(StopIdIdx, Values, StopId),
    nth0(SeqIdx, Values, SeqVal),

    % Optional pickup type
    (nth0(PickupIdx, ColumnNames, pickup_type),
     nth0(PickupIdx, Values, PickupVal) -> true ; PickupVal = ''),

    % Parse times
    parse_gtfs_time(ArrivalStr, ArrivalTime),
    parse_gtfs_time(DepartureStr, DepartureTime),

    % Parse sequence
    (number(SeqVal) -> Sequence = SeqVal
    ; atom_number(SeqVal, Sequence) -> true
    ; Sequence = 0),

    % Parse pickup type
    (number(PickupVal) -> Pickup = PickupVal
    ; atom_number(PickupVal, Pickup) -> true
    ; Pickup = 0),

    assertz(stop_time(TripId, StopId, ArrivalTime, DepartureTime, Sequence, Pickup)).

% Keep old version for compatibility with other loaders
process_stop_time_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],
    Row2 =.. [row|Values],
    process_stop_time_row_vals(ColumnNames, Row2).

/**
 * load_calendar(+File)
 * Loads calendar.txt
 * Format: service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday
 */
load_calendar(File) :-
    csv_read_file(File, [Header|Rows], [convert(false)]),
    maplist(process_calendar_row(Header), Rows),
    aggregate_all(count, calendar(_, _, _, _, _, _, _, _), Count),
    format('  Loaded ~w calendar entries~n', [Count]).

process_calendar_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],

    % Find column indices
    nth0(ServiceIdIdx, ColumnNames, service_id),
    nth0(MonIdx, ColumnNames, monday),
    nth0(TueIdx, ColumnNames, tuesday),
    nth0(WedIdx, ColumnNames, wednesday),
    nth0(ThuIdx, ColumnNames, thursday),
    nth0(FriIdx, ColumnNames, friday),
    nth0(SatIdx, ColumnNames, saturday),
    nth0(SunIdx, ColumnNames, sunday),

    % Get values
    nth0(ServiceIdIdx, Values, ServiceId),
    nth0(MonIdx, Values, MonVal),
    nth0(TueIdx, Values, TueVal),
    nth0(WedIdx, Values, WedVal),
    nth0(ThuIdx, Values, ThuVal),
    nth0(FriIdx, Values, FriVal),
    nth0(SatIdx, Values, SatVal),
    nth0(SunIdx, Values, SunVal),

    % Parse day values
    (number(MonVal) -> Mon = MonVal ; atom_number(MonVal, Mon) -> true ; Mon = 0),
    (number(TueVal) -> Tue = TueVal ; atom_number(TueVal, Tue) -> true ; Tue = 0),
    (number(WedVal) -> Wed = WedVal ; atom_number(WedVal, Wed) -> true ; Wed = 0),
    (number(ThuVal) -> Thu = ThuVal ; atom_number(ThuVal, Thu) -> true ; Thu = 0),
    (number(FriVal) -> Fri = FriVal ; atom_number(FriVal, Fri) -> true ; Fri = 0),
    (number(SatVal) -> Sat = SatVal ; atom_number(SatVal, Sat) -> true ; Sat = 0),
    (number(SunVal) -> Sun = SunVal ; atom_number(SunVal, Sun) -> true ; Sun = 0),

    assertz(calendar(ServiceId, Mon, Tue, Wed, Thu, Fri, Sat, Sun)).

% Old process_calendar_row retained temporarily for reference
process_calendar_row_old(calendar_row(ServiceId, MonAtom, TueAtom, WedAtom, ThuAtom, FriAtom, SatAtom, SunAtom, _Start, _End)) :-
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
    csv_read_file(File, [Header|Rows], [convert(false)]),
    maplist(process_agency_row(Header), Rows),
    aggregate_all(count, agency(_, _, _, _), Count),
    format('  Loaded ~w agencies~n', [Count]).

process_agency_row(Header, Row) :-
    Row =.. [_Functor|Values],
    Header =.. [_HeaderFunctor|ColumnNames],

    % Find column indices for required fields
    nth0(NameIdx, ColumnNames, agency_name),
    nth0(UrlIdx, ColumnNames, agency_url),
    nth0(TimezoneIdx, ColumnNames, agency_timezone),

    % Get values
    nth0(NameIdx, Values, Name),
    nth0(UrlIdx, Values, Url),
    nth0(TimezoneIdx, Values, Timezone),

    % Optional agency_id (use name as ID if not present)
    (nth0(IdIdx, ColumnNames, agency_id),
     nth0(IdIdx, Values, AgencyIdVal),
     AgencyIdVal \= '' -> AgencyId = AgencyIdVal ; AgencyId = Name),

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
