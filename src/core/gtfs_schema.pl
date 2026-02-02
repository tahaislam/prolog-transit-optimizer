/**
 * GTFS Schema Definitions
 *
 * Defines the data structures for GTFS entities following the specification:
 * https://gtfs.org/schedule/reference/
 */

:- module(gtfs_schema, [
    % Dynamic predicates for GTFS data
    stop/7,           % stop(StopId, Name, Lat, Lon, WheelchairBoarding, LocationType, ParentStation)
    route/6,          % route(RouteId, ShortName, LongName, Type, Color, Agency)
    trip/5,           % trip(TripId, RouteId, ServiceId, Headsign, DirectionId)
    stop_time/6,      % stop_time(TripId, StopId, ArrivalTime, DepartureTime, StopSequence, PickupType)
    calendar/8,       % calendar(ServiceId, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
    calendar_date/3,  % calendar_date(ServiceId, Date, ExceptionType)
    transfer/4,       % transfer(FromStopId, ToStopId, TransferType, MinTransferTime)
    agency/4,         % agency(AgencyId, Name, URL, Timezone)

    % Helper predicates
    route_type_name/2,
    wheelchair_boarding/2,
    transfer_type/2,
    clear_all_gtfs_data/0
]).

% Dynamic predicates to store GTFS data in memory
:- dynamic stop/7.
:- dynamic route/6.
:- dynamic trip/5.
:- dynamic stop_time/6.
:- dynamic calendar/8.
:- dynamic calendar_date/3.
:- dynamic transfer/4.
:- dynamic agency/4.

% Indexing for performance
% Note: SWI-Prolog automatically indexes on the first argument(s) of predicates
% Modern SWI-Prolog performs automatic just-in-time multi-argument indexing
:- dynamic stop/7.
:- dynamic stop_time/6.

/**
 * route_type_name(?Type, ?Name)
 * Maps GTFS route type codes to human-readable names
 */
route_type_name(0, 'Tram/Light Rail').
route_type_name(1, 'Subway/Metro').
route_type_name(2, 'Rail').
route_type_name(3, 'Bus').
route_type_name(4, 'Ferry').
route_type_name(5, 'Cable Tram').
route_type_name(6, 'Aerial Lift').
route_type_name(7, 'Funicular').
route_type_name(11, 'Trolleybus').
route_type_name(12, 'Monorail').

/**
 * wheelchair_boarding(?Code, ?Status)
 * Maps wheelchair boarding codes to status
 */
wheelchair_boarding(0, unknown).
wheelchair_boarding(1, accessible).
wheelchair_boarding(2, not_accessible).

/**
 * transfer_type(?Code, ?Description)
 * Maps transfer type codes to descriptions
 */
transfer_type(0, recommended).    % Recommended transfer point
transfer_type(1, timed).          % Timed transfer (vehicle waits)
transfer_type(2, minimum_time).   % Minimum transfer time required
transfer_type(3, not_possible).   % Transfers not possible

/**
 * pickup_type(?Code, ?Description)
 * Maps pickup/dropoff type codes
 */
pickup_type(0, regular).
pickup_type(1, none).
pickup_type(2, phone_agency).
pickup_type(3, coordinate_driver).

% Data validation helpers
valid_stop(StopId) :-
    stop(StopId, _, _, _, _, _, _).

valid_route(RouteId) :-
    route(RouteId, _, _, _, _, _).

valid_trip(TripId) :-
    trip(TripId, _, _, _, _).

/**
 * stop_coordinates(?StopId, ?Lat, ?Lon)
 * Helper to extract coordinates for a stop
 */
stop_coordinates(StopId, Lat, Lon) :-
    stop(StopId, _, Lat, Lon, _, _, _).

/**
 * trip_route(?TripId, ?RouteId)
 * Helper to get route for a trip
 */
trip_route(TripId, RouteId) :-
    trip(TripId, RouteId, _, _, _).

/**
 * clear_all_gtfs_data
 * Removes all loaded GTFS data from memory
 */
clear_all_gtfs_data :-
    retractall(stop(_, _, _, _, _, _, _)),
    retractall(route(_, _, _, _, _, _)),
    retractall(trip(_, _, _, _, _)),
    retractall(stop_time(_, _, _, _, _, _)),
    retractall(calendar(_, _, _, _, _, _, _, _)),
    retractall(calendar_date(_, _, _)),
    retractall(transfer(_, _, _, _)),
    retractall(agency(_, _, _, _)).
