/**
 * Search Helper Predicates
 *
 * Useful utilities for searching and querying GTFS data
 * Makes it easier to find stops, routes, and trips
 */

:- module(search_helpers, [
    find_stop/2,
    find_stop_case_insensitive/2,
    find_route/2,
    find_route_case_insensitive/2,
    list_stops_containing/1,
    list_routes_containing/1,
    nearby_stops/4
]).

:- use_module('../core/gtfs_schema').
:- use_module('../routing/transfers').

/**
 * find_stop(+SearchTerm, -StopInfo)
 * Case-SENSITIVE search for stops by name
 *
 * Example:
 *   ?- find_stop('Union', Info).
 */
find_stop(SearchTerm, stop(StopId, Name, Lat, Lon, Wheelchair, LocType, Parent)) :-
    stop(StopId, Name, Lat, Lon, Wheelchair, LocType, Parent),
    sub_atom(Name, _, _, _, SearchTerm).

/**
 * find_stop_case_insensitive(+SearchTerm, -StopInfo)
 * Case-INSENSITIVE search for stops by name
 *
 * Example:
 *   ?- find_stop_case_insensitive('union', Info).
 *   ?- find_stop_case_insensitive('UNION', Info).
 */
find_stop_case_insensitive(SearchTerm, stop(StopId, Name, Lat, Lon, Wheelchair, LocType, Parent)) :-
    stop(StopId, Name, Lat, Lon, Wheelchair, LocType, Parent),
    downcase_atom(Name, NameLower),
    downcase_atom(SearchTerm, SearchLower),
    sub_atom(NameLower, _, _, _, SearchLower).

/**
 * find_route(+SearchTerm, -RouteInfo)
 * Case-SENSITIVE search for routes by name
 *
 * Example:
 *   ?- find_route('Yonge', Info).
 */
find_route(SearchTerm, route(RouteId, ShortName, LongName, Type, Color, Agency)) :-
    route(RouteId, ShortName, LongName, Type, Color, Agency),
    (   sub_atom(ShortName, _, _, _, SearchTerm)
    ;   sub_atom(LongName, _, _, _, SearchTerm)
    ).

/**
 * find_route_case_insensitive(+SearchTerm, -RouteInfo)
 * Case-INSENSITIVE search for routes by name
 *
 * Example:
 *   ?- find_route_case_insensitive('subway', Info).
 */
find_route_case_insensitive(SearchTerm, route(RouteId, ShortName, LongName, Type, Color, Agency)) :-
    route(RouteId, ShortName, LongName, Type, Color, Agency),
    downcase_atom(SearchTerm, SearchLower),
    (   downcase_atom(ShortName, ShortLower),
        sub_atom(ShortLower, _, _, _, SearchLower)
    ;   downcase_atom(LongName, LongLower),
        sub_atom(LongLower, _, _, _, SearchLower)
    ).

/**
 * list_stops_containing(+SearchTerm)
 * Pretty-print all stops matching search term (case-insensitive)
 *
 * Example:
 *   ?- list_stops_containing('union').
 */
list_stops_containing(SearchTerm) :-
    format('~nStops containing "~w":~n', [SearchTerm]),
    format('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n', []),
    findall(
        stop(StopId, Name, Lat, Lon, Wheelchair),
        (
            find_stop_case_insensitive(SearchTerm, stop(StopId, Name, Lat, Lon, Wheelchair, _, _))
        ),
        Stops
    ),
    length(Stops, Count),
    (   Count =:= 0
    ->  format('No stops found matching "~w"~n', [SearchTerm])
    ;   forall(
            member(stop(Id, StopName, StopLat, StopLon, WC), Stops),
            (
                (WC =:= 1 -> WCIcon = '♿' ; WCIcon = '  '),
                format('~w ~w: ~w (~f, ~f)~n', [WCIcon, Id, StopName, StopLat, StopLon])
            )
        ),
        format('~nTotal: ~w stops~n', [Count])
    ),
    format('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n', []).

/**
 * list_routes_containing(+SearchTerm)
 * Pretty-print all routes matching search term (case-insensitive)
 *
 * Example:
 *   ?- list_routes_containing('bloor').
 */
list_routes_containing(SearchTerm) :-
    format('~nRoutes containing "~w":~n', [SearchTerm]),
    format('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n', []),
    findall(
        route(RouteId, ShortName, LongName, Type),
        (
            find_route_case_insensitive(SearchTerm, route(RouteId, ShortName, LongName, Type, _, _))
        ),
        Routes
    ),
    length(Routes, Count),
    (   Count =:= 0
    ->  format('No routes found matching "~w"~n', [SearchTerm])
    ;   forall(
            member(route(Id, Short, Long, RouteType), Routes),
            (
                route_type_name(RouteType, TypeName),
                format('~w (~w): ~w - ~w~n', [Short, Id, Long, TypeName])
            )
        ),
        format('~nTotal: ~w routes~n', [Count])
    ),
    format('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n', []).

/**
 * nearby_stops(+Lat, +Lon, +MaxDistanceMeters, -NearbyStops)
 * Find all stops within MaxDistance meters of a location
 *
 * Example:
 *   ?- nearby_stops(43.6532, -79.3832, 500, Stops).
 */
nearby_stops(Lat, Lon, MaxDistance, NearbyStops) :-
    findall(
        nearby(StopId, Name, Distance),
        (
            stop(StopId, Name, StopLat, StopLon, _, _, _),
            haversine_distance(Lat, Lon, StopLat, StopLon, Distance),
            Distance =< MaxDistance
        ),
        NearbyStops
    ).

/**
 * Helper: route_type_name/2
 * Already defined in gtfs_schema, but including local version for safety
 */
route_type_name(Type, Name) :-
    gtfs_schema:route_type_name(Type, Name), !.
route_type_name(_, 'Unknown').
