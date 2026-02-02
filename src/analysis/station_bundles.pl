:- module(station_bundles, [
    station_bundle/2,
    bundle_representative/2,
    bundle_name/2,
    stops_in_same_bundle/2,
    get_bundle_for_stop/2,
    all_bundles/1,
    get_cache_key/2
]).

/** <module> Station Bundling for Transit Stops
 *
 * Complete TTC Subway Station Bundles - All 73 Stations
 *
 * This module implements stop bundling to optimize isochrone caching.
 *
 * Key Insight (from user):
 * "The same subway station might have multiple stop IDs to represent
 * different subway directions, e.g., northbound and southbound of
 * Museum Station."
 *
 * Impact: 54% reduction in pre-computation time (160 platforms → 73 stations)
 *
 * Generated: December 2025
 * Stations: 73 TTC subway stations
 * Platforms: 160 platform stops
 * Reduction: 54% fewer cache entries needed
 */

%% station_bundle(?BundleId, ?Stops) is nondet.
%
%  Define bundles of stops that should share cached isochrones.
%  The first stop in the list is the representative.

% Bathurst Station (2 platforms)
station_bundle(bathurst, [14481,14516]).
bundle_name(bathurst, 'Bathurst Station').
% Bay Station (2 platforms)
station_bundle(bay, [14484,14513]).
bundle_name(bay, 'Bay Station').
% Bayview Station (2 platforms)
station_bundle(bayview, [14531,14538]).
bundle_name(bayview, 'Bayview Station').
% Bessarion Station (2 platforms)
station_bundle(bessarion, [14532,14537]).
bundle_name(bessarion, 'Bessarion Station').
% Bloor Station (2 platforms)
station_bundle(bloor, [14414,14457]).
bundle_name(bloor, 'Bloor Station').
% Broadview Station (2 platforms)
station_bundle(broadview, [14488,14509]).
bundle_name(broadview, 'Broadview Station').
% Castle Frank Station (2 platforms)
station_bundle(castle_frank, [14487,14510]).
bundle_name(castle_frank, 'Castle Frank Station').
% Chester Station (2 platforms)
station_bundle(chester, [14489,14508]).
bundle_name(chester, 'Chester Station').
% Christie Station (2 platforms)
station_bundle(christie, [14480,14517]).
bundle_name(christie, 'Christie Station').
% College Station (2 platforms)
station_bundle(college, [14416,14455]).
bundle_name(college, 'College Station').
% Coxwell Station (2 platforms)
station_bundle(coxwell, [14493,14504]).
bundle_name(coxwell, 'Coxwell Station').
% Davisville Station (2 platforms)
station_bundle(davisville, [14410,14461]).
bundle_name(davisville, 'Davisville Station').
% Don Mills Station (2 platforms)
station_bundle(don_mills, [14534,14535]).
bundle_name(don_mills, 'Don Mills Station').
% Donlands Station (2 platforms)
station_bundle(donlands, [14491,14506]).
bundle_name(donlands, 'Donlands Station').
% Downsview Park Station (2 platforms)
station_bundle(downsview_park, [15698,15699]).
bundle_name(downsview_park, 'Downsview Park Station').
% Dufferin Station (2 platforms)
station_bundle(dufferin, [14478,14519]).
bundle_name(dufferin, 'Dufferin Station').
% Dundas St West at Dundas West Outer Platform (1 platforms)
station_bundle(dundas_st_west_at_dundas_west_outer_platform, [300]).
bundle_name(dundas_st_west_at_dundas_west_outer_platform, 'Dundas St West at Dundas West Outer Platform').
% Dundas Station (2 platforms)
station_bundle(dundas, [14417,14454]).
bundle_name(dundas, 'Dundas Station').
% Dundas West Station (2 platforms)
station_bundle(dundas_west, [14476,14521]).
bundle_name(dundas_west, 'Dundas West Station').
% Dupont Station (2 platforms)
station_bundle(dupont, [14428,14443]).
bundle_name(dupont, 'Dupont Station').
% Eglinton Station (2 platforms)
station_bundle(eglinton, [14409,14462]).
bundle_name(eglinton, 'Eglinton Station').
% Eglinton West Station (2 platforms)
station_bundle(eglinton_west, [14430,14441]).
bundle_name(eglinton_west, 'Eglinton West Station').
% Finch Station (2 platforms)
station_bundle(finch, [14404,14467]).
bundle_name(finch, 'Finch Station').
% Finch West Station (2 platforms)
station_bundle(finch_west, [15696,15697]).
bundle_name(finch_west, 'Finch West Station').
% Glencairn Station (2 platforms)
station_bundle(glencairn, [14431,14440]).
bundle_name(glencairn, 'Glencairn Station').
% Greenwood Station (2 platforms)
station_bundle(greenwood, [14492,14505]).
bundle_name(greenwood, 'Greenwood Station').
% High Park Station (2 platforms)
station_bundle(high_park, [14474,14523]).
bundle_name(high_park, 'High Park Station').
% Highway 407 Station (2 platforms)
station_bundle(highway_407, [15700,15701]).
bundle_name(highway_407, 'Highway 407 Station').
% Islington Station (2 platforms)
station_bundle(islington, [14469,14528]).
bundle_name(islington, 'Islington Station').
% Jane Station (2 platforms)
station_bundle(jane, [14472,14525]).
bundle_name(jane, 'Jane Station').
% Keele Station (2 platforms)
station_bundle(keele, [14475,14522]).
bundle_name(keele, 'Keele Station').
% Kennedy Station (12 platforms)
station_bundle(kennedy, [14228,14229,14230,14231,14498,14499,24665,24682,24683,24684,24699,24700]).
bundle_name(kennedy, 'Kennedy Station').
% Kennedy Station Drop-off Platform (1 platforms)
station_bundle(kennedy_station_drop_off_platform, [24709]).
bundle_name(kennedy_station_drop_off_platform, 'Kennedy Station Drop-off Platform').
% King Station (2 platforms)
station_bundle(king, [14419,14452]).
bundle_name(king, 'King Station').
% Kipling Station (2 platforms)
station_bundle(kipling, [14468,14529]).
bundle_name(kipling, 'Kipling Station').
% Lansdowne Station (2 platforms)
station_bundle(lansdowne, [14477,14520]).
bundle_name(lansdowne, 'Lansdowne Station').
% Lawrence Station (2 platforms)
station_bundle(lawrence, [14408,14463]).
bundle_name(lawrence, 'Lawrence Station').
% Lawrence West Station (2 platforms)
station_bundle(lawrence_west, [14432,14439]).
bundle_name(lawrence_west, 'Lawrence West Station').
% Leslie Station (2 platforms)
station_bundle(leslie, [14533,14536]).
bundle_name(leslie, 'Leslie Station').
% Main Street Station (2 platforms)
station_bundle(main_street, [14495,14502]).
bundle_name(main_street, 'Main Street Station').
% Museum Station (2 platforms)
station_bundle(museum, [14425,14446]).
bundle_name(museum, 'Museum Station').
% North York Centre Station (2 platforms)
station_bundle(north_york_centre, [14405,14466]).
bundle_name(north_york_centre, 'North York Centre Station').
% Old Mill Station (2 platforms)
station_bundle(old_mill, [14471,14526]).
bundle_name(old_mill, 'Old Mill Station').
% Osgoode Station (2 platforms)
station_bundle(osgoode, [14422,14449]).
bundle_name(osgoode, 'Osgoode Station').
% Ossington Station (2 platforms)
station_bundle(ossington, [14479,14518]).
bundle_name(ossington, 'Ossington Station').
% Pape Station (2 platforms)
station_bundle(pape, [14490,14507]).
bundle_name(pape, 'Pape Station').
% Pioneer Village Station (2 platforms)
station_bundle(pioneer_village, [15692,15693]).
bundle_name(pioneer_village, 'Pioneer Village Station').
% Queen Station (2 platforms)
station_bundle(queen, [14418,14453]).
bundle_name(queen, 'Queen Station').
% Queen's Park Station (2 platforms)
station_bundle(queens_park, [14424,14447]).
bundle_name(queens_park, 'Queen\'s Park Station').
% Rosedale Station (2 platforms)
station_bundle(rosedale, [14413,14458]).
bundle_name(rosedale, 'Rosedale Station').
% Royal York Station (2 platforms)
station_bundle(royal_york, [14470,14527]).
bundle_name(royal_york, 'Royal York Station').
% Runnymede Station (2 platforms)
station_bundle(runnymede, [14473,14524]).
bundle_name(runnymede, 'Runnymede Station').
% Sheppard West Station (2 platforms)
station_bundle(sheppard_west, [14435,14436]).
bundle_name(sheppard_west, 'Sheppard West Station').
% Sheppard-Yonge Station (4 platforms)
station_bundle(sheppard_yonge, [14406,14465,14530,14539]).
bundle_name(sheppard_yonge, 'Sheppard-Yonge Station').
% Sherbourne Station (2 platforms)
station_bundle(sherbourne, [14486,14511]).
bundle_name(sherbourne, 'Sherbourne Station').
% Spadina Station (4 platforms)
station_bundle(spadina, [14427,14444,14482,14515]).
bundle_name(spadina, 'Spadina Station').
% St Andrew Station (2 platforms)
station_bundle(st_andrew, [14421,14450]).
bundle_name(st_andrew, 'St Andrew Station').
% St Clair Station (2 platforms)
station_bundle(st_clair, [14411,14460]).
bundle_name(st_clair, 'St Clair Station').
% St Clair West Station (2 platforms)
station_bundle(st_clair_west, [14429,14442]).
bundle_name(st_clair_west, 'St Clair West Station').
% St George Station (4 platforms)
station_bundle(st_george, [14426,14445,14483,14514]).
bundle_name(st_george, 'St George Station').
% St Patrick Station (2 platforms)
station_bundle(st_patrick, [14423,14448]).
bundle_name(st_patrick, 'St Patrick Station').
% Summerhill Station (2 platforms)
station_bundle(summerhill, [14412,14459]).
bundle_name(summerhill, 'Summerhill Station').
% Union Station (2 platforms)
station_bundle(union, [14420,14451]).
bundle_name(union, 'Union Station').
% Vaughan Metropolitan Centre Station (2 platforms)
station_bundle(vaughan_metropolitan_centre, [15702,15703]).
bundle_name(vaughan_metropolitan_centre, 'Vaughan Metropolitan Centre Station').
% Victoria Park Station (2 platforms)
station_bundle(victoria_park, [14496,14501]).
bundle_name(victoria_park, 'Victoria Park Station').
% Warden Station (2 platforms)
station_bundle(warden, [14497,14500]).
bundle_name(warden, 'Warden Station').
% Wellesley Station (2 platforms)
station_bundle(wellesley, [14415,14456]).
bundle_name(wellesley, 'Wellesley Station').
% Wilson Station (2 platforms)
station_bundle(wilson, [14434,14437]).
bundle_name(wilson, 'Wilson Station').
% Woodbine Station (2 platforms)
station_bundle(woodbine, [14494,14503]).
bundle_name(woodbine, 'Woodbine Station').
% Yonge Station (2 platforms)
station_bundle(yonge, [14485,14512]).
bundle_name(yonge, 'Yonge Station').
% York Mills Station (2 platforms)
station_bundle(york_mills, [14407,14464]).
bundle_name(york_mills, 'York Mills Station').
% York University (2 platforms)
station_bundle(york_university, [15694,15695]).
bundle_name(york_university, 'York University').
% Yorkdale Station (2 platforms)
station_bundle(yorkdale, [14433,14438]).
bundle_name(yorkdale, 'Yorkdale Station').

%% bundle_representative(?BundleId, ?RepresentativeStop) is nondet.
bundle_representative(BundleId, Representative) :-
    station_bundle(BundleId, [Representative|_]).

%% stops_in_same_bundle(?Stop1, ?Stop2) is nondet.
stops_in_same_bundle(Stop1, Stop2) :-
    station_bundle(_, Stops),
    member(Stop1, Stops),
    member(Stop2, Stops).

%% get_bundle_for_stop(?StopId, ?BundleInfo) is nondet.
get_bundle_for_stop(StopId, BundleInfo) :-
    station_bundle(BundleId, Stops),
    member(StopId, Stops),
    Stops = [Representative|_],
    bundle_name(BundleId, Name),
    BundleInfo = _{
        bundle_id: BundleId,
        bundle_name: Name,
        representative: Representative,
        all_stops: Stops,
        is_representative: (StopId = Representative)
    }.

%% all_bundles(?Bundles) is det.
all_bundles(Bundles) :-
    findall(BundleId, station_bundle(BundleId, _), Bundles).

%% get_cache_key(+StopId, -CacheKey) is det.
get_cache_key(StopId, CacheKey) :-
    (   get_bundle_for_stop(StopId, BundleInfo)
    ->  CacheKey = BundleInfo.representative
    ;   CacheKey = StopId
    ).

%% bundle_stats(-Stats) is det.
bundle_stats(Stats) :-
    findall(BundleId, station_bundle(BundleId, _), BundleIds),
    length(BundleIds, NumBundles),
    findall(Stop, (station_bundle(_, Stops), member(Stop, Stops)), AllBundledStops),
    length(AllBundledStops, NumBundledStops),
    length(BundleIds, NumRepresentatives),
    ReductionPct is round((1 - NumRepresentatives / NumBundledStops) * 100),
    Stats = _{
        num_bundles: NumBundles,
        num_bundled_stops: NumBundledStops,
        num_representatives: NumRepresentatives,
        reduction_percentage: ReductionPct
    }.

%% print_bundle_info(+BundleId) is det.
print_bundle_info(BundleId) :-
    station_bundle(BundleId, Stops),
    bundle_name(BundleId, Name),
    Stops = [Representative|Rest],
    length(Stops, NumStops),
    format('~n=== Bundle: ~w ===~n', [Name]),
    format('Bundle ID: ~w~n', [BundleId]),
    format('Representative: ~w~n', [Representative]),
    format('Total stops: ~w~n', [NumStops]),
    format('All stops: ~w~n', [Stops]),
    (   Rest \= []
    ->  format('Other stops (will use representative): ~w~n', [Rest])
    ;   format('(Single platform station)~n')
    ),
    format('~n').

%% print_all_bundles is det.
print_all_bundles :-
    format('~n╔════════════════════════════════════════╗~n'),
    format('║  Station Bundling Configuration        ║~n'),
    format('║  ALL 73 TTC Subway Stations            ║~n'),
    format('╚════════════════════════════════════════╝~n'),
    findall(BundleId, station_bundle(BundleId, _), BundleIds),
    length(BundleIds, NumBundles),
    format('~nTotal stations: ~w~n~n', [NumBundles]),
    bundle_stats(Stats),
    format('Summary Statistics:~n'),
    format('  Total bundles: ~w~n', [Stats.num_bundles]),
    format('  Total bundled stops: ~w~n', [Stats.num_bundled_stops]),
    format('  Representatives needed: ~w~n', [Stats.num_representatives]),
    format('  Computation reduction: ~w%~n~n', [Stats.reduction_percentage]),
    format('Without bundling: ~w isochrone computations~n', [Stats.num_bundled_stops]),
    format('With bundling: ~w isochrone computations~n', [Stats.num_representatives]),
    format('Savings: ~w computations avoided!~n~n',
           [Stats.num_bundled_stops - Stats.num_representatives]).
