/**
 * Time Utilities
 *
 * Utilities for handling GTFS time formats and date/time calculations
 */

:- module(time_utils, [
    parse_gtfs_time/2,
    gtfs_time_to_minutes/2,
    minutes_to_gtfs_time/2,
    time_add/3,
    time_diff/3,
    time_between/3,
    current_gtfs_date/1,
    day_of_week/2,
    service_active_on/2
]).

:- use_module(library(clpfd)).
:- use_module('../core/gtfs_schema').

/**
 * parse_gtfs_time(+TimeString, -Time)
 * Parses GTFS time string (HH:MM:SS) to time(H, M, S)
 * Note: GTFS allows hours >= 24 for trips past midnight
 */
parse_gtfs_time(TimeString, time(H, M, S)) :-
    atomic_list_concat([HS, MS, SS], ':', TimeString),
    atom_number(HS, H),
    atom_number(MS, M),
    atom_number(SS, S).

/**
 * gtfs_time_to_minutes(+Time, -Minutes)
 * Converts time(H, M, S) to total minutes since midnight
 */
gtfs_time_to_minutes(time(H, M, _S), Minutes) :-
    Minutes is H * 60 + M.

/**
 * minutes_to_gtfs_time(+Minutes, -Time)
 * Converts minutes since midnight to time(H, M, 0)
 */
minutes_to_gtfs_time(Minutes, time(H, M, 0)) :-
    H is Minutes // 60,
    M is Minutes mod 60.

/**
 * time_add(+Time1, +MinutesToAdd, -Time2)
 * Adds minutes to a time
 */
time_add(Time1, MinutesToAdd, Time2) :-
    gtfs_time_to_minutes(Time1, Minutes1),
    Minutes2 is Minutes1 + MinutesToAdd,
    minutes_to_gtfs_time(Minutes2, Time2).

/**
 * time_diff(+Time1, +Time2, -DiffMinutes)
 * Calculates difference between two times in minutes (Time2 - Time1)
 */
time_diff(Time1, Time2, DiffMinutes) :-
    gtfs_time_to_minutes(Time1, Minutes1),
    gtfs_time_to_minutes(Time2, Minutes2),
    DiffMinutes is Minutes2 - Minutes1.

/**
 * time_between(+Time, +StartTime, +EndTime)
 * Checks if Time is between StartTime and EndTime
 */
time_between(Time, StartTime, EndTime) :-
    gtfs_time_to_minutes(Time, TM),
    gtfs_time_to_minutes(StartTime, SM),
    gtfs_time_to_minutes(EndTime, EM),
    TM >= SM,
    TM =< EM.

/**
 * current_gtfs_date(-Date)
 * Gets current date in GTFS format YYYYMMDD
 */
current_gtfs_date(Date) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(date, DateTime, date(Y, M, D)),
    format(atom(Date), '~w~|~`0t~d~2+~|~`0t~d~2+', [Y, M, D]).

/**
 * day_of_week(+Date, -DayName)
 * Gets day of week (monday, tuesday, etc.) for a GTFS date
 */
day_of_week(DateAtom, DayName) :-
    atom_string(DateAtom, DateStr),
    sub_string(DateStr, 0, 4, _, YS),
    sub_string(DateStr, 4, 2, _, MS),
    sub_string(DateStr, 6, 2, _, DS),
    number_string(Y, YS),
    number_string(M, MS),
    number_string(D, DS),
    day_of_the_week(date(Y, M, D), DayNum),
    day_name(DayNum, DayName).

% Day number to name mapping (1=Monday, 7=Sunday)
day_name(1, monday).
day_name(2, tuesday).
day_name(3, wednesday).
day_name(4, thursday).
day_name(5, friday).
day_name(6, saturday).
day_name(7, sunday).

/**
 * service_active_on(+ServiceId, +Date)
 * Checks if a service is active on a given date
 * Considers both calendar and calendar_dates (exceptions)
 */
service_active_on(ServiceId, Date) :-
    % Check for exception first
    (   calendar_date(ServiceId, Date, ExceptionType),
        (   ExceptionType = 1 -> true  % Service added
        ;   ExceptionType = 2 -> fail  % Service removed
        )
    ;   % No exception, check regular calendar
        day_of_week(Date, DayName),
        calendar_day_active(ServiceId, DayName)
    ).

calendar_day_active(ServiceId, monday) :-
    calendar(ServiceId, 1, _, _, _, _, _, _).
calendar_day_active(ServiceId, tuesday) :-
    calendar(ServiceId, _, 1, _, _, _, _, _).
calendar_day_active(ServiceId, wednesday) :-
    calendar(ServiceId, _, _, 1, _, _, _, _).
calendar_day_active(ServiceId, thursday) :-
    calendar(ServiceId, _, _, _, 1, _, _, _).
calendar_day_active(ServiceId, friday) :-
    calendar(ServiceId, _, _, _, _, 1, _, _).
calendar_day_active(ServiceId, saturday) :-
    calendar(ServiceId, _, _, _, _, _, 1, _).
calendar_day_active(ServiceId, sunday) :-
    calendar(ServiceId, _, _, _, _, _, _, 1).

/**
 * format_time_display(+Time, -Display)
 * Formats time for human-readable display
 */
format_time_display(time(H, M, _S), Display) :-
    H24 is H mod 24,
    (   H24 >= 12 ->
        AMPM = 'PM',
        H12 is (H24 = 12 -> 12 ; H24 - 12)
    ;   AMPM = 'AM',
        H12 is (H24 = 0 -> 12 ; H24)
    ),
    format(atom(Display), '~w:~|~`0t~d~2+ ~w', [H12, M, AMPM]).

/**
 * parse_gtfs_date(+DateString, -Date)
 * Parses GTFS date string YYYYMMDD
 */
parse_gtfs_date(DateAtom, date(Y, M, D)) :-
    atom_string(DateAtom, DateStr),
    sub_string(DateStr, 0, 4, _, YS),
    sub_string(DateStr, 4, 2, _, MS),
    sub_string(DateStr, 6, 2, _, DS),
    number_string(Y, YS),
    number_string(M, MS),
    number_string(D, DS).
