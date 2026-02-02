% reasoning.pl - Reasoning trace collection for explainable routing
%
% This module collects decision traces during route planning to enable
% explanation of why routes were chosen.

:- module(reasoning, [
    init_trace/0,
    add_decision/3,
    add_decision/4,
    get_trace/1,
    clear_trace/0,
    trace_enabled/0,
    enable_tracing/0,
    disable_tracing/0,
    find_route_with_trace/6
]).

:- use_module('../routing/route_planner').

% Thread-local storage for reasoning trace
:- thread_local current_trace/1.
:- thread_local tracing_enabled/0.

%% init_trace
%  Initialize a new reasoning trace
init_trace :-
    retractall(current_trace(_)),
    asserta(current_trace([])).

%% enable_tracing
%  Enable trace collection
enable_tracing :-
    (tracing_enabled -> true ; asserta(tracing_enabled)).

%% disable_tracing
%  Disable trace collection
disable_tracing :-
    retractall(tracing_enabled).

%% trace_enabled
%  Check if tracing is currently enabled
trace_enabled :- tracing_enabled.

%% add_decision(+Type, +Context, +Justification)
%  Add a decision to the trace with automatic timestamp
add_decision(Type, Context, Justification) :-
    get_time(Timestamp),
    add_decision(Type, Context, Justification, Timestamp).

%% add_decision(+Type, +Context, +Justification, +Timestamp)
%  Add a decision to the trace with explicit timestamp
add_decision(Type, Context, Justification, Timestamp) :-
    (trace_enabled ->
        retract(current_trace(Trace)),
        Decision = decision(Type, Context, Justification, Timestamp),
        append(Trace, [Decision], NewTrace),
        asserta(current_trace(NewTrace))
    ; true).  % No-op if tracing disabled

%% get_trace(-Trace)
%  Get the current reasoning trace
get_trace(Trace) :-
    (current_trace(Trace) -> true ; Trace = []).

%% clear_trace
%  Clear the current reasoning trace
clear_trace :-
    retractall(current_trace(_)),
    asserta(current_trace([])).

%% find_route_with_trace(+FromStop, +ToStop, +StartTime, +Options, -Route, -Trace)
%  Wrapper for find_route that collects reasoning trace
%
%  This predicate wraps the existing find_route/4 to collect a trace
%  of all decisions made during route planning.
%
%  @param FromStop    Origin stop ID
%  @param ToStop      Destination stop ID
%  @param StartTime   Start time as time(H,M,S)
%  @param Options     List of options (constraints, preferences)
%  @param Route       Resulting route solution
%  @param Trace       List of decision(Type, Context, Justification, Timestamp)
find_route_with_trace(FromStop, ToStop, StartTime, Options, Route, Trace) :-
    % Initialize tracing
    init_trace,
    enable_tracing,

    % Log start of route search
    add_decision(
        route_start,
        context(from(FromStop), to(ToStop), time(StartTime), options(Options)),
        'Beginning route search'
    ),

    % Call existing route planner
    % Note: This will be enhanced later to collect more decision points
    (find_route(FromStop, ToStop, StartTime, Options, Route) ->
        add_decision(
            route_complete,
            Route,
            'Route found successfully'
        ),
        Success = true
    ;
        add_decision(
            route_failed,
            context(from(FromStop), to(ToStop)),
            'No route found between specified stops'
        ),
        Success = false
    ),

    % Collect trace
    get_trace(Trace),

    % Clean up
    clear_trace,
    disable_tracing,

    % Succeed only if route was found
    Success = true.

%% log_route_selection(+RouteId, +Alternatives, +Reason)
%  Hook for logging route selection decisions
%  This can be called from route_planner.pl
log_route_selection(RouteId, Alternatives, Reason) :-
    trace_enabled,
    add_decision(
        route_selection,
        context(route(RouteId), alternatives(Alternatives)),
        Reason
    ).

%% log_transfer(+StopId, +Quality, +Reason)
%  Hook for logging transfer decisions
log_transfer(StopId, Quality, Reason) :-
    trace_enabled,
    add_decision(
        transfer_point,
        context(stop(StopId), quality(Quality)),
        Reason
    ).

%% log_constraint(+ConstraintType, +Impact, +Explanation)
%  Hook for logging constraint applications
log_constraint(ConstraintType, Impact, Explanation) :-
    trace_enabled,
    add_decision(
        constraint_applied,
        context(type(ConstraintType), impact(Impact)),
        Explanation
    ).

%% log_alternative_rejected(+Route, +Reason, +Metrics)
%  Hook for logging rejected alternatives
log_alternative_rejected(Route, Reason, Metrics) :-
    trace_enabled,
    add_decision(
        alternative_rejected,
        context(route(Route), metrics(Metrics)),
        Reason
    ).
