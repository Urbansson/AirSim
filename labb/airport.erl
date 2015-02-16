%%%==============================================================
%%% @Copyright (C) 1999-2015, Erlang Solutions Ltd
%%% @Author Robert Virding <robert.virding@erlang-solutions.com>
%%% @doc Airport process for the KTH HI2011 assignment.
%%% @end
%%%==============================================================

%% This is a simple implementation of an airport. It is one runway and
%% requires at least 5 min, ?RWINT, between takeoffs and landings.

-module(airport).

-behaviour(gen_server).

%% Management API.
-export([start_link/1,stop/1]).

%% Airplane API.
-export([schedule_landing/2,request_takeoff/1,request_landing/1]).

%% Gen_server callbacks.
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,
	 code_change/3]).

-compile(export_all).				%For testing

-define(RWINT, 5).				%Runway safety interval

%% The airport state record.
-record(airport, {name,				%Airport name
		  queue,			%Runway queue
		  notify			%Next notification | none
		 }).

%% Mangament API.

start_link(Name) ->
    gen_server:start_link(?MODULE, {Name}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% Airplane API.

schedule_landing(Airport, Time) ->
    %% Get the right airport pid.
    {ok,Pid} = sim_lib:get_airport(Airport),
    gen_server:call(Pid, {schedule_landing,self(),Time}).

request_takeoff(Airport) ->
    %% Get the right airport pid.
    {ok,Pid} = sim_lib:get_airport(Airport),
    gen_server:call(Pid, {request_takeoff,self()}).

request_landing(Airport) ->
    %% Get the right airport pid.
    {ok,Pid} = sim_lib:get_airport(Airport),
    gen_server:call(Pid, {request_landing,self()}).

%% Gen_server callbacks.

init({Name}) ->
    St = #airport{name=Name,queue=[],notify=none},
    sim_lib:add_airport(Name),
    {ok,St}.

terminate(_, _) ->
    ok.

handle_call({schedule_landing,From,Time}, _, #airport{queue=Q0}=St0) ->
    %% Reserves a place in the queue but does schedule notification.
    {NewT,Q1} = insert_queue(Time, From, schedule_landing, Q0),
    St1 = St0#airport{queue=Q1},
    {reply,{ok,NewT},St1};
handle_call({request_takeoff,From}, _, #airport{queue=Q0}=St0) ->
    %% Add to queue and (re)schedule notification.
    {NewT,Q1} = insert_queue(sim_lib:get_now(), From, request_takeoff, Q0),
    St1 = St0#airport{queue=Q1},
    St2 = maybe_cancel(St1),
    St3 = maybe_notify(St2),
    {reply,{ok,NewT},St3};
handle_call({request_landing,From}, _, #airport{queue=Q0}=St0) ->
    %% Replace to queue and (re)schedule notification.
    {NewT,Q1} = replace_queue(sim_lib:get_now(), From, request_landing, Q0),
    St1 = St0#airport{queue=Q1},
    St2 = maybe_cancel(St1),
    St3 = maybe_notify(St2),
    {reply,{ok,NewT},St3};
handle_call(stop, _, St) ->
    {stop,normal,ok,St};
handle_call(C, _, St) ->
    {reply,{error,C},St}.

handle_info(next_plane, #airport{queue=[{_,P,W}|Q]}=St0) ->
    %% Takeoff/land the first plane in the queue.
    case W of
	request_landing -> airplane:land(P);
	request_takeoff -> airplane:takeoff(P);
    _ -> io:format("Error ~p\n",[W]) %% <---- WTF

    end,
    %% Reset queue.
    St1 = St0#airport{queue=Q,notify=none},
    %% Notify the next plane in the queue.
    St2 = maybe_notify(St1),
    {noreply,St2};
handle_info(_, St) ->				%Ignore unknown messages.
    {noreply,St}.

%% Internal functions.

%% insert_queue(Time, FromPid, What, Queue) -> {InsertTime,Queue}.
%%  Insert a new entry in the queue at Time if there is a large enough
%%  slot, > 2 * ?RWINT, else insert it at the first available later
%%  time. Return when the slot was added.
%%
%%  Queue has the structure [{Time,Pid,What}].

insert_queue(Time, From, W, Q) -> insert_queue(Time, From, W, Q, []).

insert_queue(Time, From, What, [{T,_,_}=E|Q], Acc) ->
    if Time + ?RWINT =< T ->
	    %% We can put this entry first.
	    {Time,lists:reverse(Acc, [{Time,From,What},E|Q])};
       true ->
	    %% Must put it after this entry, set new time after this entry.
	    NewT = max(Time, T + ?RWINT),
	    insert_queue(NewT, From, What, Q, [E|Acc])
    end;
insert_queue(Time, From, What, [], Acc) ->
    {Time,lists:reverse(Acc, [{Time,From,What}])}.

replace_queue(Time, From, W, Q0) ->
    Q1 = lists:keydelete(From, 2, Q0),		%Remove our reserved slot
    insert_queue(Time, From, W, Q1).		%Add us

maybe_notify(#airport{queue=[{T,_,_}|_],notify=none}=St) ->
    {ok,Ref} = sim_lib:notify_at(T, next_plane),
    St#airport{notify=Ref};
maybe_notify(St) -> St.

maybe_cancel(#airport{notify=Ref}=St) when Ref =/= none ->
    sim_lib:cancel_notification(Ref),
    St#airport{notify=none};
maybe_cancel(St) -> St.

%% Unused default callbacks.

handle_cast(_, St) -> {noreply,St}.		%Ignore unknown casts.

code_change(_, St, _) -> {ok,St}.
