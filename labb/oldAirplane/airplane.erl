-module(airplane).

-behaviour(gen_fsm).

%% Management API.
-export([start_link/1,stop/0]).

%% Airplane API.
-export([load_schedule/2, run_schedule/1, get_state/1, delay/2, takeoff/1,land/1]).

%% gen_fsm Exports
-export([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([at_the_gate/3, waiting_for_takeoff/3,in_flight/3, waiting_for_landing/3]).
-export([get_data/1]).



-record(airplane, {planeId,
          flightQueue,
          current, %%{ Flight{From,To,TakeoffTime}, }
          notisRefs %%{{Ref,time}}, {Ref,Time}}}
         }).

%% Mangament API.

start_link(PlaneId) ->
    gen_fsm:start_link(?MODULE, {PlaneId}, []).
  
stop() ->
    ok.  

%% Airplane API.
load_schedule(PlaneId, ScheduleFile) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_fsm:sync_send_all_state_event(Pid, {load_schedule, ScheduleFile}), %% async kanske är bättre
    ok.

run_schedule(PlaneId) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_fsm:send_all_state_event(Pid, {run_schedule}), %% async kanske är bättre
    ok.

get_state(PlaneId) ->
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    {ok, gen_fsm:sync_send_all_state_event(Pid, {get_state})}.%%{State,EndTime}}.

get_data(PlaneId) ->
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    {ok, gen_fsm:sync_send_all_state_event(Pid, {get_data})}.

delay(PlaneId, SimTime) -> 
    ok.

takeoff(PlaneId) -> 
    ok.

land(PlaneId) -> 
    ok.

%%state callbacks

at_the_gate({taxiing}, _From, Data) -> 
    {reply, ok, Data}.

waiting_for_takeoff({takeoff}, _From, Data) -> 
    {reply, ok, Data}.

in_flight({prepare}, _From, Data) -> 
    {reply, ok, Data}.

waiting_for_landing({land}, _From, Data) -> 
    {reply, ok, Data}.


%% Gen_server callbacks.
init({Planeid}) ->
    Data = #airplane{planeId=Planeid,flightQueue=[],current=none,notisRefs={none,none}},
    sim_lib:add_airplane(Planeid),
    {ok, at_the_gate, Data}. %%Change data format

handle_sync_event({get_state}, _From, StateName, Data) ->
    {reply, {StateName, null}, StateName, Data};
handle_sync_event({get_data}, _From, StateName, Data) ->
    {reply, {Data}, StateName, Data};


handle_sync_event({load_schedule, ScheduleData}, _From, StateName, Data) ->
    {reply, StateName, StateName, Data#airplane{flightQueue=ScheduleData}}.


handle_event({run_schedule}, at_the_gate, #airplane{flightQueue=[Current|Q]}=St0) ->
    {From,To,Time} = Current,
    {ok, Ref} = sim_lib:notify_at(Time, boarding_complete),
    Data = St0#airplane{flightQueue=Q,current=Current, notisRefs={{Ref,Time},none}},
    {next_state, at_the_gate, Data};

handle_event({run_schedule}, State, #airplane{flightQueue=[]}=Data) ->
    {next_state, State, Data}.

terminate(_Reason, _StateName, _State) ->
    ok.

%% Internal functions.




%% Unused default callbacks.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_info(boarding_complete, StateName, #airplane{current={From,To,Time}}=St0) ->
    io:format("handle_info boarding_complete\n"),   
    airport:schedule_landing(To, sim_lib:get_now()+sim_lib:flight_time(From, To)), 
    airport:request_takeoff(From), 
    {next_state, waiting_for_takeoff, St0};
handle_info(_Info, StateName, State) ->
    io:format("handle_info  "),
    {next_state, StateName, State}.