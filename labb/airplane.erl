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
    gen_fsm:sync_send_all_state_event(Pid, {load_schedule, ScheduleFile}),
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
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_fsm:sync_send_event(Pid, {delay,SimTime}), 
    ok.

takeoff(PlanePid) ->
    gen_fsm:sync_send_event(PlanePid, {takeoff}), 
    ok.

land(PlanePid) -> 
    io:format("land \n"),  
    gen_fsm:sync_send_event(PlanePid, {land}), 
    ok.

%%state callbacks

at_the_gate({delay, Time}, _From, #airplane{notisRefs={{WaitRef,WaitTimer},_Any}}=St0) -> 
    sim_lib:cancel_notification(WaitRef), 
    {ok,Ref} = sim_lib:notify_at(WaitTimer+Time, boarding_complete),
    St1 = St0#airplane{notisRefs={{Ref,WaitTimer+Time},none}},
    {reply, ok, at_the_gate,St1};

at_the_gate(Any, _From, Data) -> 
        {reply, {error,Any}, at_the_gate,Data}.


in_flight({delay, Time}, _From, #airplane{notisRefs={{WaitRef,WaitTimer},Fuel}}=St0) -> 
    sim_lib:cancel_notification(WaitRef), 
    {ok,Ref} = sim_lib:notify_at(WaitTimer+Time, approaching_airport),
    St1 = St0#airplane{notisRefs={{Ref,WaitTimer+Time},Fuel}},
    {reply, ok, in_flight,St1};

in_flight(Any, _From, Data) -> 
        {reply, {error,Any}, in_flight,Data}.

waiting_for_takeoff({takeoff}, _From, #airplane{current={From,To,Time}}=St0) ->
    {ok, FlightTime} = sim_lib:flight_time(From, To),

    FuelTimer = sim_lib:get_now()+(FlightTime*2),    
    {ok, FuelRef} = sim_lib:notify_at(FuelTimer, no_fuel),

    FlightTimer = sim_lib:get_now()+FlightTime,
    {ok, FlightRef} = sim_lib:notify_at(FlightTimer, approaching_airport),

    St1 = St0#airplane{notisRefs={{FlightRef,FlightTimer},{FuelRef, FuelTimer}}},
    {reply, ok, in_flight,St1};

waiting_for_takeoff({delay, _Time}, _From, Data) ->
        {reply, {error, delay}, waiting_for_takeoff,Data}.


waiting_for_landing({land}, _From, #airplane{flightQueue=[Current|Q], notisRefs={{FlightRef,FlightTimer},{FuelRef, FuelTimer}}}=St0) -> 

    sim_lib:cancel_notification(FuelRef), 
    {From,To,Time} = Current,
    io:format("Request Time: ~p~n",[Time]),
    io:format("Sim Time: ~p~n",[sim_lib:get_now()]),

    TimeNow = sim_lib:get_now(),
    {ok, Ref} = if 
        Time >= TimeNow -> sim_lib:notify_at(Time, boarding_complete);
        Time < TimeNow -> sim_lib:notify_at(TimeNow+10, boarding_complete);
        true -> ok
    end,
    St1 = St0#airplane{flightQueue=Q,current=Current, notisRefs={{Ref,Time},none}},

    {reply, ok,at_the_gate, St1};

waiting_for_landing({land}, _From, #airplane{flightQueue=[], notisRefs={{FlightRef,FlightTimer},{FuelRef, FuelTimer}}}=St0) -> 
    sim_lib:cancel_notification(FuelRef), 
    St1 = St0#airplane{notisRefs={{none,infinity},none}},
    {reply, ok,at_the_gate, St0};

waiting_for_landing({delay, _Time}, _From, Data) ->
        {reply, {error, delay}, waiting_for_landing,Data}.
%% Gen_server callbacks.
init({Planeid}) ->
    Data = #airplane{planeId=Planeid,flightQueue=[],current=none,notisRefs={none,none}},
    sim_lib:add_airplane(Planeid),
    {ok, at_the_gate, Data}. %%Change data format

handle_sync_event({get_state}, _From, StateName,#airplane{notisRefs={{FlightRef,FlightTimer},_Any}}=Data) -> 
    {reply, {StateName, FlightTimer}, StateName, Data};
handle_sync_event({get_state}, _From, StateName, Data) ->
    {reply, {StateName, infinity}, StateName, Data};

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

handle_info(boarding_complete, StateName, #airplane{current={From,To,Time}}=St0) ->
    io:format("handle_info boarding_complete\n"),  
    {ok, FlightTime} = sim_lib:flight_time(From, To),
    airport:schedule_landing(To, sim_lib:get_now()+FlightTime), 
    airport:request_takeoff(From), 
    {next_state, waiting_for_takeoff, St0};

handle_info(approaching_airport, StateName, #airplane{current={From,To,Time}}=St0) ->
    io:format("approaching_airport \n"),  
    airport:request_landing(To),
    {next_state, waiting_for_landing, St0};

handle_info(no_fuel, StateName, #airplane{current={From,To,Time}}=St0) ->
    io:format("no_fuel\n"),  
    {stop, normal, St0};

handle_info(_Info, StateName, State) ->
    io:format("handle_info  "),
    {next_state, StateName, State}.

%% Internal functions.


%% Unused default callbacks.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

