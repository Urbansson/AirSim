-module(airplane).

-behaviour(gen_server).

%% Management API.
-export([start_link/1,stop/1]).

%% Airplane API.
-export([load_schedule/2, run_schedule/1, get_state/1, delay/2, takeoff/1,land/1]).

%% Gen_server callbacks.
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,
	 code_change/3]).


-export([get_data/1,test_notis/1]).

%% The airplane state record.
-record(airplane, {planeId,
        state,
        flightQueue,
        current %%{Source, Destinaton, TakeOffTime, NotifyRef}
        }).

%% Mangament API.

start_link(PlaneId) ->
    gen_server:start_link(?MODULE, {PlaneId}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% Airplane API.

load_schedule(PlaneId, ScheduleFile) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, {load_schedule,ScheduleFile}).

run_schedule(PlaneId) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, run_schedule).

get_state(PlaneId) ->
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, get_state).

delay(PlaneId, SimTime) -> 
    ok.

takeoff(PlaneId) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, takeoff).

land(PlaneId) -> 
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, land).




get_data(PlaneId) ->
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, get_data).

test_notis(PlaneId) ->
    {ok,Pid} = sim_lib:get_airplane(PlaneId),
    gen_server:call(Pid, test_notis).
%% Gen_server callbacks.

init({PlaneId}) ->
    St = #airplane{planeId=PlaneId,flightQueue=[],current={at_the_gate,none,none,infinity}},
    sim_lib:add_airplane(PlaneId),
    {ok,St}.

terminate(_, _) ->
    ok.

handle_call(get_state, _, #airplane{current={State,none,none,infinity}}=St) ->
     {reply,{ok,{State,infinity}},St};
handle_call(get_state, _, #airplane{current={State,none,none,EndTime}}=St) ->
     {reply,{ok,{State,EndTime-sim_lib:get_now()}},St};


handle_call(run_schedule, _, St) ->
    sim_lib:notify_at(sim_lib:get_now()+10,boarding_completed),
    {reply,ok,St};
handle_call({load_schedule,ScheduleFile}, _, #airplane{flightQueue=[]}=St0) ->
    St1 = St0#airplane{flightQueue=ScheduleFile},
    {reply,ok,St1};


handle_call(test_notis, _, St) ->
    sim_lib:notify_at(sim_lib:get_now()+4,serverMessage),
    {reply,ok,St};
handle_call(get_data, _, Data) ->
    {reply,Data,Data};


handle_call(stop, _, St) ->
    {stop,normal,ok,St};
handle_call(C, _, St) ->
    {reply,{error,C},St}.


handle_info(boarding_completed, St) ->    
    io:format("boarding_completed \n"),           %Ignore unknown messages.
    {noreply,St};
handle_info(prepare_landing, St) ->    
    io:format("prepare_landing \n"),           %Ignore unknown messages.
    {noreply,St};      
handle_info(Call, St) ->	
    io:format("handle_Info ~p\n",[Call]),			%Ignore unknown messages.
    {noreply,St}.   

%% Internal functions.


%% Unused default callbacks.

handle_cast(_, St) -> 
    {noreply,St}.		%Ignore unknown casts.

code_change(_, St, _) -> {ok,St}.
