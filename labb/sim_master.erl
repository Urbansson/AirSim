-module(sim_master).

-export([rundefualt/0,run/2,load_config/1,load_schedule/1,run/0]).
-export([trace_all/0,trace_plane/1,trace_airport/1]).


rundefualt() ->
    sim_lib:start_link(0,400),
    load_config("config.txt"),
    load_schedule("schedule.txt"),
    run().

run(Cfile, Sfile) ->
    load_config(Cfile),
    load_schedule(Sfile),
    run().

load_config(File) ->
    {ok,C} = file:consult(File),
    {airports,As} = proplists:lookup(airports, C),
    {flight_times,Fs} = proplists:lookup(flight_times, C),
    {airplanes,Ps} = proplists:lookup(airplanes, C),
    ok = lists:foreach(fun (A) -> airport:start_link(A) end, As),
    ok = lists:foreach(fun (P) -> airplane:start_link(P) end, Ps),
    ok = lists:foreach(fun ({A1,A2,T}) ->
                   sim_lib:add_flight_time(A1, A2, T)
               end, Fs).

load_schedule(File) ->
    {ok,Scheds} = file:consult(File),
    Fun = fun ({P,S}) ->
          airplane:load_schedule(P, S)
      end,
    lists:foreach(Fun, Scheds).

run() ->
    sim_lib:start_clock(),          %Must start this first
    %% {ok,C} = file:consult(File),
    %% {airplanes,Ps} = proplists:lookup(airplanes, C),
    Ps = sim_lib:get_airplanes(),
    lists:foreach(fun (P) ->
              airplane:run_schedule(P)
          end, Ps).

trace_plane(P) ->
    {ok,Pid} = sim_lib:get_airplane(P),
    sys:trace(Pid, true).

trace_airport(P) ->
    {ok,Pid} = sim_lib:get_airport(P),
    sys:trace(Pid, true).

trace_all() ->
    As = sim_lib:get_airports(),
    Ps = sim_lib:get_airplanes(),
    lists:foreach(fun (A) -> trace_airport(A) end, As),
    lists:foreach(fun (P) -> trace_plane(P) end, Ps).
