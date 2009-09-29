%%%-------------------------------------------------------------------
%%% File    : director_apache_process.erl
%%% Author  : Dave Bryson <dbryson AT mitre.org>
%%% Description : Bot to monitor HTTP Service 
%%%-------------------------------------------------------------------
-module(director_apache_process).

-behaviour(gen_fsm).

%% API
-export([start/1,stop/0,status/0]).

%% gen_fsm callbacks
-export([init/1, check_service/2, run_script/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(MINUTE,60000).

%% DEFAULT POLL DELAY IS 1 MINUTE.
-define(POLL_DELAY, ?MINUTE).
-define(USER_AGENT,"Director/Apache Monitor Agent").

-record(state,{url,config}).

start(Config) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

status() ->
    [{script,"apache process"},{cpu,"*"},{mem,"*"}].

init(Config) ->
    Url = proplists:get_value(url,Config),
    State = #state{url=Url,config=Config},
    %% Delay on startup
    director_node_events:event("Starting the HTTP monitor"),
    erlang:start_timer(5000, self(), end_wait ),
    {ok,check_service, State}.

check_service(_Event, State) ->
    error_logger:info_msg("Checking the service~n"),
    Url = State#state.url,
    {NextState,Delay} = case http:request(get, {Url,[{"User-Agent",?USER_AGENT}]},
					  [{timeout, 20000}], [{sync, true}]) of
			    {ok, {{_Httpver, 200, _Reason}, _Headers, _Body}} -> 
				{check_service,?POLL_DELAY};
			    {ok, {{_Httpver, 200, _Reason}, _Body}} ->
				{check_service,?POLL_DELAY};
			    _ ->
				{run_script,2000}
			end,
    
    {next_state,NextState,State,Delay}.

run_script(timeout,State) ->
    director_node_events:event("Found the HTTP service down. Restarting..."),
    error_logger:info_msg("Service is down - running script~n~n"),
    Script = build_command(State#state.config),
    
    os:cmd(Script),
    director_node_events:event("HTTP Service restarted"),
    {next_state,check_service,State,5000}.

handle_info({timeout, _Ref, _C},check_service, State) ->
    gen_fsm:send_event(?MODULE, State),
    {next_state,check_service,State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% 
%% Internal methods
%%

%% Build the shell command: ASSUMES a shell script
build_command(Config) ->
    Script = filename:join([proplists:get_value(appdir,Config),
			    proplists:get_value(cmd,Config)]),

    "sh " ++ Script.
    




