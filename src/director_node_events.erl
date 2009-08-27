-module(director_node_events).

-behaviour(gen_server).

-define(NUM_MSGS,5).

%% API
-export([start/0,event/1,get_events/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% Store and event
%% Key is the UUID for the process
%% Message is a String message
event(Message) ->
    gen_server:cast(?MODULE,{event,Message}).

%% Return the event message by Key
%% {ok,Message} or error if the key doesn't exist
get_events() ->
    gen_server:call(?MODULE,{status}).

init([]) ->
    {ok,[]}.

handle_call({status}, _From, State) ->
    {reply, State, State}.

handle_cast({event,Message}, State) ->
    State1 =  trim_msgs(Message,State),
    {noreply, State1}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Stuff %%

%% Make Job IDs for messages - NOT CURRENTLY USED
%v4() ->
%    v4(random:uniform(math:pow(2, 48)) - 1, 
%       random:uniform(math:pow(2, 12)) - 1, 
%       random:uniform(math:pow(2, 32)) - 1, 
%       random:uniform(math:pow(2, 30)) - 1).
%v4(R1, R2, R3, R4) ->
%    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

%get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
%    [TL, TM, THV, CSR, CSL, N].

%make_id() ->
%    U = v4(),
%    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))). 


%% Keep the queue length at 5
trim_msgs(E,Msgs) when length(Msgs) >= ?NUM_MSGS ->
    {Last,_} = lists:split(4,Msgs),
    [E|Last];
trim_msgs(E,Msgs) ->
    [E|Msgs].
