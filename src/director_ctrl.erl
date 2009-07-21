-module(director_ctrl).

-export([start/0,process/1]).

start() ->
    case init:get_plain_arguments() of
	[NodeName | Args] ->
	    Node = list_to_atom(NodeName),
	    io:format("~p~n", [Node]),
	    case rpc:call(Node, ?MODULE, process, [Args]) of
		{badrpc, Reason} ->
		    io:format("badrpc to ~p: ~p~n", [Node, Reason]),
		    halt(1);
		_ ->
		    halt(0)
	    end;
	_ ->
	    %%print_usage(),
	    halt(1)
    end.

process(["stop"]) ->
    init:stop().

