-module(ping).

-export([start_ping/1]).



start_ping(N) -> 
    RemotePids = gen_server:call({pong_server, 'erl-demo@127.0.0.1'}, {start, N}),
    LocalPids = [spawn(fun() -> ping(number, P, self()) end) || P <- RemotePids],
    handleReport(LocalPids).

handleReport(L) ->
    receive
        {report, X} ->
            

            handleReport(L);
        Unknown ->
            io:format("unknown report ~p~n", Unknown)
            handleReport(L)

    after 50000 ->
          io:format("timeout. shutting down~n"),
          {pong_server, 'erl-demo@127.0.0.1'} ! stop
    end. 

ping(number, P, ReportTo) ->
    ping(12345, P, ReportTo);
ping(string, P, ReportTo) ->
    ping("hello world", P, ReportTo);
ping(pid, P, ReportTo) ->
    ping(self(), P, ReportTo);
ping(list, P, ReportTo) ->
    L = [self() || _ <- lists:seq(1,100)],
    ping(L, P, ReportTo);

ping(Message, P, ReportTo) ->

