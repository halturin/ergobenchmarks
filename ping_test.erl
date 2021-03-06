-module(ping_test).

-export([start_ping/2]).



start_ping(N, V) ->
    spawn(fun() ->
    RemotePids = gen_server:call({pong_server, 'erl-demo@127.0.0.1'}, {start, N}),
    ReportTo = self(),
    LocalPids = [spawn(fun() -> ping(V, P, ReportTo, 0, erlang:timestamp()) end) || P <- RemotePids],
    handleReport(LocalPids, [])
          end).

handleReport([], R) ->
    {pong_server, 'erl-demo@127.0.0.1'} ! stop,
    Result = lists:foldl(fun({_,X}, Sum) -> X + Sum end, 0, R),
    io:format("done. RPS = ~p~n", [Result/10]);

handleReport(L, R) ->
    receive
        {report, T, N, P} ->
            R1 = [{T,N}| R],
            L1 = lists:filter(fun(X) -> X /= P end, L),
            handleReport(L1, R1);

        {failed, P} ->
            io:format("failed result from ~p~n", [P]),
            L1 = lists:filter(fun(X) -> X /= P end, L),
            handleReport(L1, R);

        Unknown ->
            io:format("unknown report ~p~n", Unknown),
            handleReport(L, R)

    after 50000 ->
          io:format("timeout. shutting down~n"),
          {pong_server, 'erl-demo@127.0.0.1'} ! stop
    end. 

ping(number, P, ReportTo, N, StartedAt) ->
    ping(1234567890, P, ReportTo, N, StartedAt);
ping(map, P, ReportTo, N, StartedAt) ->
    M = #{
    key1 => 1234567890,
    "key2" => 1.23456789,
    1234567890 => "value1",
    1.23456789 => value2,
    pid1 => self(),
    key2 => 1234567890,
    "key3" => 1.23456789,
    1234567891 => "value2",
    1.23456790 => value3,
    pid2 => self()
    },
    ping(M, P, ReportTo, N, StartedAt);
ping(tuple, P, ReportTo, N, StartedAt) ->
    T = { key1, 1234567890, "key2" , 1.23456789, 1234567890 , "value1", 1.23456789 , value2, pid1 , self()},
    ping(T, P, ReportTo, N, StartedAt);
ping(list, P, ReportTo, N, StartedAt) ->
    L = [self() || _ <- lists:seq(1,200)],
    ping(L, P, ReportTo, N, StartedAt);

ping(Message, P, ReportTo, N, StartedAt) ->
    Ref = make_ref(),
    P ! {'$gen_call', {self(), Ref}, Message},
    receive
        {Ref, ok} ->
            T = timer:now_diff(erlang:timestamp(), StartedAt),
            % run 10 seconds 
            if T > 10000000 ->
                ReportTo ! {report, T, N, self()};
            true ->
                ping(Message, P, ReportTo, N+1, StartedAt)
            end       

    after 5000 ->
         ReportTo ! {failed, self()}
    end.

