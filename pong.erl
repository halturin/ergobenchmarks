% ergo -> erlang

% run erlang
% erl -name erl-demo@127.0.0.1 -setcookie cookie123
% (erl-demo@127.0.0.1)1> pong:start_pong().
%
% run benchmark
% go test -bench=. -benchmem -benchtime=10s -message=pid [string,pid,list]

-module(pong).

-export([start_pong/0]).

start_pong() ->
    register(pong_server, spawn(fun() -> pong_server([]) end)).

pong_server(P) ->
    receive
        {'$gen_call', {From,Ref}, {start, N }} ->
            P1 = [spawn(fun() -> pong() end) || _ <- lists:seq(1,N)],
            io:format("started ~p pong servers~n", [N]),
            From ! {Ref, P1},
            pong_server(P1);

        stop ->
            io:format("stopping server...~n"),
            [X ! stop || X <- P];

       Unknown ->
            % ignore this message
            io:format("server. unknown request ~p~n",[Unknown]),
            pong_server(P)

    after 60000 ->
          io:format("timeout. shutting down~n")
    end.


pong() -> 
    receive
        {'$gen_call', {ReplyTo, Ref}, _} ->
            ReplyTo ! {Ref, ok}, 
            pong();

        stop ->
            io:format("stopping ~p~n", [self()]);

        Unknown ->
            % ignore
            io:format("pong. unknown request ~p~n",[Unknown]),
            pong()

    after 60000 ->
              io:format("timeout. shutting down ~p~n", [self()])
    end.
            
    

