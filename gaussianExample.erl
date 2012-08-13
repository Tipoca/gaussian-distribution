-module(gaussianExample).
-export([start/0]).

start() ->
    lists:foreach(fun show/1,results()).

show({X,N,V,E}) ->
    io:format("x: ~p, n: ~p, value: ~p, error bound: ~p~n",[X,N,V,E]).

results() ->
    [{X,N,gaussianDistribution:integral(X,N),gaussianDistribution:error_term(X,N)} || X<-[1.96,5], N<-[1,2,10,30,50,100,200]].
