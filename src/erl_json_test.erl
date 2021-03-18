-module(erl_json_test).
-export([start/0]).
-define(RESULTS_FILE, "results.csv").
-define(NUM_TESTS, 300).
-define(PARSERS,
        [
         {"jsone", fun jsone:encode/1, fun jsone:decode/1},
         {"yawsjson2", fun json2:encode/1, fun json2:decode/1},
         {"jiffy", fun jiffy:encode/1, fun jiffy:decode/1},
         {"mochijson2", fun mochijson2:encode/1, fun mochijson2:decode/1},
         {"jsx", fun jsx:encode/1, fun jsx:decode/1}
        ]).
-define(TESTFILES,
        [
         {"1x", "1x.json"},
         {"3x", "3x.json"},
         {"9x", "9x.json"},
         {"27x", "27x.json"},
         {"81x", "81x.json"},
         {"243x", "243x.json"}
        ]).
-define(NPROC, 10).

start() ->
    JSONs = [begin
                 FullName = "priv/" ++ FileName,
                 {ok, File} = file:read_file(FullName),
                 {Name, File}
             end
             || {Name, FileName} <- ?TESTFILES],
    _A = [ jsone:encode(jsone:decode(File)) || {_, File} <- JSONs],
    _B = [ jiffy:encode(jiffy:decode(File)) || {_, File} <- JSONs],
    _C = [ mochijson2:encode(mochijson2:decode(File)) || {_, File} <- JSONs],
    _D = [ jsx:encode(jsx:decode(File)) || {_, File} <- JSONs],
    NProc = ?NPROC,
    ResultsDeep = [ parallel(NProc,
                             fun (I) ->
                                     [ begin
                                           T = {ParserName, TestName, size(JSON),
                                                bench(EncFun, DecFun, JSON), I, NProc},
                                           io:format("~s ~s done~n", [ParserName, TestName]),
                                           T
                                       end || {TestName, JSON} <- JSONs ]
                             end)
                    || {ParserName, EncFun, DecFun} <- ?PARSERS],
    Results = lists:flatten(ResultsDeep),
    format_results(Results),
    init:stop().

parallel(NProc, BenchF) ->
    Ref = make_ref(),
    Self = self(),
    Pids = [ spawn_link(fun () -> worker(Self, Ref, BenchF, I) end) || I <- lists:seq(1, NProc) ],
    collect(Ref, Pids).

worker(Parent, Ref, BenchF, I) ->
    worker(Parent, Ref, BenchF, I, 1).

worker(Parent, Ref, BenchF, I, N) ->
    R = BenchF(I),
    if
        N == 1 -> Parent ! {results, Ref, R};
        N /= 1 -> ok
    end,
    receive
        stop -> Parent ! {stopped, Ref}
        after 0 -> worker(Parent, Ref, BenchF, N + 1)
    end.

collect(Ref, Pids) ->
    Results = [ receive {results, Ref, R} -> R end || _ <- Pids ],
    [ P ! stop || P <- Pids ],
    [ receive {stopped, Ref} -> ok end || _ <- Pids ],
    io:format("all workers stopped\n", []),
    Results.

bench(EncFun, DecFun, TestJSON) ->
    DecThunk = fun() -> times(DecFun, TestJSON, ?NUM_TESTS) end,
    {DecTime, Decoded} = timer:tc(DecThunk),
    EncThunk = fun() -> times(EncFun, Decoded, ?NUM_TESTS) end,
    {EncTime, _} = timer:tc(EncThunk),
    {EncTime, DecTime}.

format_results(Results) ->
    Header = io_lib:format("\"Parser\","
                           "\"Test\","
                           "\"TestSize\","
                           "\"ResultEnc\","
                           "\"ResultDec\","
                           "\"Proc\","
                           "\"NProc\"~n", []),
    Out = [Header |
           [io_lib:format("\"~s\",\"~s (~pb)\",~p,~p,~p,~p,~p~n",
                          [Parser, Test, TestSize, TestSize,
                           round(ResultEnc / ?NUM_TESTS),
                           round(ResultDec / ?NUM_TESTS),
                           Proc, NProc])
            || {Parser, Test, TestSize, {ResultEnc, ResultDec}, Proc, NProc} <- Results]],
    file:write_file(?RESULTS_FILE, lists:flatten(Out)).

times(F, X,  0) -> F(X);
times(F, X, N) -> F(X), times(F, X, N-1).
