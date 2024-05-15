-module(rabbit_fifo_q_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-export([
         ]).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [
     {group, tests}
    ].


all_tests() ->
    [
     basics,
     single_priority_behaves_like_queue
    ].


groups() ->
    [
     {tests, [], all_tests()}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

basics(_Config) ->
    Q0 = rabbit_fifo_q:new(),
    Q1 = lists:foldl(
           fun ({P, I}, Q) ->
                   rabbit_fifo_q:in(P, I, Q)
           end, Q0, [
                     {hi, hi1},
                     {lo, lo1},
                     {hi, hi2},
                     {lo, lo2},
                     {hi, hi3}
                    ]),
    {{value, hi1}, Q2} = rabbit_fifo_q:out(Q1),
    {{value, hi2}, Q3} = rabbit_fifo_q:out(Q2),
    {{value, lo1}, Q4} = rabbit_fifo_q:out(Q3),
    {{value, hi3}, Q5} = rabbit_fifo_q:out(Q4),
    {{value, lo2}, _Q6} = rabbit_fifo_q:out(Q5),
    ok.



-type op() :: {in, integer()} | out.

single_priority_behaves_like_queue(_Config) ->
    run_proper(
      fun () ->
              ?FORALL({P, Ops}, {oneof([hi, lo]), op_gen(256)},
                      queue_prop(P, Ops))
      end, [], 25),
    ok.

queue_prop(P, Ops) ->
    ct:pal("Running queue_prop for ~s", [P]),
    Que = queue:new(),
    Sut = rabbit_fifo_q:new(),
    {Queue, FifoQ} = lists:foldl(
                       fun ({in, V}, {Q0, S0}) ->
                               Q = queue:in(V, Q0),
                               S = rabbit_fifo_q:in(P, V, S0),
                               case queue:len(Q) == rabbit_fifo_q:len(S) of
                                   true ->
                                       {Q, S};
                                   false ->
                                       throw(false)
                               end;
                           (out, {Q0, S0}) ->
                               {V1, Q} = queue:out(Q0),
                               {V2, S} = rabbit_fifo_q:out(S0),
                               case V1 == V2 of
                                   true ->
                                       {Q, S};
                                   false ->
                                       throw(false)
                               end
                       end, {Que, Sut}, Ops),

    queue:len(Queue) == rabbit_fifo_q:len(FifoQ).




%%% helpers

op_gen(Size) ->
    ?LET(Ops,
         resize(Size,
                list(
                  frequency(
                    [
                     {20, {in, non_neg_integer()}},
                     {20, out}
                    ]
                   ))), Ops
        ).

run_proper(Fun, Args, NumTests) ->
    ?assert(
       proper:counterexample(
         erlang:apply(Fun, Args),
         [{numtests, NumTests},
          {on_output, fun(".", _) -> ok; % don't print the '.'s on new lines
                         (F, A) -> ct:pal(?LOW_IMPORTANCE, F, A)
                      end}])).
