-module(rabbit_fifo_q).

-include("rabbit_fifo.hrl").
-export([
         new/0,
         in/3,
         out/1,
         get/1,
         len/1,
         from_lqueue/1,
         normalize/2,
         get_lowest_index/1
         ]).

-define(WEIGHT, 2).

%% a simple weighted priority queue with only two priorities

-record(?MODULE, {hi = queue:new() :: queue:queue(),
                  lo = queue:new() :: queue:queue(),
                  len = 0 :: non_neg_integer(),
                  dequeue_counter = 0 :: non_neg_integer()}).

-opaque state() :: #?MODULE{}.

-export_type([state/0]).

-spec new() -> state().
new() ->
    #?MODULE{}.

-spec in(hi | lo, msg(), state()) -> state().
in(hi, Item, #?MODULE{hi = Hi, len = Len} = State) ->
    State#?MODULE{hi = queue:in(Item, Hi),
                  len = Len + 1};
in(lo, Item, #?MODULE{lo = Lo, len = Len} = State) ->
    State#?MODULE{lo = queue:in(Item, Lo),
                  len = Len + 1}.

-spec out(state()) ->
    {empty, state()} | {hi | lo, msg(), state()}.
out(#?MODULE{len = 0} = S) ->
    {empty, S};
out(#?MODULE{hi = Hi0,
             lo = Lo0,
             len = Len,
             dequeue_counter = C} = State) ->
    case ?WEIGHT == C of
        true ->
            %% try lo before hi
            case queue:out(Lo0) of
                {empty, _} ->
                    {{value, Ret}, Hi} = queue:out(Hi0),
                    {hi, Ret, State#?MODULE{hi = Hi,
                                            dequeue_counter = 0,
                                            len = Len - 1}};
                {{value, Ret}, Lo} ->
                    {lo, Ret, State#?MODULE{lo = Lo,
                                            dequeue_counter = 0,
                                            len = Len - 1}}
            end;
        false ->
            case queue:out(Hi0) of
                {empty, _} ->
                    {{value, Ret}, Lo} = queue:out(Lo0),
                    {lo, Ret, State#?MODULE{lo = Lo,
                                            dequeue_counter = C + 1,
                                            len = Len - 1}};
                {{value, Ret}, Hi} ->
                    {hi, Ret, State#?MODULE{hi = Hi,
                                            dequeue_counter = C + 1,
                                            len = Len - 1}}
            end
    end.

-spec get(state()) -> empty | msg().
get(#?MODULE{len = 0}) ->
    empty;
get(#?MODULE{hi = Hi0,
             lo = Lo0,
             dequeue_counter = C}) ->
    case ?WEIGHT == C of
        true ->
            %% try lo before hi
            case queue:peek(Lo0) of
                empty ->
                    {value, Ret} = queue:peek(Hi0),
                    Ret;
                {value, Ret} ->
                    Ret
            end;
        false ->
            case queue:peek(Hi0) of
                empty ->
                    {value, Ret} = queue:peek(Lo0),
                    Ret;
                {value, Ret} ->
                    Ret
            end
    end.

-spec len(state()) -> non_neg_integer().
len(#?MODULE{len = Len}) ->
    Len.

-spec from_lqueue(lqueue:lqueue(msg())) -> state().
from_lqueue(LQ) ->
    lqueue:fold(fun (Item, Acc) ->
                        in(lo, Item, Acc)
                end, new(), LQ).

-spec normalize(state(), state()) -> state().
normalize(Q0, Acc) ->
    case out(Q0) of
        {empty, _} ->
            Acc;
        {P, Msg, Q} ->
            normalize(Q, in(P, Msg, Acc))
    end.

-spec get_lowest_index(state()) -> undefined | ra:index().
get_lowest_index(#?MODULE{len = 0}) ->
    undefined;
get_lowest_index(#?MODULE{hi = Hi, lo = Lo}) ->
    case queue:peek(Hi) of
        empty ->
            {value, ?MSG(LoIdx, _)} = queue:peek(Lo),
            LoIdx;
        {value, ?MSG(HiIdx, _)} ->
            case queue:peek(Lo) of
                {value, ?MSG(LoIdx, _)} ->
                    max(HiIdx, LoIdx);
                empty ->
                    HiIdx
            end
    end.

%% internals


