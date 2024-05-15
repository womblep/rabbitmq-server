-module(rabbit_fifo_q).

-export([
         new/0,
         in/3,
         out/1,
         get/1,
         len/1,
         from_lqueue/1
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

-spec in(hi | lo, term(), state()) -> state().
in(hi, Item, #?MODULE{hi = Hi, len = Len} = State) ->
    State#?MODULE{hi = queue:in(Item, Hi),
                  len = Len + 1};
in(lo, Item, #?MODULE{lo = Lo, len = Len} = State) ->
    State#?MODULE{lo = queue:in(Item, Lo),
                  len = Len + 1}.

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
                    {{value, _} = Ret, Hi} = queue:out(Hi0),
                    {Ret, State#?MODULE{hi = Hi,
                                        dequeue_counter = 0,
                                        len = Len - 1}};
                {Ret, Lo} ->
                    {Ret, State#?MODULE{lo = Lo,
                                        dequeue_counter = 0,
                                        len = Len - 1}}
            end;
        false ->
            case queue:out(Hi0) of
                {empty, _} ->
                    {{value, _} = Ret, Lo} = queue:out(Lo0),
                    {Ret, State#?MODULE{lo = Lo,
                                        dequeue_counter = C + 1,
                                        len = Len - 1}};
                {Ret, Hi} ->
                    {Ret, State#?MODULE{hi = Hi,
                                        dequeue_counter = C + 1,
                                        len = Len - 1}}
            end
    end.

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
                    queue:peek(Hi0);
                {value, _} = Ret ->
                    Ret
            end;
        false ->
            case queue:peek(Hi0) of
                empty ->
                    queue:peek(Lo0);
                {value, _} = Ret ->
                    Ret
            end
    end.

len(#?MODULE{len = Len}) ->
    Len.

from_lqueue(LQ) ->
    lqueue:fold(
      fun (Item, Acc) ->
              in(lo, Item, Acc)
      end, new(), LQ).

%% internals


