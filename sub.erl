-module(sub).

-record(state, {kv=maps:new(), last_evt=0}).
-export([main/2]).

-define(TIMEOUT, 1000).

main(Publisher, StateManager) ->
	say("[SUB] Subscribing to publisher"),
	Publisher ! {subscribe, self()},

	say("[SUB] Requesting snapshot"),
	%% Get state snapshot
	StateManager ! {icanhaz, self()},

	{State, Events} = get_snapshot(),
	State2 = apply_events(Events, State),
	say("[SUB] Applied stashed events: ~p~n", [State2#state.kv]),
	say("[SUB] Switching to normal operation"),
	normal(State2).

get_snapshot() -> get_snapshot(#state{}, []).

get_snapshot(#state{kv=Kv}=State, Acc) ->
	receive
		{snapshot_item, {K, V}} ->
			State2 = State#state{kv=maps:put(K,V,Kv)},
			get_snapshot(State2, Acc);
		{kthxbai, I} ->
			say("[SUB] Received snapshot completely. Current state: ~p~n", [State#state.kv]),
			say("[SUB] Snapshot seq num: ~p~n", [I]),
			say("[SUB] Events stashed: ~p~n", [Acc]),
			{State#state{last_evt=I}, lists:reverse(Acc)};
		Other ->
			get_snapshot(State, [Other|Acc])
	end.

apply_events([], State) -> State;
apply_events([{event, {I,_,_}}|Evts], #state{last_evt=J}=State) when I =< J ->
	%% This means it's an old event we already got in a snapshot
	apply_events(Evts, State);
apply_events([{event, {I,K,V}}|Evts], #state{kv=Kv}=State) ->
	%% Apply all the past events
	apply_events(Evts, State#state{kv=maps:put(K,V,Kv), last_evt=I}).

normal(#state{kv=Kv, last_evt=J}=State) ->
	receive
		{event, {I,_,_}=Event} when I =< J ->
			%% This is an old event - ignore it
			say("[SUB] (normal) Ignoring old event: ~p~n", [Event]);
		{event, {I, K,V}} ->
			normal(State#state{last_evt=I, kv=maps:put(K,V, Kv)})
	after ?TIMEOUT ->
		say("[SUB] State Stable: ~p~n",[Kv])
	end.

say(Msg) ->
	io:format(Msg ++ "~n").

say(Msg, Args) ->
	io:format(Msg, Args).