%% Responsible for sending random updates, and keeping
%% a snapshot of it's state.
%% Respnosible for sending a state to any client that requests it.
-module(pub).
-define(STEP, 100).
-define(DELAY, 100).
-define(TIMEOUT, 1000).
-record(state, {kv=maps:new(), seq_num=0}).
-record(pub_state, {subs=[], count=0}).
-export([main/0]).
-define(LIMIT,100).

main() ->
	say("[PUB] Spawning publisher"),
	Publisher = spawn_link(fun() -> publisher(#pub_state{}) end),
	say("[PUB] Spawning state manager"),
	StateManager = spawn_link(fun() -> state_manager(#state{}) end),
	say("[PUB] Spawning event generator"),
	spawn_link(fun() -> event_generator(Publisher, StateManager, 1) end),
	{ok, Publisher, StateManager}.	

event_generator(Publisher, StateManager, C) ->
	%% Generate a random event
	Event = generate_event(C),
	% say("Generated event: ~p~n", [Event]),
	%% Send to publisher
	Publisher ! {publish, {event, Event}},
	%% Save to state
	StateManager ! {event, Event},
	timer:sleep(?STEP),
	case C of
		?LIMIT -> say("[PUB] Done generating events!");
		_ -> event_generator(Publisher, StateManager, C + 1)
	end.

publisher(#pub_state{subs=Subs, count=C}=State) ->
	receive
		{subscribe, Pid} ->
			say("[PUB] Subscription received!"),
			publisher(State#pub_state{subs=[Pid|Subs]});
		{publish, Msg} ->
			% say("[PUB] Publishing msg: ~p~n", [Msg]),
			publish(Msg, Subs),
			publisher(State#pub_state{count=C+1})
	end.

publish(_, []) -> ok;
publish(Msg, [Sub|Subs]) ->
	Sub ! Msg,
	publish(Msg, Subs).

state_manager(#state{kv=Kv}=State) ->
	receive 
		{event, {I, Key, Value}} ->
			State2 = State#state{kv=maps:put(Key, Value, Kv), seq_num=I},
			state_manager(State2);
		{icanhaz, From} ->
			say("[PUB] Snapshot request received. Current state: ~p~n", [Kv]),
			send_state(State, From),
			say("[PUB] Snapshot state sent. ~n"),
			state_manager(State)
	after ?TIMEOUT ->
		say("[PUB] State Stable: ~p~n", [Kv])
	end.

send_state(#state{kv=Kv, seq_num=SeqNum}, To) ->
	send_snapshot_items(To, maps:to_list(Kv)),
	To ! {kthxbai, SeqNum}.

send_snapshot_items(_, []) -> ok;
send_snapshot_items(To, [Item|Items]) ->
	%% Simulate slow snapshot sending
	timer:sleep(?DELAY),
	To ! {snapshot_item, Item},

	send_snapshot_items(To, Items).

%% Generates {X,Y} where  0 <= X <= 10 and     100 <= Y <= 110
generate_event(N) ->
	{N, round( rand:uniform() * 10), 
	 round( rand:uniform() * 10) + 100}.

say(Msg) ->
	io:format(Msg ++ "~n").

say(Msg, Args) ->
	io:format(Msg, Args).