-module(go).

-export([main/0]).

-define(WAIT, 2000).

main() ->
	compile:file(pub),
	compile:file(sub),
	%% Spawn the pub server
	{ok, Publisher, StateManager} = pub:main(),

	%% Wait a bit so we miss some messages
	io:format("[CONTROLLER] Missing a few events on purpose...~n"),
	timer:sleep(?WAIT),

	%% Now spawn the subscriber
	sub:main(Publisher, StateManager).