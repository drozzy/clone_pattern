Reliable Pub-Sub (Clone Pattern)
================================

This is an implementation of reliable pub-sub (clone pattern) from ZeroMQ guide,
in pure Erlang.

See here: http://zguide.zeromq.org/page:all#toc119


About
-----
This examples attemps to create a scenario where the subscriber joins late,
so as to miss some of the packets sent out by the publisher.

After that, he catches up, but makes sure to ignore any packets that are old.

For the theory behind how this works, see the original [zguide chapter](http://zguide.zeromq.org/page:all#toc119).

Usage
-----

Start an erlang shell from the current directory, compile the `go` module, and run it:

    $ erl
    > c(go).
    > go:main().

