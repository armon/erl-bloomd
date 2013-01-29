erl-bloomd
==========

erl-bloomd provides an Erlang client library to interface with
bloomd servers. The library does not support any advanced features
such as filter discovery and sharding. The library does support
key hashing and automatically does pipelining of all requests.

Features
--------


* Provides a simple API for using bloomd
* Command pipelining to reduce latency
* Connection multiplexing for multiple clients
* Key-hashing to support arbitrary keys


Install
-------

Add as a dependency in rebar, does not require any external libraries.

Example
------

Using erl-bloomd is very simple:

    # Create a new connection
    C = bloomd:new("east-bloomd-001", 8673).

    # Get a list of filters, with their info
    Filters = [{F, bloomd:filter_info(I)} || {F, I} <- bloomd:list(C)].

    # Get a handle to the "test" filter, create it if necessary
    F = bloomd:filter(C, "test").
    bloomd:create(F).

    # Set a property and check it exists
    bloomd:set(F, "Test Key!").
    true = bloomd:check(F, "Test Key!").


Using pipelining is transparent:

    # Create a new connection
    C = bloomd:new("east-bloomd-001", 8673).

    # Get a handle to the "test" filter, create it if necessary
    F = bloomd:filter(C, "test").
    bloomd:create(F).

    # Spawn and run in parallel
    spawn(fun() -> bloomd:multi(F, ["a", "b", "c"]) end).
    spawn(fun() -> bloomd:bulk(F, ["d", "e", "f"]) end).


