%%%
% This module provides a convenient interface for using bloomd.
% It uses a gen_server per connection to bloomd, and abstracts
% that away using this interface.
%%%
-module(bloomd).
-export([new/0, new/2, new/3, filter/2, create/3, create/2,
        list/1, filter_info/1, check/2, multi/2, set/2,
        bulk/2, drop/1, close/1, clear/1, info/1, flush/1]).

-record(conn, {
        % Pid of the gen_server
        pid,

        % Enables hashing of keys, which is
        % required if they will contain spaces or newlines
        hash_keys
    }).

-record(filter, {
        % Connection this belongs to
        conn,
        % The name of the filter
        name}).


% Connects to localhost, default settings with hashing
new() -> new("127.0.0.1", 8125, true).

% Connects to a server and port, hashing enabled
new(Server, Port) -> new(Server, Port, true).

% Connects to a server and port with given hash settings
-spec new(string(), integer(), boolean()) -> #conn{}.
new(Server, Port, HashKeys) ->
    {ok, Pid} = bloomd_conn:start_link(Server, Port),
    #conn{pid=Pid, hash_keys=HashKeys}.

% Returns a filter record for the given connection
filter(Conn, Filter) ->
    #filter{conn=Conn, name=Filter}.


%%%
% Non-filter specific commands
%%%

% Given a connection, creates a new filter
-type create_option() :: {capacity, integer()} | {in_memory, integer()} | {probability, float()}.
-spec create(#conn{}, string(), [create_option()]) -> done | exists | {error, command_failed}.
create(Conn, Filter, Options) ->
    gen_server:call(Conn#conn.pid, {create, Filter, Options}).

% Create with default options
create(Conn, Filter) -> create(Conn, Filter, []).

% Lists the existing filters. Returns a proplist of the
% filter name to a 'FilterInfo' line.
list(Conn) ->
    gen_server:call(Conn#conn.pid, {list}).

% Parses a filter info line into a proplist. This
% should be the line that is returned as the corresponding
% value of the list command
filter_info(Line) ->
    [Prob, Bytes, Capacity, Size] = binary:split(Line, [<<" ">>]),
    Info = [{probability, list_to_float(binary_to_list(Prob))},
            {bytes, list_to_integer(binary_to_list(Bytes))},
            {capacity, list_to_integer(binary_to_list(Capacity))},
            {size, list_to_integer(binary_to_list(Size))}
            ],
    Info.


%%%
% Filter specific commands
%%%

% Checks for a given key
-spec check(#filter{}, iolist()) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
check(Filt, Key) ->
    AdjKey = Key,
    gen_server:call(Filt#filter.conn#conn.pid, {check, Filt#filter.name, AdjKey}).

% Checks for multiple keys
-spec multi(#filter{}, [iolist()]) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
multi(Filt, Keys) ->
    AdjKeys = Keys,
    gen_server:call(Filt#filter.conn#conn.pid, {multi, Filt#filter.name, AdjKeys}).

% Sets a given key
-spec set(#filter{}, iolist()) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
set(Filt, Key) ->
    AdjKey = Key,
    gen_server:call(Filt#filter.conn#conn.pid, {set, Filt#filter.name, AdjKey}).

% Sets a set of keys
-spec bulk(#filter{}, [iolist()]) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
bulk(Filt, Keys) ->
    AdjKeys = Keys,
    gen_server:call(Filt#filter.conn#conn.pid, {bulk, Filt#filter.name, AdjKeys}).

% Deletes a filter from memory and disk
-spec drop(#filter{}) -> {error, no_filter} | {error, command_failed} | done.
drop(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {drop, Filt#filter.name}).

% Removes a filter from memory, leaves it in bloomd
-spec close(#filter{}) -> {error, no_filter} | {error, command_failed} | done.
close(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {close, Filt#filter.name}).

% For non paged in filters, removes them from bloomd, but leaves on disk
-spec clear(#filter{}) -> {error, no_filter} | {error, not_proxied} | {error, command_failed} | done.
clear(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {clear, Filt#filter.name}).

-spec info(#filter{}) -> {error, no_filter} | {error, command_failed} | list().
info(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {info, Filt#filter.name}).


%%%
% Hybrid, handles conn or filter
%%%

% Flushes to disk
-spec flush(#conn{} | #filter{}) -> done | {error, no_filter} | {error, command_failed}.
flush(Handle) ->
    case Handle of
        #conn{} ->
            gen_server:call(Handle#conn.pid, {flush, undefined});
        #filter{} ->
            gen_server:call(Handle#filter.conn#conn.pid, {flush, Handle#filter.name})
    end.


