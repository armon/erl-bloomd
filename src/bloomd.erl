%%%
% This module provides a convenient interface for using bloomd.
% It uses a gen_server per connection to bloomd, and abstracts
% that away using this interface.
%%%
-module(bloomd).
-export([new/0, new/1, new/2, new/3, new/4, filter/2, create/3, create/2,
        create/1, list/1, filter_info/1, info_proplist/1, check/2, multi/2, set/2,
        bulk/2, drop/1, close/1, clear/1, info/1, flush/1,
        close_conn/1, set_timeout/2]).

% Default timeout for bloomd in milliseconds
-define(TIMEOUT, 300000).

-record(conn, {
        % Pid of the gen_server
        pid,

        % Timeout for calls
        timeout=?TIMEOUT,

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
new() -> new("127.0.0.1", 8673, true).

% Connects to host, default settings with hashing
new(Server) -> new(Server, 8673, true).

% Connects to a server and port, hashing enabled
new(Server, Port) -> new(Server, Port, true).

% Connects to a server and port with given hash settings
-spec new(string(), integer(), boolean()) -> #conn{}.
new(Server, Port, HashKeys) -> new(Server, Port, HashKeys, ?TIMEOUT).

% Connects to a server and port with given hash settings
-spec new(string(), integer(), boolean(), integer() | infinity) -> #conn{}.
new(Server, Port, HashKeys, Timeout) ->
    {ok, Pid} = bloomd_conn:start_link(Server, Port),
    unlink(Pid),
    #conn{pid=Pid, hash_keys=HashKeys, timeout=Timeout}.

% Closes a connection
-spec close_conn(#conn{}) -> ok.
close_conn(Conn) ->
    gen_server:cast(Conn#conn.pid, stop).

% Sets the timeout on the connection
-spec set_timeout(#conn{}, integer() | infinity) -> #conn{}.
set_timeout(Conn, Timeout) ->
    Conn#conn{timeout=Timeout}.

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
    gen_server:call(Conn#conn.pid, {create, Filter, Options}, Conn#conn.timeout).

% Create with default options
create(Conn, Filter) -> create(Conn, Filter, []).

% Create with a filter object
create(Filter) ->
    Conn = Filter#filter.conn,
    Name = Filter#filter.name,
    create(Conn, Name, []).

% Lists the existing filters. Returns a proplist of the
% filter name to a 'FilterInfo' line.
list(Conn) ->
    gen_server:call(Conn#conn.pid, {list}, Conn#conn.timeout).

% Parses a filter info line into a proplist. This
% should be the line that is returned as the corresponding
% value of the list command
filter_info(Line) ->
    [Prob, Bytes, Capacity, Size] = binary:split(Line, [<<" ">>], [global]),
    Info = [{probability, list_to_float(binary_to_list(Prob))},
            {bytes, list_to_integer(binary_to_list(Bytes))},
            {capacity, list_to_integer(binary_to_list(Capacity))},
            {size, list_to_integer(binary_to_list(Size))}
            ],
    Info.

% Parses the proplist that is returned from
% bloomd:info() into a more usable proplist form.
info_proplist(InfoBlock) ->
    lists:map(fun({K, V}) ->
        Key = binary_to_atom(K, utf8),
        Val = case Key of
            probability -> list_to_float(binary_to_list(V));
            _ -> list_to_integer(binary_to_list(V))
        end,
        {Key, Val}
    end, InfoBlock).


%%%
% Filter specific commands
%%%

% Hashes the keys using SHA1 if key hashing is enabled,
% otherwise does not modify them
adjust_key(Conn, Key) ->
    case Conn#conn.hash_keys of
        false -> Key;
        true -> bin_to_hex(crypto:hash(sha, Key))
    end.
adjust_keys(Conn, Keys) ->
    case Conn#conn.hash_keys of
        false -> Keys;
        true -> [bin_to_hex(crypto:hash(sha, K)) || K <- Keys]
    end.

% Checks for a given key
-spec check(#filter{}, iolist()) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
check(Filt, Key) ->
    AdjKey = adjust_key(Filt#filter.conn, Key),
    gen_server:call(Filt#filter.conn#conn.pid, {check, Filt#filter.name, AdjKey},
                    Filt#filter.conn#conn.timeout).

% Checks for multiple keys
-spec multi(#filter{}, [iolist()]) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
multi(Filt, Keys) ->
    AdjKeys = adjust_keys(Filt#filter.conn, Keys),
    gen_server:call(Filt#filter.conn#conn.pid, {multi, Filt#filter.name, AdjKeys},
                   Filt#filter.conn#conn.timeout).

% Sets a given key
-spec set(#filter{}, iolist()) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
set(Filt, Key) ->
    AdjKey = adjust_key(Filt#filter.conn, Key),
    gen_server:call(Filt#filter.conn#conn.pid, {set, Filt#filter.name, AdjKey},
                   Filt#filter.conn#conn.timeout).

% Sets a set of keys
-spec bulk(#filter{}, [iolist()]) -> {ok, [boolean()]} | {error, no_filter} | {error, command_failed}.
bulk(Filt, Keys) ->
    AdjKeys = adjust_keys(Filt#filter.conn, Keys),
    gen_server:call(Filt#filter.conn#conn.pid, {bulk, Filt#filter.name, AdjKeys},
                   Filt#filter.conn#conn.timeout).

% Deletes a filter from memory and disk
-spec drop(#filter{}) -> {error, no_filter} | {error, command_failed} | done.
drop(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {drop, Filt#filter.name},
                   Filt#filter.conn#conn.timeout).

% Removes a filter from memory, leaves it in bloomd
-spec close(#filter{}) -> {error, no_filter} | {error, command_failed} | done.
close(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {close, Filt#filter.name},
                   Filt#filter.conn#conn.timeout).

% For non paged in filters, removes them from bloomd, but leaves on disk
-spec clear(#filter{}) -> {error, no_filter} | {error, not_proxied} | {error, command_failed} | done.
clear(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {clear, Filt#filter.name},
                   Filt#filter.conn#conn.timeout).

-spec info(#filter{}) -> {error, no_filter} | {error, command_failed} | list().
info(Filt) ->
    gen_server:call(Filt#filter.conn#conn.pid, {info, Filt#filter.name},
                   Filt#filter.conn#conn.timeout).


%%%
% Hybrid, handles conn or filter
%%%

% Flushes to disk
-spec flush(#conn{} | #filter{}) -> done | {error, no_filter} | {error, command_failed}.
flush(Handle) ->
    case Handle of
        #conn{} ->
            gen_server:call(Handle#conn.pid, {flush, undefined}, Handle#conn.timeout);
        #filter{} ->
            gen_server:call(Handle#filter.conn#conn.pid, {flush, Handle#filter.name},
                           Handle#filter.conn#conn.timeout)
    end.


%%%
% Converts binary to hex
%%%

bin_to_hex(Bin) -> bin_to_hex(Bin, []).

bin_to_hex(<<>>, Accum) -> lists:reverse(Accum);
bin_to_hex(<<Char1:4, Char2:4, Rest/binary>>, Accum) ->
    Letter1 = case Char1 of
        Num1 when Num1 < 10 -> $0 + Num1;
        _ -> $a + (Char1 - 10)
    end,
    Letter2 = case Char2 of
        Num2 when Num2 < 10 -> $0 + Num2;
        _ -> $a + (Char2 - 10)
    end,
    bin_to_hex(Rest, [Letter2, Letter1 | Accum]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bin_to_hex_test() ->
    B = <<"\x00\x16\xFF">>,
    ?assertEqual("0016ff", bin_to_hex(B)).

-endif.

