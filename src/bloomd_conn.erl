%%%
% This module provides a gen_server implementation that is used
% to interface with a remote bloomd server. Internally, it is non-blocking
% so that clients can pipeline commands and minimize network latency.
%%
-module(bloomd_conn).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/0, start_link/2]).

-record(state, {
        sock,
        server,
        port,

        % Queue of commands, FIFO
        % Each item is composed of {Client, Cmd}
        cmd_queue,

        % Buffer of incoming data
        buf=[]
        }).

start_link() -> start_link("127.0.0.1", 8125).
start_link(Server, Port) ->
    gen_server:start_link(?MODULE, [Server, Port], []).


%%%
% Gen Server API
%%%

init([Server, Port]) ->
    % Attempt to connect to the server
    {ok, Sock} = connect(Server, Port),

    % Create a command queue
    Queue = queue:new(),

    % Initialize our state
    State = #state{sock=Sock, server=Server, port=Port, cmd_queue=Queue},
    {ok, State}.


handle_call(Msg, From, State) ->
    % Attempt to format the command
    case format_cmd(Msg) of
        Cmd when is_list(Cmd) ->
            case get_socket(State) of
                {ok, Sock, NS1} ->
                    % Try the send
                    case gen_tcp:send(Sock, Cmd) of
                        % Enqueue the response for later
                        ok ->
                            NS2 = enqueue_command(NS1, From, Msg),
                            {noreply, NS2};

                        % Fail all pending commands, and this one
                        {error, _Reason} ->
                            NS2 = fail_all_queued(NS1),
                            {reply, {error, command_failed}, NS2}
                    end;

                % If we can't connect, fail immediately
                {error, NS} ->
                    {reply, {error, command_failed}, NS}
            end;

        % Fail if the command is bad
        error -> {reply, {error, bad_cmd}, State}
    end.


% Gracefully stop if we get a cast
handle_cast(stop, State) ->
    {stop, normal, State}.


% Handle incoming data
handle_info({tcp, _Sock, Data}, State) ->
    % Merge the buffers
    Buf = iolist_to_binary([State#state.buf, Data]),
    NS = process_buffer(State#state{buf=Buf}),
    {noreply, NS};

% Handle a closed socket
handle_info({tcp_closed, _Sock}, State) ->
    NS = fail_all_queued(State),
    {noreply, NS};

% Handle tcp errors
handle_info({tcp_error, _Sock, _Reason}, State) ->
    NS = fail_all_queued(State),
    {noreply, NS}.


terminate(_Reason, State) ->
    % Close our socket before terminating
    gen_tcp:close(State#state.sock), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%
% Internal api
%%%


% Attemps a connection to the given server and port
connect(State) ->
    connect(State#state.server, State#state.port).

connect(Server, Port) ->
    gen_tcp:connect(Server, Port, [binary, {active, true},
                                  {nodelay, true}, {keepalive, true}]).


% Attempts to get or re-create the socket
-spec get_socket(#state{}) -> {ok, term(), term()} | {error, term()}.
get_socket(State) ->
    case State#state.sock of
        undefined ->
            case connect(State) of
                {ok, S} -> {ok, S, State#state{sock=S}};
                _ -> {error, State}
            end;

        S -> {ok, S, State}
    end.


% Enqueues a new client command
enqueue_command(State, Client, Cmd) ->
    % Get the first client in the queue
    Q = State#state.cmd_queue,

    % Add to the end of the queue
    NQ = queue:in({Client, Cmd}, Q),
    State#state{cmd_queue=NQ}.


% Fails all the queued commands, and returns a state
% with no commands or data queued.
fail_all_queued(State) ->
    % Fail each of the commands in the queue
    lists:foldl(fun({Client, _CMD}) ->
        gen_server:reply(Client, {error, command_failed})
    end, queue:to_list(State#state.cmd_queue)),

    % Reset the state
    State#state{sock=undefined, cmd_queue=queue:new(), buf=[]}.


% Responds to the first client in the queue
respond_to_first(State, Response) ->
    % Get the first client in the queue
    Q = State#state.cmd_queue,
    {Client, _Cmd} = queue:head(Q),

    % Reply to the client
    gen_server:reply(Client, Response),

    % Drop that command from the queue
    State#state{cmd_queue=queue:tail(Q)}.


% Parses the incoming command buffer
process_buffer(State=#state{buf=Buf}) ->
    case binary:split(Buf, [<<"\n">>]) of
        % No further commands can be processed, return remaining buffer
        [_] -> State;

        % Process each available command
        [Line, Remain] ->
            S1 = process_response(State#state{buf=Remain}, Line),
            process_buffer(S1)
    end.


% Processes a line of boolean Yes / No results
process_bool_line(State, Accum, <<"">>) ->
    Results = lists:reverse(Accum),
    respond_to_first(State, {ok, Results});

process_bool_line(State, Accum, <<" Yes", Rest/binary>>) ->
    process_bool_line(State, [true | Accum], Rest);

process_bool_line(State, Accum, <<" No", Rest/binary>>) ->
    process_bool_line(State, [false | Accum], Rest).


% Processes a response block, which is wrapped in a START/END
process_response_block(State=#state{buf=Buf}) ->
    case binary:split(Buf, [<<"END\n">>]) of
        % There is no end block received yet, we need to wait for more data
        [_] ->
            % Re-add the start block to the buffer
            NewBuf = iolist_to_binary([<<"START\n">>, Buf]),
            State#state{buf=NewBuf};

        % Split into the inner and outer blocks
        [Inner, Remain] ->
            % Get the inner lines of the block
            InnerLines = binary:split(Inner, [<<"\n">>]),

            % Convert to a property list
            ResultList = lists:foldl(fun(Line) ->
                [Key, Val] = binary:split(Line, [<<" ">>]),
                {Key, Val}
            end, InnerLines),

            % Respond, and seek the buffer to the remaining input
            respond_to_first(State#state{buf=Remain}, ResultList)
    end.


% Parses a single response
process_response(State, <<"Yes", Rest/binary>>) ->
    process_bool_line(State, [true], Rest);

process_response(State, <<"No", Rest/binary>>) ->
    process_bool_line(State, [false], Rest);

process_response(State, <<"Done">>) ->
    respond_to_first(State, done);

process_response(State, <<"START">>) ->
    process_response_block(State);

process_response(State, <<"Exists">>) ->
    respond_to_first(State, exists);

process_response(State, <<"Filter does not exist">>) ->
    respond_to_first(State, {error, no_filter});

process_response(State, <<"Filter is not proxied", _Rest/binary>>) ->
    respond_to_first(State, {error, not_proxied});

process_response(State, <<"Internal Error">>) ->
    respond_to_first(State, {error, internal_error});

process_response(State, <<"Client Error: ", Err/binary>>) ->
    respond_to_first(State, {error, {client_error, Err}}).


% Formats a command to be sent to bloomd
format_cmd({check, Filter, Key}) -> [<<"c ">>, Filter, <<" ">>, Key, <<"\n">>];
format_cmd({multi, Filter, Keys}) when Keys =:= [] ->
    KeyList = [[<<" ">>, K] || K <- Keys],
    [<<"m ">>, Filter, KeyList, <<"\n">>];
format_cmd({set, Filter, Key}) -> [<<"s ">>, Filter, <<" ">>, Key, <<"\n">>];
format_cmd({bulk, Filter, Keys}) when Keys =:= [] ->
    KeyList = [[<<" ">>, K] || K <- Keys],
    [<<"b ">>, Filter, KeyList, <<"\n">>];
format_cmd({list}) -> [<<"list\n">>];
format_cmd({create, Filter, Options}) ->
    % Get all the options
    Op1 = case proplists:get_value(capacity, Options) of
        undefined -> [];
        Cap -> [<<" capacity=">>,io_lib:format("~p", [Cap])]
    end,
    Op2 = case proplists:get_value(probability, Options) of
        undefined -> Op1;
        Prob -> [Op1, [<<" prob=">>, io_lib:format("~.12f", [Prob])]]
    end,
    Op3 = case proplists:get_value(in_memory, Options) of
        undefined -> Op2;
        InMem ->  [Op2, [<<" in_memory=">>, io_lib:format("~p", [InMem])]]
    end,

    % Create the command
    [<<"create ">>, Filter, Op3, <<"\n">>];
format_cmd({drop, Filter}) -> [<<"drop ">>, Filter, <<"\n">>];
format_cmd({close, Filter}) -> [<<"close ">>, Filter, <<"\n">>];
format_cmd({clear, Filter}) -> [<<"clear ">>, Filter, <<"\n">>];
format_cmd({info, Filter}) -> [<<"info ">>, Filter, <<"\n">>];
format_cmd({flush, undefined}) -> [<<"flush\n">>];
format_cmd({flush, Filter}) -> [<<"flush ">>, Filter, <<"\n">>];
format_cmd(_) -> error.

