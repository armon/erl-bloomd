-module(bloomd_conn_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

main_test_() ->
    {foreach,
     spawn,
     fun setup/0,
     fun cleanup/1,
     [
      fun clean_stop/1,
      fun perform_list/1,
      fun close_conn/1,
      fun multi_cmd/1,
      fun partial_response/1,
      fun key_hash/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

new_conn() -> new_conn(false).
new_conn(Hash) ->
    {ok, Sock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Sock),
    Conn = bloomd:new("127.0.0.1", Port, Hash),
    {Sock, Port, Conn}.

clean_stop(_) ->
    ?_test(
        begin
            {_, _, Conn} = new_conn(),
            {conn, Pid, _, _} = Conn,
            process_flag(trap_exit, true),
            link(Pid),
            gen_server:cast(Pid, stop),
            receive
                {'EXIT', Pid, normal} -> ok
            after 500 ->
                ?assertEqual(true, false)
            end
        end
    ).

perform_list(_) ->
    ?_test(
        begin
            % Accept the client
            {Sock, _, Conn} = new_conn(),
            S = self(),
            {ok, Client} = gen_tcp:accept(Sock),

            % Perform a list
            spawn(fun() ->
                Res = bloomd:list(Conn),
                S ! Res
            end),

            % Send the result back
            {ok, Cmd} = gen_tcp:recv(Client, 5),
            ?assertEqual(<<"list\n">>, Cmd),
            gen_tcp:send(Client, <<"START\ntubez 0.001 500 100 0\nEND\n">>),

            % Wait for the result
            receive
                D -> ?assertEqual([{<<"tubez">>, <<"0.001 500 100 0">>}], D)
            after 1000 ->
                ?assertEqual(false, true)
            end
        end
    ).

close_conn(_) ->
    ?_test(
        begin
            % Accept the client
            {Sock, _, Conn} = new_conn(),
            S = self(),
            {ok, Client} = gen_tcp:accept(Sock),

            % Perform a list
            spawn(fun() ->
                Res = bloomd:list(Conn),
                S ! Res
            end),
            spawn(fun() ->
                Res = bloomd:list(Conn),
                S ! Res
            end),

            % Read and kill the conn
            {ok, Cmd} = gen_tcp:recv(Client, 5),
            ?assertEqual(<<"list\n">>, Cmd),
            {ok, Cmd} = gen_tcp:recv(Client, 5),
            ?assertEqual(<<"list\n">>, Cmd),
            gen_tcp:close(Client),

            % Wait for the result
            receive
                D -> ?assertEqual({error, command_failed}, D)
            after 1000 ->
                ?assertEqual(false, true)
            end,
            receive
                D1 -> ?assertEqual({error, command_failed}, D1)
            after 1000 ->
                ?assertEqual(false, true)
            end
        end
    ).

multi_cmd(_) ->
    ?_test(
        begin
            % Accept the client
            {Sock, _, Conn} = new_conn(),
            S = self(),
            {ok, Client} = gen_tcp:accept(Sock),

            % Perform a list
            Pid = spawn_link(fun() ->
                F = bloomd:filter(Conn, "test"),
                Res = bloomd:multi(F, ["a", "b", "c"]),
                S ! Res
            end),
            ?assert(is_pid(Pid)),

            % Send the result back
            {ok, Cmd} = gen_tcp:recv(Client, 13, 1000),
            ?assertEqual(<<"m test a b c\n">>, Cmd),
            gen_tcp:send(Client, <<"Yes No Yes\n">>),

            % Wait for the result
            receive
                D -> ?assertEqual({ok, [true, false, true]}, D)
            after 1000 ->
                ?assertEqual(false, true)
            end
        end
    ).

partial_response(_) ->
    ?_test(
        begin
            % Accept the client
            {Sock, _, Conn} = new_conn(),
            S = self(),
            {ok, Client} = gen_tcp:accept(Sock),

            % Perform a list
            spawn(fun() ->
                Res = bloomd:list(Conn),
                S ! Res
            end),

            % Send the result back
            {ok, Cmd} = gen_tcp:recv(Client, 5),
            ?assertEqual(<<"list\n">>, Cmd),
            gen_tcp:send(Client, <<"START\ntubez 0.001 500 100 0\n">>),
            timer:sleep(250),
            gen_tcp:send(Client, <<"foo 0.001 500 100 0\nEND\n">>),

            % Wait for the result
            receive
                D -> ?assertEqual([{<<"tubez">>, <<"0.001 500 100 0">>},
                                    {<<"foo">>, <<"0.001 500 100 0">>}
                                  ], D)
            after 1000 ->
                ?assertEqual(false, true)
            end
        end
    ).

key_hash(_) ->
    ?_test(
        begin
            % Accept the client
            {Sock, _, Conn} = new_conn(true),
            S = self(),
            {ok, Client} = gen_tcp:accept(Sock),

            % Perform a list
            spawn(fun() ->
                F = bloomd:filter(Conn, "test"),
                Res = bloomd:check(F, "a"),
                S ! Res
            end),

            % Send the result back
            {ok, Cmd} = gen_tcp:recv(Client, 48),
            ?assertEqual(<<"c test 86f7e437faa5a7fce15d1ddcb9eaeaea377667b8\n">>, Cmd),
            gen_tcp:send(Client, <<"No\n">>),

            % Wait for the result
            receive
                D -> ?assertEqual({ok, [false]}, D)
            after 1000 ->
                ?assertEqual(false, true)
            end
        end
    ).

