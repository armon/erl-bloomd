-module(bloomd_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

filter_info_test() ->
    Line = <<"0.001000 1024000 512000 256">>,
    Out = bloomd:filter_info(Line),
    Expect = [{probability, 0.001}, {bytes, 1024000}, {capacity, 512000}, {size, 256}],
    ?assertEqual(Expect, Out).


info_proplist_test() ->
    Inp = [
            {<<"capacity">>,<<"1000">>},
            {<<"size">>, <<"0">>},
            {<<"probability">>, <<"0.05">>}
    ],
    Out = bloomd:info_proplist(Inp),
    Expect = [{capacity, 1000}, {size, 0}, {probability, 0.05}],
    ?assertEqual(Expect, Out).

