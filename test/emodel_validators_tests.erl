-module(emodel_validators_tests).

-include_lib("eunit/include/eunit.hrl").


get_validator(Type) ->
    get_validator(Type, #{
       validators => fun get_validator/2
    }).

get_validator(Type, Opts) ->
    emodel_validators:get_validator(Type, Opts).

fun1_test() ->
    Validator = get_validator(
        fun (a) -> ok;
            (_) -> {error, <<"unknown">>}
        end),
    ?assertEqual(ok, Validator(a, undefined)),
    ?assertEqual({error, <<"unknown">>}, Validator(b, undefined)).

fun2_test() ->
    Validator = get_validator(
        fun (a,a) -> ok;
            (_,_) -> {error, <<"unknown">>}
        end),
    ?assertEqual(ok, Validator(a, a)),
    ?assertEqual({error, <<"unknown">>}, Validator(a, b)),
    ?assertEqual({error, <<"unknown">>}, Validator(b, undefined)).

'>_test'() ->
    Validator = get_validator({'>', 10}),
    ?assertEqual(ok, Validator(11, undefined)),
    ?assertEqual({error, <<"must be greater than 10">>}, Validator(10, undefined)),
    ?assertEqual({error, <<"must be greater than 10">>}, Validator(1, undefined)).

'>=_test'() ->
    Validator = get_validator({'>=', 10}),
    ?assertEqual(ok, Validator(11, undefined)),
    ?assertEqual(ok, Validator(10, undefined)),
    ?assertEqual({error, <<"must be greater than or equals to 10">>}, Validator(1, undefined)).

'<_test'() ->
    Validator = get_validator({'<', 10}),
    ?assertEqual(ok, Validator(1, undefined)),
    ?assertEqual({error, <<"must be less than 10">>}, Validator(10, undefined)),
    ?assertEqual({error, <<"must be less than 10">>}, Validator(11, undefined)).

'=<_test'() ->
    Validator = get_validator({'=<', 10}),
    ?assertEqual(ok, Validator(1, undefined)),
    ?assertEqual(ok, Validator(10, undefined)),
    ?assertEqual({error, <<"must be less than or equals to 10">>}, Validator(11, undefined)).

non_empty_test() ->
    Validator = get_validator(non_empty),
    ?assertEqual(ok, Validator(<<"a">>, undefined)),
    ?assertEqual(ok, Validator([1], undefined)),
    ?assertEqual(ok, Validator(#{a => 1}, undefined)),

    ?assertEqual({error, <<"is empty">>}, Validator(<<"">>, undefined)),
    ?assertEqual({error, <<"is empty">>}, Validator([], undefined)),
    ?assertEqual({error, <<"is empty">>}, Validator(#{}, undefined)).

each_test() ->
    Validator = get_validator({each, [{'>', 0}, {'<', 10}]}),
    ?assertEqual(ok, Validator([1], undefined)),
    ?assertEqual({error, [
        {1, <<"must be greater than 0">>},
        {3, <<"must be less than 10">>}
    ]}, Validator([-1, 1, 11], undefined)).

unique_test() ->
    Validator = get_validator(unique),
    ?assertEqual(ok, Validator([1,2], undefined)),
    ?assertEqual({error, [
        {3, <<"not unique">>}
    ]}, Validator([1, 2, 1], undefined)).
