-module(emodel_converters_tests).

-include_lib("eunit/include/eunit.hrl").


get_converter(Type) ->
    get_converter(Type, #{
       converters => fun get_converter/2
    }).

get_converter(Type, Opts) ->
    emodel_converters:get_converter(Type, Opts).

fun1_test() ->
    Converter = get_converter(
        fun (a) -> {ok, a};
            (_) -> {error, <<"unknown">>}
        end),
    ?assertEqual({ok, a}, Converter(a, undefined)),
    ?assertEqual({error, <<"unknown">>}, Converter(b, undefined)).

fun2_test() ->
    Converter = get_converter(
        fun (a,a) -> {ok, a};
            (_,_) -> {error, <<"unknown">>}
        end),
    ?assertEqual({ok, a}, Converter(a, a)),
    ?assertEqual({error, <<"unknown">>}, Converter(a, b)),
    ?assertEqual({error, <<"unknown">>}, Converter(b, undefined)).

interger_test() ->
    Converter = get_converter(integer),
    ?assertEqual({ok, 1}, Converter(1, undefined)),
    ?assertEqual({ok, 1}, Converter(1.0, undefined)),
    ?assertEqual({ok, 1}, Converter(<<"1">>, undefined)),
    ?assertEqual({ok, 1}, Converter(<<"1.0">>, undefined)),

    ?assertEqual({error, <<"bad integer">>}, Converter(1.1, undefined)),
    ?assertEqual({error, <<"bad integer">>}, Converter(<<"1.1">>, undefined)),
    ?assertEqual({error, <<"bad integer">>}, Converter(a, undefined)),
    ?assertEqual({error, <<"bad integer">>}, Converter(<<>>, undefined)),
    ?assertEqual({error, <<"bad integer">>}, Converter(<<"not int">>, undefined)).

float_test() ->
    Converter = get_converter(float),
    ?assertEqual({ok, 1.0}, Converter(1, undefined)),
    ?assertEqual({ok, 1.0}, Converter(1.0, undefined)),
    ?assertEqual({ok, 1.0}, Converter(<<"1">>, undefined)),
    ?assertEqual({ok, 1.0}, Converter(<<"1.0">>, undefined)),

    ?assertEqual({error, <<"bad float">>}, Converter(a, undefined)),
    ?assertEqual({error, <<"bad float">>}, Converter(<<>>, undefined)),
    ?assertEqual({error, <<"bad float">>}, Converter(<<"not float">>, undefined)).

boolean_test() ->
    Converter = get_converter(boolean),
    ?assertEqual({ok, true}, Converter(true, undefined)),
    ?assertEqual({ok, false}, Converter(false, undefined)),
    ?assertEqual({ok, true}, Converter(<<"true">>, undefined)),
    ?assertEqual({ok, false}, Converter(<<"false">>, undefined)),

    ?assertEqual({error, <<"bad boolean">>}, Converter(true1, undefined)),
    ?assertEqual({error, <<"bad boolean">>}, Converter(false2, undefined)),
    ?assertEqual({error, <<"bad boolean">>}, Converter(<<"true1">>, undefined)),
    ?assertEqual({error, <<"bad boolean">>}, Converter(<<"false2">>, undefined)),
    ?assertEqual({error, <<"bad boolean">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad boolean">>}, Converter(1.1, undefined)).

date_test() ->
    Converter = get_converter(date),
    ?assertEqual({ok, {2016,01,01}}, Converter(<<"2016-01-01">>, undefined)),
    ?assertEqual({ok, {2016,01,01}}, Converter({2016,01,01}, undefined)),

    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2016-13-01">>, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2016-01-32">>, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2015-02-29">>, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter({2016,13,01}, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter({2016,01,32}, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter({2015,02,29}, undefined)),
    ?assertEqual({error, <<"bad date">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad date">>}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad date">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad date">>}, Converter(1.1, undefined)).

time_test() ->
    Converter = get_converter(time),
    ?assertEqual({ok, {10,12,05}}, Converter(<<"10:12:05">>, undefined)),

    ?assertEqual({error, <<"invalid time">>}, Converter(<<"24:00:00">>, undefined)),
    ?assertEqual({error, <<"invalid time">>}, Converter(<<"00:60:00">>, undefined)),
    ?assertEqual({error, <<"invalid time">>}, Converter(<<"00:00:60">>, undefined)),
    ?assertEqual({error, <<"bad time">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad time">>}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad time">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad time">>}, Converter(1.1, undefined)).

datetime_test() ->
    Converter = get_converter(datetime),
    ?assertEqual({ok, {{2016,01,01},{10,12,05}}}, Converter(<<"2016-01-01 10:12:05">>, undefined)),

    ?assertEqual({ok, {{2016,01,01},{10,12,05}}}, Converter(<<"2016-01-01T10:12:05">>, undefined)),
    ?assertEqual({ok, {{2016,01,01},{10,12,05}}}, Converter(<<"2016-01-01t10:12:05">>, undefined)),

    ?assertEqual({ok, {{2016,01,01},{10,12,05}}}, Converter(<<"2016-01-01T10:12:05Z">>, undefined)),
    ?assertEqual({ok, {{2016,01,01},{10,12,05}}}, Converter(<<"2016-01-01t10:12:05z">>, undefined)),

    ?assertEqual({ok, {{2016,01,01},{7,12,05}}}, Converter(<<"2016-01-01t10:12:05+03:00">>, undefined)),
    ?assertEqual({ok, {{2016,01,01},{13,12,05}}}, Converter(<<"2016-01-01t10:12:05-03:00">>, undefined)),

    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2016-13-01 10:12:05">>, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2016-01-32 10:12:05">>, undefined)),
    ?assertEqual({error, <<"invalid date">>}, Converter(<<"2015-02-29 10:12:05">>, undefined)),
    ?assertEqual({error, <<"invalid time">>}, Converter(<<"2016-01-01 24:00:00">>, undefined)),
    ?assertEqual({error, <<"invalid time">>}, Converter(<<"2016-01-01 00:60:00">>, undefined)),
    ?assertEqual({error, <<"invalid time">>}, Converter(<<"2016-01-01 00:00:60">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(<<"2016-13-01 10:12:05+1">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(<<"2016-13-01 10:12:05+1:1">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(<<"2016-13-01 10:12:05+01:1">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(<<"2016-13-01 10:12:05+1:01">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad datetime">>}, Converter(1.1, undefined)).

string_test() ->
    Converter = get_converter(string),
    ?assertEqual({ok, <<"string">>}, Converter(<<"string">>, undefined)),

    ?assertEqual({error, <<"bad string">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad string">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad string">>}, Converter(1.1, undefined)).

binary_test() ->
    Converter = get_converter(binary),
    ?assertEqual({ok, <<"string">>}, Converter(<<"string">>, undefined)),

    ?assertEqual({error, <<"bad string">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad string">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad string">>}, Converter(1.1, undefined)).

enum_test() ->
    AConverter = get_converter({enum, [a,b]}),
    ?assertEqual({ok, a}, AConverter(a, undefined)),
    ?assertEqual({ok, a}, AConverter(<<"a">>, undefined)),

    ?assertEqual({ok, b}, AConverter(b, undefined)),
    ?assertEqual({ok, b}, AConverter(<<"b">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, AConverter(c, undefined)),
    ?assertEqual({error, <<"unknown">>}, AConverter(<<"c">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, AConverter(1, undefined)),
    ?assertEqual({error, <<"unknown">>}, AConverter(1.1, undefined)),

    BConverter = get_converter({enum, [<<"a">>,<<"b">>]}),
    ?assertEqual({error, <<"unknown">>}, BConverter(a, undefined)),
    ?assertEqual({ok, <<"a">>}, BConverter(<<"a">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, BConverter(b, undefined)),
    ?assertEqual({ok, <<"b">>}, BConverter(<<"b">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, BConverter(c, undefined)),
    ?assertEqual({error, <<"unknown">>}, BConverter(<<"c">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, BConverter(1, undefined)),
    ?assertEqual({error, <<"unknown">>}, BConverter(1.1, undefined)),

    EConverter = get_converter({enum, #{a => 1, b => 2}}),
    ?assertEqual({ok, 1}, EConverter(a, undefined)),
    ?assertEqual({ok, 1}, EConverter(<<"a">>, undefined)),

    ?assertEqual({ok, 2}, EConverter(b, undefined)),
    ?assertEqual({ok, 2}, EConverter(<<"b">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, EConverter(c, undefined)),
    ?assertEqual({error, <<"unknown">>}, EConverter(<<"c">>, undefined)),

    ?assertEqual({error, <<"unknown">>}, EConverter(1, undefined)),
    ?assertEqual({error, <<"unknown">>}, EConverter(1.1, undefined)).

list_test() ->
    Converter = get_converter({list, integer}),

    ?assertEqual({ok, [1,2]}, Converter([1,2], undefined)),

    ?assertEqual({error, [{2, <<"bad integer">>}]}, Converter([1,a], undefined)),
    ?assertEqual({error, [{2, <<"bad integer">>}, {3, <<"bad integer">>}]}, Converter([1,a,3.1], undefined)),

    ?assertEqual({error, <<"bad array">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(1.1, undefined)).

ulist_test() ->
    Converter = get_converter({ulist, integer}),

    ?assertEqual({ok, [1,2]}, Converter([1,2], undefined)),

    ?assertEqual({error, [{2, <<"bad integer">>}]}, Converter([1,a], undefined)),
    ?assertEqual({error, [{2, <<"bad integer">>}, {3, <<"not unique">>}]}, Converter([1,a,1], undefined)),
    ?assertEqual({error, [{2, <<"bad integer">>}, {3, <<"bad integer">>}]}, Converter([1,a,3.1], undefined)),

    ?assertEqual({error, <<"bad array">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(1, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad array">>}, Converter(1.1, undefined)).

strlist_test() ->
    Converter = get_converter({strlist, integer}),

    ?assertEqual({ok, [1]}, Converter(<<"1">>, undefined)),
    ?assertEqual({ok, [1,2]}, Converter(<<"1,2">>, undefined)),

    ?assertEqual({error, [{2, <<"bad integer">>}]}, Converter(<<"1,a">>, undefined)),
    ?assertEqual({error, [{2, <<"bad integer">>}, {3, <<"bad integer">>}]}, Converter(<<"1,a,3.1">>, undefined)),

    ?assertEqual({error, <<"bad string list">>}, Converter(atom, undefined)),
    ?assertEqual({error, <<"bad string list">>}, Converter(1, undefined)),
    ?assertEqual({error, [{1, <<"bad integer">>}]}, Converter(<<"bin">>, undefined)),
    ?assertEqual({error, <<"bad string list">>}, Converter(1.1, undefined)).
