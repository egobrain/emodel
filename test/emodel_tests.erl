-module(emodel_tests).

-include_lib("eunit/include/eunit.hrl").

-record(r, {i,s,d}).

simple_test_() ->
    RExpected = #r{i=1, s = <<"a">>, d = {2015,12,01}},
    MExpected = #{i => 1, s => <<"a">>, d => {2015,12,01}},
    Data = #{
        <<"int">> => 1,
        <<"str">> => <<"a">>,
        <<"date">> => <<"2015-12-01">>
    },
    ProplistData = maps:to_list(Data),
    MapModel = [
        {<<"int">>,  required, integer, #r.i, []},
        {<<"str">>,  required, string, #r.s, []},
        {<<"date">>, required, date, #r.d, []}
    ],
    TupleModel = [
        {<<"int">>,  required, integer, i, []},
        {<<"str">>,  required, string, s, []},
        {<<"date">>, required, date, d, []}
    ],

    [
     %% Map
     ?_assertEqual(
          {ok, RExpected},
          emodel:from_map(Data, #r{}, MapModel)),
     ?_assertEqual(
          {ok, MExpected},
          emodel:from_map(Data, #{}, TupleModel)),
     %% Proplist
     ?_assertEqual(
          {ok, RExpected},
          emodel:from_proplist(ProplistData, #r{}, MapModel)),
     ?_assertEqual(
          {ok, MExpected},
          emodel:from_proplist(ProplistData, #{}, TupleModel)),
     %% Error case
     ?_assertEqual(
          {error, <<"Object required">>},
          emodel:from_map(ProplistData, #{}, TupleModel)),
     ?_assertEqual(
          {error, [{<<"int">>, required}]},
          emodel:from_map(maps:without([<<"int">>], Data), #{}, MapModel)),
     ?_assertEqual(
          {error, [{<<"int">>, required}]},
          emodel:from_map(maps:without([<<"int">>], Data), #{i => null}, MapModel)),
     ?_assertEqual(
          {error, [{<<"int">>, required}]},
          emodel:from_map(Data#{<<"int">> => null}, #{}, MapModel))
    ].

req_test_() ->
    ReqFun = fun(#{a := A}) when A > 0 -> required; (_) -> ignore end,
    Model = [
        {<<"a">>, required, integer, a, []},
        {<<"b">>, ReqFun, integer, b, []}
    ],
    [
     ?_assertEqual(
         {ok, #{a => 1, b => 2}},
         emodel:from_map(#{<<"a">> => 1, <<"b">> => 2}, #{}, Model)),
     ?_assertEqual(
         {ok, #{a => -1}},
         emodel:from_map(#{<<"a">> => -1, <<"b">> => 2}, #{}, Model)),
     ?_assertEqual(
         {ok, #{a => null}},
         emodel:from_map(#{<<"a">> => null}, #{a => 3}, [
             {<<"a">>, optional, integer, a, []}
         ]))
    ].

default_test_() ->
    [
     ?_assertEqual(
          {ok, #{ d => 1 }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], emodel:default_value(1)}
          ])),
     ?_assertEqual(
          {error, [{<<"d">>, required}]},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], emodel:default_value(null)}
          ])),
     ?_assertEqual(
          {error, [{<<"d">>, required}]},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], emodel:default_value(undefined)}
          ])),
     ?_assertEqual(
          {error, [{<<"d">>, required}]},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(Model, Setter) -> Setter(undefined, Model) end}
          ])),
     ?_assertEqual(
          {ok, #{d => 1}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(Model, Setter) -> Setter(1, Model) end}
          ])),
     ?_assertEqual(
          {ok, #{d => 1}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(_Model) -> {ok, 1} end}
          ])),
     ?_assertEqual(
          {ok, #{a => 3}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(Model, _Setter) -> {ok, Model#{a => 3}} end}
          ])),
     ?_assertEqual(
          {error, [{<<"d">>, bad_default}]},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(_Model, _Setter) -> {error, bad_default} end}
          ])),
     %% Optional
     ?_assertEqual(
          {ok, #{}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, []}
          ])),
     ?_assertEqual(
          {ok, #{ d => 1 }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], 1}
          ])),
     ?_assertEqual(
          {ok, #{ d => null }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], emodel:default_value(null)}
          ])),
     ?_assertEqual(
          {ok, #{ d => null }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], fun(Model, Setter) -> Setter(null, Model) end}
          ])),
     ?_assertEqual(
          {ok, #{}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], emodel:default_value(undefined)}
          ])),
     ?_assertEqual(
          {ok, #{}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], fun(Model, Setter) -> Setter(undefined, Model) end}
          ])),
     ?_assertEqual(
          {ok, #{d => 2}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, fun(_) -> error(badarg) end, d,
               [fun(_) -> error(badarg) end], emodel:default_value(2)}
          ])),
     ?_assertEqual(
          {ok, #{ d => 3 }},
          emodel:from_map(#{}, #{d => 3}, [
              {<<"d">>, optional, integer, d, [], 1}
          ])),
     %% Error test
     ?_assertEqual(
          {error, [{<<"d">>, msg}]},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, required, integer, d, [], fun(_) -> {error, msg} end}
          ]))
    ].

compile_test() ->
    Data = #{
        <<"user">> => #{
            <<"login">> => <<"ost">>,
            <<"password">> => <<"123456">>
        },
        <<"notifyAt">> => <<"2015-02-21 00:00:12,2015-12-31 23:59:59">>,
        <<"message">> => <<"Hello!!!">>
    },
    CheckUserPassword =
        fun(Password, #{login := Login}) ->
            case {Login, Password} of
                {<<"ost">>, <<"123456">>} -> ok;
                _ -> {error, <<"bad password">>}
            end;
           (_, _) -> ok
        end,

    UserC = emodel:compile([
        {<<"login">>, required, string, login, [non_empty]},
        {<<"password">>, required, string, password, [CheckUserPassword]}
    ], map),
    DataModel = [
        {<<"user">>, required,
         fun(D, Model) ->
             case emodel:from_map(D, #{}, UserC) of
                 {ok, _} ->
                     {ok, Model#{session => <<"sid">>}};
                 {error, _Reason} = Err -> Err
             end
         end},
        {<<"notifyAt">>, required, {strlist, datetime}, notifyAt, []},
        {<<"message">>, required, string, message, [non_empty]}
    ],
    ?assertEqual(
        {ok, #{message => <<"Hello!!!">>,
               notifyAt => [
                {{2015,2,21},{0,0,12}},
                {{2015,12,31},{23,59,59}}
               ],
               session => <<"sid">>}},
        emodel:from_map(Data, #{}, DataModel)).

custom_opts_test_() ->
    IntConverter =
        fun(A) ->
            try binary_to_integer(A) of B -> {ok, B}
            catch _:_ -> {error, bad_int}
            end
        end,
    NonZeroVal = fun(0, _) -> {error, zero}; (_, _) -> ok end,
    Converters = fun(int, _Opts) -> IntConverter end,
    Validators = fun(non_zero, _Opts) -> NonZeroVal end,
    [
     %% Custom converters
     ?_assertEqual(
          {ok, #{a => 42}},
          emodel:from_map(#{<<"a">> => <<"42">>}, #{}, [
              {<<"a">>, required, int, a, []}
          ], #{converters => Converters})),
     ?_assertEqual(
          {error, [{<<"a">>, bad_int}]},
          emodel:from_map(#{<<"a">> => 1}, #{}, [
              {<<"a">>, required, int, a, []}
          ], #{converters => Converters})),
     ?_assertEqual(
          {error, [{<<"a">>, <<"must be greater than 0">>}]},
          emodel:from_map(#{<<"a">> => <<"-1">>}, #{}, [
              {<<"a">>, required, int, a, [{'>', 0}]}
          ], #{converters => Converters})),
     %% Custom validators
     ?_assertEqual(
          {ok, #{a => 42}},
          emodel:from_map(#{<<"a">> => <<"42">>}, #{}, [
              {<<"a">>, required, integer, a, [non_zero]}
          ], #{validators => Validators})),
     ?_assertEqual(
          {error, [{<<"a">>, zero}]},
          emodel:from_map(#{<<"a">> => <<"0">>}, #{}, [
              {<<"a">>, required, integer, a, [non_zero]}
          ], #{validators => Validators})),
     %% Both
     ?_assertEqual(
          {error, [{<<"a">>, zero}]},
          emodel:from_map(#{<<"a">> => <<"0">>}, #{}, [
              {<<"a">>, required, int, a, [non_zero]}
          ], #{converters => Converters, validators => Validators}))
    ].

dependent_converter_test_() ->
    Model = [
        {<<"a">>, required,
         fun(A, #{b := true}) -> {ok, A};
            (A, _) -> {error, A}
         end, a, []}
    ],
    [
     ?_assertEqual(
          {ok, #{a => 1, b => true}},
          emodel:from_map(#{<<"a">> => 1}, #{b => true}, Model)),
     ?_assertEqual(
          {error, [{<<"a">>, 1}]},
          emodel:from_map(#{<<"a">> => 1}, #{}, Model))
    ].
