-module(emodel).

-export([
         compile/2,
         from_map/3,
         from_proplist/3,

         compile/3,
         from_map/4,
         from_proplist/4,

         default_value/1
        ]).

%% -type opts() :: #{ converters => fun((any(), opts()) -> converter() }.
%% -type opts() :: #{ validators => fun((any(), opts()) -> validator() }.

-type opts() :: #{}.

-type req_opts() :: required | optional | ignore.
-type req_fun(M) :: fun((M) -> req_opts()).

-type required(M) :: req_opts() | req_fun(M).

-type position() :: non_neg_integer() | %% For tuple models
                    term(). %% form maps models

-type default(A, M, R) :: fun((M) -> {ok, A} | {error, R}).
-type model_type() :: tuple | map.

-type field() ::
          {
              Name :: binary(),
              req_fun(Model),
              Getter :: fun((Model) -> Value),
              Setter :: fun((Value, Model) -> {ok, Model} | {error, Reason :: any()}),
              default(Value, Model, Reason :: any())
          }.

-opaque model() :: {'$compiled', [field()]}.
-type pre_model() ::
          [{
               Name :: binary(),
               required(M),
               Type :: emodel_converters:known_types(A :: any(), B, Reason) | any(),
               Position :: position(),
               Validators :: [emodel_validators:validator(B, Model, Reason :: any()) | any()]
           } |
           {
               Name :: binary(),
               required(M),
               Type :: emodel_converters:known_types(A :: any(), B, Reason) | any(),
               Position :: position(),
               Validators :: [emodel_validators:validator(B, Model, Reason :: any()) | any()],
               default(B, M, Reason :: any()) | B
           } |
           {
               Name :: binary(),
               required(M),
               Setter :: fun((any(), Model) -> {ok, Model} | {error, Reason :: any()})
           }].

-export_type([model/0, pre_model/0]).

-define(DEFAULT_OPTS, #{
        converters => fun emodel_converters:get_converter/2,
        validators => fun emodel_validators:get_validator/2
    }).

-define(IS_UNDEFINED(V), (V =:= undefined orelse V =:= null)).

%% =============================================================================
%% Default versions
%% =============================================================================

-spec compile(pre_model(), model_type()) -> model().
compile(PreModel, ModelType) ->
    compile(PreModel, ModelType, ?DEFAULT_OPTS).

-spec from_proplist(proplists:proplist(), Model, model() | pre_model()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_proplist(Proplist, Model, ModelDescription) ->
    from_proplist(Proplist, Model, ModelDescription, ?DEFAULT_OPTS).

-spec from_map(any(), Model, model() | pre_model()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_map(Map, Model, ModelDescription0) ->
    from_map(Map, Model, ModelDescription0, ?DEFAULT_OPTS).

%% =============================================================================
%% API
%% =============================================================================

-spec compile(pre_model(), model_type(), opts()) -> model().
compile(PreModel, ModelType, Opts) ->
    CompileRow = fun(R) -> compile_row(R, ModelType, Opts) end,
    {'$compiled', lists:map(CompileRow, PreModel)}.

-spec type(tuple() | map()) -> model_type().
type(Model) when is_tuple(Model) -> tuple;
type(Model) when is_map(Model) -> map.

compile_row({Name, Required, Setter}, _ModelType, _Opts) ->
    Getter = fun(_) -> undefined end,
    {Name, req_fun(Required), Getter, Setter, undefined};
compile_row({Name, Required, Type, Position, Validators},
            ModelType, Opts) ->
    compile_row({Name, Required, Type, Position, Validators, undefined},
        ModelType, Opts);
compile_row({Name, Required, Type, Position, Validators, Default}, ModelType,
        #{converters := ConvertersF, validators := ValidatorsF}=Opts) ->
    Converter = ConvertersF(Type, Opts),
    ValidatorsC = [ValidatorsF(V, Opts) || V <- Validators],
    DefaultC = compile_default(Default),
    Getter = default_getter(ModelType, Position),
    Setter = default_setter(ModelType, Converter, Position, ValidatorsC),
    {Name, req_fun(Required), Getter, Setter, DefaultC}.

compile_default(undefined) -> undefined;
compile_default(Fun) when is_function(Fun,1) -> Fun;
compile_default(Value) -> default_value(Value).

-spec from_proplist(proplists:proplist(), Model, model() | pre_model(), opts()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_proplist(Proplist, Model, ModelDescription, Opts) ->
    from_map(maps:from_list(Proplist), Model, ModelDescription, Opts).

-spec from_map(any(), Model, model() | pre_model(), opts()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_map(Map, Model, ModelDescription0, Opts) when is_map(Map) ->
    {_, ModelDescription} = try_compile(Model, ModelDescription0, Opts),
    emodel_utils:error_writer_foldl(
        fun({Name, ReqFun, Getter, Setter, Default}, M) ->
            case ReqFun(M) of
                ignore ->
                    {ok, M};
                Required ->
                    case maps:find(Name, Map) of
                        {ok, null} when Required =:= required ->
                            {error, {Name, required}};
                        {ok, Value} ->
                            case Setter(Value, M) of
                                {ok, _Model} = Ok -> Ok;
                                {error, Reason} -> {error, {Name, Reason}}
                            end;
                        error ->
                            case Getter(M) of
                                undefined ->
                                    try_default(Required, Default, Setter, Name, M);
                                _ -> {ok, M}
                            end
                    end
            end
        end, Model, ModelDescription);
from_map(_, _, _, _) ->
    {error, <<"Object required">>}.

%% =============================================================================
%%% Internal functions
%% =============================================================================

try_default(required, undefined, _Setter, Name, _Model) ->
    {error, {Name, required}};
try_default(_, undefined, _Setter, _Name, Model) ->
    {ok, Model};
try_default(Required, Default, Setter, Name, Model) ->
    case Default(Model) of
        {ok, Value} when ?IS_UNDEFINED(Value) andalso Required =:= required ->
            {error, {Name, required}};
        {ok, Value} ->
            case Setter(Value, Model) of
                {ok, _Model} = Ok -> Ok;
                {error, Reason} -> {error, {Name, Reason}}
            end;
        {error, Reason} ->
            {error, {Name, Reason}}
    end.

try_compile(_Model, {'$compiled', _}=Compiled, _Opts) ->
    Compiled;
try_compile(Model, ModelDescription, Opts) ->
    compile(ModelDescription, type(Model), Opts).

req_fun(Fun) when is_function(Fun, 1) -> Fun;
req_fun(R) -> fun(_) -> R end.

default_getter(map, Position) ->
    fun(M) ->
        case maps:find(Position, M) of
            {ok, V} -> V;
            _ -> undefined
        end
    end;
default_getter(tuple, Position) ->
    fun(T) -> element(Position, T) end.

default_setter(ModelType, Converter, Position, Validators) ->
    Setter =
        case ModelType of
            map ->
                fun(undefined, M) -> maps:remove(Position, M);
                   (V, M) -> maps:put(Position, V, M)
                end;
            tuple -> fun(V, T) -> setelement(Position, T, V) end
        end,
    fun(Value, Model) when ?IS_UNDEFINED(Value) ->
           {ok, Setter(Value, Model)};
       (Data, Model) ->
           case Converter(Data) of
               {ok, ConvertedData} ->
                   case emodel_validators:apply_validation_rules(Validators, Model, ConvertedData) of
                       ok -> {ok, Setter(ConvertedData, Model)};
                       {error, _Reason} = Err -> Err
                   end;
               {error, _Reason} = Err -> Err
           end
    end.

%% =============================================================================
%%% Utils
%% =============================================================================

-spec default_value(V) -> fun((any()) -> {ok, V}).
default_value(Value) -> fun(_Model) -> {ok, Value} end.

-ifdef(TEST).
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
    [
     ?_assertEqual(
          {ok, RExpected},
          emodel:from_map(Data, #r{}, [
              {<<"int">>,  required, integer, #r.i, []},
              {<<"str">>,  required, string, #r.s, []},
              {<<"date">>, required, date, #r.d, []}
          ])),
     ?_assertEqual(
          {ok, MExpected},
          emodel:from_map(Data, #{}, [
              {<<"int">>,  required, integer, i, []},
              {<<"str">>,  required, string, s, []},
              {<<"date">>, required, date, d, []}
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
     %% Optional
     ?_assertEqual(
          {ok, #{ d => 1 }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], emodel:default_value(1)}
          ])),
     ?_assertEqual(
          {ok, #{ d => null }},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], emodel:default_value(null)}
          ])),
     ?_assertEqual(
          {ok, #{}},
          emodel:from_map(#{}, #{}, [
              {<<"d">>, optional, integer, d, [], emodel:default_value(undefined)}
          ]))
    ].

-endif.
