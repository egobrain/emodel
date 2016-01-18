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

-type opts() :: #{}.

-type req_opts() :: required | optional | ignore.
-type req_fun(M) :: fun((M) -> req_opts()).
-type default(B,M,R) ::
          B |
          fun((M) -> {ok, B} | {error, R}) |
          fun((M, S :: fun((B,M) -> {ok, M} | {error, R})) -> {ok, M} | {error, R}).

-type required(M) :: req_opts() | req_fun(M).

-type position() :: non_neg_integer() | %% For tuple models
                    term(). %% form maps models

-type model_type() :: tuple | map.

-type pre_model() ::
          [{
               Name :: any(),
               required(M),
               Type :: emodel_converters:converter(A :: any(), B, Model, Reason) | any(),
               Position :: position(),
               Validators :: [emodel_validators:validator(B, Model, Reason :: any()) | any()]
           } |
           {
               Name :: any(),
               required(M),
               Type :: emodel_converters:converter(A :: any(), B, Model, Reason) | any(),
               Position :: position(),
               Validators :: [emodel_validators:validator(B, Model, Reason :: any()) | any()],
               default(B, Model, Reason :: any())
           } |
           {
               Name :: any(),
               required(M),
               Setter :: fun((any(), Model) -> {ok, Model} | {error, Reason :: any()})
           }].

-opaque model() :: {'$compiled', [{
              Name :: binary(),
              req_fun(Model),
              Getter :: fun((Model) -> Value),
              Setter :: fun((Value, Model) -> {ok, Model} | {error, Reason :: any()}),
              Default :: fun((required | optional, Model) -> {ok, Model} | {error, Reason :: any()})
           }]}.

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
    compile(PreModel, ModelType, #{}).

-spec from_proplist(proplists:proplist(), Model, model() | pre_model()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_proplist(Proplist, Model, ModelDescription) ->
    from_proplist(Proplist, Model, ModelDescription, #{}).

-spec from_map(any(), Model, model() | pre_model()) ->
          {ok, Model} | {error, Reason :: any()} when Model :: tuple() | map().
from_map(Map, Model, ModelDescription0) ->
    from_map(Map, Model, ModelDescription0, #{}).

%% =============================================================================
%% API
%% =============================================================================

-spec compile(pre_model(), model_type(), opts()) -> model().
compile(PreModel, ModelType, Opts0) ->
    Opts = maps:merge(?DEFAULT_OPTS, Opts0),
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
    Converter = lift2(ConvertersF(Type, Opts)),
    ValidatorsC = [lift2(ValidatorsF(V, Opts)) || V <- Validators],
    Getter = default_getter(ModelType, Position),
    Setter = default_setter(ModelType, Position),
    SetValueFun = set_value_fun(Setter, Converter, ValidatorsC),
    SetDefaultFun = set_default_fun(Setter, Default),
    {Name, req_fun(Required), Getter, SetValueFun, SetDefaultFun}.

lift2(F) when is_function(F, 2) -> F;
lift2(F) when is_function(F, 1) -> fun(V, _) -> F(V) end.

set_default_fun(_Setter, undefined) -> undefined;
set_default_fun(Setter, Fun) when is_function(Fun,1) ->
    set_default_fun(Setter,
        fun(Model, ReqSetter) ->
            case Fun(Model) of
                {ok, V} -> ReqSetter(V, Model);
                {error, _R} = Err -> Err
            end
        end);
set_default_fun(Setter, Fun) when is_function(Fun,2) ->
    fun(Required, Model) ->
        ReqSetter =
            case Required of
                required ->
                    fun(V, _M) when ?IS_UNDEFINED(V) -> {error, required};
                       (V, M) -> {ok, Setter(V, M)}
                    end;
                _ -> fun(V, M) -> {ok, Setter(V, M)} end
            end,
        Fun(Model, ReqSetter)
    end;
set_default_fun(Setter, Value) ->
    set_default_fun(Setter, default_value(Value)).

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
                                    try_default(Required, Default, Name, M);
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

try_default(required, undefined, Name, _Model) ->
    {error, {Name, required}};
try_default(_, undefined, _Name, Model) ->
    {ok, Model};
try_default(Required, Default, Name, Model) ->
    case Default(Required, Model) of
        {ok, _Model} = Ok -> Ok;
        {error, Reason} -> {error, {Name, Reason}}
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

default_setter(map, Position) ->
    fun(undefined, M) -> maps:remove(Position, M);
       (V, M) -> maps:put(Position, V, M)
    end;
default_setter(tuple, Position) ->
    fun(V, T) -> setelement(Position, T, V) end.

set_value_fun(Setter, Converter, Validators) ->
    fun(Value, Model) when ?IS_UNDEFINED(Value) ->
           {ok, Setter(Value, Model)};
       (Value, Model) ->
           case Converter(Value, Model) of
               {ok, ConvertedValue} ->
                   case emodel_validators:apply_validation_rules(Validators, Model, ConvertedValue) of
                       ok -> {ok, Setter(ConvertedValue, Model)};
                       {error, _Reason} = Err -> Err
                   end;
               {error, _Reason} = Err -> Err
           end
    end.

%% =============================================================================
%%% Utils
%% =============================================================================

-spec default_value(V) -> fun((any()) -> {ok, V}).
default_value(Value) -> fun(Model, Setter) -> Setter(Value, Model) end.
