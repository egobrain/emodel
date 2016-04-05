-module(emodel_validators).

-export([
         get_validator/2
        ]).

-export([
         '>'/1,
         '>='/1,
         '<'/1,
         '=<'/1,
         non_empty/1,
         each/2,
         unique/1
        ]).

-export([
         apply_validation_rules/3
        ]).

-type validator(A, M, R) :: fun((A) -> ok | {error, R}) |
                            fun((A, M) -> ok | {error, R}).

-export_type([
              validator/3
             ]).

-define(error(F, A), {error, iolist_to_binary(io_lib:format(F, A))}).

%% =============================================================================
%% Validators
%% =============================================================================

get_validator({each, Vs}, Opts) ->
    ?MODULE:each(Vs, Opts);
get_validator({Fun, A}, _Opts) ->
    emodel_utils:lift2(?MODULE:Fun(A));
get_validator(non_empty, _Opts) -> emodel_utils:lift2(fun non_empty/1);
get_validator(unique, _Opts) -> emodel_utils:lift2(fun unique/1);
get_validator(Fun, _Opts) when is_function(Fun, 1) ->
    emodel_utils:lift2(Fun);
get_validator(Fun, _Opts) when is_function(Fun, 2) ->
    Fun.

get_top_validator(Type, #{validators := ValidatorsF}=Opts) ->
    ValidatorsF(Type, Opts).

%% =============================================================================
%% Simple Validators
%% =============================================================================

'>'(Expected) ->
    fun(Value, _) ->
        case Value > Expected of
            true -> ok;
            false ->
                ?error("must be greater than ~p", [Expected])
        end
    end.

'>='(Expected) ->
    fun(Value) ->
        case Value >= Expected of
            true -> ok;
            false ->
                ?error("must be greater than or equals to ~p", [Expected])
        end
    end.

'<'(Expected) ->
    fun(Value) ->
        case Value < Expected of
            true -> ok;
            false ->
                ?error("must be less than ~p", [Expected])
        end
    end.

'=<'(Expected) ->
    fun(Value) ->
        case Value =< Expected of
            true -> ok;
            false ->
                ?error("must be less than or equals to ~p", [Expected])
        end
    end.

non_empty(<<>>) -> {error, <<"is empty">>};
non_empty(_) -> ok.

each(Validators0, Opts) ->
    Validators = [get_top_validator(V, Opts) || V <- Validators0],
    fun(List, Model) ->
        emodel_utils:error_writer_foreach(
            fun({I, Data}) ->
                case apply_validation_rules(Validators, Model, Data) of
                    ok -> ok;
                    {error, Reason} -> {error, {I, Reason}}
                end
            end, emodel_utils:enumerate(List))
    end.

unique(List) when is_list(List) ->
    Result = emodel_utils:error_writer_foldl(
        fun({I, E}, S) ->
            case sets:is_element(E, S) of
                true -> {error, {I, <<"not unique">>}};
                false -> {ok, sets:add_element(E, S)}
            end
        end, sets:new(), emodel_utils:enumerate(List)),
    case Result of
        {ok, _} -> ok;
        {error, _Reasons} = Err -> Err
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================

apply_validation_rules([], _Model, _Data) -> ok;
apply_validation_rules([H|T], Model, Data) when is_function(H, 2) ->
    case H(Data, Model) of
        ok -> apply_validation_rules(T, Model, Data);
        {error, _Reason}=Err -> Err
    end.
