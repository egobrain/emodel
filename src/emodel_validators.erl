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
         enum/1,
         each/2
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
    ?MODULE:Fun(A);
get_validator(non_empty, _Opts) -> fun non_empty/1;
get_validator(Fun, _Opts) when is_function(Fun, 1) ->
    fun(Data, _Model) -> Fun(Data) end;
get_validator(Fun, _Opts) when is_function(Fun, 2) ->
    Fun.

get_top_validator(Type, #{validators := ValidatorsF}=Opts) ->
    ValidatorsF(Type, Opts).

%% =============================================================================
%% Simple Validators
%% =============================================================================

'>'(Expected) ->
    fun(Value) ->
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

enum(List) ->
    fun(Value) ->
        case lists:member(Value, List) of
            true -> ok;
            false -> {error, unknown}
        end
    end.

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

%% =============================================================================
%% Internal functions
%% =============================================================================

apply_validation_rules([], _Model, _Data) -> ok;
apply_validation_rules([H|T], Model, Data) when is_function(H, 2) ->
    case H(Data, Model) of
        ok -> apply_validation_rules(T, Model, Data);
        {error, _Reason}=Err -> Err
    end.
