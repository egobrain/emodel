-module(emodel_converters).

-export([
         get_converter/2
        ]).

%% Converters

-export([
         integer/1,
         float/1,
         boolean/1,
         date/1,
         time/1,
         datetime/1,
         string/1,
         list/2,
         ulist/2,
         strlist/2
        ]).

%% =============================================================================
%% Converters
%% =============================================================================

get_converter(Fun, _Opts) when is_function(Fun, 1) -> Fun;
%% Simple converters
get_converter(binary, _Opts) -> fun string/1;
get_converter(string, _Opts) -> fun string/1;
get_converter(boolean, _Opts) -> fun boolean/1;
get_converter(datetime, _Opts) -> fun datetime/1;
get_converter(date, _Opts) -> fun date/1;
get_converter(integer, _Opts) -> fun integer/1;
get_converter(time, _Opts) -> fun time/1;
%% Complex converters
get_converter({list, Type}, Opts) -> list(get_top_converter(Type, Opts));
get_converter({ulist, Type}, Opts) -> ulist(get_top_converter(Type, Opts));
get_converter({strlist, Type}, Opts) -> strlist(get_top_converter(Type, Opts)).

get_top_converter(Type, #{converters := ConvertersF}=Opts) ->
    ConvertersF(Type, Opts).

%% =============================================================================
%% Simple Converters
%% =============================================================================

string(Bin) when is_binary(Bin) ->
    {ok, Bin};
string(_) ->
    {error, <<"bad string">>}.

boolean(Bool) when is_boolean(Bool) ->
    {ok, Bool};
boolean(Bin) when is_binary(Bin) ->
    case string:to_lower(binary_to_list(Bin)) of
        "true" -> {ok, true};
        "false" -> {ok, false};
        _ -> {error, <<"bad boolean">>}
    end;
boolean(_) ->
    {error, <<"bad boolean">>}.

date({_, _, _} = Date) ->
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> {error, <<"bad date">>}
    end;
date(Bin) when is_binary(Bin) ->
    try gh_date:parse_date(binary_to_list(Bin)) of
        Date -> {ok, Date}
    catch _:_ ->
        {error, <<"bad date">>}
    end;
date(_Other) ->
    {error, <<"bad date">>}.

time(Bin) when is_binary(Bin) ->
    try gh_date:parse_time(binary_to_list(Bin)) of
        Time -> {ok, Time}
    catch {bad_time, _} ->
        {error, <<"bad time">>}
    end;
time(_) ->
    {error, <<"bad time">>}.

datetime(Bin) when is_binary(Bin) ->
    try {ok, gh_date:datetime_to_now(Bin, [iso8601_simple])}
    catch _:_ -> {error, <<"bad datetime">>}
    end;
datetime(_Other) ->
    {error, <<"bad datetime">>}.

integer(Int) when is_integer(Int) ->
    {ok, Int};
integer(Float) when is_float(Float) -> %% Support float format (1.0)
    Int = trunc(Float),
    case Int == Float of
        true -> {ok, Int};
        false -> {error, <<"integer value required">>}
    end;
integer(Bin) when is_binary(Bin) ->    %% Support number as string format
    try binary_to_integer(Bin) of
        Int -> {ok, Int}
    catch error:badarg ->
        try binary_to_float(Bin) of
            Float -> integer(Float)
        catch error:badarg ->
            {error, <<"bad integer">>}
        end
    end;
integer(_) ->
    {error, <<"bad integer">>}.

float(Float) when is_float(Float) ->
    {ok, Float};
float(Integer) when is_integer(Integer) ->
    {Integer * 1.0};
float(Bin) when is_binary(Bin) ->
    try binary_to_float(Bin) of Float -> {ok, Float}
    catch error:badarg ->
        try binary_to_integer(Bin) of Int -> {ok, Int * 1.0}
        catch error:badarg -> {<<"error bad float">>}
        end
    end.

%% =============================================================================
%% Complex converters
%% =============================================================================

list(Converter) ->
    fun(Data) -> list(Data, Converter) end.

list(List, Converter) when is_list(List) ->
    gh_utils:error_writer_map(
        fun({I, E}) ->
            case Converter(E) of
                {ok, _V} = Ok -> Ok;
                {error, Reason} -> {error, {I, Reason}}
            end
        end, gh_utils:enumerate(List));
list(_Data, _Converter) ->
    {error, <<"bad array">>}.

ulist(Converter) ->
    fun(List) -> ulist(List, Converter) end.

ulist(List, Converter) when is_list(List) ->
    Result =
        gh_utils:error_writer_mapfoldl(
            fun({I, E}, S) ->
                case sets:is_element(E, S) of
                    true -> {error, {I, <<"not unique">>}};
                    false ->
                        case Converter(E) of
                            {ok, V} -> {ok, V, sets:add_element(E, S)};
                            {error, Reason} -> {error, {I, Reason}}
                        end
                end
            end, sets:new(), gh_utils:enumerate(List)),
    case Result of
        {ok, UList, _} -> {ok, lists:reverse(UList)};
        {error, _Reasons} = Err -> Err
    end;
ulist(_Data, _Converter) ->
    {error, <<"bad array">>}.

strlist(Converter) ->
    fun(Value) -> strlist(Value, Converter) end.
strlist(List, Converter) when is_list(List) ->
    gh_converters:list(List, Converter);
strlist(Bin, Converter) when is_binary(Bin) ->
    strlist(binary:split(Bin, <<",">>, [global]), Converter);
strlist(_, _Converter) ->
    {error, <<"bad string list">>}.
