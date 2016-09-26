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
         enum/1,
         list/3,
         ulist/3,
         strlist/3
        ]).

-type converter(A, B, M, R) :: fun((A) -> {ok, B} | {error, R}) |
                               fun((A, M) -> {ok, B} | {error, R}).

-export_type([
              converter/4
             ]).

%% =============================================================================
%% Converters
%% =============================================================================

get_converter(Fun, _Opts) when is_function(Fun, 1) ->
    fun(V, _) -> Fun(V) end;
get_converter(Fun, _Opts) when is_function(Fun, 2) -> Fun;
%% Simple converters
get_converter(binary, Opts) -> get_converter(fun string/1, Opts);
get_converter(string, Opts) -> get_converter(fun string/1, Opts);
get_converter(boolean, Opts) -> get_converter(fun boolean/1, Opts);
get_converter(datetime, Opts) -> get_converter(fun datetime/1, Opts);
get_converter(date, Opts) -> get_converter(fun date/1, Opts);
get_converter(integer, Opts) -> get_converter(fun integer/1, Opts);
get_converter(float, Opts) -> get_converter(fun float/1, Opts);
get_converter(time, Opts) -> get_converter(fun time/1, Opts);
%% Complex converters
get_converter({enum, Data}, Opts) -> get_converter(enum(Data), Opts);
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
        false -> {error, <<"invalid date">>}
    end;
date(Bin) when is_binary(Bin) ->
    Re = "^(?<y>\\d{4})-(?<m>\\d{1,2})-(?<d>\\d{1,2})$",
    case re:run(Bin, Re, [{capture, [y, m, d], binary}]) of
        {match, [Y,M,D]} ->
            Date = {
                binary_to_integer(Y),
                binary_to_integer(M),
                binary_to_integer(D)
            },
            case calendar:valid_date(Date) of
                true -> {ok, Date};
                false -> {error, <<"invalid date">>}
            end;
        _ -> {error, <<"bad date">>}
    end;
date(_Other) ->
    {error, <<"bad date">>}.

time(Bin) when is_binary(Bin) ->
    Re = "^(?<hh>\\d{1,2}):(?<mm>\\d{1,2}):(?<ss>\\d{1,2})$",
    case re:run(Bin, Re, [{capture, [hh, mm, ss], binary}]) of
        {match, [H,M,S]} ->
            Time = {
                binary_to_integer(H),
                binary_to_integer(M),
                binary_to_integer(S)
            },
            case is_time_valid(Time) of
                true -> {ok, Time};
                false -> {error, <<"invalid time">>}
            end;
        _ -> {error, <<"bad time">>}
    end;
time(_) ->
    {error, <<"bad time">>}.

is_time_valid({Hh, Mm, Ss}) ->
    Hh >= 0 andalso Hh < 24 andalso
    Mm >= 0 andalso Mm < 60 andalso
    Ss >= 0 andalso Ss < 60.

datetime(Bin) when is_binary(Bin) ->
    Re =
        "^(?<y>\\d{4})-(?<m>\\d{1,2})-(?<d>\\d{1,2})[Tt ](?<hh>\\d{1,2}):"
        "(?<mm>\\d{1,2}):(?<ss>\\d{1,2})(?<ms>\\.\\d{1,})?"
        "(?<offset>([Zz])|([+-]\\d{2}:\\d{2}))?$",
    case re:run(Bin, Re, [{capture,[y,m,d,hh,mm,ss,ms,offset],binary}]) of
        {match, [Y,M,D,Hh,Mm,Ss,_Ms,Offset]} ->
            TimeDiff =
                case Offset of
                    _ when Offset =:= <<"z">>; Offset =:= <<"Z">> ->
                        0;
                    <<>> ->
                        0;
                    <<Sign, OffsetH:2/binary, ":", OffsetM:2/binary>> ->
                        SecOffset =
                            binary_to_integer(OffsetH) * 60 * 60
                            +
                            binary_to_integer(OffsetM) * 60,
                        case Sign of
                            $+ -> -SecOffset;
                            $- -> SecOffset
                        end
                end,
            Date =
                {binary_to_integer(Y),
                 binary_to_integer(M),
                 binary_to_integer(D)},
            Time =
                {binary_to_integer(Hh),
                 binary_to_integer(Mm),
                 binary_to_integer(Ss)},
            case calendar:valid_date(Date) of
                true ->
                    case is_time_valid(Time) of
                        false -> {error, <<"invalid time">>};
                        true ->
                            DateTime0 =
                                calendar:datetime_to_gregorian_seconds(
                                    {Date, Time}) + TimeDiff,
                            DateTime =
                                calendar:gregorian_seconds_to_datetime(DateTime0),
                            {ok, DateTime}
                    end;
                false ->
                    {error, <<"invalid date">>}
            end;
        _ ->
            {error, <<"bad datetime">>}
    end;
datetime(_Other) ->
    {error, <<"bad datetime">>}.

integer(Int) when is_integer(Int) ->
    {ok, Int};
integer(Float) when is_float(Float) -> %% Support float format (1.0)
    Int = trunc(Float),
    case Int == Float of
        true -> {ok, Int};
        false -> {error, <<"bad integer">>}
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
float(Int) when is_integer(Int) ->
    {ok, Int * 1.0};
float(Bin) when is_binary(Bin) ->
    try binary_to_float(Bin) of Float -> {ok, Float}
    catch error:badarg ->
        try binary_to_integer(Bin) of Int -> {ok, Int * 1.0}
        catch error:badarg -> {error, <<"bad float">>}
        end
    end;
float(_) ->
    {error, <<"bad float">>}.

%% =============================================================================
%% Complex converters
%% =============================================================================

enum(Map) when is_map(Map) ->
    Map2 = maps:fold(
        fun (K, V, M) when is_atom(K) ->
                maps:put(atom_to_binary(K, latin1), V, M);
            (_, _, M) -> M
        end, Map, Map),
    fun(Key) ->
        case maps:find(Key, Map2) of
            {ok, _V} = Ok -> Ok;
            error -> {error, <<"unknown">>}
        end
    end;
enum(List) when is_list(List) ->
    enum(maps:from_list([{E, E} || E <- List])).

list(Converter) ->
    fun(Data, Model) -> list(Data, Model, Converter) end.

list(List, Model, Converter) when is_list(List) ->
    emodel_utils:error_writer_map(
        fun({I, E}) ->
            case Converter(E, Model) of
                {ok, _V} = Ok -> Ok;
                {error, Reason} -> {error, {I, Reason}}
            end
        end, emodel_utils:enumerate(List));
list(_Data, _Model, _Converter) ->
    {error, <<"bad array">>}.

ulist(Converter) ->
    fun(List, Model) -> ulist(List, Model, Converter) end.

ulist(List, Model, Converter) when is_list(List) ->
    Result =
        emodel_utils:error_writer_mapfoldl(
            fun({I, E}, S) ->
                case sets:is_element(E, S) of
                    true -> {error, {I, <<"not unique">>}};
                    false ->
                        case Converter(E, Model) of
                            {ok, V} -> {ok, V, sets:add_element(E, S)};
                            {error, Reason} -> {error, {I, Reason}}
                        end
                end
            end, sets:new(), emodel_utils:enumerate(List)),
    case Result of
        {ok, UList, _} -> {ok, UList};
        {error, Reasons} -> {error, lists:reverse(Reasons)}
    end;
ulist(_Data, _Model, _Converter) ->
    {error, <<"bad array">>}.

strlist(Converter) ->
    fun(Value, Model) -> strlist(Value, Model, Converter) end.

strlist(Bin, Model, Converter) when is_binary(Bin) ->
    List = binary:split(Bin, <<",">>, [global]),
    emodel_converters:list(List, Model, Converter);
strlist(_, _Model, _Converter) ->
    {error, <<"bad string list">>}.
