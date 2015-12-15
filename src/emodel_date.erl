-module(emodel_date).

-export([parse_date/1,
         parse_time/1,
         parse_datetime/1,
         parse_datetime/2,
         date_to_binary/1,
         time_to_binary/1,
         datetime_to_binary/1,
         datetime_to_binary/2,
         datetime_to_now/1,
         datetime_to_now/2,
         now_to_binary/1,
         now_to_binary/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type epgsql_time() :: {0..23, 0..59, float()}.
-type time() :: calendar:time() | epgsql_time().
-type datetime() :: calendar:datetime() | {calendar:date(), epgsql_time()}.

-export_type([time/0, datetime/0]).

%%--------------------------------------------------------------------
%% @doc
%% Parses date in fotmat YYYY-MM-DD to {YYYY,MM,DD}
%% @end
%%--------------------------------------------------------------------

-spec parse_date(Date :: string()) -> calendar:date().
parse_date(Date) ->
    Tokens = string:tokens(Date, "-"),
    case Tokens of
        [Y, M, D] = Tokens ->
            Res = try
                      YY = case list_to_integer(Y) of
                               %% 294276 is the maximum supported Year in bocore
                               R when R >= 1970 andalso R =< 294276 ->
                                   R;
                               _ ->
                                   throw({bad_date, Date})
                           end,
                      {YY,
                       list_to_integer(M),
                       list_to_integer(D)}
                  catch
                      _:_ ->
                          throw({bad_date, Date})
                  end,
            case calendar:valid_date(Res) of
                true ->
                    Res;
                false ->
                    throw({bad_date, Date})
            end;
        _ ->
            throw({bad_date, Date})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parses time in format HH:MM or HH:MM:SS to {HH,MM,00} or {HH,MM,SS}
%% @end
%%--------------------------------------------------------------------

-spec parse_time(Time :: string()) ->
    {Hours :: integer(), Minutes :: integer(), Seconds :: integer()}.
parse_time(Time) ->
    Tokens = string:tokens(Time, ":"),
    case length(Tokens) of
        R when R == 2 ->
            % Assume HH:MM
            [H, M] = Tokens,
            HH = validate_time(23, H),
            MM = validate_time(59, M),
            case HH >= 0 andalso MM >= 0 of
                true ->
                    {HH, MM, 0};
                false ->
                    throw({bad_time, Time})
            end;
        R when R == 3 ->
            % Assume HH:MM:SS
            [H, M, S] = Tokens,
            HH = validate_time(23, H),
            MM = validate_time(59, M),
            SS = validate_time(59, S),
            case HH >= 0 andalso MM >= 0 andalso SS >= 0 of
                true ->
                    {HH, MM, SS};
                false ->
                    throw({bad_time, Time})
            end;
        _ ->
            throw({bad_time, Time})
    end.

validate_time(Max, Value) ->
    try
      N = list_to_integer(Value),
      case N =< Max andalso N >= 0 of
          true ->
              N;
          false ->
              -1
      end
    catch
        _:_ ->
            -1
    end.

%%--------------------------------------------------------------------
%% @doc
%% Parses datetime formats:
%%     - "YYYY-MM-DD HH:mm[:SS]"
%%     - "YYYY-MM-DDTHH:mm[:SS][.sss]Z"
%% to:
%%     - {{YYYY,MM,DD}, {HH,mm,SS}}
%%     - {{YYYY,MM,DD}, {HH,mm,SS.sss}}
%% @end
%%--------------------------------------------------------------------
-spec parse_datetime(string()) -> datetime().
parse_datetime(String) ->
    try
        parse_datetime(String, [iso8601_simple])
    catch throw:{bad_datetime, _} ->
        parse_datetime(String, [])
    end.

-spec parse_datetime(string(), datetime_opts()) -> datetime().
%% old format: "YYYY-MM-DD HH:mm[:SS][.sss]"
parse_datetime(String, []) ->
    parse_datetime(old_format_to_iso8601(String), [iso8601_simple]);

%% iso8601_simple format: "YYYY-MM-DDTHH:mm[:SS][.sss]Z"
parse_datetime(String, [iso8601_simple]) ->
    {DS, TS, Ms} =
        case string:tokens(String, "T.") of
            [DateString, TimeString, MsString] ->
                try
                    Truncated = three_digits1(drop_utc_zone_designator(MsString)),
                    F = <<"0", (Truncated)/binary>>,
                    {DateString, TimeString, binary_to_float(F)}
                catch _:_ ->
                    throw({bad_datetime, String})
                end;
            [DateString, TimeString] ->
                try
                    {DateString, drop_utc_zone_designator(TimeString), undefined}
                catch _:_ ->
                    throw({bad_datetime, String})
                end;
            _ -> throw({bad_datetime, String})
        end,
    try
        {H, M, S} = parse_time(TS),
        Time = case Ms of
                   undefined -> {H, M, S};
                   _         -> {H, M, S + Ms}
               end,
        {parse_date(DS), Time}
    catch _:_ ->
        throw({bad_datetime, String})
    end.

%% drops UTC +0 zone designator
drop_utc_zone_designator([]) -> throw(no_zone_designator);
drop_utc_zone_designator("Z") -> [];
drop_utc_zone_designator([H|T]) ->
    [H | drop_utc_zone_designator(T)].

%% replaces space with "T" and appends "Z" zone designator
old_format_to_iso8601([]) -> [$Z];
old_format_to_iso8601([$  | T]) ->
    [$T | old_format_to_iso8601(T)];
old_format_to_iso8601([H | T]) ->
    [H | old_format_to_iso8601(T)].


-spec date_to_binary(calendar:date()) -> binary().
date_to_binary({Year, Month, Day}) ->
    <<  (integer_to_binary(Year))/binary, <<"-">>/binary,
        (two_digits(Month))/binary, <<"-">>/binary,
        (two_digits(Day))/binary >>.

-spec time_to_binary(time()) -> binary().
time_to_binary({Hour, Minute, Second}) ->
    SecondsBin =
        case Second of
            _ when is_integer(Second) ->
                two_digits(Second);
            _ when is_float(Second) ->
                Ss = trunc(Second),
                Ms = trunc(Second*1000) - Ss*1000,
                <<(two_digits(Ss))/binary, ".", (three_digits(Ms))/binary>>
        end,
    <<  (two_digits(Hour))/binary, ":",
        (two_digits(Minute))/binary, ":",
        SecondsBin/ binary >>.

three_digits(Int) when is_integer(Int) ->
    if Int =:= 0 -> <<"000">>;
       Int < 10 -> <<"00", (integer_to_binary(Int))/binary>>;
       Int < 100 -> <<"0", (integer_to_binary(Int))/binary>>;
       Int < 1000 -> integer_to_binary(Int)
    end.

three_digits1([]) -> <<".000">>;
three_digits1([Ch1]) -> <<$., Ch1, "00">>;
three_digits1([Ch1,Ch2]) -> <<$., Ch1, Ch2, "0">>;
three_digits1([Ch1,Ch2,Ch3|_]) -> <<$., Ch1, Ch2, Ch3>>.

%%--------------------------------------------------------------------
%% @doc
%% Converts calendar:datetime() to "YYYY-MM-DD HH:mm:SS" or
%% "YYYY-MM-DDTHH:mm:SSZ".
%% @since 0.8.3
%% @end
%%--------------------------------------------------------------------

-spec datetime_to_binary(datetime()) -> binary().
datetime_to_binary({Date, Time}) ->
    datetime_to_binary({Date, Time}, []).

-type datetime_opts() :: [datetime_opt()].
-type datetime_opt() :: iso8601_simple.

-spec datetime_to_binary(datetime(), datetime_opts()) -> binary().
datetime_to_binary({Date, Time}, [iso8601_simple]) ->
    << (date_to_binary(Date))/binary, "T",
       (time_to_binary(Time))/binary, "Z" >>;
datetime_to_binary({Date, Time}, []) ->
    << (date_to_binary(Date))/binary, " ",
       (time_to_binary(Time))/binary >>.

%%--------------------------------------------------------------------
%% @doc
%% Converts erlang:timestamp() to "YYYY-MM-DD HH:mm:SS.Milli" or
%% "YYYY-MM-DDTHH:mm:SS.MicroZ".
%% @since 0.8.3
%% @end
%%--------------------------------------------------------------------
-spec now_to_binary(erlang:timestamp()) -> binary().
now_to_binary(Now) ->
    now_to_binary(Now,[]).

-spec now_to_binary(erlang:timestamp(), datetime_opts()) -> binary().
now_to_binary(Now = {_, _, Micro},[]) ->
    {Date, {H, M, S}} = calendar:now_to_universal_time(Now),
    Time = {H, M, S + (Micro / 1000000)},
    << (date_to_binary(Date))/binary, " ",
       (time_to_binary(Time))/binary >>;
now_to_binary(Now = {_, _, Micro},[iso8601_simple]) ->
    {Date, {H, M, S}} = calendar:now_to_universal_time(Now),
    Time = {H, M, S + (Micro / 1000000)},
    << (date_to_binary(Date))/binary, "T",
       (time_to_binary(Time))/binary, "Z" >>.

two_digits(D) when is_integer(D) andalso D < 10 ->
    << <<"0">>/binary, (integer_to_binary(D))/binary >>;
two_digits(D) when is_integer(D) ->
    integer_to_binary(D);
two_digits(F) when is_float(F) andalso F < 10.0 ->
    << <<"0">>/binary, (list_to_binary(emodel_num:digits(F)))/binary >>;
two_digits(F) when is_float(F) ->
    list_to_binary(emodel_num:digits(F)).

%%--------------------------------------------------------------------
%% @doc
%% Converts datetime in format "YYYY-MM-DD HH:mm:SS[.XXXXXX]" to
%% erlang's timestamp.
%% @end
%%--------------------------------------------------------------------

-spec datetime_to_now(binary()) -> erlang:timestamp().
datetime_to_now(Value) ->
    datetime_to_now(Value, []).

-spec datetime_to_now(binary(), datetime_opts()) -> erlang:timestamp().
datetime_to_now(Value, DatetimeFormat) ->
    ValueStr = binary_to_list(Value),
    {Date, {H, M, Sm}} = emodel_date:parse_datetime(ValueStr, DatetimeFormat),
    S = trunc(Sm),
    Micros = trunc(Sm * 1000000 - S * 1000000),
    Datetime = {Date, {H, M, S}},
    {MegaSec, Sec, _} = emodel_utils:datetime_to_now(Datetime),
    {MegaSec, Sec, Micros}.

%%--------------------------------------------------------------------
%% Unit tests
%%--------------------------------------------------------------------

-ifdef(TEST).

-define(DATE, ["2011-09-28", {2011,09,28}]).
-define(TIME, ["17:00:00", {17,00,00}]).
-define(DATETIME, [hd(?DATE) ++ " " ++ hd(?TIME), {hd(tl(?DATE)), hd(tl(?TIME))}]).

validate_parsers_test_() ->
    [
     {"Test good date and time...",
      [
       ?_test(?assertEqual(tl(?DATE), [parse_date(hd(?DATE))])),
       ?_test(?assertEqual(tl(?TIME), [parse_time(hd(?TIME))])),
       ?_test(?assertEqual(tl(?DATETIME), [parse_datetime(hd(?DATETIME))])),
       ?_test(?assertEqual({{2011,09,28}, {17,00,00.000}}, parse_datetime("2011-09-28T17:00:00.000Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,00,00.000}}, parse_datetime("2011-09-28T17:00:00.Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,00,00.120}}, parse_datetime("2011-09-28T17:00:00.12Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,12,34.567}}, parse_datetime("2011-09-28T17:12:34.56789012Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,12,34.567}}, parse_datetime("2011-09-28T17:12:34.567Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,12,34.0}}, parse_datetime("2011-09-28T17:12:34.Z"))),
       ?_test(?assertEqual({{2011,09,28}, {17,12,34}}, parse_datetime("2011-09-28T17:12:34Z"))),
       %% FIXME iso8601 parsing quirks, to be removed after dropping support of old datetime format
       ?_test(?assertEqual({{2011,09,28}, {17,12,0.567}}, parse_datetime("2011-09-28T17:12.567Z"))),     %% no seconds
       ?_test(?assertEqual({{2011,09,28}, {17,00,00.000}}, parse_datetime("2011-09-28T17:00:00.Z"))),    %% no seconds and ms
       %%
       ?_test(?assertEqual(<<"2014-06-24 17:00:00">>, datetime_to_binary({{2014,06,24},{17,0,0}}))),
       ?_test(?assertEqual(<<"2014-06-24T17:00:00Z">>, datetime_to_binary({{2014,06,24},{17,0,0}},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24 17:00:00.000">>, datetime_to_binary({{2014,06,24},{17,0,0.0}}))),
       ?_test(?assertEqual(<<"2014-06-24T17:00:00.000Z">>, datetime_to_binary({{2014,06,24},{17,0,0.0}},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24 08:52:56.248">>, now_to_binary({1403,599976,248444}))),
       ?_test(?assertEqual(<<"2014-06-24T08:52:56.248Z">>, now_to_binary({1403,599976,248444},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24T08:52:56.300Z">>, now_to_binary({1403,599976,300000},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24T08:52:56.030Z">>, now_to_binary({1403,599976,030000},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24T08:52:56.003Z">>, now_to_binary({1403,599976,003000},[iso8601_simple]))),
       ?_test(?assertEqual(<<"2014-06-24T08:52:56.000Z">>, now_to_binary({1403,599976,000000},[iso8601_simple]))),
       fun() ->
               Now = emodel_date:datetime_to_now(<<"2014-07-09 07:43:38.85">>),
               ?assertEqual({1404,891818,850000}, Now),
               ?assertEqual(Now, emodel_date:datetime_to_now(emodel_date:now_to_binary(Now)))
       end,
       ?_test(?assertEqual(<<"2014-01-03 12:11:13.001">>, now_to_binary({1388,751073,1200}))),
       ?_test(?assertEqual(<<"2015-03-20 12:11:57">>, datetime_to_binary({{2015,03,20},{12,11,57}}))),
       ?_test(?assertEqual(<<"2015-03-20 12:11:57.000">>, datetime_to_binary({{2015,03,20},{12,11,57.000}}))),
       ?_test(?assertEqual(<<"2015-03-20 12:11:57.001">>, datetime_to_binary({{2015,03,20},{12,11,57.001}}))),
       ?_test(?assertEqual(<<"2015-03-20 12:11:57.123">>, datetime_to_binary({{2015,03,20},{12,11,57.123456}}))),
       ?_test(?assertEqual(<<"2015-03-20 12:11:00.000">>, datetime_to_binary({{2015,03,20},{12,11,00.000728}})))
      ]},
     {"Test bad date and time...",
      [
       ?_test(?assertThrow({bad_date, _}, parse_date("huy"))),
       ?_test(?assertThrow({bad_date, _}, parse_date("11-01-01"))),
       ?_test(?assertThrow({bad_date, _}, parse_date("2011-15-01"))),
       ?_test(?assertThrow({bad_date, _}, parse_date("2011-12-41"))),
       ?_test(?assertThrow({bad_time, _}, parse_time("23"))),
       ?_test(?assertThrow({bad_time, _}, parse_time("23:60"))),
       ?_test(?assertThrow({bad_time, _}, parse_time("23:30:-1"))),
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("2011-09-28T.Z"))),                           %% malformed
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("2011-09-28T17:12:34.567", [iso8601_simple]))), %% no Z
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("2011-09-28T17:12:34", [iso8601_simple]))),     %% no Z
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("2011-09-28 17:12:34.567Z", [iso8601_simple]))),%% " " instead of "T"
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("11-11-11 23:30"))),
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("11-11-11     23:30"))),
       ?_test(?assertThrow({bad_datetime, _}, parse_datetime("2011-11-11 23:30 am")))
      ]}
    ].

precision_test_() ->
    [
     ?_test(?assertEqual(
         <<"2015-08-27T15:40:48.370Z">>,
         now_to_binary(datetime_to_now(<<"2015-08-27T15:40:48.370Z">>, [iso8601_simple]), [iso8601_simple])))
    ].

-endif.
