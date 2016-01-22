-module(emodel_utils).

-export([
         error_writer_map/2,
         error_writer_foreach/2,
         error_writer_foldl/3,
         error_writer_mapfoldl/3
        ]).

-export([
         enumerate/1,
         lift2/1
        ]).

%% =============================================================================
%% Error list Functions
%% =============================================================================

-spec error_writer_foreach(Fun, List) -> ok | {error, [Reason]} when
      List :: [Elem],
      Fun :: fun((Elem) -> ok | {error, Reason}).
error_writer_foreach(Fun, List) ->
    case error_writer_foreach_(Fun, List, []) of
        [] -> ok;
        Errors -> {error, lists:reverse(Errors)}
    end.

error_writer_foreach_(_Fun, [], Errors) -> Errors;
error_writer_foreach_(Fun, [H|T], Errors) ->
    case Fun(H) of
        ok -> error_writer_foreach_(Fun, T, Errors);
        {error, R} -> error_writer_foreach_(Fun, T, [R|Errors])
    end.

-spec error_writer_map(Fun, List) -> {ok, NewList} | {error, Reasons} when
      List :: [Elem],
      Fun :: fun((Elem) -> {ok, NewElem} | {error, Reason}),
      NewList :: [NewElem],
      Reasons :: [Reason].
error_writer_map(Fun, List) ->
    Result = error_writer_foldl(
        fun(V, Acc) ->
            case Fun(V) of
                {ok, NewV} -> {ok, [NewV|Acc]};
                {error, _Reason} = Err -> Err
            end
        end, [], List),
    case Result of
        {ok, NewList} -> {ok, lists:reverse(NewList)};
        {error, _Reasons} = Err -> Err
    end.

-spec error_writer_foldl(Fun, State, List) -> {ok, NewState} | {error, Reasons} when
      List :: [Elem],
      Fun :: fun((Elem, State) -> {ok, NewState} | {error, Reason}),
      Reasons :: [Reason].
error_writer_foldl(Fun, InitState, Opts) ->
    case error_foldl(Fun, InitState, Opts) of
        {ResultState, []} -> {ok, ResultState};
        {_ResultState, ResultErrors} -> {error, lists:reverse(ResultErrors)}
    end.

-spec error_writer_mapfoldl(Fun, State, List) -> {ok, NewList, State} | {error, Reasons} when
      List :: [Elem],
      Fun :: fun((Elem, State) -> {ok, NewElem, State} | {error, Reason}),
      NewList :: [NewElem],
      Reasons :: [Reason].
error_writer_mapfoldl(Fun, State, List) ->
    Result =
        lists:foldl(
            fun(V, {S, Acc, Errs}) ->
                case Fun(V, S) of
                    {ok, V2, S2} ->
                        {S2, [V2|Acc], Errs};
                    {error, R} ->
                        {S, Acc, [R|Errs]}
                end
            end, {State, [], []}, List),
    case Result of
        {S, Acc, []} ->
            {ok, lists:reverse(Acc), S};
        {_, _, Errors} ->
            {error, Errors}
    end.

-spec error_foldl(Fun, State, List) -> {NewState, Reasons} when
      List :: [Elem],
      Fun :: fun((Elem, State) -> {ok, NewState} | {error, Reason}),
      Reasons :: [Reason].
error_foldl(Fun, InitState, Opts) ->
    lists:foldl(
        fun(Val, {State, Errors}) ->
            case Fun(Val, State) of
                ok ->
                    {State, Errors};
                {ok, State2} ->
                    {State2, Errors};
                {error, Reason} ->
                    {State, [Reason | Errors]}
            end
        end,
        {InitState, []},
        Opts).

enumerate(List) -> enumerate_(List, 1).
enumerate_([], _I) -> [];
enumerate_([H|T], I) -> [{I, H}|enumerate_(T, I+1)].

lift2(F) when is_function(F, 2) -> F;
lift2(F) when is_function(F, 1) -> fun(V, _) -> F(V) end.
