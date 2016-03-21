EModel
======

Erlang data transformation/validation library.

Example
===

Map model
---

```erlang
1> Model = [
    {<<"login">>, required, string, login, [non_empty]},
    {<<"notifyAt">>, required, datetime, notify_at, []},
    {<<"type">>, required, {strlist, string}, type, [
        {each, [
            {enum, [<<"sms">>, <<"email">>, <<"twitter">>, <<"fb">>]}
        ]}
    ], [<<"email">>]},
    {<<"title">>, required, string, title, [non_empty]},
    {<<"message">>, optional, string, msg, []}
].
```

### Success

```erlang
2> Data = #{
    <<"login">> => <<"egobrain">>,
    <<"notifyAt">> => <<"2088-12-07T8:00:00Z">>,
    <<"type">> => <<"email,sms">>,
    <<"title">> => <<"Happy 100 birth day!">>,
    <<"message">> => <<"Hooray!!!">>
}.

3> emodel:from_map(Data, #{}, Model).
{ok,#{login => <<"egobrain">>,
      msg => <<"Hooray!!!">>,
      notify_at => {{2088,12,7},{8,0,0}},
      title => <<"Happy 100 birth day!">>,
      type => [<<"email">>,<<"sms">>]}}.
```

### Error

```erlang
3> Data = #{
    <<"login">> => <<"">>,
    <<"notifyAt">> => <<"2088-12-07T8-00-00Z">>,
    <<"type">> => <<"email,sms,vk">>,
    <<"msg">> => <<"Hooray!">>
}.
4> emodel:from_map(Data, #{}, Model).
{error,[{<<"login">>,<<"is empty">>},
        {<<"notifyAt">>,<<"bad datetime">>},
        {<<"type">>,[{3,unknown}]},
        {<<"title">>,required}]}
```

Tuple model
---

```erlang
1> rr(data, {login, notify_at, type, title, msg}).

2> Model = [
    {<<"login">>, required, string, #data.login, [non_empty]},
    {<<"notifyAt">>, required, datetime, #data.notify_at, []},
    {<<"type">>, required, {strlist, string}, #data.type, [
        {each, [
            {enum, [<<"sms">>, <<"email">>, <<"twitter">>, <<"fb">>]}
        ]}
    ], [<<"email">>]},
    {<<"title">>, required, string, #data.title, [non_empty]},
    {<<"message">>, optional, string, #data.msg, []}
].

3> emodel:from_map(Data, #data{}, Model).
{ok,#data{login = <<"egobrain">>,
          notify_at = {{2088,12,7},{8,0,0}},
          type = [<<"email">>,<<"sms">>],
          title = <<"Happy 100 birth day!">>,msg = <<"Hooray!!!">>}}.
```

Model description
===

Model description is a list of rules which will be applied from top to down.

```erlang
%% M - is a model (tuple or map)
%% A - is a value stored by ext_key() in given data (map or proplist)
%% B - converted data
%% R - error reason

-type rule(M) ::
{name(), required(M), data_converter(A,B,M,R), position(), [validator(B,M,R)], default_value(M,B,R)} |
%% Default may be omitted
{name(), required(M), data_converter(A,B,M,R), position(), [validator(B,M,R)]} |
%% Also for complex cases rule can be
{name(), required(M), setter(A,M,R)}.

-type name() :: any(). %% Key in given map or proplist.
-type position() :: %% Key or position in model
    any() | %%  for map
    non_neg_integer().  %%  for tuple

-type required(M) :: req_opt() | fun((M) -> req_opt()).
-type req_opt() ::
    optional | %% Value in resulting model is optional
    required | %% Value is required
    ignore   . %% rule will be ignored

-type data_converter(A,B,M,R) :: converter(A,B,M,R) | Type :: term().
-type converter(A,B,M,R) :: fun((A) -> {ok,B} | {error,R}), %% declared in emodel_converters.
                         :: fun((A,M) -> {ok,B} | {error,R}).

-type data_validator(B,M,R) ::
        Type :: term(),
        fun((B) -> ok | {error, R}) |
        validator(A,B,R).
-type validator(B,M,R) ::
        fun((B) -> ok | {error,R}) | %% declared in emodel_validators.
        fun((B,M) -> ok | {error,R}).

-type default_value(M,B,R) ::
        B :: any() |
        fun((M) -> {ok,B} | {error,R}) |
        fun((M, Setter) -> {ok, M} | {error, R}) where
            Setter :: fun((B,M) -> {ok,M}|{error,R}).

-type setter(A,M,R) -> fun((A,M) -> {ok,M} | {error,R}).
```

All converter or validator ```Type``` params will be converted to
```converter(A,B,R)``` or ```validator(B,M,R)``` at compile call,
using ```converters``` and ```validator``` **options** or
default ```emodel_converters:get_converter/2``` and ```emodel_validators:get_validator/2``` functions.
So you can declare your own simple or complex types and validators.

Types
---

Simple:
- integer
- float
- boolean
- date
- time
- datetime
- string

Complex types are:
- list (example, ```{list, integer}```)
- ulist (unique list, ```{ulist, integer}```)
- strlist (list as string like ```<<"1,2,3,4">>```)

Validators
---

- '>' (numbers validation, usage example ```{'>', 3}```
- '>='
- '<'
- '=<'
- non_empty (check that string is non empty)
- enum (value must exists in given list, ```{enum, [<<"sms">>, <<"email">>]}```
- each (check each array item with the given rules, ```{each, [non_empty]}```

Custom converters and validators
===

You can define your custom converter or validator right in code

```erlang
[
 {<<"type">>, required, string, type, [{enum, [<<"daily">>, <<"monthly">>]}]},
 {<<"month">>,
  fun(#{type := <<"monthly">>}) -> require; %% Custom req fun
     (_) -> ignore
  end,
  fun(<<"Jan">>) -> {ok, 1}; %% Custom converter
     (<<"Feb">>) -> {ok, 2};
     ...
     (_) -> {error, <<"Must be valid month short name">>}
  end,
  m,
  [
   fun(V) -> %% Custom validator
      case V > element(2, erlang:date()) of
        true -> ok;
        false -> {error, <<"Must be greater than current month">>}
      end
   end
  ],
  fun(_ModelMap) -> {_,M,_} = erlang:date(), M end %% Lazy default value
 }
].
```

or define easily reusable type via options

```erlang
month_short_name(<<"Jan">>) -> {ok, 1};
month_short_name(<<"Feb">>) -> {ok, 2};
     ...
month_short_name(_) -> {error, <<"Must be valid month short name">>}.

gt_than_cur_month(V, _Model) ->
    {_, CurMonth, _} = erlang:date(),
    fun(V, _Model) -> %% Custom validator
      case V > CurMonth of
        true -> ok;
        false -> {error, <<"Must be greater than current month">>}
      end
   end.

get_converter(month, _Opts) -> fun month_short_name/1;
get_converter(Type, Opts) -> emodel_converters:get_converter(Type, Opts).

get_validator('> cur_month', _Opts) -> fun gt_than_cur_month/1;
get_validator(V, Opts) -> emodel_validators:get_validator(V, Opts).

%% In this case it's better to define your own function with default opts
from_map(Data, Model, Description) ->
    emodel:from_map(Data, Model, Description, #{
        converters => fun get_converter/2,
        validators => fun get_validator/2
    }).

%% Usage
from_map(Data, #{}, [
    {<<"month">>, required, month, m, ['> cur_month']}
]).

```

Compile
===

Model will be compiled automatically each time you use it via ```from_map/_``` or ```from_proplist/_``` functions.
If you want to use model several times, it's event better to compile model first.
For example, when you want to parse list of objects, it's better to write 

```erlang

1> CompiledModel = emodel:compile([
    {<<"login">>, required, string, login, []},
    {<<"password">>, required, string, password, []},
    ...
], map). %% You must explicitly specify the type of model you want to build

2> emodel:list(Data, fun(ItemData) -> emodel:from_map(ItemData, #{}, CompiledModel) end).

{ok, [#{login => <<"james">>, password => <<"qw67HJ1">>},
      #{login => ...
      ...
     ]}.
```

Real world example
===

```erlang
%% cowboy 1.0 handler
get_json(Req, State) ->
    {QsVals, Req2} = cowboy_req:qs_vals(Req),
    Result = emodel:from_proplist(QsVals, #{}, [
        {<<"limit">>, required, integer, limit, [{'>', 0}, {'<', 10000}], emodel:default_value(1000)},
        {<<"offset">>, required, integer, offset, [{'>=', 0}], emodel:default_value(0)},
        {<<"fields">>, required, {strlist, binary}, fields, [
            {each, [
                {enum, [<<"id">>, <<"name">>, <<"isArchived">>]}
            ]}
        ], emodel:default_value([<<"id">>, <<"name">>])},
    case Result of
        {ok, #{limit := L, offset := O, fields := F}} ->
            %% Get data using L,O,F
        {error, Reason} ->
           %% Encode end set error
    end.
```
