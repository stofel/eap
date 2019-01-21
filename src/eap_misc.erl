-module(eap_misc).

-export([datetime_to_binary/0]).



%% @doc Converts the datetime to binary
-spec datetime_to_binary() -> binary().
datetime_to_binary() ->
    datetime_to_binary(calendar:local_time()).

-spec datetime_to_binary(calendar:datetime()) -> binary().
datetime_to_binary({{YYYY, MM, D}, {H, M, S}}) ->
    <<(int_fmt(YYYY))/binary, "-", (int_fmt(MM))/binary,
    "-", (int_fmt(D))/binary, " ", (int_fmt(H))/binary,
    ":", (int_fmt(M))/binary, ":", (int_fmt(S))/binary>>.

%% Internal functions
-spec int_fmt(non_neg_integer()) -> binary().
int_fmt(N) when N < 10 ->
    <<$0, (integer_to_binary(N))/binary>>;
int_fmt(N) ->
    integer_to_binary(N).

