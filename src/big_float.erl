%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(big_float).

%% API
-export([
  from_float/1,
  from_float/2,
  from_string/1,
  from_int/1,
  create_zero/0,
  is_negate/1,
  is_zero/1,
  add/2,
  compare/2,
  negate/1,
  to_string/1
]).

-type sign() :: positive | zero | negative.

-record(fract_part,
{
  value = 0 :: non_neg_integer(),
  acc = 6 :: non_neg_integer(),
  shift = 0 :: non_neg_integer()
}).

-record(bigfloat,
{
  int = 0 :: integer(),
  fract = #fract_part{},
  sign = positive ::sign()
}).

-spec from_float(float()) -> #bigfloat{}.
from_float(Float) ->
  from_float(Float, 6).

-spec from_float(float(), pos_integer()) -> #bigfloat{}.
from_float(0.0, _) -> create_zero();
from_float(Float, Acc) when Acc >= 0, is_float(Float) ->
  Int = trunc(Float),
  {F, S} = get_fract(abs(Float - Int), Acc),
  #bigfloat
  {
    int = Int,
    fract = #fract_part
    {
      value = F,
      acc = Acc,
      shift = S
    },
    sign = get_sign(Float)
  }.

-spec from_string(string()) -> #bigfloat{}.
from_string(Str) ->
  case is_zerostring(Str) of
    true -> create_zero();
    false ->
      List = string:tokens(Str, "."),
      [I, Temp] = case length(List) of
                    1 -> [hd(List), ""];
                    2 -> List
                  end,
      Acc = length(Temp),
      {F, Shift} = get_fract_shift(Temp, Acc),
      Int = list_to_integer(I),
      #bigfloat
      {
        int = Int,
        fract = #fract_part
        {
          value = F,
          acc = Acc,
          shift = Shift
        },
        sign = sign_char(hd(Str))
      }
  end.

-spec from_int(integer()) -> #bigfloat{}.
from_int(0) -> create_zero();
from_int(Int) when is_integer(Int) ->
  #bigfloat
  {
    int = Int,
    fract = create_zero_fract(),
    sign = get_sign(Int)
  }.

-spec add(#bigfloat{}, #bigfloat{}) -> #bigfloat{}.
add(#bigfloat{sign = zero}, #bigfloat{} = N) -> N;
add(#bigfloat{} = N, #bigfloat{sign = zero}) -> N;
add(#bigfloat{int = I1, fract = #fract_part{value = F, shift = S}} = A,
    #bigfloat{int = I2, fract = #fract_part{value = F, shift = S}} = B) ->
  case I1 =:= -I2 of
    true -> create_zero();
    false -> add(A, B)
  end;
add(#bigfloat{} = A, #bigfloat{fract = #fract_part{value = 0}} = B) ->
  add(B, A);
add(#bigfloat{int = Ai, fract = #fract_part{value = 0}, sign = S},
    #bigfloat{int = Bi, fract = Bf, sign = S}) ->
  #bigfloat
  {
    int = Ai + Bi,
    fract = Bf,
    sign = S
  };
add(#bigfloat{int = Ai, fract = Af, sign = S},
    #bigfloat{int = Bi, fract = Bf, sign = S}) ->
  {Q, Fract} = add_fract(Af, Bf),
  #bigfloat
  {
    int = Q + Ai + Bi,
    fract = Fract,
    sign = S
  };
add(#bigfloat{sign = negative} = A, #bigfloat{} = B) ->
  add(B, A);
add(#bigfloat{int = Ai, fract = Af},
    #bigfloat{int = Bi,
      fract = #fract_part{value = Bf, acc = Bacc} = F}) ->
  Add = get_addition(Bf, Bacc),
  Temp = set_value(F, Add),
  {Q, Fract} = add_fract(Temp, Af),
  Int = Ai + Bi - 1 + Q,
  #bigfloat
  {
    int = Int,
    fract = Fract,
    sign = get_sign(Int)
  }.

-spec get_fract(float(), pos_integer()) -> {pos_integer(), pos_integer()}.
get_fract(0.0, _) -> {0, 0};
get_fract(Float, Acc) ->
  Str = float_to_list(Float),
  {match, [_, _, {_, N}, {I, Len}]} = re:run(Str, "(\\d)[.](\\d+)e-0?(\\d+)"),
  T1 = string:copies("0", list_to_integer(string:substr(Str, I + 1, Len)) - 1),
  T2 = string:substr(Str, 1, 1),
  T3 = string:substr(Str, 3, N),
  get_fract_shift(string:join([T1, T2, T3], ""), Acc).

-spec get_fract_shift(string(), pos_integer()) ->
  {pos_integer(), pos_integer()}.
get_fract_shift("", _) -> {0, 0};
get_fract_shift(Str, Acc) ->
  List = string:substr(Str, 1, Acc),
  Int = list_to_integer(List),
  case re:run(List, "^0+") of
    nomatch -> {Int, 0};
    {match, [{_, Acc}]} -> {Int, 0};
    {match, [{_, L}]} -> {Int, L}
  end.

-spec to_string(#bigfloat{}) -> string().
to_string(#bigfloat{sign = zero}) -> "0";
to_string(#bigfloat
{
  int = Int,
  fract = #fract_part
  {
    value = Fract,
    shift = Shift
  },
  sign = S
}) ->
  Add = case S of
          negative -> "-";
          positive -> ""
        end,
  S1 = integer_to_list(Int),
  S2 = fract_to_string(Fract, Shift),
  string:concat(Add, string:join([S1, ".", S2], "")).

-spec negate(#bigfloat{}) -> #bigfloat{}.
negate(#bigfloat{sign = zero} = BF) -> BF;
negate(#bigfloat{int = Int, sign = positive} = BF) ->
  BF#bigfloat{int = -Int, sign = negative};
negate(#bigfloat{int = Int, sign = negative} = BF) ->
  BF#bigfloat{int = -Int, sign = positive}.

-spec compare(#bigfloat{}, #bigfloat{}) -> integer().
compare(#bigfloat{sign = S} = A, #bigfloat{sign = S} = B) ->
  if
    A#bigfloat.int > B#bigfloat.int -> 1;
    A#bigfloat.int < B#bigfloat.int -> -1;
    true -> if
              A#bigfloat.fract > B#bigfloat.fract -> 1;
              A#bigfloat.fract < B#bigfloat.fract -> -1;
              true -> 0
            end
  end;
compare(#bigfloat{sign = negative}, #bigfloat{}) ->
  -1;
compare(#bigfloat{sign = positive}, #bigfloat{}) ->
  1;
compare(#bigfloat{}, #bigfloat{sign = negative}) ->
  1;
compare(#bigfloat{}, #bigfloat{}) ->
  -1.

-spec is_negate(#bigfloat{}) -> boolean().
is_negate(#bigfloat{sign = S}) -> S =:= negative.

-spec is_zero(#bigfloat{}) -> boolean().
is_zero(#bigfloat{sign = S}) -> S =:= zero.

-spec create_zero() -> #bigfloat{}.
create_zero() ->
  #bigfloat
  {
    fract = create_zero_fract(),
    sign = zero
  }.

-spec get_sign(number()) -> sign().
get_sign(0) -> zero;
get_sign(0.0) -> zero;
get_sign(N) when N < 0 -> negative;
get_sign(N) when N > 0 -> positive.

-spec sign_char(char()) -> negative | positive.
sign_char($-) -> negative;
sign_char(_) -> positive.

-spec pow10(integer()) -> integer().
pow10(N) when N =:= 0 -> 1;
pow10(N) -> 10 * pow10(N - 1).

-spec fract_to_string(integer(), non_neg_integer()) -> string().
fract_to_string(F, 0) ->
  integer_to_list(F);
fract_to_string(F, Shift) ->
  string:concat(string:copies("0", Shift), integer_to_list(F)).

-spec split(string(), non_neg_integer()) -> {integer(), integer()}.
split(Str, 0) ->
  {list_to_integer(Str), 0};
split(Str, Pos) ->
  Count = length(Str) - Pos,
  {list_to_integer(string:substr(Str, 1, Count)),
    list_to_integer(string:substr(Str, Count + 1, Pos))}.

-spec is_zerostring(string()) -> boolean().
is_zerostring(Str) ->
  lists:all(fun(X) -> X =:= $. orelse X =:= $- orelse X =:= $0 end, Str).

-spec get_addition(pos_integer(), pos_integer()) -> pos_integer().
get_addition(Number, Len) ->
  pow10(Len) - Number.

-spec create_zero_fract() -> #fract_part{}.
create_zero_fract() ->
  #fract_part{acc = 0}.

-spec set_value(#fract_part{}, integer()) -> #fract_part{}.
set_value(#fract_part{} = N, Value) ->
  N#fract_part{value = Value}.

-spec add_fract(#fract_part{}, #fract_part{})
      -> {integer(), #fract_part{}}.
add_fract(#fract_part{value = Af, acc = Aacc, shift = As},
    #fract_part{value = Bf, acc = Bacc, shift = Bs}) ->
  D = max(Aacc, Bacc),
  {Number, Split_Number, Shift, Diff} = case D =:= Aacc of
                                          true -> {Bf, Af, As, D - Bacc};
                                          false -> {Af, Bf, Bs, D - Aacc}
                                        end,
  {First_Part, Second_Part} = split(fract_to_string(Split_Number, Shift), Diff),
  C = (Number + First_Part) * pow10(Diff) + Second_Part,
  F = C rem pow10(D),
  {C div pow10(D),
    #fract_part
    {
      value = F,
      acc = D,
      shift = D - length(integer_to_list(F))
    }}.