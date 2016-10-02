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
  is_int/1,
  set_int/2,
  set_fract/2,
  set_sign/2,
  add/2,
  mul/2,
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
  fract = #fract_part{} :: #fract_part{},
  sign = positive :: sign()
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
      Int = list_to_integer(I),
      case is_zerostring(Temp) of
        true -> from_int(Int);
        false ->
          Acc = length(Temp),
          {F, Shift} = get_fract_shift(Temp, Acc),
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
      end
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

-spec mul(#bigfloat{}, #bigfloat{}) -> #bigfloat{}.
mul(#bigfloat{sign = zero} = Z, #bigfloat{}) -> Z;
mul(#bigfloat{}, #bigfloat{sign = zero} = Z) -> Z;
mul(#bigfloat{int = AInt, fract = #fract_part{value = 0}},
    #bigfloat{int = BInt, fract = #fract_part{value = 0}}) ->
  from_int(AInt * BInt);
mul(#bigfloat{sign = S} = A, #bigfloat{sign = S} = B) ->
  mul(A, B, positive);
mul(#bigfloat{} = A, #bigfloat{} = B) ->
  mul(A, B, negative).

-spec mul(#bigfloat{}, #bigfloat{}, sign()) -> #bigfloat{}.
mul(
    #bigfloat{int = AInt, fract = #fract_part{value = AF, acc = AL}},
    #bigfloat{int = BInt, fract = #fract_part{value = BF, acc = BL}},
    Sign) ->
  {I1, F1} = split_number(AInt * BF, BL),
  {I2, F2} = split_number(BInt * AF, AL),
  Temp1 = case is_zero(F1) of
            true -> create_zero();
            false -> set_sign(set_fract(create_zero(), F1), positive)
          end,
  Temp2 = case is_zero(F2) of
            true -> create_zero();
            false -> set_sign(set_fract(create_zero(), F2), positive)
          end,
  Temp3 = if
            AF =:= 0; BF =:= 0 -> create_zero();
            true ->
              set_sign(set_fract(create_zero(),
                create_fract(AF * BF, AL + BL)), positive)
          end,
  BNumber = add_list([Temp1, Temp2, Temp3]),
  #bigfloat
  {
    int = AInt * BInt + I1 + I2 + get_int(BNumber),
    fract = get_fract(BNumber),
    sign = Sign
  }.

-spec get_fract(float(), pos_integer()) -> {pos_integer(), pos_integer()}.
get_fract(0.0, _) -> {0, 0};
get_fract(Float, Acc) ->
  Str = float_to_list(Float),
  Re = "(\\d)[.](\\d+)e-0?(\\d+)",
  {match, [_, _, {_, N}, {I, Len}]} = re:run(Str, Re),
  T1 = string:copies("0",
    list_to_integer(string:substr(Str, I + 1, Len)) - 1),
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
  Add = case (S =:= negative) and (Int =:= 0) of
          true -> "-";
          false -> ""
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
compare(#bigfloat{int = AI, fract = #fract_part{value = AF}, sign = S},
    #bigfloat{int = BI, fract = #fract_part{value = BF}, sign = S}) ->
  if
    AI > BI -> 1;
    AI < BI -> -1;
    true -> if
              AF > BF -> 1;
              AF < BF -> -1;
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

-spec is_zero(#bigfloat{} | #fract_part{}) -> boolean().
is_zero(#bigfloat{sign = zero}) -> true;
is_zero(#bigfloat{}) -> false;
is_zero(#fract_part{value = 0}) -> true;
is_zero(#fract_part{}) -> false.

-spec is_int(#bigfloat{}) -> boolean().
is_int(#bigfloat{fract = #fract_part{value = 0}}) -> true;
is_int(#bigfloat{}) -> false.

-spec create_zero() -> #bigfloat{}.
create_zero() ->
  #bigfloat
  {
    fract = create_zero_fract(),
    sign = zero
  }.

-spec set_int(#bigfloat{}, number()) -> #bigfloat{}.
set_int(X = #bigfloat{}, Number) ->
  case get_sign(Number) of
    zero -> X#bigfloat{int = Number};
    Other -> X#bigfloat{int = Number, sign = Other}
  end.

-spec set_sign(#bigfloat{}, sign()) -> #bigfloat{}.
set_sign(X = #bigfloat{}, Sign) ->
  X#bigfloat{sign = Sign}.

-spec set_fract
    (#bigfloat{}, non_neg_integer()) -> #bigfloat{};
    (#bigfloat{}, #fract_part{}) -> #bigfloat{}.
set_fract(X = #bigfloat{}, Number) when is_integer(Number), Number >= 0 ->
  F = #fract_part{value = Number, acc = length(integer_to_list(Number))},
  X#bigfloat{fract = F};
set_fract(X = #bigfloat{}, F = #fract_part{}) ->
  X#bigfloat{fract = F}.

-spec get_fract(#bigfloat{}) -> #fract_part{}.
get_fract(#bigfloat{fract = R}) -> R.

-spec get_int(#bigfloat{}) -> integer().
get_int(#bigfloat{sign = zero}) -> 0;
get_int(#bigfloat{int = V, sign = negative}) -> -abs(V);
get_int(#bigfloat{int = V}) -> V.

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

-spec split
    (string(), non_neg_integer(), number) ->
  {non_neg_integer(), non_neg_integer()};
    (string(), non_neg_integer(), string) ->
  {string(), string()}.
split(Str, 0, number) ->
  {list_to_integer(Str), 0};
split(Str, Pos, Type) ->
  Count = length(Str) - Pos,
  First = string:substr(Str, 1, Count),
  Second = string:substr(Str, Count + 1, Pos),
  case Type of
    string -> {First, Second};
    number -> case First of
                [] -> {0, list_to_integer(Second)};
                _ -> {list_to_integer(First), list_to_integer(Second)}
              end
  end.

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
  {Number, S_Number, Shift, Diff} = case D =:= Aacc of
                                      true -> {Bf, Af, As, D - Bacc};
                                      false -> {Af, Bf, Bs, D - Aacc}
                                    end,
  {Part1, Part2} = split(fract_to_string(S_Number, Shift), Diff, number),
  C = (Number + Part1) * pow10(Diff) + Part2,
  F = C rem pow10(D),
  {C div pow10(D),
    #fract_part
    {
      value = F,
      acc = D,
      shift = D - length(integer_to_list(F))
    }}.

-spec add_list(list(#bigfloat{})) -> #bigfloat{}.
add_list(List) ->
  lists:foldl(fun(El, Acc) ->
    add(Acc, El) end, create_zero(), List).

-spec split_number(integer(), non_neg_integer())
      -> {non_neg_integer(), #fract_part{}}.
split_number(0, _) -> {0, create_zero_fract()};
split_number(N, Index) when N < 0 ->
  split_number(abs(N), Index);
split_number(Number, Index) ->
  Str = integer_to_list(Number),
  Length = length(Str),
  if
    Length =:= Index -> {0, create_fract(Number, 0, Index)};
    Length > Index ->
      {W, F} = split(Str, Index, string),
      Whole = list_to_integer(W),
      Value = list_to_integer(F),
      IsZero = fun(X) -> X =:= $0 end,
      {Whole, create_fract(Value, count_first(F, IsZero), Index)};
    Length < Index ->
      {0, create_fract(Number, Index - Length, Index)}
  end.

-spec count_first(list(T), fun((Elem :: T) -> boolean()))
      -> non_neg_integer().
count_first(List, Pred) ->
  count_first(List, Pred, 0).

-spec count_first(list(T), fun((Elem :: T) -> boolean()), non_neg_integer())
      -> non_neg_integer().
count_first([], _, Acc) -> Acc;
count_first([H | T], Pred, Acc) ->
  case Pred(H) of
    true -> count_first(T, Pred, Acc + 1);
    false -> Acc
  end.

-spec create_fract(non_neg_integer(), non_neg_integer())
      -> #fract_part{}.
create_fract(0, _) -> create_zero_fract();
create_fract(Number, Length) ->
  Str = integer_to_list(Number),
  create_fract(Number, Length - length(Str), Length).

-spec create_fract(non_neg_integer(), non_neg_integer(), non_neg_integer())
      -> #fract_part{}.
create_fract(Number, Shift, Length) ->
  #fract_part
  {
    value = Number,
    acc = Length,
    shift = Shift
  }.
