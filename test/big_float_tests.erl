%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(big_float_tests).
-include_lib("eunit/include/eunit.hrl").

from_test() ->
  Str = "-0.123987",
  First = big_float:from_string(Str),
  Zero = big_float:create_zero(),
  ?assertEqual(Str, big_float:to_string(First)),
  ?assertEqual(Zero, big_float:from_int(0)),
  ?assertEqual(Zero, big_float:from_float(0.0)),
  ?assertEqual(Zero, big_float:from_string("0.0")),
  ?assertEqual(big_float:from_string("7"), big_float:from_int(7)).

to_test() ->
  Str = "-120.123456",
  ?assertEqual(Str, big_float:to_string(big_float:from_string(Str))).

is_test() ->
  N1 = big_float:from_string("-1.0"),
  N2 = big_float:from_string("0.00"),
  N3 = big_float:from_string("11.0"),
  ?assert(big_float:is_negate(N1)),
  ?assertNot(big_float:is_negate(N2)),
  ?assertNot(big_float:is_negate(N3)),
  ?assertNot(big_float:is_zero(N1)),
  ?assert(big_float:is_zero(N2)),
  ?assertNot(big_float:is_zero(N3)).

negate_test() ->
  First = big_float:from_int(12),
  Second = big_float:from_int(-12),
  ?assertEqual(big_float:negate(First), Second).

compare_test() ->
  N1 = big_float:from_string("-12.329"),
  N2 = big_float:from_string("14.0"),
  N3 = big_float:from_int(14),
  ?assertEqual(big_float:compare(N1, N2), -1),
  ?assertEqual(big_float:compare(N3, N1), 1),
  ?assertEqual(big_float:compare(N2, N3), 0).

add_test() ->
  N1 = big_float:from_string("-0.123"),
  N2 = big_float:from_string("13.0123"),
  R1 = "12.8893",
  ?assertEqual(big_float:to_string(big_float:add(N1,N2)),R1),
  N3 = big_float:from_string("27.5"),
  N4 = big_float:from_string("12.65"),
  R2 = "40.15",
  ?assertEqual(big_float:to_string(big_float:add(N3,N4)),R2),
  N5 = big_float:from_string("-10.899"),
  N6 = big_float:from_string("10.899"),
  ?assert(big_float:is_zero(big_float:add(N5,N6))).

mul_test() ->
  Pi = big_float:from_string("3.14159265358979323846"),
  R = big_float:from_string("123212"),
  R2 = big_float:mul(R,R),
  R3 = big_float:mul(big_float:from_int(4),R2),
  ?assertEqual(big_float:from_string("190772547167.88087896520326106496"),
    big_float:mul(R3,Pi)),
  N3 = big_float:from_int(25),
  N4 = big_float:from_string("0.001"),
  ?assertEqual(big_float:from_string("0.025"),
    big_float:mul(N3, N4)).




