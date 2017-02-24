%%% @doc Functional Programming in Erlang example code
-module(example).
-include_lib("eunit/include/eunit.hrl").
-export([perimeter/2,bits/1,tail_bits/1,area/2,enclose/2]).

% Provide functions to calculate perimeter for various objects. The
% atom provided as first parameter identifies the object type. The tuple
% that follows provides object details.
perimeter(rectangle, {_X, _Y, H, W}) ->
  (H * 2) + (W * 2);
perimeter(circle, {_X, _Y, R}) ->
  2 * math:pi() * R;
perimeter(triangle, {A, B, C}) ->
  A + B + C.

% Provide functions to calculate the area of various objects. Rectangle and
% circle are straight-forward. Triangle is calculated using Heron's formula
% http://www.mathwarehouse.com/geometry/triangles/area/herons-formula-triangle-area.php
area(rectangle, {_X, _Y, H, W}) ->
  H * W;
area(circle, {_X, _Y, R}) ->
  math:pi() * R * R;
area(triangle, {A, B, C}) ->
  S = (A+B+C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

% Provide functions to return the smallest enclosing rectangle of the caller's
% object.
enclose(rectangle, {X, Y, H, W}) ->
  {rectangle, {X, Y, H, W}};
enclose(circle, {X, Y, R}) ->
  {rectangle, {X, Y, 2*R, 2*R}};
enclose(triangle, {A, B, C}) ->
   W = max(A,max(B,C)),
   T = {A, B, C},
   H = area(triangle, T)/(1/2*W),
   {rectangle, {W/2, H/2, H, W}}.

% Provide function to calculate number of bits in a positive integer using
% direct recursion. We count bits by using Erlang bitwise operators.
bits(0) -> 0;
bits(N) when N > 0 ->
  bits(N bsr 1) + (N band 1).

% Provide function to calculate number of bits in a positive integer using
% tail recursion. Same bit technique as direct.
tail_bits(0) -> 0;
tail_bits(N) -> tail_bits(N, 0).
tail_bits(0, S) -> S;
tail_bits(N, S) ->
  tail_bits(N bsr 1, S + (N band 1)).

% Tests - using eunit. From Erlang shell.
% 1> c(example).
% {ok,example}
% 2> example:test().
%   All 9 tests passed.
% ok

% Perimeter tests
perimeter_triangle_test() -> ?assert(perimeter(triangle, {3, 4, 5}) =:= 12).
perimeter_circle_test() -> ?assert(perimeter(circle, {2, 2, 4}) =:= 25.132741228718345).
perimeter_rectangle_test() -> ?assert(perimeter(rectangle, {2, 2, 4, 4}) =:= 16).

% Bits test (direct recursion)
bits_0_test() -> ?assert(bits(0) =:= 0).
bits_1_test() -> ?assert(bits(1) =:= 1).
bits_large_test() -> ?assert(bits(63) =:= 6).

% Bits test (tail recursion)
tail_bits_0_test() -> ?assert(tail_bits(0) =:= 0).
tail_bits_1_test() -> ?assert(tail_bits(1) =:= 1).
tail_bits_large_test() -> ?assert(tail_bits(63) =:= 6).
