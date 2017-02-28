%%% @doc Functional Programming in Erlang example code
-module(double).
-include_lib("eunit/include/eunit.hrl").
-export([double/1,evens/1,median/1,modes/1]).

% Define an Erlang function double/1 to double the elements of a list of numbers.
double(Xs) when Xs =/= [] ->  double(Xs, []).

double([], Acc) -> lists:reverse(Acc);
double([X|Xs],Acc) ->
  double(Xs, [X * 2 | Acc]).

% Define a function evens/1 that extracts the even numbers from a list of integers.
evens(Xs) when Xs =/= [] -> evens(Xs, []).

evens([], Acc) -> lists:reverse(Acc);
evens([X|Xs], Acc) ->
  case X rem 2 of
    0 -> evens(Xs, [X | Acc]);
    _ -> evens(Xs, Acc)
  end.

% Find the median of a list of numbers. This is the middle element
% when the list is ordered (if the list is of even length you should
% average the middle two)
median(Xs) when Xs =/= [] ->
  Length = length(Xs),
  median(Xs, Length, (Length rem 2)).

% Match on even number of elements in List
median(Xs, Length, 0) ->
  [X,Y|_] = lists:nthtail((Length div 2)-1, Xs),
  (X + Y) / 2;

% Match on odd number of elements in List
median(Xs, Length, _) ->
  [X|_] = lists:nthtail((Length div 2), Xs),
  X.

% Find the modes of a list of numbers: this is a list consisting of the numbers
% that occur most frequently in the list; if there is is just one, this will be
% a list with one element only.
%
% This code works by getting occurrence counts of each element in the caller's
% list into the CL (counted list) variable where each element in the list is
% a struct of {Count,Value}. Then we find the highest count in that list. Then
% we find all Values in the list with that Count.
modes(L) when L =/= [] ->
    CL = count_elements(L,[]),
    Mode = find_mode(CL, {0,0}),
    modes(CL, Mode, []).

modes([], _, Acc) -> Acc;
modes([X|Xs], {ModeCount, _ModeVal} = Mode, Acc) ->
  {Count,Element} = X,
  case Count == ModeCount of
    true -> modes(Xs, Mode, [Element | Acc]);
    _ -> modes(Xs, Mode, Acc)
  end.

find_mode([], Mode) -> Mode;
find_mode([X|Xs], {ModeCount, _ModeVal} = Mode) ->
  {Count,_Element} = X,
  case Count > ModeCount of
    true -> find_mode(Xs, X);
    _ -> find_mode(Xs, Mode)
  end.

% For each Value in the List add 1 to its current Count. If the Value hasn't
% been found yet we set its Count to 1.
count_elements([], Acc) -> Acc;
count_elements([X|Xs], Acc) ->
  count_elements(Xs, increment(Acc, X, [], 0)).

% Increment the Count for a Value. If we haven't found the Value in our
% new "Counted List" so far we insert it with a Count of 1.
increment([], Val, Acc, Found) ->
  case Found of
    0 -> [{1,Val} | Acc];
    _ -> Acc
  end;

increment([X|Xs], Val, Acc, Found) ->
  {Count,Element} = X,
  case Element of
    Val -> increment(Xs, Val, [{Count + 1, Element} | Acc], 1);
    _ -> increment(Xs, Val, [X | Acc], Found)
  end.


% Test double
double_single_test() -> ?assert(double([1]) =:= [2]).
double_long_test() -> ?assert(double([1,2,3,4]) =:= [2,4,6,8]).

% Test evens
evens_none_test() -> ?assert(evens([1]) =:= []).
evens_one_test() ->  ?assert(evens([2]) =:= [2]).
evens_many_test() -> ?assert(evens([1,2,3,4,5,6,7,8]) =:= [2,4,6,8]).

% Test median
median_one_test() -> ?assert(median([1]) =:= 1).
median_even_test() -> ?assert(median([1,2,3,4]) =:= 2.5).
median_odd_test() -> ?assert(median([1,2,3,4,5]) =:= 3).

% Test modes()
modes_single_test() -> ?assert(modes([1]) =:= [1]).
modes_long_test() -> ?assert(modes([1,1,2,2,3,3,3,1,1]) =:= [1]).
modes_multiple_test() -> ?assert(double:modes([1,1,2,2,3,3,3,1,1,3]) =:= [3,1]).
