-module(rps).
-export([beat/1,lose/1,result/2,tournament/2]).
-include_lib("eunit/include/eunit.hrl").

-type play() :: rock | paper | scissors.

% Rock paper scissors. Return what play beats caller's play
-spec beat(play()) -> play().
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

% Rock paper scissors. Return what play loses to caller's play
-spec lose(play()) -> play().
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.


% Rock paper scissors. Given both player's play return 0 for draw (both
% players chose same play), 1 if first beats second, -1 if first loses to
% second
-spec result(play(), play()) -> integer().
result(X,X) -> 0;
result(X,Y) ->
  case beat(Y) of
    X -> 1;
    _ -> -1
  end.

% Rock paper scissors. Given two lists of players move return positive integer if
% first player wins, negative integer if first player loses and 0 if draw
-spec tournament(list(), list()) -> integer().
tournament(Xs,Ys) when length(Xs) == length(Ys) ->
  lists:sum(lists:zipwith(fun result/2, Xs, Ys)).


% Tests
tournament_lose_test() ->
    Left  = [rock, rock,  paper,    paper],
    Right = [rock, paper, scissors, rock],
    ?assertEqual(-1, tournament(Left, Right)).

tournament_draw_test() ->
  Left  = [rock, paper, scissors, rock],
  Right = [rock, paper, scissors, rock],
  ?assertEqual(0, tournament(Left, Right)).

tournament_win_test() ->
  Left = [rock, paper, scissors, rock],
  Right  = [rock, rock,  paper,    paper],
  ?assertEqual(1, tournament(Left, Right)).
