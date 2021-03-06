-module(index).
-export([get_file_contents/1,show_file_contents/1,analyze_file_contents/1,tokenize/1,words/1,lower/1,exclude_word/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.

% Given a file contents, the output of this function should be a list of entries
% consisting of a word and a list of the ranges of lines on which it occurs.
%
% For example, the entry
%
% { "foo" , [{3,5},{7,7},{11,13}] }
%
% means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.
%
% The output is sorted so that the words occur in lexicographic order.

analyze_file_contents(L) ->
  TK = tokenize(L),
  Words = unique_sorted_tokens(TK),
  build_entries(Words, TK).

% Build a List of structs. The struct contain a List of words (in lower-case)
% that are over 3 letters long and not in our excluded list, and a LineNumber
% for where the words were found.
%
% For example :
%
% > index:tokenize(["Now is the time", "For all good men to come to the aid", "of their country"]).
% [{["time"],1},{["good","come"],2},{["country"],3}]
%
tokenize(L) ->
  lists:reverse(tokenize(L, 1, [])).

tokenize([], _, Acc) -> Acc;
tokenize([Xs|Xss], N, Acc) ->
  tokenize(Xss, N + 1, [{words(Xs), N} | Acc]).

% Take a List of Words (space delimited) and remove standard punctuation and
% return a List of words that are lower-cased.
%
% For example :
%
% > index:words("Hello is it me you are looking for?").
%  ["hello","looking"]
words(L) ->
  lists:reverse(words(string:tokens(L, ",. "), [])).

words([], Acc) -> Acc;
words([H|T], Acc) ->
  LowH = lower(H),
  case exclude_word(LowH) of
    false -> words(T, [LowH | Acc]);
    _ -> words(T, Acc)
  end.

% Lowercase a Word
%
% For example :
%
% > index:lower("HELLO").
% "hello"
lower(L) -> lists:reverse(lower(L, [])).

lower([], Acc) -> Acc;
lower([X|Xs], Acc) ->
  Lower = lower_it(X),
  case Lower >= $a andalso Lower =< $z of
    true ->
      lower(Xs, [Lower | Acc]);
    false ->
      lower(Xs, Acc)
  end.

lower_it(X) ->
  case X >= $A andalso X =< $Z of
    true -> X + 32;
    false -> X
  end.

% Determine whether to exclude a word based on our rules. Either the word is
% too short (<=3 letters) or its too common (based on a list of what we
% judge are common words).
%
% For example :
%
% > index:exclude_word("the").
% true
% > index:exclude_word("liberty").
% false
exclude_word(H) ->
  length(H) =< 3 orelse common_word(H).

common_word(H) ->
  lists:member(H, ["this", "thus", "that", "those", "these", "from", "here", "have",
                   "will", "what", "their", "which", "they", "should", "gave", "give"]).


% Find all the unique words within the data structure made by calling tokenize
% on some text. Sort them alphabetically.

unique_sorted_tokens(L) ->
 lists:sort(unique_tokens(L)).

% Find all the unique words within the data structure made by calling tokenize
% on some text.
unique_tokens(L) ->
 unique_tokens(L, []).

unique_tokens([], Acc) -> Acc;
unique_tokens([H|T], Acc) ->
 {L,_LineNumber} = H,
 unique_tokens(T, add_tokens(L, Acc)).

% Add a token (Word) to a List as long as its not curently a member of the List
add_tokens([], Acc) -> Acc;
add_tokens([Word|Words], Acc) ->
 case lists:member(Word, Acc) of
   true -> add_tokens(Words, Acc);
   _ -> add_tokens(Words, [Word | Acc])
 end.

% Given a set of unique Words (lower-cased) and a TokenList build by the
% tokenize() function, build the entries expected by analyze_file_contents
% function.

build_entries(Words, TokenList) ->
  lists:reverse(build_entries(Words, TokenList, [])).

build_entries([], _TokenList, Acc) -> Acc;
build_entries([Word|Words], TokenList, Acc) ->
  FoundOnLines = find_word_in_token_list(Word, TokenList, []),
  build_entries(Words, TokenList, [build_entry(Word, FoundOnLines) | Acc]).

% Look for a Word in a TokenList entry. Each item in our TokenList consists of
% a List of words (lower-cased) and the LineNumber on which they were found.
%
% For example :
%
% {["test", "second", "watch"], 3}
%
% Indicates that the words "test", "second", and "watch" were found on line
% number 3.
%

find_word_in_token_list(_Word, [], Acc) -> Acc;
find_word_in_token_list(Word, [H|T], Acc) ->
  {L,LineNumber} = H,
  case lists:member(Word, L) of
    true -> find_word_in_token_list(Word, T, [LineNumber | Acc]);
    _ -> find_word_in_token_list(Word, T, Acc)
  end.

% Build our Final output for a Word required by analyze_file_contents
%
% For example :
%
% build_entry("test", [4,5,9,10]).
%
% would return :
%   {"test", [{4,5}, {9,10}]}
%
build_entry(Word, FoundOnLines) ->
  SortedFoundOnLines = lists:sort(FoundOnLines),
  {Word, build_line_struct(SortedFoundOnLines)}.

% Build up our required Line Structure to indicate lines where Word appears.
% If a Word appears on consecutive lines then the pair of numbers in the
% Struct indicate the Start line and End line. If a Word is not on a consecutive
% line then the Start and End will be the same.
%
build_line_struct(L) ->
  lists:reverse(build_line_struct(L, 0, 0, [])).

build_line_struct([], StartLine, CurrentLine, Acc) ->
  case CurrentLine of
    0 -> Acc;
    _ -> [{StartLine,CurrentLine} | Acc]
  end;

build_line_struct([H|T], 0, 0, Acc) ->
  build_line_struct(T, H, H, Acc);

build_line_struct([H|T], StartLine, CurrentLine, Acc) ->
  case CurrentLine + 1 == H of
    true -> build_line_struct(T, StartLine, H, Acc);
    _ -> build_line_struct(T, H, H, [{StartLine,CurrentLine} | Acc])
  end.
