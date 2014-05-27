
-module(example2).
-export([sumSeq/1, sumList/1, genSeq/1, test_range/0]).

sumSeq(Num) ->
  % List = lists:seq(1, Num),
  List = genSeq(Num),
  sumList(List, 0).

sumList(List) -> sumList(List, 0).

sumList([], Acc) -> Acc;
sumList([H|T], Acc) -> sumList(T, H + Acc).

genSeq(Num) -> genSeq(Num, []).

genSeq(1, List) -> [1 | List];
genSeq(Num, List) -> genSeq(Num - 1, [Num|List]).

test_range() ->
  Res1 = genSeq(10),
  Res1 = lists:seq(1, 10),
  ok.
