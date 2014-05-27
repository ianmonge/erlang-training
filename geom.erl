
-module(geom).
-export([area/2, area/3, areaTupla/1, abs/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
area(N, M) ->
  true = N >= 0,
  true = M >= 0,
  N * M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
area(rectangle, N, M) -> area(N, M);
area(triangle, N, M) -> area(N, M) / 2;
area(ellipse, N, M) -> math:pi() * area(N, M);
area(_, _, _) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
areaTupla({rectangle, N, M}) -> area(N, M);
areaTupla({triangle, N, M}) -> area(N, M) / 2;
areaTupla({ellipse, N, M}) -> math:pi() * area(N, M);
areaTupla({_, _, _}) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abs(N) when N < 0 -> -N;
abs(N) -> N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
