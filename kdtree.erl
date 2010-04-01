-module(kdtree).

-record(point, {id, coords}).
-record(node, {point=#point{}, left = nil, right = nil}).

-export([create_point/2, build/1, points_from_file/1, find_neighbors/3]).

create_point(Id, Coords) ->
	#point{id=Id, coords=Coords}.

for_each_line(Device, Proc, Accum) ->
	case io:get_line(Device, "") of
		eof	-> file:close(Device), Accum;
		Line -> NewAccum = Proc(Line, Accum),
				for_each_line(Device, Proc, NewAccum)
	end.
points_from_file(Filename) ->
	{ok, Device} = file:open(Filename, [read]),
	for_each_line(Device, fun(Line, Accum) ->
		[Id, X, Y] = string:tokens(Line, " "),
		{{X1,_}, {Y1,_}} = {string:to_float(X), string:to_float(Y)},
		[create_point(Id,[X1,Y1]) | Accum]
	end, []).
build([]) ->
	nil;
build(Points) ->
	build(Points, 0).
build([], _) ->
	nil;
build(Points, Depth) ->
	Axis = (Depth rem 2) + 1,
	SortedPoints = lists:sort(fun(A,B) -> lists:nth(Axis, A#point.coords) =< lists:nth(Axis, B#point.coords) end, Points),
	Median = round(length(SortedPoints) / 2),
	#node{
		point = lists:nth(Median, SortedPoints),
		left = build(lists:sublist(SortedPoints, Median-1), Depth + 1),
		right = build(lists:sublist(SortedPoints, Median+1, length(SortedPoints)))
	}.

% find_neighbors(Node, Coords, Size) ->
% 	find_neighbors(Node, Coords, Size, 0).
% find_neighbors(nil, _, _, _) ->
% 	nil;
% find_neighbors(Node, Coords, Size, Depth) ->
% 	Axis = (Depth rem 2) + 1,
% 	{CoordsAxis, NodeAxis} = {lists:nth(Axis, Coords), lists:nth(Axis,(Node#node.point)#point.coords)},
% 	if (Node#node.right =:= nil) or ((Node#node.left =/= nil) and (CoordsAxis =< NodeAxis)) ->
% 		{Nearer, Further} = {Node#node.left, Node#node.right};
% 	true ->
% 		{Nearer, Further} = {Node#node.right, Node#node.left}
% 	end.