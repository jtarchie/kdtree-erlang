-module(kdtree).

-record(point, {id, coords}).
-record(node, {point=#point{}, left = nil, right = nil}).
-record(neighbor, {id = -1, distance = 0}).

-export([create_point/2, build/1, points_from_file/1, build_from_file/1]).
-export([find_neighbors/3, distance/2, neighbors_for_points/3, pretty_print_neighbors_for_points/1]).

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
		{{X1,_}, {Y1,_},{Id1,_}} = {string:to_float(X), string:to_float(Y), string:to_integer(Id)},
		[create_point(Id1,[X1,Y1]) | Accum]
	end, []).
neighbors_for_points(Points, Tree, K) ->
	SortedPoints = lists:sort(fun(A,B) -> A#point.id =< B#point.id end, Points),
	PointsNeighbors = lists:map(
		fun(Point) ->
			io:format("~p", [Point#point.id]),
			[_OriginPoint | Neighbors] = kdtree:find_neighbors(Tree, Point#point.coords, K + 1),
			Neighbors
		end,
		SortedPoints
	),
	lists:zip(SortedPoints, PointsNeighbors).
pretty_print_neighbors_for_points([]) ->
	ok;
pretty_print_neighbors_for_points([{Point, PointNeighbors} | Neighbors]) ->
	io:format("~p ", [Point#point.id]),
	io:format("~s~n", [string:join(lists:map(fun(PN)-> integer_to_list(PN#neighbor.id) end, PointNeighbors),",")]),
	pretty_print_neighbors_for_points(Neighbors).
build_from_file(Filename) ->
	build(points_from_file(Filename)).
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

distance(A, B) ->
	lists:sum([ math:pow(An-Bn, 2) || {An,Bn} <- lists:zip(A,B)]).
found_neighbor(Node, Coords, K, Neighbors) when length(Neighbors) < K ->
	D = distance((Node#node.point)#point.coords, Coords),
	lists:sort(
		fun(A,B) -> B#neighbor.distance =< A#neighbor.distance end,
		[#neighbor{id=(Node#node.point)#point.id, distance=D} | Neighbors]
	);
found_neighbor(Node, Coords, _K, Neighbors) ->
	D = distance((Node#node.point)#point.coords, Coords),
	[LastNeighbor | _] = Neighbors,
	if D < LastNeighbor#neighbor.distance ->
		[_ | NewNeighbors] = lists:sort(
			fun(A,B) -> B#neighbor.distance =< A#neighbor.distance end,
			[#neighbor{id=(Node#node.point)#point.id, distance=D} | Neighbors]
		),
		NewNeighbors;
	true ->
		Neighbors	
	end.
find_neighbors(Node, Coords, K) ->
	lists:reverse(find_neighbors(Node, Coords, K, [], 0)).
find_neighbors(nil, _Coords, _K, Neighbors, _Depth) ->
	Neighbors;
find_neighbors(Node, Coords, K, Neighbors, _Depth) when (Node#node.left =:= nil) andalso (Node#node.right =:= nil) ->
	found_neighbor(Node, Coords, K, Neighbors);
find_neighbors(Node, Coords, K, Neighbors, Depth) ->
	Axis = (Depth rem 2) + 1,
	Ad = lists:nth(Axis, Coords) - lists:nth(Axis, (Node#node.point)#point.coords),
	NearNeighbors = near_neighbors(Node, Coords, K, Neighbors, Depth, Ad),
	NewestNeighbors = far_neighbors(Node, Coords, K, NearNeighbors, Depth, Ad),
	found_neighbor(Node, Coords, K, NewestNeighbors).
near_neighbors(Node, Coords, K, Neighbors, Depth, Ad) when Ad =< 0 ->
	find_neighbors(Node#node.left, Coords, K, Neighbors, Depth + 1);
near_neighbors(Node, Coords, K, Neighbors, Depth, _Ad) ->
	find_neighbors(Node#node.right, Coords, K, Neighbors, Depth + 1).
far_neighbors(Node, Coords, K, Neighbors, Depth, Ad) when length(Neighbors) =< K andalso Ad =< 0 ->
	find_neighbors(Node#node.right, Coords, K, Neighbors, Depth + 1);
far_neighbors(Node, Coords, K, Neighbors, Depth, Ad) when length(Neighbors) =< K andalso Ad > 0 ->
	find_neighbors(Node#node.left, Coords, K, Neighbors, Depth + 1);
far_neighbors(Node, Coords, K, [LastNeighbor | Neighbors], Depth, Ad) when ((Ad * Ad) < LastNeighbor#neighbor.distance) andalso (Ad =< 0) ->
	find_neighbors(Node#node.right, Coords, K, [LastNeighbor | Neighbors], Depth + 1);
far_neighbors(Node, Coords, K, [LastNeighbor | Neighbors], Depth, Ad) when ((Ad * Ad) < LastNeighbor#neighbor.distance) andalso (Ad > 0) ->
	find_neighbors(Node#node.left, Coords, K, [LastNeighbor | Neighbors], Depth + 1);
far_neighbors(_Node, _Coords, _K, Neighbors, _Depth, _Ad) ->
	Neighbors.