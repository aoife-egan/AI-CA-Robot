:- dynamic item/2.
:- dynamic robot/3.
:- dynamic connected/3.

%ASSUMPTIONS:
% 1. Paul is in r101.
% 2. A door has 3 states: open,closed & locked.
% 3. An unlocked door is a closed door.
% 4. If a door is not open, it is locked.

%TEST QUERY: ?- solve([deliver_item(coffee, r101)]).

%FACTS:
%Corridor Section -> Corridor Section Connections
connected(c0, cs1, open).
connected(cs1, c101, open).
connected(c101, c103, open).
connected(c103, c105, open).
connected(c105, c107, open).
connected(c107, c109, open).
connected(c109, c111, open).
connected(c109, c113, open).
connected(c113, c115, open).
connected(c115, c117, open).
connected(c117, c118, open).
connected(c118, c119, open).
connected(c119, c121, open).
connected(c121, c123, open).
connected(c123, c125, open).
connected(c125, c127, open).
connected(c127, c129, open).
connected(c129, c131, open).
connected(c131, c132, open).
connected(c132, c133, open).
connected(c133, c0, open).
connected(c118, cc118, open).
% Lab -> Lab Connections
connected(lab1, lab2, open).
connected(lab1, lab4, locked).
connected(lab2, lab3, locked).
% Corridor section -> Lab Connections
connected(cs1, lab1, locked).
connected(c101, lab1, locked).
connected(c103, lab2, locked).
connected(c107, lab2, locked).
connected(c123, lab3, locked).
connected(c125, lab4, locked).
connected(c129, lab4, locked).
% Corridor Section -> Room Connection
connected(c101, r101, locked).
connected(c103, r103, locked).
connected(c105, r105, locked).
connected(c107, r107, locked).
connected(c109, r109, locked).
connected(c111, r111, locked).
connected(c113, r113, locked).
connected(c115, r115, locked).
connected(c117, r117, open).
connected(c119, r119, locked).
connected(c121, r121, locked).
connected(c123, r123, locked).
connected(c125, r125, locked).
connected(c127, r127, locked).
connected(c129, r129, locked).
connected(c131, r131, locked).
connected(cc118, canteen, locked).
%is_Connected(A,B,door_state)
is_connected(A, B, S) :-
  connected(A, B, S).
is_connected(A, B, S) :-
  connected(B, A, S).

%Items and their locations
item(key, r117).
item(coffee, canteen).
% robot(current_location,current_power,current_items)
robot(c0, 150, []).

%cost(state, cost)
cost(open, 1).   % move(1)
cost(closed, 4). % open(3) + move(1)
cost(locked, 6). % open(3) + move(1) + unlock(2)

%RULES:
solve([]).
solve([Goal|Rest]) :-
  format("Robot will try to: ~w \n", [Goal]),
  Goal, %call deliver_item rule
 solve(Rest).

deliver_item(CurrentItem, FinalDestination) :-
  %check where the current item is
  item(CurrentItem, ItemLocation),
  %check if the destination door is locked
  is_locked(FinalDestination),
  format("Final destination is locked. Key needed\n"),
  item(key, KeyLocation),
  get_path(KeyLocation),
  get_item(key),
  get_path(ItemLocation),
  get_item(CurrentItem),
  get_path(FinalDestination),
  give(Item),
  format("The ~a was  delivered to ~a \n", [Item, FinalDestination]).

% Check if door to goal is locked
is_locked(Goal) :-
  is_connected(_, Goal, locked),
  \+ has(key).

has(Item) :-
  robot(_, _, I),
  member(Item, I).

% Using uniform cost search to get the optimal path to goals.
get_path(Goal) :-
  format("Searching for a path to get to: ~w \n", [Goal]),
  % Check the current location of the robot
  robot(L, _, _),
  % Search for paths to the goal from the robots location
  uni_cost([[L]], Goal, Path, ExploredNodes),
  % Reversing the path found to output & travel in order
  reverse(Path, Reverse),
  format("Total nodes explored: ~a \n", [ExploredNodes]),
  format("Optimal path: ~w \n", [Reverse]),
  %traverse the optimal path chosen
  traverse(Reverse),
  format("The goal: ~a has been reached. \n", [Goal]).

uni_cost([[Goal|Path]|_],Goal,[Goal|Path],0).
uni_cost([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths),
    append(Queue,NewPaths,Queue1),
    sort_queue(Queue1,NewQueue),
    uni_cost(NewQueue,Goal,FinalPath,M),
    N is M+1.

% Will use this rule to expand paths if the robot DOESN'T have the key.
% Does care about the state of the doors.
extend([Node|Path], NewPaths) :-
  \+ has(key),
  findall([NewNode, Node|Path],
    ((is_connected(Node, NewNode, open); is_connected(Node, NewNode, closed)),
    \+ member(NewNode, Path)),
    NewPaths).


% Will use this rule to expand paths if the robot has the key.
% Doesn't care about the state of the doors.
extend([Node|Path], NewPaths) :-
  has(key),
  findall([NewNode, Node|Path],
    (is_connected(Node, NewNode, _),
    \+ member(NewNode, Path)),
    NewPaths).

sort_queue(L,L2) :-
    swap(L,L1), !,
    sort_queue(L1,L2).
sort_queue(L,L).

swap([X,Y|T],[Y,X|T]) :-
    path_cost(X,CX),
    path_cost(Y,CY),
    CX>CY.
swap([X|T],[X|V]) :-
    swap(T,V).

% Calculate the cost of the paths found
path_cost([A,B],Cost) :-
    cost_of_path(A,B,Cost).
path_cost([A,B|T],Cost) :-
    cost_of_path(A,B,Cost1),
    path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

cost_of_path(A, B, Cost) :-
  connected(A, B, DoorState),
  cost(DoorState, Cost).
cost_of_path(A, B, Cost) :-
  connected(B, A, DoorState),
  cost(DoorState, Cost).

% Traverse the optimal path moving one node at a time
traverse([]).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  is_connected(L, Node, open),
  %PowerCost is 1,
  move_robot(L, Node), %,PowerCost),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  is_connected(L, Node, locked),
  unlock_door(L, Node),
  open_door(L, Node),
  move_robot(L, Node),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  is_connected(L, Node, closed),
  open_door(L, Node),
  move_robot(L, Node),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node == L,
  traverse(Path).

% Acutally move robot along path
% Checking forward connection A -> B
move_robot(A, B):-
  %,PowerCost) :-
  robot(A, Power, _),
  Power >= 1,
  connected(A, B, open),
  format("Robot moving from ~a to ~a \n", [A, B]),
  retract(robot(A, Power, Item)),
  ActualPower is Power - 1, %PowerCost
  assert(robot(B, ActualPower, Item)).
% Checking backward connection B -> A
move_robot(A, B):- %, PowerCost) :-
  robot(A, Power, _),
  Power >= 1,
  connected(B, A, open),
  format("Robot moving from ~a to ~a \n", [A, B]),
  retract(robot(A, Power, Item)),
  ActualPower is Power - 1, %PowerCost
  assert(robot(B, ActualPower, Item)).

% Unlocking doors & changing their state to closed
unlock_door(A, B) :-
  connected(A, B, locked),
  robot(A, P, _),
  P >= 2,
  has(key),
  format("Door unlocked between ~a and ~a \n", [A, B]),
  retract(connected(A, B, locked)),
  retract(robot(L, P, I)),
  ActualPower is P - 2,
  assert(connected(A, B, closed)),
  assert(robot(L, ActualPower, I)).
unlock_door(A, B) :-
  connected(B, A, locked),
  robot(A, P, _),
  P >= 2,
  has(key),
  format("Door unlocked between ~a and ~a \n", [A, B]),
  retract(connected(B, A, locked)),
  retract(robot(L, P, I)),
  ActualPower is P - 2,
  assert(connected(B, A, closed)),
  assert(robot(L, ActualPower, I)).

% Change the door state to open if the door is closed
open_door(A, B) :-
  robot(A, P, _),
  P >= 3,
  connected(A, B, closed),
  format("Door opened between ~a and ~a \n", [A, B]),
  retract(connected(A, B, closed)),
  retract(robot(L, P, I)),
  ActualPower is P - 3,
  assert(connected(A, B, open)),
  assert(robot(L, ActualPower, I)).
open_door(A, B) :-
  robot(A, P, _),
  P >= 3,
  connected(B, A, closed),
  format("Door opened between ~a and ~a \n", [A, B]),
  retract(connected(B, A, closed)),
  retract(robot(L, P, I)),
  ActualPower is P - 3,
  assert(connected(B, A, open)),
  assert(robot(L, ActualPower, I)).

get_item(Item) :-
  robot(L, _, _),
  item(Item, L),
  format("Getting ~a in ~a \n", [Item, L]),
  retract(item(Item, L)),
  retract(robot(L,P,I)),
  assert(robot(L,P,[Item|I])).

% Give item
give(Item) :-
  has(Item),
  format("Giving ~a \n", [Item]),
  retract(robot(L, P, I)),
  delete(I, Item, NewI),
  assert(item(Item, L)),
  assert(robot(L, P, NewI)).


