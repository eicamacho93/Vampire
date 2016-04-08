:- [map].
:- [obj].

%used to hold the information of the current room the agent is in
:- dynamic in_room/1.
%in_room(room(wineCellar,'The only clue that this was once a wine cellar is the broken bottles in the corner, and large areas with no dust against the wall where racks used to be.',[],[holyWater],[exit(basement,visible,unlocked)])).
in_room(room(frontEntrance,'You are in the front of a misterious castle, there is a door that you can open.',[],[],[exit(lobby,visible,unlocked)])).

%used to hold list of possible exits at current room
:- dynamic roomExits/1.
roomExits([]).

%holds a list of visited rooms
:- dynamic roomsVisited/1.
roomsVisited([]).

%used to hold list of objects at current room
:- dynamic roomObjs/1.
roomObjs([]).

%holds a list of objects that agent carries
:- dynamic agentObjs/2.
agentObjs(objects,[]).


%used to add to explored rooms list
:- dynamic exploredRooms/2.
exploredRooms(room(Name, Description, Agents, Objects, Exits),L).



%starts game sequence
start:-
retract(in_room(room(Name, Description, Agents, Objects, Exits))),
retract(roomExits(ExOps)),
assertz(roomExits(Exits)),
write('Welcome to Vampire King Game!\n\n'),write('You are at the '),write(Name),write('\n'),
write(Description),write('You have the following exits:\n'),
displayExits, write('\nmake your move\n').


%prints out list of possible exits at current room
displayExits:-
retract(roomExits(Exits)),possExits(Exits).
possExits([]):- write('\n\n').
possExits(ExitList):- [H|T] = ExitList,exit(Exit,Visi,Status) = H,write(Exit),possExits(T).


displayObjs:-
retract(in_room(room(_,_,_,Objs,_))),
roomObjs(Objs).
roomObjs([]).
roomObjs(Objs):- [H|T] = Objs, write(H), roomObjs(T).


displayVamp:-
retract(in_room(room(_,_,Vamp,_,_))),
showVamp(Vamp).
showVamp([]):- write('There is no vampire here').
showVamp(Vamp):- [H|T] = Vamp, H = vampire -> write('There is a vampire here').



addToExplored(room(Name, Description, Agents, Objects, Exits),L):- 
assert(exploredRooms(room(Name, Description, Agents, Objects, Exits),L)).

%add objects to agent
addToObjs(Obj):-
appendTo(Obj).

%get list of objects from agent
displayObjects :-
agentObjs(objects,Objs),
printObjects(Objs).

printObjects([]) :- write('You have no objects').
printObjects(Objs) :-
write('Test'),
[H|T] = Objs, 
write(H), 
printObjects(T).



decision:-
write('Enter your selected action:\n').

dante(Act):-
Act = move -> write('you have opened the door').

dante(move(Room)):-
retract(in_room(room(Name, Description, Agents, Objects, Exits))),
write(Exits).

appendTo(Obj):-
    %list(safe,S),
    %not(checkL(S,X,Y)),
    agentObjs(objects,L),
    append(L,[[Obj]], NL),
    retract(agentObjs(objects,L)),
    assertz(agentObjs(objects,NL)).
append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).

