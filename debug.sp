%% debug

%% - SORTS

#const numSteps = 5.
sorts

#place = {room1, room2, room3, room4}.
#exit = {door}.

#agent = {robot, human}. 
#object = {thing1, thing2, thing3}. 
#surf = {table, floor, bookshelf}. 

#thing = #object + #agent. 

#step = 0..numSteps. 

#inertial_fluent = on(#thing, #surf) +
		   location(#thing, #place) + 
		   in_hand(#agent, #object) +
		   in_range(#agent, #object, #surf).
#fluent = #inertial_fluent.

%% -- ACTIONS

#action = go_to(#agent, #surf) +
		move_to(#agent, #object, #surf) +
		go_through(#agent, #exit, #place) +
		pick_up(#agent, #object) +
		hand_to(#agent, #agent, #object) +
		request_help(#agent, #agent, #object).


%% -- PREDICATES

predicates

holds(#fluent, #step).
occurs(#action, #step).

has_exit(#place, #exit).
connects(#place, #place). 
can_reach(#agent, #surf).

%for planning%
success().
goal(#step). 
something_happened(#step).

rules


%% CAUSAL LAWS

% 1. If you go somewhere, then you're there.
holds(on(A, S), I+1) :- occurs(go_to(A, S), I).

% 2. If you leave a place, then you don't remain there.
-holds(on(A, S1), I+1) :- occurs(go_to(A, S2), I),
			 holds(on(A, S1), I). 

% 3. If you move something somewhere, then it's there.
holds(on(O, S), I+1) :- occurs(move_to(A, O, S), I).

% 4. If you move something somewhere, then it doesn't remain in your hand.
-holds(in_hand(A, O), I+1) :- occurs(move_to(A, O, S), I).


% 5. If you go out of a room, then you're in another room.
holds(location(A, L2), I+1) :- occurs(go_through(A, E, L1), I), L1 != L2.

% 6. If you go out of a room, then you're no longer in that room.
-holds(location(A, L), I+1) :- occurs(go_through(A, E, L), I).

% 7. If you pick something up, then it's in your hand.
holds(in_hand(A, O), I+1) :- occurs(pick_up(A, O), I).

% 8. Are the timesteps right here????????????????????
-holds(on(O, S), I+1) :- occurs(pick_up(A, O), I),
			 holds(on(O, S), I-1).

% 9. If you hand an object to someone, that that someone has it in their hand.
holds(in_hand(A2, O), I+1) :- occurs(hand_to(A1, A2, O), I).

%10. If you hand an object to someone, then it's no longer in your hand.
-holds(in_hand(A1,O), I+1) :- occurs(hand_to(A1, A2, O), I).

%11.occurs(hand_to(A2, A1, O), I+1) :- occurs(request_help(A1, A2, O), I).


%% STATE CONSTRAINTS

%1.'[\'
holds(on(O2, S), I) :- -holds(on(O1, S), I), O1 != O2.

%2.
holds(on(O, S1), I) :- -holds(on(O, S2), I), S1 != S2.

%3.
-holds(location(O, L1), I) :- holds(location(O, L2), I), L1 != L2.

%4.
-holds(in_hand(A1, O), I) :- holds(in_hand(A2, O), I), A1 != A2.

%5.
-holds(in_hand(A, O1), I) :- holds(in_hand(A, O2), I), O1 != O2.

%6.
holds(in_range(A, O, S), I) :- holds(on(O, S), I),
							can_reach(A, S).

%7.
-holds(in_range(A, O, S), I) :- -holds(on(O, S), I),
							 -can_reach(A, S).


%% EXECUTABILITY CONDITIONS

%1.
-occurs(go_to(A, S), I) :- holds(on(O,S), I).

%2.
-occurs(go_to(A1, S), I) :- holds(in_hand(A2, S), I), A1 != A2.

%3.
-occurs(go_to(A, S), I) :- holds(on(A, S), I).

%4.
-occurs(move_to(A, O, S), I) :- not holds(in_hand(A, O), I).

%5.
-occurs(move_to(A, O1, S), I) :- holds(on(O2, S), I), O1 != O2.

%6.
-occurs(go_through(A, E, L2), I) :- not holds(location(A, L1), I),
								not has_exit(L1, E),
								not has_exit(L2, E),
								not connects(L1, L2),
								L1 != L2.

%7.
-occurs(pick_up(A, O1), I) :- holds(in_hand(A, O2), I),
							O1 != O2.
%8.
-occurs(hand_to(A1, A2, O), I+1) :- not holds(in_hand(A1, O), I),
A1 != A2.

% 9. 
-occurs(hand_to(A1, A2, O), I+1) :- not occurs(request_help(A2, A1, O), I).

%% Inertia Axiom + CWA

% Inertial fluents
holds(F, I+1) :- #inertial_fluent(F),
		holds(F, I),
		not -holds(F, I+1).

-holds(F, I+1) :- #inertial_fluent(F),
		 -holds(F, I),
		 not holds(F, I+1). 


% not really needed here but in case:
% CWA for Defined fluents
%-holds(F,I) :- not holds(F,I), #defined_fluent(F).


% CWA for actions
-occurs(A, I) :- not occurs(A, I).

%% PLANNING

success :- goal(I),
           I <= n. 
:- not success.

% an action must occur at each step
occurs(A,I) | -occurs(A,I) :- not goal(I).

% do not allow concurrent actions
:- occurs(A1, I),
   occurs(A2, I),
   A1!=A2.

% forbid agents from procrastinating
something_happened(I) :- occurs(A,I).

:- not something_happened(I),
   something_happened(I+1).

%:- goal(I), goal(I-1),
%   J < I,
%   not something_happened(J).


%%%%%%%%%% works up to here apart from causal law 12

%% INITIAL CONDITION

has_exit(room1, door).
has_exit(room2, door).
has_exit(room3, door).
has_exit(room4, door).

connects(room1, room2).
connects(room3, room4).
connects(room2, room4).

can_reach(robot, table).
can_reach(robot, floor).

can_reach(human, table).
can_reach(human, floor).
can_reach(human, bookshelf).

holds(on(thing1, floor), 0).
holds(on(thing2, table), 0).
holds(on(thing3, bookshelf), 0).

holds(location(robot, room1), 0).
holds(location(human, room4), 0).
holds(location(thing1, room2), 0).
holds(location(thing2, room3), 0).
holds(location(thing3, room1), 0).

%%%%%%% QUERIES


%% GOALS

%goal(0) :- holds(on(thing1, floor),0).
goal(I) :- holds(in_hand(robot, thing3), I).
%goal(I) :- holds(in_hand(robot, thing2), I).
%goal(I) :- holds(in_hand(robot, thing3), I).
%goal(I) :- holds(in_hand(human, thing2), I).
%goal(I) :- holds(location(robot, room4), I).
%goal(I) :- holds(location(human, room1), I).


display
occurs.
holds. 
