#!/usr/bin/env swipl -g main

:- use_module(library(clpfd)).
:- use_module(library(csv)).

:- use_module(sort).
:- use_module(data).


%% schedule(?Schedule)
%

schedule(S) :-
	(nonvar(S) -> sort(S, Schedule) ; S = Schedule),
	findall(stand(ID, _), yield(ID,_,_,_,_), Schedule),
	findall(edge(A,B), edge(A,B), Edges),
	schedule_(Schedule, Edges).

schedule_(_, []).
schedule_(Schedule, [edge(A,B)|Edges]) :-
	member(stand(A, TimeA), Schedule),
	member(stand(B, TimeB), Schedule),
	[TimeA, TimeB] ins 0..3,
	all_distinct([TimeA, TimeB]),
	schedule_(Schedule, Edges).


%% label_schedule(+Schedule)
%

label_schedule(S) :-
	label_schedule_(S, Times),
	labeling([ff, down], Times).

label_schedule_([], []).
label_schedule_([stand(_, Time)|Stands], [Time|Times]) :-
	label_schedule_(Stands, Times).



%% error(Target, Schedule, Error)
%

error(Target, S, Error) :-
	error_(S, Target, Error, 0, 0, 0).

error_([], Target, Error, T1, T2, T3) :-
	Error is (T1 - Target)^2 + (T2 - Target)^2 + (T3 - Target)^2.

error_([stand(_, 0)|Stands], Target, Error, T1, T2, T3) :-
	error_(Stands, Target, Error, T1, T2, T3).

error_([stand(ID, 1)|Stands], Target, Error, T1, T2, T3) :-
	yield(ID, Acres, Additional, _, _),
	!,
	NextT1 is T1 + Acres*Additional,
	error_(Stands, Target, Error, NextT1, T2, T3).

error_([stand(ID, 2)|Stands], Target, Error, T1, T2, T3) :-
	yield(ID, Acres, _, Additional, _),
	!,
	NextT2 is T2 + Acres*Additional,
	error_(Stands, Target, Error, T1, NextT2, T3).

error_([stand(ID, 3)|Stands], Target, Error, T1, T2, T3) :-
	yield(ID, Acres, _, _, Additional),
	!,
	NextT3 is T3 + Acres*Additional,
	error_(Stands, Target, Error, T1, T2, NextT3).


%% mutate(+MutationRate, +Base, -NewSchedule)
%

mutate(Rate, Base, New) :-
	length(Base, L),
	N is round(L * Rate),
	mutate_(N, Base, New),
	label_schedule(New).

mutate_(0, S, S) :- !.

mutate_(N, [stand(ID,Time)|Base], [stand(ID,NewTime)|New]) :-
	NewTime in 0..3,
	Time #\= NewTime,
	N0 is N - 1,
	mutate_(N0, Base, New).


%% conflict(+Schedule, ?ID, ?ConflictID)
%

conflict(S, ID, ConflictID) :-
	select(stand(ID, Time), S, Rest),
	edge(ID, ConflictID),
	member(stand(ConflictID, Time), Rest).


%% conflict_count(+Schedule, +Stand, -Count)
%

conflict_count(S, stand(ID,Time), Count) :-
	member(stand(ID, Time), S),
	findall(ConflictID, (
		edge(ID, ConflictID),
		member(stand(ConflictID, Time), S)
	), Conflicts),
	length(Conflicts, Count).


%% repair(+Broken, -Fixed)
%

repair(Broken, Fixed) :-
	mergesort(Broken, descending(conflict_count(Broken)), Conflicts),
	repair(Conflicts, Broken, Fixed).


%% repair(+Queue, +Broken, -Fixed)
%

repair(_, Fixed, Fixed) :-
	schedule(Fixed),
	!.

repair([stand(ID, _)|Queue], Broken, Fixed) :-
	select(stand(ID, _), Broken, Rest),
	Next = [stand(ID, _) | Rest],
	repair(Queue, Next, Fixed).


%% shuffle(+In, -Out)
%

shuffle([], []) :- !.
shuffle(In, [X|Out]) :-
	random_select(X, In, Rest),
	shuffle(Rest, Out).


%% move(+MutationRate, +Base, -Move)
%

move(BeamSize, MutationRate, Base, BestMove) :-
	target(T),
	shuffle(Base, Shuffled),
	findnsols(BeamSize, M, mutate(MutationRate, Shuffled, M), Mutants),
	mergesort(Mutants, ascending(error(T)), [BestInvalid|_]),
	repair(BestInvalid, Partial),
	findnsols(BeamSize, Partial, label_schedule(Partial), Moves),
	mergesort(Moves, ascending(error(T)), [BestMove|_]).


%% main
%

:- dynamic best/2.

main :-

	% Disable multithreaded mergesort. Seems broken in SWI 7.1.23.
	% debug(mergesort),

	% Parameters:
	BeamSize = 512,
	MutationRate = 0.1,

	% Reset the best known solution
	retractall(best(_,_)),
	assert(best(infinity, _)),

	% Get the party started
	schedule(S),
	once(label_schedule(S)),
	main(BeamSize, MutationRate, 0, S).

main(BeamSize, MutationRate, I, Base) :-
	target(T),
	move(BeamSize, MutationRate, Base, Next),

	% Print only if we find something better
	error(T, Next, Error),
	(
		best(OldBestError, _),
		once((OldBestError = infinity ; Error < OldBestError)),
		format("~w,~w,~w\n", [I, Error, Next]),
		retractall(best(_,_)),
		assert(best(Error, Next))
	;
		true
	),

	I1 is I + 1,
	!,
	main(BeamSize, MutationRate, I1, Next).
