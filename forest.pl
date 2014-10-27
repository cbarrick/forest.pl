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
	schedule_(Schedule, Edges),
	label_schedule(Schedule).

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


%% mutate(+Energy, +Base, -NewSchedule)
%

mutate(0, S, S) :- !.

mutate(N, Base, New) :-
	random_select(stand(ID, Time), Base, Rest),
	between(0, 3, NewTime),
	NewTime \= Time,
	N0 is N - 1,
	mutate(N0, [stand(ID, NewTime)|Rest], New).



%% conflict(+Schedule, ?ID, ?ConflictID)
%

conflict(S, ID, ConflictID) :-
	select(stand(ID, Time), S, Rest),
	edge(ID, ConflictID),
	member(stand(ConflictID, Time), Rest).


%% all_conflicts(+Schedule, -Conflicts)
% Conflicts is a list of stand IDs which are in conflict, sorted by the number
% of conflicts per ID in descending order.

all_conflicts(S, Conflicts) :-

	% Collect the IDs of all stands in conflict
	findall(ID, (
		conflict(S, ID, _),
		atom_concat(conflict_count, ID, Key),
		flag(Key, N, N+1)
	), Unsorted_Uncounted_WithDups),

	% Remove dups
	sort(Unsorted_Uncounted_WithDups, Unsorted_Uncounted),

	% Collect [Count, ID] pairs
	% Count is the number of conflicts involving ID
	findall([Count, ID], (
		member(ID, Unsorted_Uncounted),
		member(stand(ID, _), S),
		atom_concat(conflict_count, ID, Key),
		flag(Key, Count, 0)
	), Compound_Unsorted),

	% Sort by most conflicts first
	sort(Compound_Unsorted, Compound_LeastConflictsFirst),
	reverse(Compound_LeastConflictsFirst, Compound_MostConflictsFirst),

	% Simplify the list of pairs to the list of IDs, maintaining order
	findall(ID, member([_, ID], Compound_MostConflictsFirst), Conflicts).


%% repair(+Broken, -Fixed)
%

repair(Broken, Fixed) :-
	all_conflicts(Broken, Conflicts),
	repair(Conflicts, Broken, Fixed).

%% repair(+Queue, +Broken, -Fixed)
%

% The base case is that Broken is a partial assignment that can be made valid.
% If so, we want to cut everything except the schedule assignments. To make
% sure we only cut when a fix is possible, we first check that a valid binding
% is possible on a copy, then cut *before* we try to bind the original.
repair(_, Fixed, Fixed) :-
	copy_term(Fixed, Copy),
	schedule(Copy),
	!,
	schedule(Fixed).

% Otherwise, we change the time corresponding to the first ID in the queue into
% a variable, and try again
repair([ID|Queue], Broken, Fixed) :-
	select(stand(ID, _), Broken, Rest),
	Next = [stand(ID, _) | Rest],
	repair(Queue, Next, Fixed).


%% test
%

test :-
	target(T),
	schedule(S),
	findall(Mutant, mutate(5, S, Mutant), Mutants),
	mergesort(Mutants, ascending(error(T)), [Best|_]),

	error(T, Best, ErrorBefore),
	debug(test, "Before fix: ~w", [ErrorBefore]),
	!,

	repair(Best, Fixed),
	error(T, Fixed, ErrorAfter),
	debug(test, "After fix: ~w", [ErrorAfter]).


%% main
%

:- dynamic best/2.
best(1000000000, _).

main :-

	Energy = 5,

	once(schedule(S)),
	main(0, S, Energy).

main(I, BaseSchedule, Energy) :-
	target(T),
	findall(Mutant, mutate(Energy, BaseSchedule, Mutant), Mutants),
	mergesort(Mutants, ascending(error(T)), [BestInvalid|_]),
	findall(Fix, repair(BestInvalid, Fix), Fixes),
	mergesort(Fixes, ascending(error(T)), [BestValid|_]),
	error(T, BestValid, Error),

	(
		best(OldBestError, _),
		Error < OldBestError,
		format("~w,~w,~w\n", [I, Error, BestValid]),
		retractall(best(_,_)),
		assert(best(Error, BestValid))
	;
		true
	),

	I1 is I + 1,
	main(I1, BestValid, Energy).
