:- module(sort, [

	quicksort/3,    % quicksort(+List, +Comparator, -Sorted)
	mergesort/3,    % mergesort(+List, +Comparator, -Sorted)
	ascending/3,    % ascending(+A, +B, -C)
	descending/3,   % descending(+A, +B, -C)
	ascending/4,    % ascending(+Eval, +A, +B, -C)
	descending/4    % descending(+Eval, +A, +B, -C)

]).

:- meta_predicate(quicksort(+, 3, -)).
:- meta_predicate(mergesort(+, 3, -)).
:- meta_predicate(ascending(2, +, +, -)).
:- meta_predicate(descending(2, +, +, -)).


%% quicksort(+List, +Comparator, -Sorted)
% True when Sorted contains the elements of List sorted by the Comparator.
% Duplicates are removed. The Comparator is the name of a 3-place predicate
% which takes two elements as the first arguments and unifies the third with
% the element which should be placed before the other.
%
% Implemented as concurrent quicksort; mergesort appears to be faster.

quicksort(List, Comparator, Sorted) :-
	sort(List, CleanList), % remove duplicates
	quicksort_(CleanList, Comparator, Sorted).

quicksort_([], _, []) :- !.

quicksort_(List, Comparator, Sorted) :-
	random_select(Pivot, List, Unsorted),
	quicksort_partition([Pivot|Unsorted], [], [], Comparator, Sorted).

quicksort_partition([Pivot], Before, After, Comparator, Sorted) :-
	!,
	thread_create(
		(
			quicksort_(Before, Comparator, SortedBefore),
			thread_exit(SortedBefore)
		),
		BeforeThread,
		[]
	),
	thread_create(
		(
			quicksort_(After, Comparator, SortedAfter),
			thread_exit(SortedAfter)
		),
		AfterThread,
		[]
	),
	thread_join(BeforeThread, exited(SortedBefore)),
	thread_join(AfterThread, exited(SortedAfter)),
	append([SortedBefore, [Pivot], SortedAfter], Sorted).


quicksort_partition([Pivot,Next|Unsorted], Before, After, Comparator, Sorted) :-
	call(Comparator, Pivot, Next, Pick),
	( Pick = Next ->
		NextBefore = [Next|Before],
		NextAfter = After
	;
		NextBefore = Before,
		NextAfter = [Next|After]
	),
	!,
	quicksort_partition([Pivot|Unsorted], NextBefore, NextAfter, Comparator, Sorted).


%% mergesort(+List, +Comparator, -Sorted)
% True when Sorted contains the elements of List sorted by the Comparator.
% Duplicates are removed. The Comparator is the name of a 3-place predicate
% which takes two elements as the first arguments and unifies the third with
% the element which should be placed before the other.
%
% Implemented as concurrent mergesort; this appears to be faster than quicksort.

mergesort(List, Comparator, Sorted) :-
	sort(List, CleanList), % remove duplicates.
	mergesort_(CleanList, Comparator, Sorted).

mergesort_([], _, []) :- !.
mergesort_([X], _, [X]) :- !.

mergesort_(List, Comparator, Sorted) :-
	length(List, L),
	Half is L // 2,
	length(Front, Half),
	append(Front, Back, List),

	(debugging(mergesort) ->
		% When debugging mergesort, dissable multithreading
		mergesort_(Front, Comparator, SortedFront),
		mergesort_(Back, Comparator, SortedBack)
	;
		thread_create(
			(
				mergesort_(Front, Comparator, SortedFront),
				thread_exit(SortedFront)
			),
			FrontThread,
			[]
		),
		thread_create(
			(
				mergesort_(Back, Comparator, SortedBack),
				thread_exit(SortedBack)
			),
			BackThread,
			[]
		),
		thread_join(FrontThread, exited(SortedFront)),
		thread_join(BackThread, exited(SortedBack))
	),

	merge(SortedFront, SortedBack, Comparator, Sorted).

merge([], Ys, _, Ys) :- !.
merge(Xs, [], _, Xs) :- !.

merge([X|XT], [Y|YT], Comparator, Merged) :-
	call(Comparator, X, Y, X),
	!,
	Merged = [X|Rest],
	merge(XT, [Y|YT], Comparator, Rest).

merge([X|XT], [Y|YT], Comparator, Merged) :-
	call(Comparator, X, Y, Y),
	!,
	Merged = [Y|Rest],
	merge([X|XT], YT, Comparator, Rest).


%% ascending(+A, +B, -C), ascending(+Evaluator, +A, +B, -C)
%% descending(+A, +B, -C), descending(+Evaluator, +A, +B, -C)
% Given an Evaluator predicate, bind C to either A or B. The Evaluator is
% called with two additional arguments; the first is the item being evaluated,
% and the second is a variable which should become bound to a numeric value
% for the item. `ascending` binds C to the item with the smallest value, and
% `descending` binds C to the item with the largest value. If no evaluator is
% given, the arguments are compared directly.

% Note: We use `msort/2` below to sort on Prolog's natural ordering.
% `msort/2` is a builtin predicate in SWI that is like `sort/2` but maintains
% duplicates. See http://www.swi-prolog.org/pldoc/man?section=builtinlist

ascending(A, B, C) :-
	msort([A, B], [C, _]).

ascending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	ascending(ValueA, ValueB, ValueC),
	(
		ValueC = ValueA ->
		C = A
	;
		ValueC = ValueB,
		C = B
	),
	!.


descending(A, B, C) :-
	msort([A, B], [_, C]).

descending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	descending(ValueA, ValueB, ValueC),
	(
		ValueC = ValueA ->
		C = A
	;
		ValueC = ValueB,
		C = B
	),
	!.
