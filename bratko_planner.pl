

plan(InitialState, Goals, Plan, FinalState) :-
    plan(InitialState, Goals, [], Plan, FinalState).

plan(State, Goals, _, [], State) :-
    satisfied(State, Goals).

plan( State, Goals, Protected, Plan, FinalState) :-
    append( PrePlan, [Action | PostPlan], Plan),
    my_select( State, Goals, Goal),
    achieves( Action, Goal),
    can( Action, Condition),
    preserves( Action, Protected),
    plan( State, Condition, Protected, PrePlan, MidState1),
    my_apply(MidStatel, Action, MidState2),
    plan( MidState2, Goals, [Goal | Protected], PostPlan, FinalState).

preserves( Action, Goals) :-
    deletes( Action, Relations),
    not( (member( Goal, Relations), member( Goal, Goals)) ).

satisfied( State, []).

satisfied( State, [Goal | Goals]) :-
    member( Goal, State),
    satisfied( State, Goals).

my_select( State, Goals, Goal) :-
    member( Goal, Goals),
    not(member( Goal, State)).


achieves( Action, Goal) :-
    adds( Action, Goals), 
    member( Goal, Goals).

my_apply( State, Action, NewState) :-
    deletes( Action, DelList),
    delete_all( State, DelList, State1),!,
    adds( Action, AddList),
    append( AddList, State1, NewState).

delete_all( [], _, []).
delete_all( [X | L1], L2, Diff) :- member( X, L2), !, delete_all( L1, L2, Diff).
delete_all( [X | L1], L2, [X | Diff  ]) :- delete_all( L1, L2, Diff).

can( move( Block, From, To), [ clear( Block), clear( To), on( Block, From)]) :- block( Block),	% Block to be moved
object( To),	% 'To' is a block or a place
To \== Block,	% Block cannot be moved to itself
object( From),	% 'From' is a block or a place
From \== To,	% Move to new position
Block \== From.	% Block not moved from itself
% adds( Action, Relationships): Action establishes Relationships 
adds( move(X,From,To), [ on(X,To), clear(From)]).
% deletes( Action, Relationships): Action destroys Relationships 
deletes( move(X,From,To), [ on(X,From), clear(To)]).
object( X) :-	% X is an object it’
place(X)	% X is a place
;	% or
block( X).	% X is a block
% A blocks world
block( a). block( b). block( c).
place( 1). place( 2). place( 3). place( 4).
% A state in the blocks world
%
% c %	a b
% place 12 3 4
