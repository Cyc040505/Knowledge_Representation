:- use_module(library(process)).

init_pyperplan :-
    ensure_loaded_domain,
    write('PDDL planner initialized.'), nl.

/* Generate Problem File */
create_problem_file(PlayerRow, PlayerCol, GuardRow, GuardCol) :-
    open('adversary_problem.pddl', write, Stream),

    write(Stream, '(define (problem adversary_problem)\n'),
    write(Stream, '    (:domain adversary)\n'),
    write(Stream, '    (:objects\n'),

    forall((between(1, 10, R), between(1, 30, C),
           desert_cell(R, C, Type), Type \= wall),
           format(Stream, '        cell_~w_~w - cell\n', [R, C])),

    write(Stream, '    )\n'),
    write(Stream, '    (:init\n'),

    forall((desert_cell(R, C, Type), Type \= wall),
            (format(Stream, '        (~w cell_~w_~w)\n', [Type, R, C]))),

    forall((desert_cell(R1, C1, Type1), Type1 \= wall,
            desert_dir_delta(_, DR, DC),
            R2 is R1 + DR, C2 is C1 + DC,
            between(1, 10, R2), between(1, 30, C2),
            desert_cell(R2, C2, Type2), Type2 \= wall),
            format(Stream, '        (adjacent cell_~w_~w cell_~w_~w)\n',
                  [R1, C1, R2, C2])),

    format(Stream, '        (at_player cell_~w_~w)\n', [PlayerRow, PlayerCol]),
    format(Stream, '        (at_guard cell_~w_~w)\n', [GuardRow, GuardCol]),

    (guard_stuck(true) ->
        write(Stream, '        (stuck)\n')
    ;
        write(Stream, '        (free)\n')
    ),

    write(Stream, '    )\n'),
    format(Stream, '    (:goal (at_guard cell_~w_~w))\n', [PlayerRow, PlayerCol]),
    write(Stream, ')'),
    close(Stream).

/* Pyperplan */
run_pyperplan(PlanFile) :-
    exists_file('adversary_problem.pddl') ->
        process_create(path('python'),
                   ['-m', 'pyperplan',
                    '-H', 'hff', '-s', 'wastar',
                    'adversary_domain.pddl', 'adversary_problem.pddl'],
                   [stdout(null), stderr(null)]),
        sleep(1),
        PlanFile = 'adversary_problem.pddl.soln'.

parse_plan(PlanFile, Action) :-
    exists_file(PlanFile) ->
        open(PlanFile, read, Stream),
        read_plan_actions(Stream, Actions),
        close(Stream),

        (Actions = [FirstAction | _] ->
            parse_action(FirstAction, Action)
        ;
            Action = wait,
            write('No valid actions in plan.'), nl
        ).

read_plan_actions(Stream, Actions) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file -> Actions = []
    ; ( Line = "" -> read_plan_actions(Stream, Actions)
    ; Actions = [Line | Rest], read_plan_actions(Stream, Rest))
    ).

parse_action(LineStr, Action) :-
    string_chars(LineStr, Chars),
    exclude(=('('), Chars, Chars1),
    exclude(=(')'), Chars1, Chars2),
    atom_chars(CleanAtom, Chars2),
    atomic_list_concat(Parts, ' ', CleanAtom),

    (Parts = [free_from_stuck, Cell] ->
        parse_cell(Cell, Row, Col),
        Action = free_from_stuck(Row, Col),
        write('Guard frees from stuck at ('), write(Row), write(','), write(Col), write(')'), nl
    ;
    Parts = [ActionType, FromCell, ToCell],
     member(ActionType, [move_to_sand, move_to_start, move_to_guard_start, move_to_quicksand]) ->
        parse_cell(FromCell, FromRow, FromCol),
        parse_cell(ToCell, ToRow, ToCol),
        (ActionType = move_to_quicksand ->
            Action = move_to_quicksand(FromRow, FromCol, ToRow, ToCol)
        ;
            Action = move(FromRow, FromCol, ToRow, ToCol)
        )
    ;
    Parts = [wait, Cell] ->
        parse_cell(Cell, Row, Col),
        Action = wait(Row, Col),
        write('Guard waits at ('), write(Row), write(','), write(Col), write(')'), nl
    ;
        Action = wait,
        write('No recognized action found, using wait.'), nl
    ).

parse_cell(CellAtom, Row, Col) :-
    atom_string(CellAtom, CellStr),
    split_string(CellStr, "_", "", ["cell", RowStr, ColStr]),
    number_string(Row, RowStr),
    number_string(Col, ColStr).

/* Guard Intelligent Move */
guard_move_pddl :-
    write('=== GUARD MOVE BY PDDL ==='), nl,

    (desert_player_pos(PR, PC) ->
        write('Player position: ('), write(PR), write(','), write(PC), write(')'), nl
    ;
        write('ERROR: Cannot get player position!'), nl,
        fail
    ),

    (desert_guard_pos(GR, GC) ->
        write('Guard position: ('), write(GR), write(','), write(GC), write(')'), nl
    ;
        write('ERROR: Cannot get guard position!'), nl,
        fail
    ),

    create_problem_file(PR, PC, GR, GC),

    run_pyperplan(PlanFile),

    parse_plan(PlanFile, Action),

    execute_action(Action, PR, PC, GR, GC),

    write('=== GUARD MOVE COMPLETE ==='), nl.

execute_action(move(GR, GC, NewRow, NewCol), _, _, _, _) :-
    retract(desert_guard_pos(GR, GC)),
    assert(desert_guard_pos(NewRow, NewCol)),
    write('Guard moved to ('), write(NewRow), write(','), write(NewCol), write(')'), nl,
    true.

execute_action(move_to_quicksand(GR, GC, NewRow, NewCol), _, _, _, _) :-
    retract(desert_guard_pos(GR, GC)),
    assert(desert_guard_pos(NewRow, NewCol)),
    assert(guard_stuck(true)),
    write('Guard moved to ('), write(NewRow), write(','), write(NewCol),
    write(') and is now stuck!'), nl,
    true.

execute_action(free_from_stuck(Row, Col), _, _, _, _) :-
    (desert_guard_pos(Row, Col) ->
        retract(guard_stuck(true)),
        assert(guard_stuck(false)),
        write('Guard is now free!'), nl
    ;
        write('ERROR: Guard not at expected position!'), nl,
        fail
    ).

/* Guard Simple Move */
guard_move_simple :-
    write('=== USING SIMPLE GUARD MOVEMENT ==='), nl,
    desert_player_pos(PR, PC),
    desert_guard_pos(GR, GC),

    write('Player: ('), write(PR), write(','), write(PC), write(')'), nl,
    write('Guard: ('), write(GR), write(','), write(GC), write(')'), nl,

    RowDiff is PR - GR,
    ColDiff is PC - GC,

    (abs(RowDiff) >= abs(ColDiff) ->
        (RowDiff > 0 -> MoveRow is GR + 1 ; MoveRow is GR - 1),
        MoveCol = GC
    ;
        (ColDiff > 0 -> MoveCol is GC + 1 ; MoveCol is GC - 1),
        MoveRow = GR
    ),

    write('Attempting to move to ('), write(MoveRow), write(','), write(MoveCol), write(')'), nl,

    (desert_cell(MoveRow, MoveCol, Type), Type \= wall ->
        retract(desert_guard_pos(GR, GC)),
        assert(desert_guard_pos(MoveRow, MoveCol)),
        write('Simple move: Guard moved to ('), write(MoveRow), write(','), write(MoveCol), write(')'), nl,

        (Type = quicksand ->
            assert(guard_stuck(true)),
            write('Guard gets stuck in quicksand!'), nl
        ;
            true
        )
    ;
        write('Preferred direction blocked, trying alternatives...'), nl,
        findall([NewR, NewC, Dir],
                (member(Dir, [north, south, east, west]),
                 desert_dir_delta(Dir, DR, DC),
                 NewR is GR + DR,
                 NewC is GC + DC,
                 desert_cell(NewR, NewC, Type2),
                 Type2 \= wall),
                Options),

        (Options \= [] ->
            random_member([NewR2, NewC2, _], Options),
            retract(desert_guard_pos(GR, GC)),
            assert(desert_guard_pos(NewR2, NewC2)),
            write('Simple move: Guard moved to ('), write(NewR2), write(','), write(NewC2), write(')'), nl,

            (desert_cell(NewR2, NewC2, quicksand) ->
                assert(guard_stuck(true)),
                write('Guard gets stuck in quicksand!'), nl
            ;
                true
            )
        ;
            write('Simple move: Guard cannot move in any direction!'), nl
        )
    ),
    write('=== SIMPLE MOVE COMPLETE ==='), nl.

init_guard_state :-
    retractall(guard_stuck(_)),
    assert(guard_stuck(false)),
    true.