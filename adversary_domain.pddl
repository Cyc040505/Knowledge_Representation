(define (domain adversary)
    (:requirements :strips :typing)

    (:types
        cell
    )

    (:predicates
        (at_guard ?c - cell)
        (at_player ?c - cell)
        (adjacent ?from - cell ?to - cell)
        (stuck)
        (free)
        (sand ?c - cell)
        (quicksand ?c - cell)
        (start ?c - cell)
        (exit ?c - cell)
        (guard_start ?c - cell)
    )

    (:action move_to_sand
        :parameters (?from - cell ?to - cell)
        :precondition (and
            (at_guard ?from)
            (adjacent ?from ?to)
            (free)
            (sand ?to)
        )
        :effect (and
            (not (at_guard ?from))
            (at_guard ?to)
        )
    )

    (:action move_to_start
        :parameters (?from - cell ?to - cell)
        :precondition (and
            (at_guard ?from)
            (adjacent ?from ?to)
            (free)
            (start ?to)
        )
        :effect (and
            (not (at_guard ?from))
            (at_guard ?to)
        )
    )

    (:action move_to_guard_start
        :parameters (?from - cell ?to - cell)
        :precondition (and
            (at_guard ?from)
            (adjacent ?from ?to)
            (free)
            (guard_start ?to)
        )
        :effect (and
            (not (at_guard ?from))
            (at_guard ?to)
        )
    )

    (:action move_to_quicksand
        :parameters (?from - cell ?to - cell)
        :precondition (and
            (at_guard ?from)
            (adjacent ?from ?to)
            (free)
            (quicksand ?to)
        )
        :effect (and
            (not (at_guard ?from))
            (at_guard ?to)
            (stuck)
        )
    )

    (:action free_from_stuck
        :parameters (?c - cell)
        :precondition (and
            (at_guard ?c)
            (stuck)
        )
        :effect (and
            (free)
        )
    )

    (:action wait
        :parameters (?c - cell)
        :precondition (at_guard ?c)
        :effect (and)
    )
)