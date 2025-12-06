(define (problem adversary_problem)
    (:domain adversary)
    (:objects
        cell_2_2 - cell
        cell_2_3 - cell
        cell_2_4 - cell
        cell_2_5 - cell
        cell_2_6 - cell
        cell_2_7 - cell
        cell_2_9 - cell
        cell_2_10 - cell
        cell_2_11 - cell
        cell_2_12 - cell
        cell_2_14 - cell
        cell_2_15 - cell
        cell_2_16 - cell
        cell_2_18 - cell
        cell_2_19 - cell
        cell_2_20 - cell
        cell_2_22 - cell
        cell_2_23 - cell
        cell_2_24 - cell
        cell_2_26 - cell
        cell_2_27 - cell
        cell_2_28 - cell
        cell_2_29 - cell
        cell_3_2 - cell
        cell_3_3 - cell
        cell_3_5 - cell
        cell_3_7 - cell
        cell_3_9 - cell
        cell_3_11 - cell
        cell_3_14 - cell
        cell_3_16 - cell
        cell_3_18 - cell
        cell_3_19 - cell
        cell_3_22 - cell
        cell_3_24 - cell
        cell_3_26 - cell
        cell_3_28 - cell
        cell_3_29 - cell
        cell_4_2 - cell
        cell_4_3 - cell
        cell_4_5 - cell
        cell_4_7 - cell
        cell_4_8 - cell
        cell_4_9 - cell
        cell_4_11 - cell
        cell_4_12 - cell
        cell_4_13 - cell
        cell_4_14 - cell
        cell_4_16 - cell
        cell_4_17 - cell
        cell_4_18 - cell
        cell_4_19 - cell
        cell_4_20 - cell
        cell_4_21 - cell
        cell_4_22 - cell
        cell_4_24 - cell
        cell_4_25 - cell
        cell_4_26 - cell
        cell_4_27 - cell
        cell_4_28 - cell
        cell_4_29 - cell
        cell_5_2 - cell
        cell_5_4 - cell
        cell_5_5 - cell
        cell_5_6 - cell
        cell_5_7 - cell
        cell_5_9 - cell
        cell_5_11 - cell
        cell_5_12 - cell
        cell_5_14 - cell
        cell_5_16 - cell
        cell_5_18 - cell
        cell_5_19 - cell
        cell_5_20 - cell
        cell_5_22 - cell
        cell_5_24 - cell
        cell_5_26 - cell
        cell_5_28 - cell
        cell_5_29 - cell
        cell_6_2 - cell
        cell_6_4 - cell
        cell_6_6 - cell
        cell_6_8 - cell
        cell_6_9 - cell
        cell_6_10 - cell
        cell_6_11 - cell
        cell_6_12 - cell
        cell_6_14 - cell
        cell_6_16 - cell
        cell_6_18 - cell
        cell_6_20 - cell
        cell_6_22 - cell
        cell_6_24 - cell
        cell_6_26 - cell
        cell_6_28 - cell
        cell_6_29 - cell
        cell_7_2 - cell
        cell_7_3 - cell
        cell_7_4 - cell
        cell_7_5 - cell
        cell_7_6 - cell
        cell_7_8 - cell
        cell_7_9 - cell
        cell_7_11 - cell
        cell_7_12 - cell
        cell_7_13 - cell
        cell_7_14 - cell
        cell_7_15 - cell
        cell_7_16 - cell
        cell_7_18 - cell
        cell_7_20 - cell
        cell_7_22 - cell
        cell_7_23 - cell
        cell_7_24 - cell
        cell_7_26 - cell
        cell_7_27 - cell
        cell_7_28 - cell
        cell_8_2 - cell
        cell_8_4 - cell
        cell_8_6 - cell
        cell_8_8 - cell
        cell_8_9 - cell
        cell_8_11 - cell
        cell_8_14 - cell
        cell_8_16 - cell
        cell_8_18 - cell
        cell_8_20 - cell
        cell_8_22 - cell
        cell_8_24 - cell
        cell_8_26 - cell
        cell_8_28 - cell
        cell_9_2 - cell
        cell_9_3 - cell
        cell_9_4 - cell
        cell_9_5 - cell
        cell_9_6 - cell
        cell_9_7 - cell
        cell_9_8 - cell
        cell_9_9 - cell
        cell_9_10 - cell
        cell_9_11 - cell
        cell_9_12 - cell
        cell_9_13 - cell
        cell_9_14 - cell
        cell_9_15 - cell
        cell_9_16 - cell
        cell_9_17 - cell
        cell_9_18 - cell
        cell_9_19 - cell
        cell_9_20 - cell
        cell_9_21 - cell
        cell_9_22 - cell
        cell_9_23 - cell
        cell_9_24 - cell
        cell_9_25 - cell
        cell_9_26 - cell
        cell_9_27 - cell
        cell_9_28 - cell
        cell_9_29 - cell
        cell_9_30 - cell
    )
    (:init
        (start cell_2_2)
        (sand cell_2_3)
        (sand cell_2_4)
        (quicksand cell_2_5)
        (sand cell_2_6)
        (sand cell_2_7)
        (sand cell_2_9)
        (sand cell_2_10)
        (quicksand cell_2_11)
        (sand cell_2_12)
        (sand cell_2_14)
        (sand cell_2_15)
        (sand cell_2_16)
        (sand cell_2_18)
        (quicksand cell_2_19)
        (sand cell_2_20)
        (sand cell_2_22)
        (sand cell_2_23)
        (sand cell_2_24)
        (sand cell_2_26)
        (sand cell_2_27)
        (sand cell_2_28)
        (sand cell_2_29)
        (sand cell_3_2)
        (sand cell_3_3)
        (quicksand cell_3_5)
        (sand cell_3_7)
        (sand cell_3_9)
        (quicksand cell_3_11)
        (sand cell_3_14)
        (sand cell_3_16)
        (sand cell_3_18)
        (quicksand cell_3_19)
        (sand cell_3_22)
        (sand cell_3_24)
        (quicksand cell_3_26)
        (quicksand cell_3_28)
        (sand cell_3_29)
        (quicksand cell_4_2)
        (sand cell_4_3)
        (quicksand cell_4_5)
        (sand cell_4_7)
        (sand cell_4_8)
        (sand cell_4_9)
        (quicksand cell_4_11)
        (sand cell_4_12)
        (sand cell_4_13)
        (sand cell_4_14)
        (sand cell_4_16)
        (sand cell_4_17)
        (sand cell_4_18)
        (quicksand cell_4_19)
        (sand cell_4_20)
        (sand cell_4_21)
        (sand cell_4_22)
        (sand cell_4_24)
        (sand cell_4_25)
        (quicksand cell_4_26)
        (sand cell_4_27)
        (quicksand cell_4_28)
        (sand cell_4_29)
        (sand cell_5_2)
        (sand cell_5_4)
        (sand cell_5_5)
        (sand cell_5_6)
        (sand cell_5_7)
        (quicksand cell_5_9)
        (sand cell_5_11)
        (sand cell_5_12)
        (quicksand cell_5_14)
        (guard_start cell_5_16)
        (sand cell_5_18)
        (sand cell_5_19)
        (sand cell_5_20)
        (sand cell_5_22)
        (sand cell_5_24)
        (sand cell_5_26)
        (sand cell_5_28)
        (quicksand cell_5_29)
        (sand cell_6_2)
        (sand cell_6_4)
        (sand cell_6_6)
        (sand cell_6_8)
        (sand cell_6_9)
        (sand cell_6_10)
        (sand cell_6_11)
        (sand cell_6_12)
        (quicksand cell_6_14)
        (sand cell_6_16)
        (sand cell_6_18)
        (sand cell_6_20)
        (sand cell_6_22)
        (sand cell_6_24)
        (sand cell_6_26)
        (sand cell_6_28)
        (sand cell_6_29)
        (quicksand cell_7_2)
        (sand cell_7_3)
        (sand cell_7_4)
        (sand cell_7_5)
        (sand cell_7_6)
        (sand cell_7_8)
        (quicksand cell_7_9)
        (sand cell_7_11)
        (quicksand cell_7_12)
        (sand cell_7_13)
        (sand cell_7_14)
        (sand cell_7_15)
        (sand cell_7_16)
        (sand cell_7_18)
        (sand cell_7_20)
        (sand cell_7_22)
        (sand cell_7_23)
        (sand cell_7_24)
        (sand cell_7_26)
        (sand cell_7_27)
        (sand cell_7_28)
        (sand cell_8_2)
        (quicksand cell_8_4)
        (sand cell_8_6)
        (sand cell_8_8)
        (quicksand cell_8_9)
        (sand cell_8_11)
        (sand cell_8_14)
        (sand cell_8_16)
        (sand cell_8_18)
        (sand cell_8_20)
        (quicksand cell_8_22)
        (quicksand cell_8_24)
        (sand cell_8_26)
        (sand cell_8_28)
        (sand cell_9_2)
        (sand cell_9_3)
        (sand cell_9_4)
        (sand cell_9_5)
        (sand cell_9_6)
        (sand cell_9_7)
        (sand cell_9_8)
        (sand cell_9_9)
        (sand cell_9_10)
        (sand cell_9_11)
        (sand cell_9_12)
        (sand cell_9_13)
        (sand cell_9_14)
        (sand cell_9_15)
        (sand cell_9_16)
        (sand cell_9_17)
        (sand cell_9_18)
        (sand cell_9_19)
        (sand cell_9_20)
        (sand cell_9_21)
        (quicksand cell_9_22)
        (sand cell_9_23)
        (sand cell_9_24)
        (sand cell_9_25)
        (sand cell_9_26)
        (sand cell_9_27)
        (sand cell_9_28)
        (sand cell_9_29)
        (exit cell_9_30)
        (adjacent cell_2_2 cell_3_2)
        (adjacent cell_2_2 cell_2_3)
        (adjacent cell_2_3 cell_3_3)
        (adjacent cell_2_3 cell_2_2)
        (adjacent cell_2_3 cell_2_4)
        (adjacent cell_2_4 cell_2_3)
        (adjacent cell_2_4 cell_2_5)
        (adjacent cell_2_5 cell_3_5)
        (adjacent cell_2_5 cell_2_4)
        (adjacent cell_2_5 cell_2_6)
        (adjacent cell_2_6 cell_2_5)
        (adjacent cell_2_6 cell_2_7)
        (adjacent cell_2_7 cell_3_7)
        (adjacent cell_2_7 cell_2_6)
        (adjacent cell_2_9 cell_3_9)
        (adjacent cell_2_9 cell_2_10)
        (adjacent cell_2_10 cell_2_9)
        (adjacent cell_2_10 cell_2_11)
        (adjacent cell_2_11 cell_3_11)
        (adjacent cell_2_11 cell_2_10)
        (adjacent cell_2_11 cell_2_12)
        (adjacent cell_2_12 cell_2_11)
        (adjacent cell_2_14 cell_3_14)
        (adjacent cell_2_14 cell_2_15)
        (adjacent cell_2_15 cell_2_14)
        (adjacent cell_2_15 cell_2_16)
        (adjacent cell_2_16 cell_3_16)
        (adjacent cell_2_16 cell_2_15)
        (adjacent cell_2_18 cell_3_18)
        (adjacent cell_2_18 cell_2_19)
        (adjacent cell_2_19 cell_3_19)
        (adjacent cell_2_19 cell_2_18)
        (adjacent cell_2_19 cell_2_20)
        (adjacent cell_2_20 cell_2_19)
        (adjacent cell_2_22 cell_3_22)
        (adjacent cell_2_22 cell_2_23)
        (adjacent cell_2_23 cell_2_22)
        (adjacent cell_2_23 cell_2_24)
        (adjacent cell_2_24 cell_3_24)
        (adjacent cell_2_24 cell_2_23)
        (adjacent cell_2_26 cell_3_26)
        (adjacent cell_2_26 cell_2_27)
        (adjacent cell_2_27 cell_2_26)
        (adjacent cell_2_27 cell_2_28)
        (adjacent cell_2_28 cell_3_28)
        (adjacent cell_2_28 cell_2_27)
        (adjacent cell_2_28 cell_2_29)
        (adjacent cell_2_29 cell_3_29)
        (adjacent cell_2_29 cell_2_28)
        (adjacent cell_3_2 cell_2_2)
        (adjacent cell_3_2 cell_4_2)
        (adjacent cell_3_2 cell_3_3)
        (adjacent cell_3_3 cell_2_3)
        (adjacent cell_3_3 cell_4_3)
        (adjacent cell_3_3 cell_3_2)
        (adjacent cell_3_5 cell_2_5)
        (adjacent cell_3_5 cell_4_5)
        (adjacent cell_3_7 cell_2_7)
        (adjacent cell_3_7 cell_4_7)
        (adjacent cell_3_9 cell_2_9)
        (adjacent cell_3_9 cell_4_9)
        (adjacent cell_3_11 cell_2_11)
        (adjacent cell_3_11 cell_4_11)
        (adjacent cell_3_14 cell_2_14)
        (adjacent cell_3_14 cell_4_14)
        (adjacent cell_3_16 cell_2_16)
        (adjacent cell_3_16 cell_4_16)
        (adjacent cell_3_18 cell_2_18)
        (adjacent cell_3_18 cell_4_18)
        (adjacent cell_3_18 cell_3_19)
        (adjacent cell_3_19 cell_2_19)
        (adjacent cell_3_19 cell_4_19)
        (adjacent cell_3_19 cell_3_18)
        (adjacent cell_3_22 cell_2_22)
        (adjacent cell_3_22 cell_4_22)
        (adjacent cell_3_24 cell_2_24)
        (adjacent cell_3_24 cell_4_24)
        (adjacent cell_3_26 cell_2_26)
        (adjacent cell_3_26 cell_4_26)
        (adjacent cell_3_28 cell_2_28)
        (adjacent cell_3_28 cell_4_28)
        (adjacent cell_3_28 cell_3_29)
        (adjacent cell_3_29 cell_2_29)
        (adjacent cell_3_29 cell_4_29)
        (adjacent cell_3_29 cell_3_28)
        (adjacent cell_4_2 cell_3_2)
        (adjacent cell_4_2 cell_5_2)
        (adjacent cell_4_2 cell_4_3)
        (adjacent cell_4_3 cell_3_3)
        (adjacent cell_4_3 cell_4_2)
        (adjacent cell_4_5 cell_3_5)
        (adjacent cell_4_5 cell_5_5)
        (adjacent cell_4_7 cell_3_7)
        (adjacent cell_4_7 cell_5_7)
        (adjacent cell_4_7 cell_4_8)
        (adjacent cell_4_8 cell_4_7)
        (adjacent cell_4_8 cell_4_9)
        (adjacent cell_4_9 cell_3_9)
        (adjacent cell_4_9 cell_5_9)
        (adjacent cell_4_9 cell_4_8)
        (adjacent cell_4_11 cell_3_11)
        (adjacent cell_4_11 cell_5_11)
        (adjacent cell_4_11 cell_4_12)
        (adjacent cell_4_12 cell_5_12)
        (adjacent cell_4_12 cell_4_11)
        (adjacent cell_4_12 cell_4_13)
        (adjacent cell_4_13 cell_4_12)
        (adjacent cell_4_13 cell_4_14)
        (adjacent cell_4_14 cell_3_14)
        (adjacent cell_4_14 cell_5_14)
        (adjacent cell_4_14 cell_4_13)
        (adjacent cell_4_16 cell_3_16)
        (adjacent cell_4_16 cell_5_16)
        (adjacent cell_4_16 cell_4_17)
        (adjacent cell_4_17 cell_4_16)
        (adjacent cell_4_17 cell_4_18)
        (adjacent cell_4_18 cell_3_18)
        (adjacent cell_4_18 cell_5_18)
        (adjacent cell_4_18 cell_4_17)
        (adjacent cell_4_18 cell_4_19)
        (adjacent cell_4_19 cell_3_19)
        (adjacent cell_4_19 cell_5_19)
        (adjacent cell_4_19 cell_4_18)
        (adjacent cell_4_19 cell_4_20)
        (adjacent cell_4_20 cell_5_20)
        (adjacent cell_4_20 cell_4_19)
        (adjacent cell_4_20 cell_4_21)
        (adjacent cell_4_21 cell_4_20)
        (adjacent cell_4_21 cell_4_22)
        (adjacent cell_4_22 cell_3_22)
        (adjacent cell_4_22 cell_5_22)
        (adjacent cell_4_22 cell_4_21)
        (adjacent cell_4_24 cell_3_24)
        (adjacent cell_4_24 cell_5_24)
        (adjacent cell_4_24 cell_4_25)
        (adjacent cell_4_25 cell_4_24)
        (adjacent cell_4_25 cell_4_26)
        (adjacent cell_4_26 cell_3_26)
        (adjacent cell_4_26 cell_5_26)
        (adjacent cell_4_26 cell_4_25)
        (adjacent cell_4_26 cell_4_27)
        (adjacent cell_4_27 cell_4_26)
        (adjacent cell_4_27 cell_4_28)
        (adjacent cell_4_28 cell_3_28)
        (adjacent cell_4_28 cell_5_28)
        (adjacent cell_4_28 cell_4_27)
        (adjacent cell_4_28 cell_4_29)
        (adjacent cell_4_29 cell_3_29)
        (adjacent cell_4_29 cell_5_29)
        (adjacent cell_4_29 cell_4_28)
        (adjacent cell_5_2 cell_4_2)
        (adjacent cell_5_2 cell_6_2)
        (adjacent cell_5_4 cell_6_4)
        (adjacent cell_5_4 cell_5_5)
        (adjacent cell_5_5 cell_4_5)
        (adjacent cell_5_5 cell_5_4)
        (adjacent cell_5_5 cell_5_6)
        (adjacent cell_5_6 cell_6_6)
        (adjacent cell_5_6 cell_5_5)
        (adjacent cell_5_6 cell_5_7)
        (adjacent cell_5_7 cell_4_7)
        (adjacent cell_5_7 cell_5_6)
        (adjacent cell_5_9 cell_4_9)
        (adjacent cell_5_9 cell_6_9)
        (adjacent cell_5_11 cell_4_11)
        (adjacent cell_5_11 cell_6_11)
        (adjacent cell_5_11 cell_5_12)
        (adjacent cell_5_12 cell_4_12)
        (adjacent cell_5_12 cell_6_12)
        (adjacent cell_5_12 cell_5_11)
        (adjacent cell_5_14 cell_4_14)
        (adjacent cell_5_14 cell_6_14)
        (adjacent cell_5_16 cell_4_16)
        (adjacent cell_5_16 cell_6_16)
        (adjacent cell_5_18 cell_4_18)
        (adjacent cell_5_18 cell_6_18)
        (adjacent cell_5_18 cell_5_19)
        (adjacent cell_5_19 cell_4_19)
        (adjacent cell_5_19 cell_5_18)
        (adjacent cell_5_19 cell_5_20)
        (adjacent cell_5_20 cell_4_20)
        (adjacent cell_5_20 cell_6_20)
        (adjacent cell_5_20 cell_5_19)
        (adjacent cell_5_22 cell_4_22)
        (adjacent cell_5_22 cell_6_22)
        (adjacent cell_5_24 cell_4_24)
        (adjacent cell_5_24 cell_6_24)
        (adjacent cell_5_26 cell_4_26)
        (adjacent cell_5_26 cell_6_26)
        (adjacent cell_5_28 cell_4_28)
        (adjacent cell_5_28 cell_6_28)
        (adjacent cell_5_28 cell_5_29)
        (adjacent cell_5_29 cell_4_29)
        (adjacent cell_5_29 cell_6_29)
        (adjacent cell_5_29 cell_5_28)
        (adjacent cell_6_2 cell_5_2)
        (adjacent cell_6_2 cell_7_2)
        (adjacent cell_6_4 cell_5_4)
        (adjacent cell_6_4 cell_7_4)
        (adjacent cell_6_6 cell_5_6)
        (adjacent cell_6_6 cell_7_6)
        (adjacent cell_6_8 cell_7_8)
        (adjacent cell_6_8 cell_6_9)
        (adjacent cell_6_9 cell_5_9)
        (adjacent cell_6_9 cell_7_9)
        (adjacent cell_6_9 cell_6_8)
        (adjacent cell_6_9 cell_6_10)
        (adjacent cell_6_10 cell_6_9)
        (adjacent cell_6_10 cell_6_11)
        (adjacent cell_6_11 cell_5_11)
        (adjacent cell_6_11 cell_7_11)
        (adjacent cell_6_11 cell_6_10)
        (adjacent cell_6_11 cell_6_12)
        (adjacent cell_6_12 cell_5_12)
        (adjacent cell_6_12 cell_7_12)
        (adjacent cell_6_12 cell_6_11)
        (adjacent cell_6_14 cell_5_14)
        (adjacent cell_6_14 cell_7_14)
        (adjacent cell_6_16 cell_5_16)
        (adjacent cell_6_16 cell_7_16)
        (adjacent cell_6_18 cell_5_18)
        (adjacent cell_6_18 cell_7_18)
        (adjacent cell_6_20 cell_5_20)
        (adjacent cell_6_20 cell_7_20)
        (adjacent cell_6_22 cell_5_22)
        (adjacent cell_6_22 cell_7_22)
        (adjacent cell_6_24 cell_5_24)
        (adjacent cell_6_24 cell_7_24)
        (adjacent cell_6_26 cell_5_26)
        (adjacent cell_6_26 cell_7_26)
        (adjacent cell_6_28 cell_5_28)
        (adjacent cell_6_28 cell_7_28)
        (adjacent cell_6_28 cell_6_29)
        (adjacent cell_6_29 cell_5_29)
        (adjacent cell_6_29 cell_6_28)
        (adjacent cell_7_2 cell_6_2)
        (adjacent cell_7_2 cell_8_2)
        (adjacent cell_7_2 cell_7_3)
        (adjacent cell_7_3 cell_7_2)
        (adjacent cell_7_3 cell_7_4)
        (adjacent cell_7_4 cell_6_4)
        (adjacent cell_7_4 cell_8_4)
        (adjacent cell_7_4 cell_7_3)
        (adjacent cell_7_4 cell_7_5)
        (adjacent cell_7_5 cell_7_4)
        (adjacent cell_7_5 cell_7_6)
        (adjacent cell_7_6 cell_6_6)
        (adjacent cell_7_6 cell_8_6)
        (adjacent cell_7_6 cell_7_5)
        (adjacent cell_7_8 cell_6_8)
        (adjacent cell_7_8 cell_8_8)
        (adjacent cell_7_8 cell_7_9)
        (adjacent cell_7_9 cell_6_9)
        (adjacent cell_7_9 cell_8_9)
        (adjacent cell_7_9 cell_7_8)
        (adjacent cell_7_11 cell_6_11)
        (adjacent cell_7_11 cell_8_11)
        (adjacent cell_7_11 cell_7_12)
        (adjacent cell_7_12 cell_6_12)
        (adjacent cell_7_12 cell_7_11)
        (adjacent cell_7_12 cell_7_13)
        (adjacent cell_7_13 cell_7_12)
        (adjacent cell_7_13 cell_7_14)
        (adjacent cell_7_14 cell_6_14)
        (adjacent cell_7_14 cell_8_14)
        (adjacent cell_7_14 cell_7_13)
        (adjacent cell_7_14 cell_7_15)
        (adjacent cell_7_15 cell_7_14)
        (adjacent cell_7_15 cell_7_16)
        (adjacent cell_7_16 cell_6_16)
        (adjacent cell_7_16 cell_8_16)
        (adjacent cell_7_16 cell_7_15)
        (adjacent cell_7_18 cell_6_18)
        (adjacent cell_7_18 cell_8_18)
        (adjacent cell_7_20 cell_6_20)
        (adjacent cell_7_20 cell_8_20)
        (adjacent cell_7_22 cell_6_22)
        (adjacent cell_7_22 cell_8_22)
        (adjacent cell_7_22 cell_7_23)
        (adjacent cell_7_23 cell_7_22)
        (adjacent cell_7_23 cell_7_24)
        (adjacent cell_7_24 cell_6_24)
        (adjacent cell_7_24 cell_8_24)
        (adjacent cell_7_24 cell_7_23)
        (adjacent cell_7_26 cell_6_26)
        (adjacent cell_7_26 cell_8_26)
        (adjacent cell_7_26 cell_7_27)
        (adjacent cell_7_27 cell_7_26)
        (adjacent cell_7_27 cell_7_28)
        (adjacent cell_7_28 cell_6_28)
        (adjacent cell_7_28 cell_8_28)
        (adjacent cell_7_28 cell_7_27)
        (adjacent cell_8_2 cell_7_2)
        (adjacent cell_8_2 cell_9_2)
        (adjacent cell_8_4 cell_7_4)
        (adjacent cell_8_4 cell_9_4)
        (adjacent cell_8_6 cell_7_6)
        (adjacent cell_8_6 cell_9_6)
        (adjacent cell_8_8 cell_7_8)
        (adjacent cell_8_8 cell_9_8)
        (adjacent cell_8_8 cell_8_9)
        (adjacent cell_8_9 cell_7_9)
        (adjacent cell_8_9 cell_9_9)
        (adjacent cell_8_9 cell_8_8)
        (adjacent cell_8_11 cell_7_11)
        (adjacent cell_8_11 cell_9_11)
        (adjacent cell_8_14 cell_7_14)
        (adjacent cell_8_14 cell_9_14)
        (adjacent cell_8_16 cell_7_16)
        (adjacent cell_8_16 cell_9_16)
        (adjacent cell_8_18 cell_7_18)
        (adjacent cell_8_18 cell_9_18)
        (adjacent cell_8_20 cell_7_20)
        (adjacent cell_8_20 cell_9_20)
        (adjacent cell_8_22 cell_7_22)
        (adjacent cell_8_22 cell_9_22)
        (adjacent cell_8_24 cell_7_24)
        (adjacent cell_8_24 cell_9_24)
        (adjacent cell_8_26 cell_7_26)
        (adjacent cell_8_26 cell_9_26)
        (adjacent cell_8_28 cell_7_28)
        (adjacent cell_8_28 cell_9_28)
        (adjacent cell_9_2 cell_8_2)
        (adjacent cell_9_2 cell_9_3)
        (adjacent cell_9_3 cell_9_2)
        (adjacent cell_9_3 cell_9_4)
        (adjacent cell_9_4 cell_8_4)
        (adjacent cell_9_4 cell_9_3)
        (adjacent cell_9_4 cell_9_5)
        (adjacent cell_9_5 cell_9_4)
        (adjacent cell_9_5 cell_9_6)
        (adjacent cell_9_6 cell_8_6)
        (adjacent cell_9_6 cell_9_5)
        (adjacent cell_9_6 cell_9_7)
        (adjacent cell_9_7 cell_9_6)
        (adjacent cell_9_7 cell_9_8)
        (adjacent cell_9_8 cell_8_8)
        (adjacent cell_9_8 cell_9_7)
        (adjacent cell_9_8 cell_9_9)
        (adjacent cell_9_9 cell_8_9)
        (adjacent cell_9_9 cell_9_8)
        (adjacent cell_9_9 cell_9_10)
        (adjacent cell_9_10 cell_9_9)
        (adjacent cell_9_10 cell_9_11)
        (adjacent cell_9_11 cell_8_11)
        (adjacent cell_9_11 cell_9_10)
        (adjacent cell_9_11 cell_9_12)
        (adjacent cell_9_12 cell_9_11)
        (adjacent cell_9_12 cell_9_13)
        (adjacent cell_9_13 cell_9_12)
        (adjacent cell_9_13 cell_9_14)
        (adjacent cell_9_14 cell_8_14)
        (adjacent cell_9_14 cell_9_13)
        (adjacent cell_9_14 cell_9_15)
        (adjacent cell_9_15 cell_9_14)
        (adjacent cell_9_15 cell_9_16)
        (adjacent cell_9_16 cell_8_16)
        (adjacent cell_9_16 cell_9_15)
        (adjacent cell_9_16 cell_9_17)
        (adjacent cell_9_17 cell_9_16)
        (adjacent cell_9_17 cell_9_18)
        (adjacent cell_9_18 cell_8_18)
        (adjacent cell_9_18 cell_9_17)
        (adjacent cell_9_18 cell_9_19)
        (adjacent cell_9_19 cell_9_18)
        (adjacent cell_9_19 cell_9_20)
        (adjacent cell_9_20 cell_8_20)
        (adjacent cell_9_20 cell_9_19)
        (adjacent cell_9_20 cell_9_21)
        (adjacent cell_9_21 cell_9_20)
        (adjacent cell_9_21 cell_9_22)
        (adjacent cell_9_22 cell_8_22)
        (adjacent cell_9_22 cell_9_21)
        (adjacent cell_9_22 cell_9_23)
        (adjacent cell_9_23 cell_9_22)
        (adjacent cell_9_23 cell_9_24)
        (adjacent cell_9_24 cell_8_24)
        (adjacent cell_9_24 cell_9_23)
        (adjacent cell_9_24 cell_9_25)
        (adjacent cell_9_25 cell_9_24)
        (adjacent cell_9_25 cell_9_26)
        (adjacent cell_9_26 cell_8_26)
        (adjacent cell_9_26 cell_9_25)
        (adjacent cell_9_26 cell_9_27)
        (adjacent cell_9_27 cell_9_26)
        (adjacent cell_9_27 cell_9_28)
        (adjacent cell_9_28 cell_8_28)
        (adjacent cell_9_28 cell_9_27)
        (adjacent cell_9_28 cell_9_29)
        (adjacent cell_9_29 cell_9_28)
        (adjacent cell_9_29 cell_9_30)
        (adjacent cell_9_30 cell_9_29)
        (at_player cell_9_29)
        (at_guard cell_9_23)
        (free)
    )
    (:goal (at_guard cell_9_29))
)