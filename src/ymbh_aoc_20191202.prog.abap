REPORT ymbh_aoc_20191202.


CLASS lcl_opcode DEFINITION.

  PUBLIC SECTION.

    METHODS calculate_final_state
      IMPORTING
        iv_input         TYPE string
        iv_guess_mode    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_output) TYPE string.

  PRIVATE SECTION.

    METHODS calc_single_opcode
      IMPORTING
        iv_input         TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    METHODS calc_multi_opcode
      IMPORTING
        iv_input         TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

ENDCLASS.

CLASS lcl_opcode IMPLEMENTATION.

  METHOD calculate_final_state.

    IF iv_guess_mode EQ abap_true.
      rv_output = calc_multi_opcode( iv_input ).
    ELSE.
      rv_output = calc_single_opcode( iv_input ).
    ENDIF.

  ENDMETHOD.

  METHOD calc_single_opcode.

    DATA lv_opcode TYPE i.
    DATA lv_index TYPE i VALUE 1.

    DATA lv_term1_pos TYPE i.
    DATA lv_term2_pos TYPE i.
    DATA lv_sum_pos TYPE i.

    SPLIT iv_input AT ',' INTO TABLE DATA(lt_opcode).

    WHILE lv_opcode <> 99.

      lv_opcode = lt_opcode[ lv_index ].
      IF lv_opcode = 1.

        lv_term1_pos = lt_opcode[ lv_index + 1 ] + 1.
        lv_term2_pos = lt_opcode[ lv_index + 2 ] + 1.
        lv_sum_pos = lt_opcode[ lv_index + 3 ] + 1.
        lt_opcode[ lv_sum_pos ] = lt_opcode[ lv_term1_pos ] + lt_opcode[ lv_term2_pos ].

      ELSEIF lv_opcode = 2.

        lv_term1_pos = lt_opcode[ lv_index + 1 ] + 1.
        lv_term2_pos = lt_opcode[ lv_index + 2 ] + 1.
        lv_sum_pos = lt_opcode[ lv_index + 3 ] + 1.
        lt_opcode[ lv_sum_pos ] = CONV i( lt_opcode[ lv_term1_pos ] * lt_opcode[ lv_term2_pos ] ).

      ENDIF.
      lv_index = lv_index + 4.

    ENDWHILE.

    LOOP AT lt_opcode INTO DATA(ls_opcode).
      rv_output = |{ rv_output },{ ls_opcode }|.
    ENDLOOP.
    SHIFT rv_output LEFT BY 1 PLACES.
    CONDENSE rv_output NO-GAPS.

  ENDMETHOD.

  METHOD calc_multi_opcode.

    DATA lv_opcode TYPE i.
    DATA lv_index TYPE i VALUE 1.
    DATA lv_found TYPE abap_bool.

    DATA lv_loopa_index TYPE i.
    DATA lv_loopb_index TYPE i.
    DATA lv_term1_pos TYPE i.
    DATA lv_term2_pos TYPE i.
    DATA lv_sum_pos TYPE i.

    SPLIT iv_input AT ',' INTO TABLE DATA(lt_opcode).

    DO 100 TIMES.

      DO 100 TIMES.

        lt_opcode[ 2 ] = lv_loopa_index.
        lt_opcode[ 3 ] = lv_loopb_index.

        WHILE lv_opcode <> 99.

          lv_opcode = lt_opcode[ lv_index ].
          IF lv_opcode = 1.

            lv_term1_pos = lt_opcode[ lv_index + 1 ] + 1.
            lv_term2_pos = lt_opcode[ lv_index + 2 ] + 1.
            lv_sum_pos = lt_opcode[ lv_index + 3 ] + 1.
            lt_opcode[ lv_sum_pos ] = lt_opcode[ lv_term1_pos ] + lt_opcode[ lv_term2_pos ].

          ELSEIF lv_opcode = 2.

            lv_term1_pos = lt_opcode[ lv_index + 1 ] + 1.
            lv_term2_pos = lt_opcode[ lv_index + 2 ] + 1.
            lv_sum_pos = lt_opcode[ lv_index + 3 ] + 1.
            lt_opcode[ lv_sum_pos ] = CONV i( lt_opcode[ lv_term1_pos ] * lt_opcode[ lv_term2_pos ] ).

          ENDIF.
          lv_index = lv_index + 4.


        ENDWHILE.

        lv_index = 1.
        lv_opcode = 0.
        IF lt_opcode[ 1 ] = 19690720.
          lv_found = abap_true.
          EXIT.
        ENDIF.

        lv_loopb_index = lv_loopb_index + 1.
        REFRESH lt_opcode.
        SPLIT iv_input AT ',' INTO TABLE lt_opcode.

      ENDDO.

      IF lv_found EQ abap_true.
        EXIT.
      ENDIF.

      lv_loopb_index = 0.
      REFRESH lt_opcode.
      SPLIT iv_input AT ',' INTO TABLE lt_opcode.
      lv_loopa_index = lv_loopa_index + 1.

    ENDDO.

    LOOP AT lt_opcode INTO DATA(ls_opcode).
      rv_output = |{ rv_output },{ ls_opcode }|.
    ENDLOOP.
    SHIFT rv_output LEFT BY 1 PLACES.
    CONDENSE rv_output NO-GAPS.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_aoc DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS run_1_line_of_opcode FOR TESTING.
    METHODS run_1_line_of_opcode_2 FOR TESTING.
    METHODS run_1_line_of_opcode_3 FOR TESTING.
    METHODS run_2_lines_of_opcode FOR TESTING.

ENDCLASS.


CLASS ltc_aoc IMPLEMENTATION.

  METHOD run_1_line_of_opcode.

    DATA(lo_cut) = NEW lcl_opcode( ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'Das Ergebnis sollte 2,0,0,0,99 sein.'
        exp = |2,0,0,0,99|
        act = lo_cut->calculate_final_state( |1,0,0,0,99| ) ).

  ENDMETHOD.

  METHOD run_2_lines_of_opcode.

    DATA(lo_cut) = NEW lcl_opcode( ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'Das Ergebnis sollte 30,1,1,4,2,5,6,0,99 sein.'
        exp = |30,1,1,4,2,5,6,0,99|
        act = lo_cut->calculate_final_state( |1,1,1,4,99,5,6,0,99| ) ).

  ENDMETHOD.

  METHOD run_1_line_of_opcode_2.

    DATA(lo_cut) = NEW lcl_opcode( ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'Das Ergebnis sollte 2,3,0,6,99 sein.'
        exp = |2,3,0,6,99|
        act = lo_cut->calculate_final_state( |2,3,0,3,99| ) ).

  ENDMETHOD.

  METHOD run_1_line_of_opcode_3.

    DATA(lo_cut) = NEW lcl_opcode( ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'Das Ergebnis sollte 2,4,4,5,99,9801 sein.'
        exp = |2,4,4,5,99,9801|
        act = lo_cut->calculate_final_state( |2,4,4,5,99,0| ) ).

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b01.
PARAMETERS p_input TYPE string.
PARAMETERS p_gmode TYPE char1 AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA fname TYPE string.

  DATA lv_rc TYPE i.

  DATA(lo_opcode) = NEW lcl_opcode( )..
  DATA(lv_ergebnis) = lo_opcode->calculate_final_state( iv_input = p_input
                                                        iv_guess_mode = p_gmode ).
  WRITE lv_ergebnis.
