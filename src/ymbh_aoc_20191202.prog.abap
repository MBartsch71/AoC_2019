REPORT ymbh_aoc_20191202.

CLASS  lcx_opcode DEFINITION INHERITING FROM cx_static_check.

ENDCLASS.

CLASS lcl_opcode DEFINITION.

  PUBLIC SECTION.

    METHODS calculate_final_state
      IMPORTING
        iv_input         TYPE string
        iv_search_term   TYPE i OPTIONAL
      RETURNING
        VALUE(rv_output) TYPE string.

  PRIVATE SECTION.

    CONSTANTS mc_separator TYPE string VALUE ',' ##NO_TEXT.

    DATA mt_opcode TYPE TABLE OF string.

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

    METHODS calculate_term_pos
      IMPORTING
        iv_index           TYPE i
        iv_term_index      TYPE i
      RETURNING
        VALUE(rv_term_pos) TYPE i.

    METHODS summarize_instructions
      IMPORTING
        iv_index        TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

    METHODS multiply_instructions
      IMPORTING
        iv_index        TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

    METHODS increase_counter
      IMPORTING
        iv_counter      TYPE i
        iv_distance     TYPE i
      RETURNING
        VALUE(rv_value) TYPE i.

    METHODS get_new_opcode
      IMPORTING
        iv_index         TYPE i
      RETURNING
        VALUE(rv_opcode) TYPE i
      RAISING
        lcx_opcode.

    METHODS build_result_string
      RETURNING
        VALUE(rv_output) TYPE string.

    METHODS process_opcode_string.

    METHODS transform_input_to_table
      IMPORTING
        iv_input TYPE string.
ENDCLASS.

CLASS lcl_opcode IMPLEMENTATION.

  METHOD calculate_final_state.

    rv_output = SWITCH #( iv_search_term WHEN space THEN calc_single_opcode( iv_input )
                                         ELSE calc_multi_opcode( iv_input ) )  .

  ENDMETHOD.

  METHOD calc_single_opcode.

    DATA lv_index  TYPE i VALUE 1.
    DATA lv_opcode TYPE i.

    transform_input_to_table( iv_input ).
    process_opcode_string( ).
    rv_output = build_result_string( ).

  ENDMETHOD.

  METHOD process_opcode_string.

    DATA lv_index TYPE i VALUE 1.
    DATA lv_opcode TYPE i.

    DO.
      TRY.
          lv_opcode = get_new_opcode( lv_index ).
          mt_opcode[ calculate_term_pos( iv_index = lv_index iv_term_index = 3 ) ] =
            SWITCH #( lv_opcode
                 WHEN 1 THEN summarize_instructions( lv_index )
                 WHEN 2 THEN multiply_instructions( lv_index  ) ).
        CATCH lcx_opcode.
          EXIT.
      ENDTRY.
      lv_index = increase_counter( iv_counter = lv_index iv_distance = 4 ).
    ENDDO.

  ENDMETHOD.

  METHOD build_result_string.

    LOOP AT mt_opcode INTO DATA(ls_opcode).
      rv_output = |{ rv_output }{ mc_separator }{ ls_opcode }|.
    ENDLOOP.
    SHIFT rv_output LEFT BY 1 PLACES.
    CONDENSE rv_output NO-GAPS.

  ENDMETHOD.

  METHOD get_new_opcode.

    rv_opcode = mt_opcode[ iv_index ].
    IF rv_opcode EQ 99.
      RAISE EXCEPTION TYPE lcx_opcode.
    ENDIF.

  ENDMETHOD.

  METHOD increase_counter.
    rv_value = iv_counter + iv_distance.
  ENDMETHOD.

  METHOD calc_multi_opcode.

    DATA lv_found TYPE abap_bool.

    DATA lv_loopa_index TYPE i.
    DATA lv_loopb_index TYPE i.

    transform_input_to_table( iv_input ).

    DO 100 TIMES.
      DO 100 TIMES.

        mt_opcode[ 2 ] = lv_loopa_index.
        mt_opcode[ 3 ] = lv_loopb_index.

        process_opcode_string( ).

        IF mt_opcode[ 1 ] = 19690720.
          lv_found = abap_true.
          EXIT.
        ENDIF.

        lv_loopb_index = increase_counter( iv_counter  = lv_loopb_index iv_distance = 1 ).
        REFRESH mt_opcode.
        transform_input_to_table( iv_input ).

      ENDDO.

      IF lv_found EQ abap_true.
        EXIT.
      ENDIF.

      lv_loopb_index = 0.
      REFRESH mt_opcode.
      transform_input_to_table( iv_input ).
      lv_loopa_index = increase_counter( iv_counter  = lv_loopa_index iv_distance = 1 ).

    ENDDO.

    rv_output = build_result_string( ).

  ENDMETHOD.

  METHOD calculate_term_pos.
    rv_term_pos = mt_opcode[ iv_index + iv_term_index ] + 1.
  ENDMETHOD.

  METHOD summarize_instructions.
    rv_value = mt_opcode[ calculate_term_pos( iv_index = iv_index iv_term_index = 1 ) ] +
               mt_opcode[ calculate_term_pos( iv_index = iv_index iv_term_index = 2 ) ].
  ENDMETHOD.

  METHOD multiply_instructions.
    rv_value = mt_opcode[ calculate_term_pos( iv_index = iv_index iv_term_index = 1 ) ] *
               mt_opcode[ calculate_term_pos( iv_index = iv_index iv_term_index = 2 ) ].
  ENDMETHOD.

  METHOD transform_input_to_table.
    REFRESH mt_opcode.
    SPLIT iv_input AT mc_separator INTO TABLE mt_opcode.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_aoc DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO lcl_opcode.

    METHODS setup.

    METHODS run_1_line_of_opcode   FOR TESTING.
    METHODS run_1_line_of_opcode_2 FOR TESTING.
    METHODS run_1_line_of_opcode_3 FOR TESTING.
    METHODS run_2_lines_of_opcode  FOR TESTING.

    METHODS dtrmn_correct_opcdode_at_0 FOR TESTING.
    METHODS dtrmn_correct_terms_at_2_3 FOR TESTING.

ENDCLASS.


CLASS ltc_aoc IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD run_1_line_of_opcode.

    cl_abap_unit_assert=>assert_equals(
        msg = 'The result should be 2,0,0,0,99.'
        exp = |2,0,0,0,99|
        act = mo_cut->calculate_final_state( |1,0,0,0,99| ) ).

  ENDMETHOD.

  METHOD run_2_lines_of_opcode.

    cl_abap_unit_assert=>assert_equals(
        msg = 'The result should be 30,1,1,4,2,5,6,0,99.'
        exp = |30,1,1,4,2,5,6,0,99|
        act = mo_cut->calculate_final_state( |1,1,1,4,99,5,6,0,99| ) ).

  ENDMETHOD.

  METHOD run_1_line_of_opcode_2.

    cl_abap_unit_assert=>assert_equals(
        msg = 'The result should be 2,3,0,6,99.'
        exp = |2,3,0,6,99|
        act = mo_cut->calculate_final_state( |2,3,0,3,99| ) ).

  ENDMETHOD.

  METHOD run_1_line_of_opcode_3.

    cl_abap_unit_assert=>assert_equals(
        msg = 'The result should be 2,4,4,5,99,9801.'
        exp = |2,4,4,5,99,9801|
        act = mo_cut->calculate_final_state( |2,4,4,5,99,0| ) ).

  ENDMETHOD.

  METHOD dtrmn_correct_opcdode_at_0.

    DATA(lv_input_opcode) = |1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,| &
                            |27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,| &
                            |51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,| &
                            |75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,| &
                            |9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,| &
                            |0,99,2,0,14,0'|.

    SPLIT mo_cut->calculate_final_state( iv_input = lv_input_opcode )
      AT ',' INTO TABLE DATA(lt_result).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The first number in the result should be 4714701.'
        exp = 4714701
        act =  lt_result[ 1 ] ).

  ENDMETHOD.

  METHOD dtrmn_correct_terms_at_2_3.

    DATA(lv_input_opcode) = |1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,5,19,23,2,10,23,| &
                            |27,1,27,5,31,2,9,31,35,1,35,5,39,2,6,39,43,1,43,5,47,2,47,10,| &
                            |51,2,51,6,55,1,5,55,59,2,10,59,63,1,63,6,67,2,67,6,71,1,71,5,| &
                            |75,1,13,75,79,1,6,79,83,2,83,13,87,1,87,6,91,1,10,91,95,1,95,| &
                            |9,99,2,99,13,103,1,103,6,107,2,107,6,111,1,111,2,115,1,115,13,| &
                            |0,99,2,0,14,0'|.

    SPLIT mo_cut->calculate_final_state( iv_input = lv_input_opcode
                                         iv_search_term = 19690720 )
      AT ',' INTO TABLE DATA(lt_result).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The first value in the string should be 19690720.'
        exp = 19690720
        act =  lt_result[ 1 ] ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The second value at string should be 51.'
        exp = 51
        act = lt_result[ 2 ] ).

    cl_abap_unit_assert=>assert_equals(
            msg = 'The third value at string should be 21.'
            exp = 21
            act = lt_result[ 3 ] ).

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b01.
PARAMETERS p_input TYPE string.
PARAMETERS p_gmode TYPE i.

SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA fname TYPE string.

  DATA lv_rc TYPE i.

  DATA(lo_opcode) = NEW lcl_opcode( ).
  DATA(lv_ergebnis) = lo_opcode->calculate_final_state( iv_input = p_input
                                                        iv_search_term = p_gmode ).
  WRITE lv_ergebnis.
