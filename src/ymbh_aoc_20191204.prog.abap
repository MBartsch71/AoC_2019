REPORT ymbh_aoc_20191204.

CLASS lcl_aoc_password DEFINITION.

  PUBLIC SECTION.

    TYPES tt_numbers TYPE RANGE OF i.

    "! Method for determining potential passwords from a given range
    "! @parameter it_potential_passwords | Range table
    "! @parameter rv_passwords_count | Amount of potential passwords
    METHODS find_passwords_in_range
      IMPORTING
        it_potential_passwords    TYPE tt_numbers
      RETURNING
        VALUE(rv_passwords_count) TYPE i.

  PRIVATE SECTION.

    CONSTANTS mc_regex_2_digit_occurence TYPE string VALUE '(\d)(\1){1,1}' ##NO_TEXT.
    CONSTANTS mc_regex_3_digit_occurence TYPE string VALUE '(\d)(\1){2,2}' ##NO_TEXT.
    CONSTANTS mc_regex_4_digit_occurence TYPE string VALUE '(\d)(\1){3,3}' ##NO_TEXT.

    METHODS investigate_term_with_regex
      IMPORTING
        iv_number       TYPE i
        iv_regex        TYPE string
      RETURNING
        VALUE(rv_count) TYPE i.
    METHODS convert_input_term_in_string
      IMPORTING
        iv_number               TYPE i
      RETURNING
        VALUE(rv_number_string) TYPE string.

    METHODS examine_string_chars_order
      IMPORTING
        iv_number_string TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS increase_counter
      IMPORTING
        iv_current_counter    TYPE i
      RETURNING
        VALUE(rv_new_counter) TYPE i.

    METHODS term_has_adjacent_twin_digits
      IMPORTING
        iv_number        TYPE i
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS digits_are_at_ascending_order
      IMPORTING
        iv_number        TYPE i
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    METHODS search_multiple_digits
      IMPORTING
        iv_number            TYPE i
      RETURNING
      VALUE(rv_multiple_digit_count) type i.

ENDCLASS.

CLASS lcl_aoc_password IMPLEMENTATION.

  METHOD find_passwords_in_range.
    DATA(lv_current_number) = it_potential_passwords[ 1 ]-low.
    DATA(lv_upper_bound)    = it_potential_passwords[ 1 ]-high.

    WHILE lv_current_number <= lv_upper_bound.

      rv_passwords_count = COND #( WHEN term_has_adjacent_twin_digits( lv_current_number ) AND
                                        digits_are_at_ascending_order( lv_current_number )
                                        THEN increase_counter( rv_passwords_count )
                                   ELSE rv_passwords_count ).

      lv_current_number = increase_counter( lv_current_number ).

    ENDWHILE.
  ENDMETHOD.

  METHOD term_has_adjacent_twin_digits.
    rv_result = boolc( search_multiple_digits( iv_number ) > 0 ).
  ENDMETHOD.

  METHOD search_multiple_digits.
    rv_multiple_digit_count = investigate_term_with_regex(
                                           iv_number = iv_number
                                           iv_regex = mc_regex_2_digit_occurence ) -
                              investigate_term_with_regex(
                                           iv_number = iv_number
                                           iv_regex  = mc_regex_3_digit_occurence ) -
                              investigate_term_with_regex(
                                           iv_number = iv_number
                                           iv_regex  = mc_regex_4_digit_occurence  ).
  ENDMETHOD.

  METHOD investigate_term_with_regex.
    FIND ALL OCCURRENCES OF REGEX iv_regex
            IN CONV string( iv_number )
            MATCH COUNT rv_count .
  ENDMETHOD.

  METHOD digits_are_at_ascending_order.
    rv_result = examine_string_chars_order( convert_input_term_in_string( iv_number ) ).
  ENDMETHOD.

  METHOD convert_input_term_in_string.
    rv_number_string  = CONV string( iv_number ).
    CONDENSE rv_number_string NO-GAPS.
  ENDMETHOD.

  METHOD examine_string_chars_order.
    DATA lv_current_digit_pos    TYPE i VALUE 0.
    DATA lv_check_next_digit_pos TYPE i VALUE 1.

    DO strlen( iv_number_string ) - 1 TIMES.

      IF iv_number_string+lv_check_next_digit_pos(1) <
         iv_number_string+lv_current_digit_pos(1).
        RETURN.
      ENDIF.

      lv_current_digit_pos = increase_counter( lv_current_digit_pos ).
      lv_check_next_digit_pos = increase_counter( lv_check_next_digit_pos ).

    ENDDO.

    rv_result = abap_true.
  ENDMETHOD.

  METHOD increase_counter.
    rv_new_counter = iv_current_counter + 1.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_aoc DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO lcl_aoc_password.

    METHODS setup.

    METHODS find_one_suitable_passwort_1   FOR TESTING.
    METHODS find_4_suitable_passwords      FOR TESTING.
    METHODS find_3_suitable_passwords_of_4 FOR TESTING.
    METHODS dont_find_password_in_range    FOR TESTING.

ENDCLASS.

CLASS ltc_aoc IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD find_one_suitable_passwort_1.
    DATA(lt_input) = VALUE mo_cut->tt_numbers( ( sign = 'I' option = 'EQ' low = 123453 high = 123457 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The amount of correct passwords should be 1.'
        exp = 1
        act = mo_cut->find_passwords_in_range( lt_input ) ).
  ENDMETHOD.

  METHOD dont_find_password_in_range.
    DATA(lt_input) = VALUE mo_cut->tt_numbers( ( sign = 'I' option = 'EQ' low = 234560 high = 234565 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The amount of correct passwords should be 0.'
        exp = 0
        act = mo_cut->find_passwords_in_range( lt_input ) ).
  ENDMETHOD.

  METHOD find_4_suitable_passwords.
    DATA(lt_input) = VALUE mo_cut->tt_numbers( ( sign = 'I' option = 'EQ' low = 122344 high = 122347 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The amount of correct passwords should be 4.'
        exp = 4
        act = mo_cut->find_passwords_in_range( lt_input ) ).
  ENDMETHOD.

  METHOD find_3_suitable_passwords_of_4.
    DATA(lt_input) = VALUE mo_cut->tt_numbers( ( sign = 'I' option = 'EQ' low = 111121 high = 111134 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'The amount of correct passwords should be 2.'
        exp = 2
        act = mo_cut->find_passwords_in_range( lt_input ) ).
  ENDMETHOD.

ENDCLASS.

DATA: mv_number TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b01.
SELECT-OPTIONS so_num FOR mv_number.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA(lo_password_digger) = NEW lcl_aoc_password( ).
  DATA(lv_password_count) = lo_password_digger->find_passwords_in_range( so_num[] ).

  WRITE |Amount of possible passwords: { lv_password_count }|.
