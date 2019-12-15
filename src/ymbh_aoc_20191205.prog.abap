REPORT ymbh_aoc_20191205.

INTERFACE lif_opcode.

  TYPES: BEGIN OF ty_opcode,
           key   TYPE i,
           value TYPE i,
         END OF ty_opcode.
  TYPES tt_opcode TYPE SORTED TABLE OF ty_opcode
                       WITH UNIQUE KEY key.

  TYPES tt_parameters TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  DATA mv_opcode     TYPE char5.
  DATA mv_operation  TYPE char2.
  DATA mt_parameters TYPE tt_parameters.

  "! Method for processing the current instruction
  "! @parameter iv_opcode | current operation code from input string
  METHODS process_input_opcode
    IMPORTING
      iv_opcode TYPE i.

  "! Method delivers the current operation of the computer
  "! @parameter rv_operation | Return value current opcode
  METHODS get_operation
    RETURNING
      VALUE(rv_operation) TYPE char2.

  "! Method determines the parameter mode of a given parameter
  "! @parameter iv_index | Index of the parameter to read
  "! @parameter rv_parameter_mode | Return value parameter mode
  METHODS get_parameter_mode_by_index
    IMPORTING
      iv_index                 TYPE i
    RETURNING
      VALUE(rv_parameter_mode) TYPE i.

  "! Method determines the iteration interval based on the amount of parameters of the current operation
  "! @parameter rv_iteration | current operation
  METHODS get_iteration_interval
    RETURNING
      VALUE(rv_iteration) TYPE i.

ENDINTERFACE.

INTERFACE lif_output.

  "! Method outputs a value
  "! @parameter iv_value | Value to output
  METHODS output_value
    IMPORTING
      iv_value TYPE string.

ENDINTERFACE.

INTERFACE lif_input.

  "! Method takes an value from an input source
  "! @parameter rv_value | Input value
  METHODS input_value
    RETURNING
      VALUE(rv_value) TYPE i.

ENDINTERFACE.

CLASS lcx_opcode_coll DEFINITION
                      INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_opcode_contants DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS mc_opcode_addition       TYPE char2 VALUE '01'.
    CONSTANTS mc_opcode_multiplication TYPE char2 VALUE '02'.
    CONSTANTS mc_opcode_input          TYPE char2 VALUE '03'.
    CONSTANTS mc_opcode_output         TYPE char2 VALUE '04'.
    CONSTANTS mc_opcode_jump_if_true   TYPE char2 VALUE '05'.
    CONSTANTS mc_opcode_jump_if_false  TYPE char2 VALUE '06'.
    CONSTANTS mc_opcode_less_than      TYPE char2 VALUE '07'.
    CONSTANTS mc_opcode_equals         TYPE char2 VALUE '08'.
    CONSTANTS mc_operation_termination TYPE char2 VALUE '99'.

ENDCLASS.

CLASS lcl_opcode_output DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_output.
    ALIASES output_value FOR lif_output~output_value.

ENDCLASS.

CLASS lcl_opcode_output IMPLEMENTATION.

  METHOD output_value.
    WRITE / iv_value.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_opcode_input DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_input.
    ALIASES input_value FOR lif_input~input_value.

ENDCLASS.

CLASS lcl_opcode_input IMPLEMENTATION.

  METHOD input_value.
    DATA lv_answer TYPE char1.
    DATA lv_value TYPE char30.

    CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
      EXPORTING
        textline1   = |Please enter a value|
        titel       = |MBH ABAP Inmtcode computer|
        valuelength = 30
      IMPORTING
        answer      = lv_answer
        value1      = lv_value.
    IF lv_answer EQ 'J'.
      rv_value = CONV #( lv_value ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_opcode_collection DEFINITION.

  PUBLIC SECTION.

    "! Method builds the operation table from an input sequence
    "! @parameter iv_string | Input sequence
    "! @parameter rt_opcode_coll | Returns the operations table
    METHODS build_opcode_coll_from_string
      IMPORTING
        iv_string             TYPE string
      RETURNING
        VALUE(rt_opcode_coll) TYPE lif_opcode=>tt_opcode.

    "! Method read an index from the operations table requested by its index
    "! @parameter iv_index | Index to read the value
    "! @parameter rv_value | Return value
    "! @raising lcx_opcode_coll | Exception if value is not found
    METHODS get_value_by_index
      IMPORTING
        iv_index        TYPE i
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        lcx_opcode_coll.

    "! Method sets a value to the table with index provided
    "! @parameter iv_index | Index of the position of the value
    "! @parameter iv_value | Value to set
    "! @raising lcx_opcode_coll | Exception if position is not found
    METHODS set_value_to_index
      IMPORTING
        iv_index TYPE i
        iv_value TYPE i
      RAISING
        lcx_opcode_coll.

    "! Method build a string from the current operation table
    "! @parameter rv_opcode | Exception if an error occurs
    METHODS build_string_from_opcode_coll
      RETURNING
        VALUE(rv_opcode) TYPE string.

  PRIVATE SECTION.
    DATA mt_opcode_collection TYPE lif_opcode=>tt_opcode.

ENDCLASS.

CLASS lcl_opcode_collection IMPLEMENTATION.

  METHOD build_opcode_coll_from_string.

    SPLIT iv_string AT ',' INTO TABLE DATA(lt_string).
    mt_opcode_collection = VALUE #( FOR ls_string IN lt_string
                                    INDEX INTO lv_index
                                    LET x = lv_index - 1
                                    IN key = x
                                     ( value = ls_string ) ).
  ENDMETHOD.

  METHOD get_value_by_index.
    TRY.
        rv_value = mt_opcode_collection[ key = iv_index ]-value.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_opcode_coll.
    ENDTRY.
  ENDMETHOD.

  METHOD set_value_to_index.
    TRY.
        mt_opcode_collection[ key = iv_index ]-value = iv_value.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_opcode_coll.
    ENDTRY.
  ENDMETHOD.

  METHOD build_string_from_opcode_coll.
    rv_opcode = REDUCE #( INIT x TYPE string sep = ''
                          FOR ls_opcode IN mt_opcode_collection
                          NEXT x = |{ x }{ sep }{ ls_opcode-value }| sep = ',' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_aoc_opcode_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS mc_test_string_1 TYPE string VALUE '1,3,2,0,99' ##NO_TEXT.
    CONSTANTS mc_test_string_2 TYPE string VALUE '1,2,2,0,99' ##NO_TEXT.

    DATA mo_cut        TYPE REF TO lcl_opcode_collection.
    DATA mx_coll_error TYPE REF TO lcx_opcode_coll.

    METHODS setup.

    METHODS build_check_table
      RETURNING
        VALUE(rt_check_table) TYPE lif_opcode=>tt_opcode.

    METHODS check_no_exception.

    METHODS read_value_from_specific_pos   FOR TESTING.
    METHODS build_string_from_opcode_coll  FOR TESTING.

ENDCLASS.

CLASS ltc_aoc_opcode_collection IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD build_check_table.
    rt_check_table  = VALUE lif_opcode=>tt_opcode( ( key = 0 value = 1 )
                                                   ( key = 1 value = 3 )
                                                   ( key = 2 value = 2 )
                                                   ( key = 3 value = 0 )
                                                   ( key = 4 value = 99 ) ).
  ENDMETHOD.

  METHOD check_no_exception.
    cl_abap_unit_assert=>assert_not_bound(
            act  = mx_coll_error
            msg  = |Es sollte keine Ausnahme aufgetreten sein.| ).
  ENDMETHOD.

  METHOD read_value_from_specific_pos.
    mo_cut->build_opcode_coll_from_string( mc_test_string_1 ).

    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = 'The returned value should be 3'
            exp = 3
            act = mo_cut->get_value_by_index( 1 ) ).
      CATCH lcx_opcode_coll INTO mx_coll_error.
    ENDTRY.
    check_no_exception( ).
  ENDMETHOD.

  METHOD build_string_from_opcode_coll.
    mo_cut->build_opcode_coll_from_string( mc_test_string_2 ).
    TRY.
        mo_cut->set_value_to_index( iv_index = 1  iv_value = 3 ).
      CATCH lcx_opcode_coll INTO mx_coll_error.
    ENDTRY.
    check_no_exception( ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The result should match the check string'
        exp = mc_test_string_1
        act = mo_cut->build_string_from_opcode_coll( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_opcode_iterator DEFINITION.

  PUBLIC SECTION.

    "! Method provide next index of opcode
    "! @parameter rv_index | Return value index
    METHODS provide_next_index
      RETURNING
        VALUE(rv_index) TYPE i.

    "! Method iterate index to next one
    "! @parameter iv_increase | Parameter how much the index increases
    METHODS iterate
      IMPORTING
        iv_increase TYPE i.

    "! Method to set index directly
    "! @parameter iv_index | Parameter for next index
    METHODS set_index
      IMPORTING
        iv_index TYPE i.

  PRIVATE SECTION.

    DATA mv_index TYPE i.

ENDCLASS.

CLASS lcl_opcode_iterator IMPLEMENTATION.

  METHOD provide_next_index.
    rv_index = mv_index.
  ENDMETHOD.

  METHOD iterate.
    mv_index = mv_index + iv_increase.
  ENDMETHOD.

  METHOD set_index.
    mv_index = iv_index.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_opcode_iterator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO lcl_opcode_iterator.

    METHODS setup.

    METHODS provide_first_index       FOR TESTING.
    METHODS provide_next_index_step_4 FOR TESTING.
    METHODS provide_set_index         FOR TESTING.

ENDCLASS.

CLASS ltc_opcode_iterator IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD provide_first_index.
    cl_abap_unit_assert=>assert_equals(
        msg = 'The first index should be 0.'
        exp = 0
        act = mo_cut->provide_next_index( ) ).
  ENDMETHOD.

  METHOD provide_next_index_step_4.
    mo_cut->iterate( 4 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The index should be 4'
        exp = 4
        act = mo_cut->provide_next_index( ) ).
  ENDMETHOD.

  METHOD provide_set_index.
    mo_cut->set_index( 2 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The index should be 2'
        exp = 2
        act = mo_cut->provide_next_index( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_opcode_controller DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_opcode.

    ALIASES mv_opcode     FOR lif_opcode~mv_opcode.
    ALIASES mt_parameters FOR lif_opcode~mt_parameters.

    ALIASES get_operation               FOR lif_opcode~get_operation.
    ALIASES process_input_opcode        FOR lif_opcode~process_input_opcode.
    ALIASES get_parameter_mode_by_index FOR lif_opcode~get_parameter_mode_by_index.
    ALIASES get_iteration_interval      FOR lif_opcode~get_iteration_interval.

    "! Constructor of opcode controller
    "! @parameter iv_value | Input parameter new opcode
    METHODS constructor
      IMPORTING
        iv_value TYPE i.

ENDCLASS.

CLASS lcl_opcode_controller IMPLEMENTATION.

  METHOD constructor.
    process_input_opcode( iv_value ).
  ENDMETHOD.

  METHOD get_operation.
    rv_operation = mv_opcode+3(2).
  ENDMETHOD.

  METHOD get_parameter_mode_by_index.
    rv_parameter_mode = mt_parameters[ iv_index ].
  ENDMETHOD.

  METHOD process_input_opcode.
    mv_opcode = |{ iv_opcode  ALIGN = RIGHT WIDTH = 5 PAD = '0' }|.
    mt_parameters = VALUE lif_opcode=>tt_parameters( FOR index = 2 THEN index - 1 UNTIL index < 0
                                                   ( CONV i( mv_opcode+index(1) ) ) ).
  ENDMETHOD.

  METHOD get_iteration_interval.
    rv_iteration = SWITCH #( get_operation( )
                                WHEN lcl_opcode_contants=>mc_opcode_addition       THEN 4
                                WHEN lcl_opcode_contants=>mc_opcode_multiplication THEN 4
                                WHEN lcl_opcode_contants=>mc_opcode_input          THEN 2
                                WHEN lcl_opcode_contants=>mc_opcode_output         THEN 2
                                WHEN lcl_opcode_contants=>mc_opcode_jump_if_true   THEN 3
                                WHEN lcl_opcode_contants=>mc_opcode_jump_if_false  THEN 3
                                WHEN lcl_opcode_contants=>mc_opcode_less_than      THEN 4
                                WHEN lcl_opcode_contants=>mc_opcode_equals         THEN 4
                                ELSE 4 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_opcode_controller DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_operation_from_controller FOR TESTING.
    METHODS get_1st_parameter_from_cntrlr FOR TESTING.

ENDCLASS.

CLASS ltc_opcode_controller IMPLEMENTATION.

  METHOD get_operation_from_controller.
    DATA(lo_cut) = NEW lcl_opcode_controller( 1 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The operation should be 01.'
        exp = |01|
        act = lo_cut->get_operation( ) ).
  ENDMETHOD.

  METHOD get_1st_parameter_from_cntrlr.
    DATA(lo_cut) = NEW lcl_opcode_controller( 2101 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The parameter should be 1.'
        exp = 2
        act = lo_cut->get_parameter_mode_by_index( 2 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_opcode_computer DEFINITION.

  PUBLIC SECTION.

    "! Constructor of intcode computer
    METHODS constructor.

    "! Method initialise computer
    "! @parameter iv_input_data | Input sequence
    METHODS initialize_computer_with_data
      IMPORTING
        iv_input_data TYPE string.

    "! Method performs the operations of the computer
    "! @raising lcx_opcode_coll | Exception if something goes wrong
    METHODS perform_operations
      RAISING
        lcx_opcode_coll.

    "! Method provides operation result
    "! @parameter rv_result_data | Return value result
    METHODS provide_operations_result
      RETURNING
        VALUE(rv_result_data) TYPE string.

    "! Method for setting the output object
    "! @parameter io_output | Output object
    METHODS set_mo_output
      IMPORTING
        io_output TYPE REF TO lif_output.

    "! Method for setting the input object
    "! @parameter io_input | Input object
    METHODS set_mo_input
      IMPORTING
        io_input TYPE REF TO lif_input.

  PRIVATE SECTION.

    DATA mo_opcode_collection TYPE REF TO lcl_opcode_collection.
    DATA mo_opcode_iterator   TYPE REF TO lcl_opcode_iterator.
    DATA mo_opcode_controller TYPE REF TO lcl_opcode_controller.
    DATA mo_output            TYPE REF TO lif_output.
    DATA mo_input             TYPE REF TO lif_input.

    DATA mv_index TYPE i.

    METHODS perform_addition
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS perform_multiplication
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS perform_output_operation
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        lcx_opcode_coll.

    METHODS save_result_to_collection
      IMPORTING
        iv_result           TYPE i
      RETURNING
        VALUE(rv_processed) TYPE abap_bool
      RAISING
        lcx_opcode_coll.

    METHODS determine_operation_term
      IMPORTING
        iv_parameter    TYPE i
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS determine_value_from_position
      IMPORTING
        iv_parameter    TYPE i
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS determine_value
      IMPORTING
        iv_parameter    TYPE i
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS process_results
      IMPORTING
        iv_result TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS result_can_be_saved
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        lcx_opcode_coll.

    METHODS prepare_next_operation
      RAISING
        lcx_opcode_coll.
    METHODS perform_input_operation
      RETURNING
        VALUE(rv_value) TYPE i.
    METHODS perform_operation_equals
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.
    METHODS perform_operation_less_than
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS perform_jump_true_operation
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.

    METHODS set_iterator_index
      IMPORTING
        iv_index TYPE i
      RAISING
        lcx_opcode_coll.
    METHODS perform_jump_false_operation
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        lcx_opcode_coll.

ENDCLASS.

CLASS lcl_opcode_computer IMPLEMENTATION.

  METHOD constructor.
    mo_opcode_collection = NEW lcl_opcode_collection( ).
    mo_opcode_iterator   = NEW lcl_opcode_iterator( ).
    mo_output            = NEW lcl_opcode_output( ).
    mo_input             = NEW lcl_opcode_input( ).
  ENDMETHOD.

  METHOD set_mo_output.
    mo_output = io_output.
  ENDMETHOD.

  METHOD set_mo_input.
    mo_input = io_input.
  ENDMETHOD.

  METHOD initialize_computer_with_data.
    mo_opcode_collection->build_opcode_coll_from_string( iv_input_data ).
  ENDMETHOD.

  METHOD perform_operations.
    prepare_next_operation( ).
    WHILE mo_opcode_controller->get_operation( ) <> lcl_opcode_contants=>mc_operation_termination.
      TRY.
          DATA(lv_result) = SWITCH #( mo_opcode_controller->get_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_addition       THEN perform_addition( )
                                  WHEN lcl_opcode_contants=>mc_opcode_multiplication THEN perform_multiplication( )
                                  WHEN lcl_opcode_contants=>mc_opcode_input          THEN perform_input_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_jump_if_true   THEN perform_jump_true_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_jump_if_false  THEN perform_jump_false_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_output         THEN perform_output_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_less_than      THEN perform_operation_less_than( )
                                  WHEN lcl_opcode_contants=>mc_opcode_equals         THEN perform_operation_equals( )
                            ).
          process_results( lv_result ).
        CATCH lcx_opcode_coll.
      ENDTRY.
      set_iterator_index( lv_result ).
      prepare_next_operation( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD set_iterator_index.
    CASE mo_opcode_controller->get_operation( ).
      WHEN lcl_opcode_contants=>mc_opcode_jump_if_true OR
           lcl_opcode_contants=>mc_opcode_jump_if_false.
        mo_opcode_iterator->set_index( iv_index ).
      WHEN OTHERS.
        mo_opcode_iterator->iterate( mo_opcode_controller->get_iteration_interval( ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD prepare_next_operation.
    mv_index = mo_opcode_iterator->provide_next_index( ) .
    mo_opcode_controller = NEW lcl_opcode_controller( mo_opcode_collection->get_value_by_index( mv_index ) ).
  ENDMETHOD.

  METHOD save_result_to_collection.
    mo_opcode_collection->set_value_to_index(
            iv_index        =  mo_opcode_collection->get_value_by_index( mv_index + 3 )
            iv_value        =  iv_result
        ).
    rv_processed = abap_true.
  ENDMETHOD.

  METHOD provide_operations_result.
    rv_result_data = mo_opcode_collection->build_string_from_opcode_coll( ).
  ENDMETHOD.

  METHOD perform_addition.
    rv_result = determine_operation_term( 1 ) + determine_operation_term( 2 ).
  ENDMETHOD.

  METHOD perform_multiplication.
    rv_result = determine_operation_term( 1 ) * determine_operation_term( 2 ).
  ENDMETHOD.

  METHOD perform_input_operation.
    rv_value = mo_input->input_value( ).
  ENDMETHOD.

  METHOD perform_output_operation.
    rv_result = determine_operation_term( 1 ).
    mo_output->output_value( rv_result ).
  ENDMETHOD.

  METHOD perform_jump_true_operation.
    rv_result = COND #( WHEN determine_operation_term( 1 ) <> 0 THEN determine_operation_term( 2 )
                        ELSE mv_index + mo_opcode_controller->get_iteration_interval( ) ).
  ENDMETHOD.

  METHOD perform_jump_false_operation.
    rv_result = COND #( WHEN determine_operation_term( 1 ) = 0 THEN determine_operation_term( 2 )
                        ELSE mv_index + mo_opcode_controller->get_iteration_interval( ) ).
  ENDMETHOD.

  METHOD perform_operation_equals.
    rv_result = COND #( WHEN determine_operation_term( 1 ) EQ determine_operation_term( 2 ) THEN 1
                        ELSE 0 ).
  ENDMETHOD.

  METHOD perform_operation_less_than.
    rv_result = COND #( WHEN determine_operation_term( 1 ) LT determine_operation_term( 2 ) THEN 1
                        ELSE 0 ).
  ENDMETHOD.

  METHOD determine_operation_term.
    rv_value = SWITCH #( mo_opcode_controller->get_parameter_mode_by_index( iv_parameter )
                        WHEN 0 THEN determine_value_from_position( iv_parameter )
                        WHEN 1 THEN determine_value( iv_parameter )
                     ).
  ENDMETHOD.

  METHOD determine_value_from_position.
    rv_value = mo_opcode_collection->get_value_by_index(
                            mo_opcode_collection->get_value_by_index( mv_index + iv_parameter ) ).
  ENDMETHOD.

  METHOD determine_value.
    rv_value = mo_opcode_collection->get_value_by_index( mv_index + iv_parameter ).
  ENDMETHOD.

  METHOD process_results.
    DATA(lv_result) = SWITCH #( result_can_be_saved( )
                                    WHEN abap_true THEN save_result_to_collection( iv_result )
                                    ELSE abap_false ).
  ENDMETHOD.

  METHOD result_can_be_saved.
    rv_result = SWITCH #( mo_opcode_controller->get_operation( )
                            WHEN lcl_opcode_contants=>mc_opcode_addition       THEN abap_true
                            WHEN lcl_opcode_contants=>mc_opcode_multiplication THEN abap_true
                            WHEN lcl_opcode_contants=>mc_opcode_input          THEN abap_true
                            WHEN lcl_opcode_contants=>mc_opcode_output         THEN abap_false
                            WHEN lcl_opcode_contants=>mc_opcode_jump_if_true   THEN abap_false
                            WHEN lcl_opcode_contants=>mc_opcode_jump_if_false  THEN abap_false
                            WHEN lcl_opcode_contants=>mc_opcode_equals         THEN abap_true
                            WHEN lcl_opcode_contants=>mc_opcode_less_than      THEN abap_true
                            ELSE abap_false ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_opcode_computer DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES lif_output.
    INTERFACES lif_input.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO lcl_opcode_computer.
    DATA mx_coll_error TYPE REF TO lcx_opcode_coll.

    DATA mv_output TYPE string.
    DATA mv_input TYPE i.

    METHODS setup.
    METHODS check_no_exception.
    METHODS set_test_double_in_out.

    METHODS perfom_complex_operation FOR TESTING.

ENDCLASS.

CLASS ltc_opcode_computer IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW lcl_opcode_computer( ).
  ENDMETHOD.

  METHOD lif_output~output_value.
    mv_output = |{ mv_output },{ iv_value }|.
  ENDMETHOD.

  METHOD lif_input~input_value.
    rv_value = mv_input.
  ENDMETHOD.

  METHOD check_no_exception.
    cl_abap_unit_assert=>assert_not_bound(
            act  = mx_coll_error
            msg  = |Es sollte keine Ausnahme aufgetreten sein.| ).
  ENDMETHOD.

  METHOD set_test_double_in_out.

    mo_cut->set_mo_output( me ).
    mo_cut->set_mo_input( me ).

  ENDMETHOD.

  METHOD perfom_complex_operation.
    mo_cut->initialize_computer_with_data( |3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,| &
                                           |1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,| &
                                           |999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99| ).
    set_test_double_in_out( ).
    mv_input = 9.

    TRY.
        mo_cut->perform_operations( ).
      CATCH lcx_opcode_coll INTO mx_coll_error.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
        msg = 'Der Output String sollte den Vergleichswert enthalten.'
        exp = |,1001 |
        act = mv_output ).
    check_no_exception( ).
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b01.
PARAMETERS p_input TYPE string.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA(lo_intcode_computer) = NEW lcl_opcode_computer( ).
  lo_intcode_computer->initialize_computer_with_data( p_input ).
  lo_intcode_computer->perform_operations( ).

  DATA(lv_result) = lo_intcode_computer->provide_operations_result( ).
  WRITE / lv_result.
