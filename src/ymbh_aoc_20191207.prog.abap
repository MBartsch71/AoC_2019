REPORT ymbh_aoc_20191207.

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

INTERFACE lif_number_generator.

  TYPES: BEGIN OF ty_num,
           number TYPE char5,
         END OF ty_num.
  TYPES tt_num_tab TYPE STANDARD TABLE OF ty_num WITH EMPTY KEY.

  DATA mt_number_tab TYPE tt_num_tab.

  "! Method generates one number out of an number range
  "! @parameter rt_num_tab | Returns a table with numbers
  METHODS generate_number
    IMPORTING
      iv_start_number   TYPE i
    RETURNING
      VALUE(rt_num_tab) TYPE tt_num_tab.

ENDINTERFACE.

INTERFACE lif_output.

  "! Method outputs a value
  "! @parameter iv_value | Value to output
  METHODS output_value
    IMPORTING
      iv_value TYPE string.

ENDINTERFACE.

INTERFACE lif_input.

  TYPES: BEGIN OF ty_input,
           number TYPE int8,
         END OF ty_input.
  TYPES tt_input TYPE STANDARD TABLE OF ty_input WITH DEFAULT KEY.
  "! Method takes an value from an input source
  "! @parameter rv_value | Input value
  METHODS input_value
    RETURNING
      VALUE(rv_value) TYPE i.

  "! Method stores a table with input values
  "! @parameter it_input | Table with input values
  METHODS set_mt_input
    IMPORTING
      it_input TYPE tt_input.

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

  METHOD lif_input~set_mt_input.
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
                                  WHEN lcl_opcode_contants=>mc_opcode_output         THEN perform_output_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_jump_if_true   THEN perform_jump_true_operation( )
                                  WHEN lcl_opcode_contants=>mc_opcode_jump_if_false  THEN perform_jump_false_operation( )
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

    DATA(lv_index) = SWITCH #( mo_opcode_controller->get_operation( )
                                WHEN lcl_opcode_contants=>mc_opcode_addition       THEN 3
                                WHEN lcl_opcode_contants=>mc_opcode_multiplication THEN 3
                                WHEN lcl_opcode_contants=>mc_opcode_input          THEN 1
                                WHEN lcl_opcode_contants=>mc_opcode_less_than      THEN 3
                                WHEN lcl_opcode_contants=>mc_opcode_equals         THEN 3 ).

    mo_opcode_collection->set_value_to_index(
            iv_index        =  mo_opcode_collection->get_value_by_index( mv_index + lv_index )
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
    INTERFACES lif_input PARTIALLY IMPLEMENTED.

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

CLASS lcl_number_generator DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_number_generator.
    ALIASES generate_number FOR lif_number_generator~generate_number.
    ALIASES mt_num_tab      FOR lif_number_generator~mt_number_tab.

  PRIVATE SECTION.

    DATA mv_start_number TYPE i.

    METHODS get_digit
      IMPORTING
        iv_number TYPE char5
        iv_offset TYPE i.

    METHODS check_unique_digits
      IMPORTING
        iv_number        TYPE char5
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS count_digit_occurences
      IMPORTING
        iv_number       TYPE char5
        iv_index        TYPE i
      RETURNING
        VALUE(rv_count) TYPE i.

    METHODS write_number_to_table
      IMPORTING
        iv_number TYPE char5.

ENDCLASS.

CLASS lcl_number_generator IMPLEMENTATION.

  METHOD generate_number.

    DATA lv_offset TYPE i.
    DATA lv_number TYPE char5.
    DATA lv_pad TYPE c.

    mv_start_number = iv_start_number.
    lv_pad = mv_start_number.
    lv_number = |{ lv_number  ALIGN = RIGHT WIDTH = 5 PAD = lv_pad  }|.
    get_digit( iv_number = lv_number iv_offset = lv_offset ).
    rt_num_tab = mt_num_tab.

  ENDMETHOD.

  METHOD get_digit.

    DATA lv_index  TYPE i.

    DATA(lv_offset) = iv_offset.
    DATA(lv_number) = iv_number.

    lv_index = mv_start_number.
    DO 5 TIMES.
      lv_number+iv_offset(1) = lv_index.
      IF iv_offset < strlen( lv_number ) - 1.
        lv_offset = iv_offset + 1.
        get_digit( iv_number = lv_number
                   iv_offset = lv_offset ).
      ENDIF.

      write_number_to_table( lv_number ).
      lv_index = lv_index + 1.
    ENDDO.
  ENDMETHOD.

  METHOD check_unique_digits.
    DATA(lv_index) = mv_start_number + 4.
    WHILE lv_index >= mv_start_number.
      rv_result = SWITCH #( count_digit_occurences( iv_number = iv_number iv_index  = lv_index )
                            WHEN 1 THEN abap_true
                            ELSE abap_false ).
      IF rv_result EQ abap_false.
        RETURN.
      ENDIF.
      lv_index = lv_index - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD count_digit_occurences.
    FIND ALL OCCURRENCES OF CONV char1( iv_index ) IN iv_number
              MATCH COUNT rv_count.
  ENDMETHOD.

  METHOD write_number_to_table.
    IF ( check_unique_digits( iv_number ) ).
      IF NOT line_exists( mt_num_tab[ number = iv_number ] ).
        mt_num_tab = VALUE #( BASE mt_num_tab ( number = iv_number ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_number_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_number_generator.

    METHODS setup.
    METHODS generate_5_digit_number_for_0 FOR TESTING.
    METHODS generate_5_digit_number_for_5 FOR TESTING.

ENDCLASS.

CLASS ltc_number_generator IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD generate_5_digit_number_for_0.

    DATA(lt_data_tab) = mo_cut->generate_number( 0 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The generated table should have 120 lines.'
        exp = 120
        act = lines( lt_data_tab ) ).
  ENDMETHOD.

  METHOD generate_5_digit_number_for_5.
    DATA(lt_data_tab) = mo_cut->generate_number( 5 ).
    cl_abap_unit_assert=>assert_equals(
        msg = 'The generated table should have 120 lines.'
        exp = 120
        act = lines( lt_data_tab ) ).

    data(ls_data_tab) = value lif_number_generator=>ty_num( number = 56879 ).
    cl_abap_unit_assert=>assert_table_contains(
        line  = ls_data_tab
        table = lt_data_tab ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_amplifier_input DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_input.
    ALIASES set_mt_input FOR lif_input~set_mt_input.

    TYPES ty_input TYPE TABLE OF lif_number_generator=>tt_num_tab WITH DEFAULT KEY.

  PRIVATE SECTION.

    DATA mt_input TYPE lif_input=>tt_input.
    DATA mv_counter TYPE i VALUE 1.

ENDCLASS.

CLASS lcl_amplifier_input IMPLEMENTATION.

  METHOD lif_input~input_value.
    rv_value = mt_input[ mv_counter ]-number.
    mv_counter = mv_counter + 1.
  ENDMETHOD.

  METHOD lif_input~set_mt_input.
    mt_input = it_input.
    mv_counter = 1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_amplifier_circuit DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_output.

    "! Constructor method
    METHODS constructor.

    "! Method set the actual sequence number
    "! @parameter iv_sequence | Input sequence number
    METHODS set_mv_sequence
      IMPORTING
        iv_sequence TYPE string.

    "! Method start the amplifier circuit run
    "! @parameter rv_output | Actual output from one full run
    METHODS start_amplifier_circuit
      RETURNING
        VALUE(rv_output) TYPE i
      RAISING
        lcx_opcode_coll.

    "! Method provides the number generator to use
    "! @parameter iv_mo_number_generator | Number generator object
    METHODS set_mo_number_generator
      IMPORTING
        iv_mo_number_generator TYPE REF TO lif_number_generator.

  PRIVATE SECTION.

    DATA mo_intcode_input    TYPE REF TO lif_input.
    DATA mo_number_generator TYPE REF TO lif_number_generator.
    DATA mv_sequence         TYPE string.
    DATA mv_output           TYPE string.
    DATA mv_thruster_signal  TYPE i.

    METHODS prepare_run.

    METHODS determine_input_from_prev_run
      RETURNING
        VALUE(rv_input) TYPE i.

    METHODS increase_offset
      IMPORTING
        iv_offset        TYPE i
      RETURNING
        VALUE(rv_offset) TYPE i.

    METHODS rework_of_sequence_run
      RETURNING
        VALUE(rv_offset) TYPE i.
    METHODS output_result_to_thruster.
    METHODS reset_run_variables
      RETURNING
        VALUE(rv_offset) TYPE i.
    METHODS setup_intcode_computer
      IMPORTING
        io_intcode_computer TYPE REF TO lcl_opcode_computer.

ENDCLASS.

CLASS lcl_amplifier_circuit IMPLEMENTATION.

  METHOD constructor.
    mo_number_generator = NEW lcl_number_generator( ).
  ENDMETHOD.

  METHOD set_mo_number_generator.
    me->mo_number_generator = iv_mo_number_generator.
  ENDMETHOD.

  METHOD set_mv_sequence.
    mv_sequence = iv_sequence.
  ENDMETHOD.

  METHOD lif_output~output_value.
    mv_output = iv_value.
  ENDMETHOD.

  METHOD prepare_run.
    mo_intcode_input = NEW lcl_amplifier_input( ).
    mo_number_generator->generate_number( 0 ).
  ENDMETHOD.

  METHOD start_amplifier_circuit.

    DATA lv_offset TYPE i.
    DATA lv_input TYPE i.

    prepare_run( ).
    LOOP AT mo_number_generator->mt_number_tab INTO DATA(ls_number).

      DO 5 TIMES.
        lv_input = determine_input_from_prev_run( ).
        mo_intcode_input->set_mt_input( VALUE #( ( number = ls_number-number+lv_offset(1) )
                                                 ( number = lv_input ) ) ).
        DATA(lo_intcode_computer) = NEW lcl_opcode_computer( ).
        setup_intcode_computer( lo_intcode_computer ).
        lo_intcode_computer->perform_operations( ).
        lv_offset = increase_offset( lv_offset ).
      ENDDO.
      lv_offset = rework_of_sequence_run( ).

    ENDLOOP.
    rv_output = mv_thruster_signal.
  ENDMETHOD.

  METHOD setup_intcode_computer.
    io_intcode_computer->set_mo_input( mo_intcode_input ).
    io_intcode_computer->set_mo_output( me ).
    io_intcode_computer->initialize_computer_with_data( mv_sequence ).
  ENDMETHOD.

  METHOD rework_of_sequence_run.
    output_result_to_thruster( ).
    rv_offset = reset_run_variables( ).
  ENDMETHOD.

  METHOD reset_run_variables.
    rv_offset = 0.
    mv_output = 0.
  ENDMETHOD.

  METHOD output_result_to_thruster.
    IF mv_output GT mv_thruster_signal.
      mv_thruster_signal = CONV i( mv_output ).
    ENDIF.
  ENDMETHOD.

  METHOD increase_offset.
    rv_offset = iv_offset + 1.
  ENDMETHOD.

  METHOD determine_input_from_prev_run.
    rv_input = SWITCH i( mv_output WHEN '' THEN 0
                                           ELSE mv_output ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_amplifier_circuit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    INTERFACES lif_number_generator.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_amplifier_circuit.

    DATA mv_thruster_signal TYPE i.
    DATA mx_intcode_error   TYPE REF TO lcx_opcode_coll.
    DATA mt_number_tab      TYPE lif_number_generator=>tt_num_tab.

    METHODS setup.
    METHODS check_no_exception.

    METHODS run_amplifier_circuit_001 FOR TESTING.
    METHODS run_amplifier_circuit_002 FOR TESTING.
    METHODS run_amplifier_circuit_003 FOR TESTING.
ENDCLASS.

CLASS ltc_amplifier_circuit IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
    mo_cut->set_mo_number_generator( me ).
  ENDMETHOD.

  METHOD lif_number_generator~generate_number.
    lif_number_generator~mt_number_tab = mt_number_tab.
  ENDMETHOD.

  METHOD check_no_exception.
    cl_abap_unit_assert=>assert_not_bound(
        act = mx_intcode_error
        msg = |It should not occur an exception.| ).
  ENDMETHOD.

  METHOD run_amplifier_circuit_001.
    mo_cut->set_mv_sequence( |3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0| ).
    mt_number_tab = VALUE #( ( number = |43210| ) ).
    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = 'The max thruster signal should be 43210.'
            exp = 43210
            act = mo_cut->start_amplifier_circuit( ) ).
      CATCH lcx_opcode_coll INTO mx_intcode_error.
    ENDTRY.
    check_no_exception( ).
  ENDMETHOD.

  METHOD run_amplifier_circuit_002.
    mo_cut->set_mv_sequence( |3,23,3,24,1002,24,10,24,1002,23,-1,23,| &
                             |101,5,23,23,1,24,23,23,4,23,99,0,0| ).
    mt_number_tab = VALUE #( ( number = |01234| ) ).
    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = 'The max thruster signal should be 54321.'
            exp = 54321
            act = mo_cut->start_amplifier_circuit( ) ).
      CATCH lcx_opcode_coll INTO mx_intcode_error.
    ENDTRY.
    check_no_exception( ).
  ENDMETHOD.

  METHOD run_amplifier_circuit_003.
    mo_cut->set_mv_sequence( |3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,| &
                             |1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0| ).
    mt_number_tab = VALUE #( ( number = |10432| ) ).
    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = 'The max thruster signal should be 65210.'
            exp = 65210
            act = mo_cut->start_amplifier_circuit( ) ).
      CATCH lcx_opcode_coll INTO mx_intcode_error.
    ENDTRY.
    check_no_exception( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_amplifier_runfeedback DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES lif_number_generator.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_amplifier_circuit.

    DATA mv_thruster_signal TYPE i.
    DATA mx_intcode_error   TYPE REF TO lcx_opcode_coll.
    DATA mt_number_tab      TYPE lif_number_generator=>tt_num_tab.

    METHODS setup.
    METHODS check_no_exception.

    METHODS run_amplifier_feedback_001 FOR TESTING.
ENDCLASS.

CLASS ltc_amplifier_runfeedback IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
    mo_cut->set_mo_number_generator( me ).
  ENDMETHOD.

  METHOD check_no_exception.
    cl_abap_unit_assert=>assert_not_bound(
        act = mx_intcode_error
        msg = |It should not occur an exception.| ).
  ENDMETHOD.

  METHOD lif_number_generator~generate_number.
    lif_number_generator~mt_number_tab = mt_number_tab.
  ENDMETHOD.

  METHOD run_amplifier_feedback_001.
    mo_cut->set_mv_sequence( |3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,| &
                             |27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5| ).
    mt_number_tab = VALUE #( ( number = |98765| ) ).
    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = 'The max thruster signal should be 139629729.'
            exp = 139629729
            act = mo_cut->start_amplifier_circuit( ) ).
      CATCH lcx_opcode_coll INTO mx_intcode_error.
    ENDTRY.
    check_no_exception( ).

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b01.
PARAMETERS p_input TYPE string.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA(lo_amplifier) = NEW lcl_amplifier_circuit( ).
  lo_amplifier->set_mv_sequence( p_input ).
  DATA(mv_thruster_signal) = lo_amplifier->start_amplifier_circuit( ).

  WRITE / mv_thruster_signal.
