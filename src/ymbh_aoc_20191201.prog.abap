*&---------------------------------------------------------------------*
*& Report ymbh_aoc_20191201
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymbh_aoc_20191201.

CLASS lcl_fuel_calculator DEFINITION.

  PUBLIC SECTION.

    TYPES tt_fuel_table TYPE RANGE OF i.

    "! Method calculates the necessary fuel based on a mass table
    "! @parameter it_fuel_table | Table with masses of several modules of the spaceship
    "! @parameter rv_result | Result is the necessary amount of fuel respecting the mass of the fuel
    METHODS calculate_fuel_from_table
      IMPORTING
        it_fuel_table    TYPE tt_fuel_table
      RETURNING
        VALUE(rv_result) TYPE i.

  PRIVATE SECTION.

    CONSTANTS mc_mass_divisor    TYPE i VALUE 3.
    CONSTANTS mc_mass_subtractor TYPE i VALUE 2.
    CONSTANTS mc_fuel_minimum    TYPE i VALUE 0.

    METHODS calculate_fuel_required
      IMPORTING
        iv_mass        TYPE i
      RETURNING
        VALUE(rv_fuel) TYPE i.

    METHODS calculate_fuel_for_fuel
      IMPORTING
        iv_fuel        TYPE i
      RETURNING
        VALUE(rv_fuel) TYPE i.

    METHODS convert_mass_in_fuel
      IMPORTING iv_mass        TYPE i
      RETURNING
                VALUE(rv_fuel) TYPE i.

ENDCLASS.

CLASS lcl_fuel_calculator IMPLEMENTATION.

  METHOD calculate_fuel_from_table.
    rv_result = REDUCE i( INIT x = mc_fuel_minimum
                          FOR ls_fuel IN it_fuel_table
                          NEXT x = x + calculate_fuel_for_fuel( ls_fuel-low ) ).
  ENDMETHOD.

  METHOD calculate_fuel_for_fuel.
    IF calculate_fuel_required( iv_fuel ) > mc_fuel_minimum.
      rv_fuel = calculate_fuel_for_fuel( calculate_fuel_required( iv_fuel ) ).
    ENDIF.
    rv_fuel = rv_fuel + calculate_fuel_required( iv_fuel ).
  ENDMETHOD.

  METHOD calculate_fuel_required.
    rv_fuel = COND #( WHEN convert_mass_in_fuel( iv_mass ) < mc_fuel_minimum THEN mc_fuel_minimum
                      ELSE convert_mass_in_fuel( iv_mass ) ).
  ENDMETHOD.

  METHOD convert_mass_in_fuel.
    rv_fuel = ( iv_mass DIV mc_mass_divisor ) - mc_mass_subtractor.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_aoc1 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_cut TYPE REF TO lcl_fuel_calculator.

    METHODS setup.
    METHODS calculate_fuel_for_table FOR TESTING.

ENDCLASS.


CLASS ltcl_aoc1 IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD calculate_fuel_for_table.
    DATA(lt_fuel_range) = VALUE mo_cut->tt_fuel_table( ( sign = 'I' option = 'EQ' low = 100756 )
                                                       ( sign = 'I' option = 'EQ' low = 1969 ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = 'Das Ergebnis sollte 51312 sein.'
        exp = 51312
        act = mo_cut->calculate_fuel_from_table( lt_fuel_range ) ).
  ENDMETHOD.

ENDCLASS.

DATA mv_fuel TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b01.

SELECT-OPTIONS so_fuel FOR mv_fuel.

SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  DATA(lo_fuel_calc) = NEW lcl_fuel_calculator( ).
  DATA(lv_sum) = lo_fuel_calc->calculate_fuel_from_table( so_fuel[] ).

  WRITE |The result is { lv_sum } |.
