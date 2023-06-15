CLASS time_date_util_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS abapgit_valid_timestamp      FOR TESTING RAISING cx_static_check.
    METHODS abapgit_invalid_timestamp    FOR TESTING RAISING cx_static_check.
    METHODS abapgit_invalid_timestamp2   FOR TESTING RAISING cx_static_check.
    METHODS abapmerge_valid_timestamp    FOR TESTING RAISING cx_static_check.
    METHODS abapmerge_invalid_timestamp  FOR TESTING RAISING cx_static_check.
    METHODS abapmerge_invalid_timestamp2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS time_date_util_test IMPLEMENTATION.
  METHOD abapgit_valid_timestamp.
    DATA timestamp TYPE timestampl VALUE '20230611100341.6641780'.

    cl_abap_unit_assert=>assert_equals( exp = '20230611'
                                        act = time_date_util=>parse_date_from_abapgit_ts( timestamp ) ).
  ENDMETHOD.

  METHOD abapgit_invalid_timestamp.
    DATA timestamp TYPE timestampl VALUE '20231311100341.6641780'.

    TRY.
        time_date_util=>parse_date_from_abapgit_ts( timestamp ).
        cl_abap_unit_assert=>fail( ).
      CATCH time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapgit_invalid_timestamp2.
    DATA timestamp TYPE timestampl VALUE IS INITIAL.

    TRY.
        time_date_util=>parse_date_from_abapgit_ts( timestamp ).
        cl_abap_unit_assert=>fail( ).
      CATCH time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_valid_timestamp.
    TRY.
        cl_abap_unit_assert=>assert_equals(
            exp = '20230514'
            act = time_date_util=>parse_date_from_abapmerge_ts( '2023-05-14T07:07:11.781Z' ) ).
      CATCH time_date_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_invalid_timestamp.
    TRY.
        time_date_util=>parse_date_from_abapmerge_ts( 'First of April 2023' ).
        cl_abap_unit_assert=>fail( ).
      CATCH time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_invalid_timestamp2.
    TRY.
        time_date_util=>parse_date_from_abapmerge_ts( `` ).
        cl_abap_unit_assert=>fail( ).
      CATCH time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS source_reader_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES source_reader.

    DATA last_call_program_name TYPE progname READ-ONLY.

    METHODS return_source   IMPORTING !source    TYPE string_table.
    METHODS raise_exception IMPORTING !exception TYPE REF TO source_not_available.

  PRIVATE SECTION.
    DATA next_raise_exception TYPE REF TO source_not_available.
    DATA next_return_source   TYPE string_table.
ENDCLASS.


CLASS source_reader_double IMPLEMENTATION.
  METHOD source_reader~read_report.
    last_call_program_name = program_name.

    IF next_return_source IS NOT INITIAL.
      result = next_return_source.
      CLEAR next_return_source.
    ELSEIF next_raise_exception IS BOUND.
      DATA(exception) = next_raise_exception.
      CLEAR next_raise_exception.
      RAISE EXCEPTION exception.
    ELSE.
      cl_abap_unit_assert=>fail( 'Unexpected method call or missing test setup' ).
    ENDIF.
  ENDMETHOD.

  METHOD raise_exception.
    next_raise_exception = exception.
    CLEAR next_return_source.
  ENDMETHOD.

  METHOD return_source.
    next_return_source = source.
    FREE next_raise_exception.
  ENDMETHOD.
ENDCLASS.


CLASS version_analyzer_timestmp_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CONSTANTS dummy_program_name TYPE progname VALUE 'ZAGLAUNCHER_DUMMY'.

    DATA cut                  TYPE REF TO version_analyzer.
    DATA source_reader_double TYPE REF TO source_reader_double.

    DATA: BEGIN OF given,
            program_name TYPE progname,
            source       TYPE string_table,
          END OF given.
    DATA: BEGIN OF result,
            timestamp TYPE common_types=>abapmerge_timestamp,
            exception TYPE REF TO version_analyzer_error,
          END OF result.

    METHODS valid_timestamp      FOR TESTING RAISING cx_static_check.
    METHODS missing_timestamp    FOR TESTING RAISING cx_static_check.
    METHODS invalid_timestamp    FOR TESTING RAISING cx_static_check.
    METHODS source_not_available FOR TESTING RAISING cx_static_check.
    METHODS no_interface         FOR TESTING RAISING cx_static_check.
    METHODS no_comment           FOR TESTING RAISING cx_static_check.

    METHODS given_the_source           IMPORTING !source    TYPE string_table.
    METHODS given_missing_source       IMPORTING !exception TYPE REF TO source_not_available.
    METHODS given_the_progam_name      IMPORTING !program   TYPE progname.

    METHODS when_parse_requested.

    METHODS then_requ_progam_should_be IMPORTING !program   TYPE progname.
    METHODS then_timestamp_should_be   IMPORTING !timestamp TYPE common_types=>abapmerge_timestamp.
    METHODS then_should_throw          IMPORTING !previous  TYPE REF TO cx_root OPTIONAL.

    METHODS setup.
    METHODS teardown.
ENDCLASS.


CLASS version_analyzer_timestmp_test IMPLEMENTATION.
  METHOD valid_timestamp.
    given_the_progam_name( dummy_program_name ).
    given_the_source( VALUE #( ( |  " --------------------------------------------------- | )
                               ( |  INTERFACE lif_abapmerge_marker. | )
                               ( |    " abapmerge 0.15.0 - 2023-05-14T07:07:11.781Z | )
                               ( |  ENDINTERFACE. | )
                               ( |  " --------------------------------------------------- | ) ) ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_timestamp_should_be( |2023-05-14T07:07:11.781Z| ).
  ENDMETHOD.

  METHOD missing_timestamp.
    DATA no_previous_exeption TYPE REF TO cx_root.

    given_the_progam_name( dummy_program_name ).
    given_the_source( VALUE #( ( |  " --------------------------------------------------- | )
                               ( |  INTERFACE lif_abapmerge_marker. | )
                               ( |    " abapmerge 0.15.0 | )
                               ( |  ENDINTERFACE. | )
                               ( |  " --------------------------------------------------- | ) ) ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_should_throw( no_previous_exeption ).
  ENDMETHOD.

  METHOD invalid_timestamp.
    DATA no_previous_exeption TYPE REF TO cx_root.

    given_the_progam_name( dummy_program_name ).
    given_the_source( VALUE #( ( |  " --------------------------------------------------- | )
                               ( |  INTERFACE lif_abapmerge_marker. | )
                               ( |    " abapmerge 0.15.0 - 14th of March 2023 at seven am my timezone | )
                               ( |  ENDINTERFACE. | )
                               ( |  " --------------------------------------------------- | ) ) ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_should_throw( no_previous_exeption ).
  ENDMETHOD.

  METHOD source_not_available.
    given_the_progam_name( dummy_program_name ).
    DATA(exception) = NEW source_not_available( ).
    given_missing_source( exception ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_should_throw( exception ).
  ENDMETHOD.

  METHOD no_interface.
    DATA no_previous_exeption TYPE REF TO cx_root.

    given_the_progam_name( dummy_program_name ).
    given_the_source( VALUE #( ( | " ---------------------------------------------------------------------| )
                               ( | INITIALIZATION.| )
                               ( |   PERFORM adjust_toolbar USING '1001'.| )
                               ( |   lcl_password_dialog=>on_screen_init( ).| )
                               ( | | )
                               ( | START-OF-SELECTION.| )
                               ( |   PERFORM run.| )
                               ( | | )
                               ( |   " Hide Execute button from screen| )
                               ( | AT SELECTION-SCREEN OUTPUT.| )
                               ( |   IF sy-dynnr = lcl_password_dialog=>c_dynnr.| )
                               ( |     lcl_password_dialog=>on_screen_output( ).| )
                               ( |   ELSE.| )
                               ( |     PERFORM output.| )
                               ( |   ENDIF.| )
                               ( | | )
                               ( |   " SAP back command re-direction| )
                               ( | AT SELECTION-SCREEN ON EXIT-COMMAND.| )
                               ( |   PERFORM exit.| )
                               ( | | )
                               ( | AT SELECTION-SCREEN.| )
                               ( |   IF sy-dynnr = lcl_password_dialog=>c_dynnr.| )
                               ( |     lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).| )
                               ( |   ENDIF.| ) ) ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_should_throw( no_previous_exeption ).
  ENDMETHOD.

  METHOD no_comment.
    DATA no_previous_exeption TYPE REF TO cx_root.

    given_the_progam_name( dummy_program_name ).
    given_the_source( VALUE #( ( |  " --------------------------------------------------- | )
                               ( |  INTERFACE lif_abapmerge_marker. | )
                               ( |  ENDINTERFACE. | )
                               ( |  " --------------------------------------------------- | ) ) ).

    when_parse_requested( ).

    then_requ_progam_should_be( dummy_program_name ).
    then_should_throw( no_previous_exeption ).
  ENDMETHOD.

  METHOD given_the_progam_name.
    given-program_name = program.
  ENDMETHOD.

  METHOD given_missing_source.
    CLEAR given-source.
    source_reader_double->raise_exception( exception ).
  ENDMETHOD.

  METHOD given_the_source.
    given-source = source.
    source_reader_double->return_source( given-source ).
  ENDMETHOD.

  METHOD when_parse_requested.
    TRY.
        result-timestamp = cut->get_abapmerge_timestamp( given-program_name ).
      CATCH version_analyzer_error INTO result-exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD then_requ_progam_should_be.
    cl_abap_unit_assert=>assert_equals( exp = program act = source_reader_double->last_call_program_name ).
  ENDMETHOD.

  METHOD then_timestamp_should_be.
    cl_abap_unit_assert=>assert_equals( exp = timestamp act = result-timestamp ).
    cl_abap_unit_assert=>assert_not_bound( result-exception ).
  ENDMETHOD.

  METHOD then_should_throw.
    cl_abap_unit_assert=>assert_bound( result-exception ).
    cl_abap_unit_assert=>assert_initial( result-timestamp ).
    IF previous IS BOUND.
      cl_abap_unit_assert=>assert_equals( exp = previous act = result-exception->previous ).
    ELSEIF previous IS SUPPLIED.
      cl_abap_unit_assert=>assert_not_bound( act = result-exception->previous ).
    ENDIF.
  ENDMETHOD.

  METHOD setup.
    source_reader_double = NEW #( ).
    cut = NEW version_analyzer_impl( source_reader_double ).
  ENDMETHOD.

  METHOD teardown.
    FREE cut.
    FREE source_reader_double.

    CLEAR given.
    CLEAR result.
  ENDMETHOD.
ENDCLASS.
