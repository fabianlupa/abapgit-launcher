*"* use this source file for your ABAP unit test classes

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
                                        act = zagl_time_date_util=>parse_date_from_abapgit_ts( timestamp ) ).
  ENDMETHOD.

  METHOD abapgit_invalid_timestamp.
    DATA timestamp TYPE timestampl VALUE '20231311100341.6641780'.

    TRY.
        zagl_time_date_util=>parse_date_from_abapgit_ts( timestamp ).
        cl_abap_unit_assert=>fail( `Invalid timestamp should be rejected` ).
      CATCH zagl_time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapgit_invalid_timestamp2.
    DATA timestamp TYPE timestampl VALUE IS INITIAL.

    TRY.
        zagl_time_date_util=>parse_date_from_abapgit_ts( timestamp ).
        cl_abap_unit_assert=>fail( msg = `Initial timestamp should be rejected` ).
      CATCH zagl_time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_valid_timestamp.
    TRY.
        cl_abap_unit_assert=>assert_equals(
            msg = `Valid timestamp should be parsed`
            exp = '20230514'
            act = zagl_time_date_util=>parse_date_from_abapmerge_ts( '2023-05-14T07:07:11.781Z' ) ).
      CATCH zagl_time_date_error.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_invalid_timestamp.
    TRY.
        zagl_time_date_util=>parse_date_from_abapmerge_ts( 'First of April 2023' ).
        cl_abap_unit_assert=>fail( msg = `Wrong format should be rejected` ).
      CATCH zagl_time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD abapmerge_invalid_timestamp2.
    TRY.
        zagl_time_date_util=>parse_date_from_abapmerge_ts( `` ).
        cl_abap_unit_assert=>fail( `Empty timestamp should be rejected` ).
      CATCH zagl_time_date_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
