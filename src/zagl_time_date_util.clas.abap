"! <p class="shorttext synchronized">Time Date Utilities</p>
CLASS zagl_time_date_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Parse date from an abapmerge timestamp string
    "!
    "! @parameter timestamp            | abapmerge timestamp
    "! @parameter result               | Parsed date
    "! @raising   zagl_time_date_error | Error parsing timestamp
    CLASS-METHODS parse_date_from_abapmerge_ts IMPORTING !timestamp    TYPE string
                                               RETURNING VALUE(result) TYPE d
                                               RAISING   zagl_time_date_error.

    "! Parse date from an abapGit timestamp
    "!
    "! @parameter timestamp            | abapGit timestamp
    "! @parameter result               | Parsed date
    "! @raising   zagl_time_date_error | Error parsing timestamp
    CLASS-METHODS parse_date_from_abapgit_ts IMPORTING !timestamp    TYPE timestampl
                                             RETURNING VALUE(result) TYPE d
                                             RAISING   zagl_time_date_error.
ENDCLASS.


CLASS zagl_time_date_util IMPLEMENTATION.
  METHOD parse_date_from_abapmerge_ts.
    IF strlen( timestamp ) < 10.
      RAISE EXCEPTION TYPE zagl_time_date_error.
    ENDIF.
    result = timestamp(4) && timestamp+5(2) && timestamp+8(2).
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING  date                      = result
      EXCEPTIONS plausibility_check_failed = 1
                 OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zagl_time_date_error.
    ENDIF.
  ENDMETHOD.

  METHOD parse_date_from_abapgit_ts.
    TRY.
        cl_abap_tstmp=>systemtstmp_utc2syst( EXPORTING utc_tstmp = cl_abap_tstmp=>move_to_short( timestamp )
                                             IMPORTING syst_date = result ).
      CATCH cx_parameter_invalid_range cx_parameter_invalid_type INTO DATA(exception).
        RAISE EXCEPTION TYPE zagl_time_date_error
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
