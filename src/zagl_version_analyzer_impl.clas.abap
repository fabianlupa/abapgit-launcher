CLASS zagl_version_analyzer_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zagl_version_analyzer.

    METHODS constructor IMPORTING source_reader TYPE REF TO zagl_source_reader.

  PRIVATE SECTION.
    CONSTANTS abapgit_version_constant     TYPE string VALUE 'ZIF_ABAPGIT_VERSION=>C_ABAP_VERSION'.
    CONSTANTS abapmerge_timestamp_constant TYPE string VALUE 'LIF_ABAPMERGE_MARKER=>C_MERGE_TIMESTAMP'.
    CONSTANTS abapmerge_marker_pattern     TYPE string VALUE '*lif_abapmerge_marker*'.

    DATA source_reader TYPE REF TO zagl_source_reader.

    METHODS get_version_by_identifier IMPORTING absolute_identifier TYPE csequence
                                      RETURNING VALUE(result)       TYPE string
                                      RAISING   zagl_version_analyzer_error.

    METHODS get_data_obj_value_by_name IMPORTING absolute_identifier TYPE csequence
                                       CHANGING  !value              TYPE data
                                       RAISING   zagl_version_analyzer_error.
ENDCLASS.


CLASS zagl_version_analyzer_impl IMPLEMENTATION.
  METHOD constructor.
    me->source_reader = source_reader.
  ENDMETHOD.

  METHOD zagl_version_analyzer~get_dev_version_version.
    result = get_version_by_identifier( abapgit_version_constant ).
  ENDMETHOD.

  METHOD zagl_version_analyzer~get_sa_version_version.
    result = get_version_by_identifier( |\\PROGRAM={ program_name }\\INTERFACE={ abapgit_version_constant }| ).
  ENDMETHOD.

  METHOD get_version_by_identifier.
    get_data_obj_value_by_name( EXPORTING absolute_identifier = absolute_identifier
                                CHANGING  value               = result ).
  ENDMETHOD.

  METHOD get_data_obj_value_by_name.
    FIELD-SYMBOLS <data_object> TYPE data.

    TRY.
        ASSIGN (absolute_identifier) TO <data_object>.
        IF sy-subrc = 0.
          value = EXACT #( <data_object> ).
        ELSE.
          RAISE EXCEPTION TYPE zagl_version_analyzer_error.
        ENDIF.
      CATCH cx_sy_conversion_error INTO DATA(conversion_error).
        RAISE EXCEPTION TYPE zagl_version_analyzer_error
          EXPORTING previous = conversion_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zagl_version_analyzer~get_abapmerge_timestamp.
    DO 2 TIMES.
      DATA(current_try) = sy-index.

      CASE current_try.
        WHEN 1.
          " First try finding constants in the marker interface
          TRY.
              " Force load the program
              PERFORM dummy IN PROGRAM (program_name) IF FOUND.

              get_data_obj_value_by_name(
                EXPORTING absolute_identifier = |\\PROGRAM={ program_name }\\INTERFACE={ abapmerge_timestamp_constant }|
                CHANGING  value               = result ).

            CATCH cx_sy_dyn_call_illegal_form
                  cx_sy_program_not_found
                  zagl_version_analyzer_error.
              CLEAR result.
          ENDTRY.

        WHEN 2.
          " Fallback to READ REPORT which is considerably slower
          TRY.
              DATA(source) = source_reader->read_report( program_name ).
              DATA(start_position) = nmax( val1 = lines( source ) - 5 val2 = 1 ).

              LOOP AT source ASSIGNING FIELD-SYMBOL(<line>) FROM start_position WHERE table_line IS NOT INITIAL.
                DATA(line_number) = sy-tabix.

                IF to_lower( <line> ) CP abapmerge_marker_pattern.
                  result = source[ line_number + 1 ].
                  SPLIT result AT '-' INTO DATA(dummy) result ##NEEDED.
                  CONDENSE result.
                  EXIT.
                ENDIF.
              ENDLOOP.

            CATCH zagl_source_not_available INTO DATA(source_not_available).
              RAISE EXCEPTION TYPE zagl_version_analyzer_error
                EXPORTING previous = source_not_available.
          ENDTRY.
      ENDCASE.

      IF result IS NOT INITIAL AND result CP '++++-++-++T*'.
        RETURN.
      ELSEIF current_try >= 2.
        RAISE EXCEPTION TYPE zagl_version_analyzer_error.
      ENDIF.

      CLEAR result.
    ENDDO.
  ENDMETHOD.

  METHOD zagl_version_analyzer~get_dev_version_admin_data.
    DATA repo_srv TYPE REF TO object.
    DATA repo_ref TYPE REF TO data.
    DATA repo     TYPE REF TO object.
    FIELD-SYMBOLS <repo> TYPE data.

    TRY.
        CREATE DATA repo_ref TYPE REF TO ('ZIF_ABAPGIT_REPO').
        ASSIGN repo_ref->* TO <repo>.

        SELECT SINGLE devclass
          FROM tadir
          WHERE object = 'PROG'
            AND obj_name = @zagl_common_types=>abapgit_dev_version_prog_name
          INTO @DATA(package).
        IF sy-subrc <> 0 OR package IS INITIAL.
          RAISE EXCEPTION TYPE zagl_version_analyzer_error.
        ENDIF.

        CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>get_instance
          RECEIVING ri_srv = repo_srv.
        CALL METHOD repo_srv->('ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE')
          EXPORTING iv_package = package
          IMPORTING ei_repo    = <repo>.

        repo = <repo>.
        ASSIGN repo->('ZIF_ABAPGIT_REPO~MS_DATA') TO FIELD-SYMBOL(<repo_data>).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zagl_version_analyzer_error.
        ENDIF.

        result = CORRESPONDING #( <repo_data> ).
      CATCH cx_root INTO DATA(exception).
        RAISE EXCEPTION TYPE zagl_version_analyzer_error
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
