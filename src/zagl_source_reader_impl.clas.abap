CLASS zagl_source_reader_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zagl_source_reader.
ENDCLASS.


CLASS zagl_source_reader_impl IMPLEMENTATION.
  METHOD zagl_source_reader~read_report.
    READ REPORT program_name
         STATE 'A'
         INTO result.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zagl_source_not_available.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
