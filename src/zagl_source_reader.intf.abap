"! <p class="shorttext synchronized">Source Reader</p>
INTERFACE zagl_source_reader PUBLIC.
  METHODS read_report IMPORTING program_name  TYPE progname
                      RETURNING VALUE(result) TYPE string_table
                      RAISING   zagl_source_not_available.
ENDINTERFACE.
