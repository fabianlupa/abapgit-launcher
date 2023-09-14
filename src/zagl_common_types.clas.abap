CLASS zagl_common_types DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES abapgit_version     TYPE string.
    TYPES abapmerge_timestamp TYPE string.
    TYPES program_name_filter TYPE RANGE OF progname.

    CONSTANTS default_dev_version_prog_name TYPE progname VALUE 'ZABAPGIT'.
    CONSTANTS default_sa_vers_prog_name     TYPE progname VALUE 'ZABAPGIT_STANDALONE'.
    CONSTANTS old_def_sa_vers_prog_name     TYPE progname VALUE 'ZABAPGIT_FULL'.

    CLASS-DATA default_program_name_filter TYPE program_name_filter READ-ONLY.

    CLASS-METHODS class_constructor.
ENDCLASS.


CLASS zagl_common_types IMPLEMENTATION.
  METHOD class_constructor.
    default_program_name_filter = VALUE #( sign = 'I'
                                           ( option = 'EQ' low = default_dev_version_prog_name )
                                           ( option = 'CP' low = default_sa_vers_prog_name && '*' )
                                           ( option = 'CP' low = old_def_sa_vers_prog_name && '*' ) ).
  ENDMETHOD.
ENDCLASS.
