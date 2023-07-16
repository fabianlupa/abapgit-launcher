CLASS zagl_common_types DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES abapgit_version     TYPE string.
    TYPES abapmerge_timestamp TYPE string.

    CONSTANTS abapgit_dev_version_prog_name TYPE progname VALUE 'ZABAPGIT'.
    CONSTANTS abapgit_sa_vers_prog_pattern  TYPE progname VALUE 'ZABAPGIT_STANDALONE%'.
ENDCLASS.


CLASS zagl_common_types IMPLEMENTATION.
ENDCLASS.
