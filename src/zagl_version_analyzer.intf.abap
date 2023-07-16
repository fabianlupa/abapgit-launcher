"! <p class="shorttext synchronized">Version Analyzer</p>
INTERFACE zagl_version_analyzer PUBLIC.
  TYPES: BEGIN OF dev_edition_admin_data,
           deserialized_by TYPE syuname,
           deserialized_at TYPE timestampl,
           created_by      TYPE syuname,
           created_at      TYPE timestampl,
         END OF dev_edition_admin_data.

  METHODS get_dev_version_version RETURNING VALUE(result) TYPE zagl_common_types=>abapgit_version
                                  RAISING   zagl_version_analyzer_error.

  METHODS get_dev_version_admin_data RETURNING VALUE(result) TYPE dev_edition_admin_data
                                     RAISING   zagl_version_analyzer_error.

  METHODS get_sa_version_version IMPORTING program_name  TYPE progname
                                 RETURNING VALUE(result) TYPE zagl_common_types=>abapgit_version
                                 RAISING   zagl_version_analyzer_error.

  METHODS get_abapmerge_timestamp IMPORTING program_name  TYPE progname
                                  RETURNING VALUE(result) TYPE zagl_common_types=>abapmerge_timestamp
                                  RAISING   zagl_version_analyzer_error.
ENDINTERFACE.
