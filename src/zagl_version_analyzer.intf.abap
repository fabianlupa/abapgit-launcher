"! <p class="shorttext synchronized">Version Analyzer</p>
INTERFACE zagl_version_analyzer PUBLIC.
  TYPES flavor TYPE c LENGTH 1.

  CONSTANTS: BEGIN OF flavors,
               standalone_version TYPE flavor VALUE 'S',
               developer_version  TYPE flavor VALUE 'D',
             END OF flavors.

  TYPES: BEGIN OF developer_version_admin_data,
           deserialized_by TYPE syuname,
           deserialized_at TYPE timestampl,
           created_by      TYPE syuname,
           created_at      TYPE timestampl,
         END OF developer_version_admin_data.

  TYPES: BEGIN OF analysis_result,
           version                   TYPE zagl_common_types=>abapgit_version,
           flavor                    TYPE flavor,
           BEGIN OF standalone_version_details,
             abapmerge_timestamp TYPE zagl_common_types=>abapmerge_timestamp,
           END OF standalone_version_details,
           developer_version_details TYPE developer_version_admin_data,
         END OF analysis_result.

  METHODS analyze_program IMPORTING program_name  TYPE progname
                          RETURNING VALUE(result) TYPE analysis_result
                          RAISING   zagl_version_analyzer_error.

  METHODS get_dev_version_version RETURNING VALUE(result) TYPE zagl_common_types=>abapgit_version
                                  RAISING   zagl_version_analyzer_error.

  METHODS get_dev_version_admin_data IMPORTING program_name  TYPE progname DEFAULT zagl_common_types=>default_dev_version_prog_name
                                     RETURNING VALUE(result) TYPE developer_version_admin_data
                                     RAISING   zagl_version_analyzer_error.

  METHODS get_sa_version_version IMPORTING program_name  TYPE progname
                                 RETURNING VALUE(result) TYPE zagl_common_types=>abapgit_version
                                 RAISING   zagl_version_analyzer_error.

  METHODS get_abapmerge_timestamp IMPORTING program_name  TYPE progname
                                  RETURNING VALUE(result) TYPE zagl_common_types=>abapmerge_timestamp
                                  RAISING   zagl_version_analyzer_error.
ENDINTERFACE.
