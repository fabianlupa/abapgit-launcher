"! <p class="shorttext synchronized">Version Provider</p>
INTERFACE zagl_version_provider PUBLIC.
  TYPES flavor TYPE c LENGTH 1.

  CONSTANTS: BEGIN OF flavors,
               standalone_version TYPE flavor VALUE 'S',
               developer_version  TYPE flavor VALUE 'D',
             END OF flavors.

  TYPES: BEGIN OF version_data,
           program_name        TYPE progname,
           program_description TYPE repti,
           changed_on          TYPE rdir_udate,
           changed_by          TYPE unam,
           created_on          TYPE rdir_cdate,
           created_by          TYPE cnam,
           flavor              TYPE flavor,
           package             TYPE devclass,
         END OF version_data.
  TYPES version_data_tab TYPE STANDARD TABLE OF version_data WITH KEY program_name.

  "! Retrieve available abapGit versions in the system
  "!
  "! @parameter sa_version_pattern | Standalone version pattern
  "! @parameter result             | Retrieved versions
  METHODS retrieve_abapgit_versions IMPORTING sa_version_pattern TYPE progname DEFAULT zagl_common_types=>abapgit_sa_vers_prog_pattern
                                    RETURNING VALUE(result)      TYPE version_data_tab.
ENDINTERFACE.
