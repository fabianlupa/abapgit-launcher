"! <p class="shorttext synchronized">Version Provider</p>
INTERFACE zagl_version_provider PUBLIC.
  TYPES: BEGIN OF version_data,
           program_name        TYPE progname,
           program_description TYPE repti,
           changed_on          TYPE rdir_udate,
           changed_by          TYPE unam,
           created_on          TYPE rdir_cdate,
           created_by          TYPE cnam,
           package             TYPE devclass,
         END OF version_data.
  TYPES version_data_tab TYPE STANDARD TABLE OF version_data WITH KEY program_name.

  "! Retrieve available abapGit versions in the system
  "!
  "! @parameter program_name_filter | Program name filter
  "! @parameter result              | Retrieved versions
  METHODS retrieve_abapgit_versions IMPORTING program_name_filter TYPE zagl_common_types=>program_name_filter DEFAULT zagl_common_types=>default_program_name_filter
                                    RETURNING VALUE(result)       TYPE version_data_tab.
ENDINTERFACE.
