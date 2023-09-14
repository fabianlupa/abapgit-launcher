CLASS zagl_version_provider_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zagl_version_provider.
ENDCLASS.


CLASS zagl_version_provider_impl IMPLEMENTATION.
  METHOD zagl_version_provider~retrieve_abapgit_versions.
    CONSTANTS executable_program TYPE subc VALUE '1'.

    IF program_name_filter IS INITIAL.
      RETURN.
    ENDIF.

    " PROGDIR~RLOAD can be missing, therefore use TADIR~MASTERLANG

    SELECT program~name AS program_name,
           text~text    AS program_description,
           program~udat AS changed_on,
           program~unam AS changed_by,
           program~cdat AS created_on,
           program~cnam AS created_by,
           catalog~devclass AS package
      FROM trdir AS program
      LEFT OUTER JOIN tadir AS catalog ON catalog~pgmid    = 'R3TR'
                                      AND catalog~object   = 'PROG'
                                      AND catalog~obj_name = program~name
      LEFT OUTER JOIN trdirt AS text ON text~name = program~name
                            AND text~sprsl = catalog~masterlang
      WHERE program~name IN @program_name_filter
        AND subc = @executable_program
      ORDER BY program~name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
ENDCLASS.
