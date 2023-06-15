REPORT zaglauncher.

CLASS common_types DEFINITION CREATE PRIVATE ABSTRACT.
  PUBLIC SECTION.
    TYPES abapgit_version     TYPE string.
    TYPES abapmerge_timestamp TYPE string.

    CONSTANTS abapgit_dev_version_prog_name TYPE progname VALUE 'ZABAPGIT'.
    CONSTANTS abapgit_sa_vers_prog_pattern  TYPE progname VALUE 'ZABAPGIT_STANDALONE%'.
ENDCLASS.


CLASS internal_error DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.


CLASS time_date_error DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.


CLASS time_date_util DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS parse_date_from_abapmerge_ts IMPORTING !timestamp    TYPE string
                                               RETURNING VALUE(result) TYPE d
                                               RAISING   time_date_error.

    CLASS-METHODS parse_date_from_abapgit_ts IMPORTING !timestamp    TYPE timestampl
                                             RETURNING VALUE(result) TYPE d
                                             RAISING   time_date_error.
ENDCLASS.


CLASS time_date_util IMPLEMENTATION.
  METHOD parse_date_from_abapmerge_ts.
    IF strlen( timestamp ) < 10.
      RAISE EXCEPTION TYPE time_date_error.
    ENDIF.
    result = timestamp(4) && timestamp+5(2) && timestamp+8(2).
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING  date                      = result
      EXCEPTIONS plausibility_check_failed = 1
                 OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE time_date_error.
    ENDIF.
  ENDMETHOD.

  METHOD parse_date_from_abapgit_ts.
    TRY.
        cl_abap_tstmp=>systemtstmp_utc2syst( EXPORTING utc_tstmp = cl_abap_tstmp=>move_to_short( timestamp )
                                             IMPORTING syst_date = result ).
      CATCH cx_parameter_invalid_range cx_parameter_invalid_type INTO DATA(exception).
        RAISE EXCEPTION TYPE time_date_error
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


INTERFACE version_provider.
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

  METHODS select_abapgit_versions RETURNING VALUE(result) TYPE version_data_tab.
ENDINTERFACE.


CLASS version_provider_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES version_provider.
ENDCLASS.


CLASS version_provider_impl IMPLEMENTATION.
  METHOD version_provider~select_abapgit_versions.
    " PROGDIR~RLOAD can be missing, therefore use TADIR~MASTERLANG

    SELECT program~name AS program_name,
           text~text AS program_description,
           program~udat AS changed_on,
           program~unam AS changed_by,
           program~cdat AS created_on,
           program~cnam AS created_by,
           CASE program~name
             WHEN @common_types=>abapgit_dev_version_prog_name
             THEN @version_provider=>flavors-developer_version
             ELSE @version_provider=>flavors-standalone_version
           END AS flavor,
           catalog~devclass AS package
      FROM trdir AS program
      LEFT OUTER JOIN tadir AS catalog ON catalog~pgmid    = 'R3TR'
                                      AND catalog~object   = 'PROG'
                                      AND catalog~obj_name = program~name
      LEFT OUTER JOIN trdirt AS text ON text~name = program~name
                            AND text~sprsl = catalog~masterlang
      WHERE ( program~name LIKE @common_types=>abapgit_sa_vers_prog_pattern OR
              program~name = @common_types=>abapgit_dev_version_prog_name )
      ORDER BY program~name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
ENDCLASS.


CLASS source_not_available DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.


INTERFACE source_reader.
  METHODS read_report IMPORTING program_name  TYPE progname
                      RETURNING VALUE(result) TYPE string_table
                      RAISING   source_not_available.
ENDINTERFACE.


CLASS source_reader_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES source_reader.
ENDCLASS.


CLASS source_reader_impl IMPLEMENTATION.
  METHOD source_reader~read_report.
    READ REPORT program_name
         STATE 'A'
         INTO result.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE source_not_available.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS version_analyzer_error DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.


INTERFACE version_analyzer.
  TYPES: BEGIN OF dev_edition_admin_data,
           deserialized_by TYPE syuname,
           deserialized_at TYPE timestampl,
           created_by      TYPE syuname,
           created_at      TYPE timestampl,
         END OF dev_edition_admin_data.

  METHODS get_dev_version_version RETURNING VALUE(result) TYPE common_types=>abapgit_version
                                  RAISING   version_analyzer_error.

  METHODS get_dev_version_admin_data RETURNING VALUE(result) TYPE dev_edition_admin_data
                                     RAISING   version_analyzer_error.

  METHODS get_sa_version_version IMPORTING program_name  TYPE progname
                                 RETURNING VALUE(result) TYPE common_types=>abapgit_version
                                 RAISING   version_analyzer_error.

  METHODS get_abapmerge_timestamp IMPORTING program_name  TYPE progname
                                  RETURNING VALUE(result) TYPE common_types=>abapmerge_timestamp
                                  RAISING   version_analyzer_error.
ENDINTERFACE.


CLASS version_analyzer_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES version_analyzer.

    METHODS constructor IMPORTING source_reader TYPE REF TO source_reader.

  PRIVATE SECTION.
    CONSTANTS abapgit_version_constant     TYPE string VALUE 'ZIF_ABAPGIT_VERSION=>C_ABAP_VERSION'.
    CONSTANTS abapmerge_timestamp_constant TYPE string VALUE 'LIF_ABAPMERGE_MARKER=>C_MERGE_TIMESTAMP'.
    CONSTANTS abapmerge_marker_pattern     TYPE string VALUE '*lif_abapmerge_marker*'.

    DATA source_reader TYPE REF TO source_reader.

    METHODS get_version_by_identifier IMPORTING absolute_identifier TYPE csequence
                                      RETURNING VALUE(result)       TYPE string
                                      RAISING   version_analyzer_error.

    METHODS get_data_obj_value_by_name IMPORTING absolute_identifier TYPE csequence
                                       CHANGING  !value              TYPE data
                                       RAISING   version_analyzer_error.
ENDCLASS.


CLASS version_analyzer_impl IMPLEMENTATION.
  METHOD constructor.
    me->source_reader = source_reader.
  ENDMETHOD.

  METHOD version_analyzer~get_dev_version_version.
    result = get_version_by_identifier( abapgit_version_constant ).
  ENDMETHOD.

  METHOD version_analyzer~get_sa_version_version.
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
          RAISE EXCEPTION TYPE version_analyzer_error.
        ENDIF.
      CATCH cx_sy_conversion_error INTO DATA(conversion_error).
        RAISE EXCEPTION TYPE version_analyzer_error
          EXPORTING previous = conversion_error.
    ENDTRY.
  ENDMETHOD.

  METHOD version_analyzer~get_abapmerge_timestamp.
    DO 2 TIMES.
      DATA(current_try) = sy-index.

      CASE current_try.
        WHEN 1.
          " First try finding constants in the marker interface
          TRY.
              " Force load the program
              PERFORM %_init-move IN PROGRAM (program_name).

              get_data_obj_value_by_name(
                EXPORTING absolute_identifier = |\\PROGRAM={ program_name }\\INTERFACE={ abapmerge_timestamp_constant }|
                CHANGING  value               = result ).

            CATCH cx_sy_dyn_call_illegal_form
                  cx_sy_program_not_found
                  version_analyzer_error.
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

            CATCH source_not_available INTO DATA(source_not_available).
              RAISE EXCEPTION TYPE version_analyzer_error
                EXPORTING previous = source_not_available.
          ENDTRY.
      ENDCASE.

      IF result IS NOT INITIAL AND result CP '++++-++-++T*'.
        RETURN.
      ELSEIF current_try >= 2.
        RAISE EXCEPTION TYPE version_analyzer_error.
      ENDIF.

      CLEAR result.
    ENDDO.
  ENDMETHOD.

  METHOD version_analyzer~get_dev_version_admin_data.
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
            AND obj_name = @common_types=>abapgit_dev_version_prog_name
          INTO @DATA(package).
        IF sy-subrc <> 0 OR package IS INITIAL.
          RAISE EXCEPTION TYPE version_analyzer_error.
        ENDIF.

        CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>get_instance
          RECEIVING ri_srv = repo_srv.
        CALL METHOD repo_srv->('ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE')
          EXPORTING iv_package = package
          IMPORTING ei_repo    = <repo>.

        repo = <repo>.
        ASSIGN repo->('ZIF_ABAPGIT_REPO~MS_DATA') TO FIELD-SYMBOL(<repo_data>).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE version_analyzer_error.
        ENDIF.

        result = CORRESPONDING #( <repo_data> ).
      CATCH cx_root INTO DATA(exception).
        RAISE EXCEPTION TYPE version_analyzer_error
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING version_provider TYPE REF TO version_provider
                                  version_analyzer TYPE REF TO version_analyzer.

    METHODS run.

  PRIVATE SECTION.
    TYPES: BEGIN OF output.
             INCLUDE TYPE version_provider=>version_data AS version_data.
    TYPES:   abapgit_version        TYPE common_types=>abapgit_version,
             abapmerge_timestamp    TYPE common_types=>abapmerge_timestamp,
             abapmerge_date         TYPE d,
             deserialized_timestamp TYPE timestampl,
             deserialized_date      TYPE d,
             deserialized_by        TYPE syst_uname,
             sort_priority          TYPE i,
             last_updated_on        TYPE d,
             last_updated_by        TYPE syst_uname,
             flavor_icon            TYPE icon_d,
           END OF output.

    CONSTANTS unknown_value TYPE string VALUE `???`.

    DATA alv              TYPE REF TO cl_salv_table.
    DATA output_list      TYPE STANDARD TABLE OF output.

    DATA version_provider TYPE REF TO version_provider.
    DATA version_analyzer TYPE REF TO version_analyzer.

    METHODS retrieve_versions.
    METHODS display_list.
    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING !row.
    METHODS execute_report  IMPORTING program_name TYPE progname.

ENDCLASS.


CLASS main IMPLEMENTATION.
  METHOD constructor.
    me->version_provider = version_provider.
    me->version_analyzer = version_analyzer.
  ENDMETHOD.

  METHOD run.
    retrieve_versions( ).
    display_list( ).
  ENDMETHOD.

  METHOD retrieve_versions.
    output_list = CORRESPONDING #( version_provider->select_abapgit_versions( ) ).

    LOOP AT output_list ASSIGNING FIELD-SYMBOL(<version>).
      CASE <version>-flavor.
        WHEN version_provider=>flavors-developer_version.
          <version>-sort_priority = 1.
          <version>-flavor_icon   = icon_oo_class.

          TRY.
              <version>-abapgit_version = version_analyzer->get_dev_version_version( ).
            CATCH version_analyzer_error.
              <version>-abapgit_version = unknown_value.
          ENDTRY.

          TRY.
              DATA(dev_version_admin_data) = version_analyzer->get_dev_version_admin_data( ).
              <version>-deserialized_timestamp = dev_version_admin_data-deserialized_at.
              <version>-deserialized_by        = dev_version_admin_data-deserialized_by.
              <version>-last_updated_by        = <version>-deserialized_by.
              TRY.
                  <version>-deserialized_date = time_date_util=>parse_date_from_abapgit_ts(
                                                    dev_version_admin_data-deserialized_at ).
                  <version>-last_updated_on   = <version>-deserialized_date.
                CATCH time_date_error ##NO_HANDLER.
              ENDTRY.
            CATCH version_analyzer_error.
              <version>-deserialized_by = unknown_value.
              <version>-last_updated_by = unknown_value.
          ENDTRY.

        WHEN version_provider=>flavors-standalone_version.
          <version>-sort_priority = 2.
          <version>-flavor_icon   = icon_abap_local.

          TRY.
              <version>-abapgit_version = version_analyzer->get_sa_version_version( <version>-program_name ).
            CATCH version_analyzer_error.
              <version>-abapgit_version = unknown_value.
          ENDTRY.

          <version>-last_updated_by = <version>-changed_by.
          <version>-last_updated_on = <version>-changed_on.

          TRY.
              <version>-abapmerge_timestamp = version_analyzer->get_abapmerge_timestamp( <version>-program_name ).
              TRY.
                  <version>-abapmerge_date  = time_date_util=>parse_date_from_abapmerge_ts(
                                                  <version>-abapmerge_timestamp ).
                  <version>-last_updated_on = <version>-abapmerge_date.
                CATCH time_date_error ##NO_HANDLER.
              ENDTRY.
            CATCH version_analyzer_error.
              <version>-abapmerge_timestamp = unknown_value.
          ENDTRY.
      ENDCASE.
    ENDLOOP.

    SORT output_list BY sort_priority ASCENDING
                        last_updated_on DESCENDING.
  ENDMETHOD.

  METHOD display_list.
    TYPES: BEGIN OF column_position,
             column_name TYPE lvc_fname,
             position    TYPE i,
           END OF column_position.
    DATA column_positions      TYPE STANDARD TABLE OF column_position WITH KEY position.
    DATA last_launched_version TYPE progname.

    TRY.
        IF alv IS NOT BOUND.

          cl_salv_table=>factory( IMPORTING r_salv_table = alv
                                  CHANGING  t_table      = output_list ).

          DATA(columns) = alv->get_columns( ).

          LOOP AT columns->get( ) ASSIGNING FIELD-SYMBOL(<column>).
            DATA(position) = 999.
            DATA(column) = <column>-r_column.

            CASE <column>-columnname.
              WHEN 'FLAVOR_ICON'.
                position = 1.
                column->set_short_text( 'Fl.' ).
                column->set_medium_text( 'Flavor' ).
                CAST cl_salv_column_table( column )->set_icon( ).
                column->set_output_length( 3 ).
              WHEN 'FLAVOR'.
                position = 2.
                column->set_medium_text( 'Flavor ID' ).
                column->set_visible( abap_false ).
              WHEN 'PROGRAM_NAME'.
                position = 3.
                column->set_output_length( 30 ).
              WHEN 'PROGRAM_DESCRIPTION'.
                position = 4.
                column->set_output_length( 30 ).
              WHEN 'ABAPGIT_VERSION'.
                position = 5.
                column->set_short_text( 'AG Vers.' ).
                column->set_medium_text( 'abapGit Version' ).
                column->set_long_text( 'abapGit Version' ).
                column->set_output_length( 12 ).
              WHEN 'LAST_UPDATED_ON'.
                position = 6.
                column->set_short_text( 'Last upd.' ).
                column->set_medium_text( 'Last udpated on' ).
                column->set_long_text( 'Last updated on' ).
                column->set_output_length( 12 ).
              WHEN 'LAST_UPDATED_BY'.
                position = 7.
                column->set_short_text( 'Upd. by' ).
                column->set_medium_text( 'Last udpated by' ).
                column->set_long_text( 'Last updated by' ).
                column->set_output_length( 12 ).
              WHEN 'DESERIALIZED_DATE'.
                position = 8.
                column->set_short_text( 'Des. on' ).
                column->set_medium_text( 'Last deserial. on' ).
                column->set_long_text( 'Last deserialized on' ).
                column->set_output_length( 12 ).
              WHEN 'DESERIALIZED_TIMESTAMP'.
                position = 9.
                column->set_visible( abap_false ).
              WHEN 'DESERIALIZED_BY'.
                position = 10.
                column->set_short_text( 'Des. by' ).
                column->set_medium_text( 'Last deserial. by' ).
                column->set_long_text( 'Last deserialized by' ).
                column->set_output_length( 12 ).
              WHEN 'ABAPMERGE_DATE'.
                position = 11.
                column->set_short_text( 'abapmerge' ).
                column->set_medium_text( 'abapmerge Date' ).
                column->set_long_text( 'abapmerge Date' ).
                column->set_output_length( 12 ).
              WHEN 'ABAPMERGE_TIMESTAMP'.
                position = 12.
                column->set_short_text( 'abapmerge' ).
                column->set_medium_text( 'abapmerge TS' ).
                column->set_long_text( 'abapmerge Timestamp' ).
                column->set_visible( abap_false ).
              WHEN 'CHANGED_ON'.
                position = 13.
                column->set_visible( abap_false ).
              WHEN 'CHANGED_BY'.
                position = 14.
                column->set_visible( abap_false ).
              WHEN 'CREATED_ON'.
                position = 15.
                column->set_visible( abap_false ).
              WHEN 'CREATED_BY'.
                position = 16.
                column->set_visible( abap_false ).
              WHEN 'PACKAGE'.
                position = 17.
                column->set_visible( abap_false ).
              WHEN 'SORT_PRIORITY'.
                column->set_technical( ).
            ENDCASE.

            INSERT VALUE #( column_name = <column>-columnname position = position ) INTO TABLE column_positions.
          ENDLOOP.

          " Need to do these in ascending order apparently
          SORT column_positions BY position ASCENDING.
          LOOP AT column_positions ASSIGNING FIELD-SYMBOL(<column_position>).
            columns->set_column_position( columnname = <column_position>-column_name
                                          position   = <column_position>-position ).
          ENDLOOP.

          DATA(tooltips) = alv->get_functional_settings( )->get_tooltips( ).
          tooltips->add_tooltip( type    = cl_salv_tooltip=>c_type_icon
                                 value   = CONV #( icon_oo_class )
                                 tooltip = 'Developer Version' ).
          tooltips->add_tooltip( type    = cl_salv_tooltip=>c_type_icon
                                 value   = CONV #( icon_abap_local )
                                 tooltip = 'Standalone Version' ).

          alv->get_functions( )->set_all( ).
          DATA(layout) = alv->get_layout( ).
          layout->set_key( VALUE #( report = sy-repid ) ).
          layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

          DATA(top) = NEW cl_salv_form_layout_grid( ).
          top->create_label( row    = 1
                             column = 1
                             text   = 'Developer Version installed:' ).
          top->create_text(
              row    = 1
              column = 2
              text   = COND #( WHEN line_exists( output_list[ flavor = version_provider=>flavors-developer_version ] )
                               THEN 'Yes'
                               ELSE 'No' ) ).
          top->create_label( row    = 2
                             column = 1
                             text   = 'Standalone Versions installed:' ).
          top->create_text( row    = 2
                            column = 2
                            text   = |{ REDUCE i( INIT i = 0
                                                  FOR line IN output_list
                                                  WHERE ( flavor = version_provider=>flavors-standalone_version )
                                                  NEXT i = i + 1 ) NUMBER = USER }| &&
                                     | (with pattern { common_types=>abapgit_sa_vers_prog_pattern })| ).
          alv->set_top_of_list( top ).

          SET HANDLER on_double_click FOR alv->get_event( ).

          GET PARAMETER ID 'ZAGLAUNCHER' FIELD last_launched_version.
          IF sy-subrc = 0 AND last_launched_version IS NOT INITIAL.
            DATA(line_to_select) = line_index( output_list[ program_name = last_launched_version ] ).
            IF line_to_select IS NOT INITIAL.
              alv->get_selections( )->set_current_cell( VALUE #( row = line_to_select columnname = 'PROGRAM_NAME' ) ).
            ENDIF.
          ENDIF.

          alv->display( ).

        ELSE.
          alv->refresh( ).
        ENDIF.
      CATCH cx_salv_error INTO DATA(error).
        RAISE EXCEPTION TYPE internal_error
          EXPORTING previous = error.
    ENDTRY.
  ENDMETHOD.

  METHOD on_double_click.
    IF row IS INITIAL.
      RETURN.
    ENDIF.

    DATA(row_ref) = REF #( output_list[ row ] ).

    execute_report( row_ref->program_name ).
  ENDMETHOD.

  METHOD execute_report.
    ASSERT program_name CP 'ZABAPGIT*'.
    SET PARAMETER ID 'ZAGLAUNCHER' FIELD program_name.
    SUBMIT (program_name).
  ENDMETHOD.
ENDCLASS.

INCLUDE zaglauncher_tests IF FOUND.

START-OF-SELECTION.
  NEW main( version_provider = NEW version_provider_impl( )
            version_analyzer = NEW version_analyzer_impl( NEW source_reader_impl( ) )
      )->run( ).
