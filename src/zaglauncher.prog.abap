REPORT zaglauncher.

TABLES progdir.

SELECTION-SCREEN BEGIN OF SCREEN 0100.
  SELECT-OPTIONS s_pname FOR progdir-name OBLIGATORY.
  PARAMETERS p_jumpb TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF SCREEN 0100.

CLASS internal_error DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.


CLASS main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS initialization.

    METHODS constructor IMPORTING version_provider TYPE REF TO zagl_version_provider
                                  version_analyzer TYPE REF TO zagl_version_analyzer.

    METHODS run.

  PRIVATE SECTION.
    TYPES: BEGIN OF output.
             INCLUDE TYPE zagl_version_provider=>version_data AS version_data.
    TYPES:   abapgit_version        TYPE zagl_common_types=>abapgit_version,
             flavor                 TYPE zagl_version_analyzer=>flavor,
             abapmerge_timestamp    TYPE zagl_common_types=>abapmerge_timestamp,
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

    DATA version_provider TYPE REF TO zagl_version_provider.
    DATA version_analyzer TYPE REF TO zagl_version_analyzer.

    METHODS retrieve_versions.
    METHODS display_list.
    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING !row.
    METHODS execute_report  IMPORTING program_name TYPE progname.

ENDCLASS.


CLASS main IMPLEMENTATION.
  METHOD initialization.
    s_pname[] = zagl_common_types=>default_program_name_filter.

    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING  report               = sy-repid
                 variant              = 'DEFAULT'
      EXCEPTIONS variant_not_existent = 1
                 variant_obsolete     = 2
                 OTHERS               = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    me->version_provider = version_provider.
    me->version_analyzer = version_analyzer.
  ENDMETHOD.

  METHOD run.
    retrieve_versions( ).
    display_list( ).
  ENDMETHOD.

  METHOD retrieve_versions.
    output_list = CORRESPONDING #( version_provider->retrieve_abapgit_versions( s_pname[] ) ).

    LOOP AT output_list ASSIGNING FIELD-SYMBOL(<version>).
      TRY.
          DATA(analysis_result) = version_analyzer->analyze_program( <version>-program_name ).
          <version>-abapgit_version = analysis_result-version.
          IF <version>-abapgit_version IS INITIAL.
            <version>-abapgit_version = unknown_value.
          ENDIF.
          <version>-flavor = analysis_result-flavor.

          CASE <version>-flavor.
            WHEN zagl_version_analyzer=>flavors-developer_version.
              <version>-sort_priority          = 1.
              <version>-flavor_icon            = icon_oo_class.

              <version>-deserialized_timestamp = analysis_result-developer_version_details-deserialized_at.
              <version>-deserialized_by        = analysis_result-developer_version_details-deserialized_by.
              <version>-last_updated_by        = <version>-deserialized_by.
              TRY.
                  <version>-deserialized_date = zagl_time_date_util=>parse_date_from_abapgit_ts(
                                                    analysis_result-developer_version_details-deserialized_at ).
                  <version>-last_updated_on   = <version>-deserialized_date.
                CATCH zagl_time_date_error ##NO_HANDLER.
              ENDTRY.
              IF <version>-deserialized_by IS INITIAL.
                <version>-deserialized_by = unknown_value.
              ENDIF.
              IF <version>-last_updated_by IS INITIAL.
                <version>-last_updated_by = unknown_value.
              ENDIF.

            WHEN zagl_version_analyzer=>flavors-standalone_version.
              <version>-sort_priority       = 2.
              <version>-flavor_icon         = icon_abap_local.

              <version>-last_updated_by     = <version>-changed_by.
              <version>-last_updated_on     = <version>-changed_on.
              <version>-abapmerge_timestamp = analysis_result-standalone_version_details-abapmerge_timestamp.
              IF <version>-abapmerge_timestamp IS INITIAL.
                <version>-abapmerge_timestamp = unknown_value.
              ELSE.
                TRY.
                    <version>-abapmerge_date  = zagl_time_date_util=>parse_date_from_abapmerge_ts(
                                                    <version>-abapmerge_timestamp ).
                    <version>-last_updated_on = <version>-abapmerge_date.
                  CATCH zagl_time_date_error ##NO_HANDLER.
                ENDTRY.
              ENDIF.
          ENDCASE.
        CATCH zagl_version_analyzer_error.
          DELETE output_list USING KEY loop_key.
      ENDTRY.
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
              text   = COND #( WHEN line_exists( output_list[
                                                     flavor = zagl_version_analyzer=>flavors-developer_version ] )
                               THEN 'Yes'
                               ELSE 'No' ) ).
          top->create_label( row    = 2
                             column = 1
                             text   = 'Standalone Versions installed:' ).
          top->create_text( row    = 2
                            column = 2
                            text   = |{ REDUCE i( INIT i = 0
                                                  FOR line IN output_list
                                                  WHERE ( flavor = zagl_version_analyzer=>flavors-standalone_version )
                                                  NEXT i = i + 1 ) NUMBER = USER }| ).
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
    ASSERT program_name CP zagl_common_types=>default_dev_version_prog_name && '*'.
    SET PARAMETER ID 'ZAGLAUNCHER' FIELD program_name.
    IF p_jumpb = abap_true.
      SUBMIT (program_name) AND RETURN.
    ELSE.
      SUBMIT (program_name).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  main=>initialization( ).

START-OF-SELECTION.
  NEW main( version_provider = NEW zagl_version_provider_impl( )
            version_analyzer = NEW zagl_version_analyzer_impl( NEW zagl_source_reader_impl( ) )
      )->run( ).
