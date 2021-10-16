CLASS zcl_abapgitnf_main DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF cc_diff_status,
                 unknown TYPE char1 VALUE '0',
                 error   TYPE char1 VALUE '1',
                 diff    TYPE char1 VALUE '2',
                 ok      TYPE char1 VALUE '3',
               END OF cc_diff_status.
    METHODS write_repo_change_stats IMPORTING iv_with_diff_only TYPE abap_bool
                                    RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.

    TYPES: BEGIN OF ty_diff,
             match TYPE i,
             _a    TYPE i,
             a_    TYPE i,
             m_    TYPE i,
             _m    TYPE i,
             mm    TYPE i,
             d_    TYPE i,
             _d    TYPE i,
           END OF ty_diff.
    TYPES: BEGIN OF ty_repo_info,
             status_icon TYPE icon_d,
             repo_name   TYPE string,
             package     TYPE devclass,
             branch_name TYPE string,
             url         TYPE string,
             status      TYPE ty_diff,
             message     TYPE string,
           END OF ty_repo_info,
           ty_repo_infos TYPE STANDARD TABLE OF ty_repo_info.

    DATA: repos      TYPE ty_repo_infos.
    DATA: header_row TYPE i.

    METHODS display IMPORTING i_repos TYPE ty_repo_infos.
    METHODS set_alv_column_texts IMPORTING i_columns TYPE REF TO cl_salv_columns_table.
    METHODS set_alv_column_text IMPORTING i_columns     TYPE REF TO cl_salv_columns_table
                                          i_column_name TYPE lvc_fname
                                          i_column_text TYPE string.
    METHODS add_alv_info IMPORTING i_header TYPE REF TO cl_salv_form_layout_grid
                                   i_label  TYPE string
                                   i_text   TYPE string.

ENDCLASS.



CLASS zcl_abapgitnf_main IMPLEMENTATION.

  METHOD write_repo_change_stats.

    DATA: repos_api   TYPE REF TO zif_abapgit_persist_repo.
    DATA: repo_api        TYPE REF TO zcl_abapgit_repo.
    DATA: diff_count  TYPE ty_diff.
    DATA: diff_status TYPE char1.
    DATA: repos       TYPE zif_abapgit_persistence=>ty_repos.

    FIELD-SYMBOLS: <state_counter> TYPE i.

    repos_api = zcl_abapgit_persist_factory=>get_repo( ).
    repos = repos_api->list( ).

    DELETE repos WHERE offline = abap_true.

    LOOP AT repos ASSIGNING FIELD-SYMBOL(<repo>).

      CLEAR diff_count.
      diff_status = cc_diff_status-ok.

      TRY.

          repo_api = zcl_abapgit_repo_srv=>get_instance( )->get( <repo>-key ).

          DATA(statuss) = repo_api->status( ).

          LOOP AT statuss ASSIGNING FIELD-SYMBOL(<status>).

            IF <status>-match = abap_true.
              diff_count-match = diff_count-match + 1.
            ELSE.
              diff_status = cc_diff_status-diff.

              TRANSLATE <status>-lstate USING ' _'.
              TRANSLATE <status>-rstate USING ' _'.
              ASSIGN COMPONENT <status>-lstate && <status>-rstate OF STRUCTURE diff_count TO <state_counter>.
              "dump if not yet defined!
              <state_counter> = <state_counter> + 1.

            ENDIF.

          ENDLOOP.

          IF   diff_status = cc_diff_status-diff
            OR iv_with_diff_only = abap_false.

            APPEND VALUE #( repo_name   = repo_api->get_name( )
                            package     = repo_api->get_package( )
                            branch_name = zcl_abapgit_git_branch_list=>get_display_name( <repo>-branch_name )
                            url         = <repo>-url
                            status      = diff_count
                            status_icon = diff_status ) TO me->repos.

          ENDIF.

        CATCH zcx_abapgit_exception INTO DATA(exception).
          diff_status = cc_diff_status-error.
          APPEND VALUE #( repo_name   = repo_api->get_name( )
                          package     = repo_api->get_package( )
                          branch_name = zcl_abapgit_git_branch_list=>get_display_name( <repo>-branch_name )
                          url         = <repo>-url
                          status      = diff_count
                          status_icon = diff_status
                          message     = exception->get_text( ) ) TO me->repos.

      ENDTRY.

    ENDLOOP.

    me->display( me->repos ).

  ENDMETHOD.

  METHOD display.
    TRY.

        DATA(repos) = i_repos.

        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
                                CHANGING  t_table      = repos ).
        salv_table->get_functions( )->set_all( if_salv_c_bool_sap=>true ).


        TRY.
            salv_table->get_columns( )->get_column( columnname = 'STATUS_ICON' )->set_alignment(  value = if_salv_c_alignment=>centered ).
            salv_table->get_columns( )->get_column( columnname = 'STATUS_ICON' )->set_output_length( value = 10 ).
            salv_table->get_columns( )->set_exception_column( value = 'STATUS_ICON' group = '2').
          CATCH cx_salv_not_found cx_salv_data_error .
        ENDTRY.


        me->set_alv_column_texts( salv_table->get_columns( ) ) .
        salv_table->get_columns( )->set_optimize( abap_true ).


        DATA(header) = NEW cl_salv_form_layout_grid( ).
        me->add_alv_info( i_header = header i_label = 'System' i_text = |{ sy-sysid }-{ sy-mandt }| ).
        me->add_alv_info( i_header = header i_label = 'Date/Time' i_text = |{ sy-datum DATE = ISO } { sy-uzeit TIME = ISO }| ).

        salv_table->set_top_of_list( header ).
        salv_table->display( ).

      CATCH cx_salv_msg
            cx_salv_not_found INTO DATA(lx_exception).
    ENDTRY.
  ENDMETHOD.

  METHOD set_alv_column_texts.

    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS_ICON'  i_column_text = 'Status' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'REPO_NAME'    i_column_text = 'Repository' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'PACKAGE'      i_column_text = 'Package' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'BRANCH_NAME'  i_column_text = 'Branch' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'URL'          i_column_text = 'Repo URL' ).
    CAST cl_salv_column_list( i_columns->get_column( columnname = 'URL' ) )->set_cell_type( if_salv_c_cell_type=>link ).


    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-MATCH' i_column_text = 'equal' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-A_'    i_column_text = 'local added' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-_A'    i_column_text = 'remote added' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-M_'    i_column_text = 'local modified' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-_M'    i_column_text = 'remote modified' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-MM'    i_column_text = 'modified' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-D_'    i_column_text = 'local deleted' ).
    set_alv_column_text( i_columns = i_columns i_column_name = 'STATUS-_D'    i_column_text = 'remote deleted' ).

  ENDMETHOD.


  METHOD set_alv_column_text.

    TRY.
        DATA(column) = i_columns->get_column( columnname = i_column_name ).
        column->set_medium_text( value = CONV #( i_column_text ) ).
        column->set_long_text( value = CONV #( i_column_text ) ).
        column->set_short_text( value = CONV #( i_column_text ) ).
      CATCH cx_salv_not_found ##no_handler.
    ENDTRY.

  ENDMETHOD.

  METHOD add_alv_info.

    header_row = header_row + 1.

    i_header->create_label( row    = header_row
                            column = 1
                            text   = i_label ).
    i_header->create_text( row    = header_row
                           column = 2
                           text   = i_text ).

  ENDMETHOD.

ENDCLASS.
