CLASS zcl_abapgitnf_main DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS write_repo_change_stats IMPORTING iv_with_diff_only TYPE abap_bool
                                    RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_diff,
             match TYPE i,
             xa    TYPE i,
             ax    TYPE i,
             m_    TYPE i,
             _m    TYPE i,
             mm    TYPE i,
             d_    TYPE i,
             _d    TYPE i,
           END OF ty_diff.

    METHODS write_error IMPORTING iv_text TYPE string.
    METHODS write_success IMPORTING iv_text TYPE string.
    METHODS write_warning IMPORTING iv_text TYPE string.
ENDCLASS.



CLASS zcl_abapgitnf_main IMPLEMENTATION.

  METHOD write_repo_change_stats.

    DATA: ls_diff         TYPE ty_diff,
          lv_has_diff     TYPE abap_bool,
          lt_repos        TYPE zif_abapgit_persistence=>tt_repo,
          lr_exception    TYPE REF TO zcx_abapgit_exception,
          lt_statuss      TYPE zif_abapgit_definitions=>ty_results_tt,
          lr_repo         TYPE REF TO zcl_abapgit_repo,
          lv_repo_heading TYPE string,
          lr_repos        TYPE REF TO zif_abapgit_persist_repo.

    FIELD-SYMBOLS: <lv_state_counter> TYPE i.
    FIELD-SYMBOLS: <ls_repo>          TYPE zif_abapgit_persistence=>ty_repo.
    FIELD-SYMBOLS: <ls_status>        LIKE LINE OF lt_statuss.

*    DATA(repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
*    LOOP AT repos ASSIGNING FIELD-SYMBOL(<repo>).

    lr_repos = zcl_abapgit_persist_factory=>get_repo( ).
    lt_repos = lr_repos->list( ).

    DELETE lt_repos WHERE offline = abap_true.
*    DELETE lt_repos WHERE local_settings-display_name = 'CI'.

    LOOP AT lt_repos ASSIGNING <ls_repo>.

      TRY.

          lr_repo = zcl_abapgit_repo_srv=>get_instance( )->get( <ls_repo>-key ).

          lt_statuss = lr_repo->status( ).

          CLEAR ls_diff.
          CLEAR lv_has_diff.

          LOOP AT lt_statuss ASSIGNING <ls_status>.

            IF <ls_status>-match = abap_true.
              ls_diff-match = ls_diff-match + 1.
            ELSE.
              lv_has_diff = abap_true.

              TRANSLATE <ls_status>-lstate USING ' _'.
              TRANSLATE <ls_status>-rstate USING ' _'.
              ASSIGN COMPONENT <ls_status>-lstate && <ls_status>-rstate OF STRUCTURE ls_diff TO <lv_state_counter>.
              "dump if not yet defined!
              <lv_state_counter> = <lv_state_counter> + 1.

            ENDIF.
*            WRITE: / <ls_status>-filename, <ls_status>-obj_name, <ls_status>-obj_type,
*                     <ls_status>-lstate, <ls_status>-rstate, <ls_status>-match.

          ENDLOOP.

          IF   lv_has_diff = abap_true
            OR iv_with_diff_only = abap_false.

            ULINE.
            IF lv_has_diff = abap_true.
              lv_repo_heading = lr_repo->get_name( ) && ` `
                                && lr_repo->get_package( ) && ` `
                                && <ls_repo>-branch_name.
              me->write_warning( lv_repo_heading  ).
            ELSE.
              me->write_success( lv_repo_heading ).
            ENDIF.
            ULINE.

            WRITE: / 'Equal:', ls_diff-match,
                   / 'remote modified:', ls_diff-_m,
                   / 'local modified:', ls_diff-m_,
                   / 'modified:', ls_diff-mm,
                   / 'remote added:', ls_diff-xa,
                   / 'local added:', ls_diff-ax,
                   / 'remote deleted:', ls_diff-_d,
                   / 'local deleted:', ls_diff-d_.

          ENDIF.

        CATCH zcx_abapgit_exception INTO lr_exception.
          write_error( lr_exception->get_text( ) ).

      ENDTRY.
      SKIP.
    ENDLOOP.
  ENDMETHOD.

  METHOD write_error.

    FORMAT COLOR COL_NEGATIVE ON.
    WRITE: / iv_text.
    FORMAT COLOR OFF.

  ENDMETHOD.

  METHOD write_success.

    FORMAT COLOR COL_POSITIVE ON.
    WRITE: / iv_text.
    FORMAT COLOR OFF.

  ENDMETHOD.


  METHOD write_warning.

    FORMAT COLOR COL_TOTAL ON.
    WRITE: / iv_text.
    FORMAT COLOR OFF.

  ENDMETHOD.


ENDCLASS.
