*&---------------------------------------------------------------------*
*& Report  ZABAPGITNF_REPOS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zabapgitnf_repos.

SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME.
  PARAMETERS: p_wdiff AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK bl1.

DATA: gr_repo_notifier TYPE REF TO zcl_abapgitnf_main.

START-OF-SELECTION.


gr_repo_notifier = NEW zcl_abapgitnf_main( ).

  TRY.
      gr_repo_notifier->write_repo_change_stats( p_wdiff ).
    CATCH zcx_abapgit_exception.
      FORMAT COLOR COL_NEGATIVE ON.
      WRITE: 'Unable to read repos'.
      FORMAT COLOR OFF.
  ENDTRY.

END-OF-SELECTION.

* This form is called by abapGit to get user/password
* Not yet supported
FORM password_popup
        USING u_repo_url TYPE string
        CHANGING c_user  TYPE string
                 c_pass  TYPE string.
  CLEAR c_user.
  CLEAR c_pass.
ENDFORM.
