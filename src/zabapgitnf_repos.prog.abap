*&---------------------------------------------------------------------*
*& Report  ZABAPGITNF_REPOS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zabapgitnf_repos.

SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETERS: pwdiff AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK bl1.

START-OF-SELECTION.


  DATA(repos) = NEW zcl_abapgitnf_main( ).

  TRY.
      repos->write_repo_change_stats( pwdiff ).
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
  FORMAT COLOR COL_NEGATIVE ON.
  WRITE: / 'Private repos not yet supported: ', u_repo_url.
  FORMAT COLOR OFF.
ENDFORM.
