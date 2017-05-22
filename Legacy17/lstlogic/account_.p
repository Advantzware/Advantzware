/* account_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME account_.
&Scoped-define LISTORDER Account No,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY account-query FOR account.

DEFINE VARIABLE begin_actnum AS CHARACTER FORMAT "X(25)" LABEL "Beginning  Account" NO-UNDO.
DEFINE VARIABLE begin_account_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_actnum AS CHARACTER FORMAT "X(25)" LABEL "Ending Account" NO-UNDO.
DEFINE VARIABLE end_account_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.
DEFINE VARIABLE show-history AS LOGICAL FORMAT "yes/no" LABEL "Show History" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY account-query FOR EACH account NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        account.actnum GE begin_actnum AND
        account.actnum LE end_actnum.
    WHEN 2 THEN
    OPEN QUERY account-query FOR EACH account NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        account.dscr GE begin_account_dscr AND
        account.dscr LE end_account_dscr.
  END CASE.
  GET FIRST account-query.
  DO WHILE AVAILABLE(account)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/account_.i}
    DOWN.
    GET NEXT account-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_actnum.
  IMPORT begin_account_dscr.
  IMPORT end_actnum.
  IMPORT end_account_dscr.
  IMPORT show-history.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_actnum COLON 40
    begin_account_dscr COLON 40
    end_actnum COLON 40
    end_account_dscr COLON 40
    show-history COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
