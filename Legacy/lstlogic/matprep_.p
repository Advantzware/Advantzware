/* matprep_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME matprep_.
&Scoped-define LISTORDER Type,Description
&Scoped-define WHERE-STATEMENT matprep.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY matprep-query FOR matprep.

DEFINE VARIABLE begin_mat AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Preparation Material" NO-UNDO.
DEFINE VARIABLE begin_matprep_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_mat AS CHARACTER FORMAT "X(5)" LABEL "Ending Preparation Material" NO-UNDO.
DEFINE VARIABLE end_matprep_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY matprep-query FOR EACH matprep NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        matprep.mat GE begin_mat AND
        matprep.mat LE end_mat.
    WHEN 2 THEN
    OPEN QUERY matprep-query FOR EACH matprep NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        matprep.dscr GE begin_matprep_dscr AND
        matprep.dscr LE end_matprep_dscr.
  END CASE.
  GET FIRST matprep-query.
  DO WHILE AVAILABLE(matprep)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/matprep_.i}
    DOWN.
    GET NEXT matprep-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_mat.
  IMPORT begin_matprep_dscr.
  IMPORT end_mat.
  IMPORT end_matprep_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_mat COLON 40
    begin_matprep_dscr COLON 40
    end_mat COLON 40
    end_matprep_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
