/* mat_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME mat_.
&Scoped-define LISTORDER Material Type,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY mat-query FOR mat.

DEFINE VARIABLE begin_mat_type AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Material Type" NO-UNDO.
DEFINE VARIABLE begin_mat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_mat_type AS CHARACTER FORMAT "X(5)" LABEL "Ending Material Type" NO-UNDO.
DEFINE VARIABLE end_mat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY mat-query FOR EACH mat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        mat.mat GE begin_mat_type AND
        mat.mat LE end_mat_type.
    WHEN 2 THEN
    OPEN QUERY mat-query FOR EACH mat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        mat.dscr GE begin_mat_dscr AND
        mat.dscr LE end_mat_dscr.
  END CASE.
  GET FIRST mat-query.
  DO WHILE AVAILABLE(mat)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/mat_.i}
    DOWN.
    GET NEXT mat-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_mat_type.
  IMPORT begin_mat_dscr.
  IMPORT end_mat_type.
  IMPORT end_mat_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_mat_type COLON 40
    begin_mat_dscr COLON 40
    end_mat_type COLON 40
    end_mat_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
