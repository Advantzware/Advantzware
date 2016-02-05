/* dept_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME dept_.
&Scoped-define LISTORDER Department,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY dept-query FOR dept.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)" LABEL "Beginning  Department" NO-UNDO.
DEFINE VARIABLE begin_dept_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" LABEL "Ending Department" NO-UNDO.
DEFINE VARIABLE end_dept_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY dept-query FOR EACH dept NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        dept.code GE begin_dept AND
        dept.code LE end_dept.
    WHEN 2 THEN
    OPEN QUERY dept-query FOR EACH dept NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        dept.dscr GE begin_dept_dscr AND
        dept.dscr LE end_dept_dscr.
  END CASE.
  GET FIRST dept-query.
  DO WHILE AVAILABLE(dept)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/dept_.i}
    DOWN.
    GET NEXT dept-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_dept.
  IMPORT begin_dept_dscr.
  IMPORT end_dept.
  IMPORT end_dept_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_dept COLON 40
    begin_dept_dscr COLON 40
    end_dept COLON 40
    end_dept_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
