/* job-cat_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME job-cat_.
&Scoped-define LISTORDER Category,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY job-cat-query FOR job-cat.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(3)" LABEL "Beginning  Job Category" NO-UNDO.
DEFINE VARIABLE begin_job-cat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(3)" LABEL "Ending Job Category" NO-UNDO.
DEFINE VARIABLE end_job-cat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY job-cat-query FOR EACH job-cat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        job-cat.cat GE begin_cat AND
        job-cat.cat LE end_cat.
    WHEN 2 THEN
    OPEN QUERY job-cat-query FOR EACH job-cat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        job-cat.dscr GE begin_job-cat_dscr AND
        job-cat.dscr LE end_job-cat_dscr.
  END CASE.
  GET FIRST job-cat-query.
  DO WHILE AVAILABLE(job-cat)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/job-cat_.i}
    DOWN.
    GET NEXT job-cat-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_cat.
  IMPORT begin_job-cat_dscr.
  IMPORT end_cat.
  IMPORT end_job-cat_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_cat COLON 40
    begin_job-cat_dscr COLON 40
    end_cat COLON 40
    end_job-cat_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
