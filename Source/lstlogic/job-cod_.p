/* job-cod_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME job-cod_.
&Scoped-define LISTORDER Code,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY job-code-query FOR job-code.

DEFINE VARIABLE begin_job-code AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Machine Charge" NO-UNDO.
DEFINE VARIABLE begin_job-code_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_job-code AS CHARACTER FORMAT "X(5)" LABEL "Ending Machine Charge" NO-UNDO.
DEFINE VARIABLE end_job-code_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY job-code-query FOR EACH job-code NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        job-code.code GE begin_job-code AND
        job-code.code LE end_job-code.
    WHEN 2 THEN
    OPEN QUERY job-code-query FOR EACH job-code NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        job-code.dscr GE begin_job-code_dscr AND
        job-code.dscr LE end_job-code_dscr.
  END CASE.
  GET FIRST job-code-query.
  DO WHILE AVAILABLE(job-code)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/job-cod_.i}
    DOWN.
    GET NEXT job-code-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_job-code.
  IMPORT begin_job-code_dscr.
  IMPORT end_job-code.
  IMPORT end_job-code_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_job-code COLON 40
    begin_job-code_dscr COLON 40
    end_job-code COLON 40
    end_job-code_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
