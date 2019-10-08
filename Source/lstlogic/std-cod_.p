/* std-cod_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME std-cod_.
&Scoped-define LISTORDER Code,Description
&Scoped-define WHERE-STATEMENT TRUE
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY std-code-query FOR std-code.

DEFINE VARIABLE begin_std-code AS CHARACTER FORMAT "X(2)" LABEL "Beginning  Standard Matrix" NO-UNDO.
DEFINE VARIABLE begin_std-code_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_std-code AS CHARACTER FORMAT "X(2)" LABEL "Ending Standard Matrix" NO-UNDO.
DEFINE VARIABLE end_std-code_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY std-code-query FOR EACH std-code NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        std-code.code GE begin_std-code AND
        std-code.code LE end_std-code.
    WHEN 2 THEN
    OPEN QUERY std-code-query FOR EACH std-code NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        std-code.dscr GE begin_std-code_dscr AND
        std-code.dscr LE end_std-code_dscr.
  END CASE.
  GET FIRST std-code-query.
  DO WHILE AVAILABLE(std-code)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/std-cod_.i}
    DOWN.
    GET NEXT std-code-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_std-code.
  IMPORT begin_std-code_dscr.
  IMPORT end_std-code.
  IMPORT end_std-code_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_std-code COLON 40
    begin_std-code_dscr COLON 40
    end_std-code COLON 40
    end_std-code_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
