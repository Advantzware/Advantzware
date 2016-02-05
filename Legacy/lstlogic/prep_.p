/* prep_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME prep_.
&Scoped-define LISTORDER Preparation Code,Description
&Scoped-define WHERE-STATEMENT prep.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY prep-query FOR prep.

DEFINE VARIABLE begin_code AS CHARACTER FORMAT "X(10)" LABEL "Beginning  Preparation Code" NO-UNDO.
DEFINE VARIABLE begin_prep_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_code AS CHARACTER FORMAT "X(10)" LABEL "Ending Preparation Code" NO-UNDO.
DEFINE VARIABLE end_prep_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY prep-query FOR EACH prep NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prep.code GE begin_code AND
        prep.code LE end_code.
    WHEN 2 THEN
    OPEN QUERY prep-query FOR EACH prep NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prep.dscr GE begin_prep_dscr AND
        prep.dscr LE end_prep_dscr.
  END CASE.
  GET FIRST prep-query.
  DO WHILE AVAILABLE(prep)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/prep_.i}
    DOWN.
    GET NEXT prep-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_code.
  IMPORT begin_prep_dscr.
  IMPORT end_code.
  IMPORT end_prep_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_code COLON 40
    begin_prep_dscr COLON 40
    end_code COLON 40
    end_prep_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
