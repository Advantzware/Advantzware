/* fgcat_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME fgcat_.
&Scoped-define LISTORDER Category,Description
&Scoped-define WHERE-STATEMENT fgcat.company = company
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY fgcat-query FOR fgcat.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Category" NO-UNDO.
DEFINE VARIABLE begin_fgcat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)" LABEL "Ending Category" NO-UNDO.
DEFINE VARIABLE end_fgcat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY fgcat-query FOR EACH fgcat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        fgcat.procat GE begin_procat AND
        fgcat.procat LE end_procat.
    WHEN 2 THEN
    OPEN QUERY fgcat-query FOR EACH fgcat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        fgcat.dscr GE begin_fgcat_dscr AND
        fgcat.dscr LE end_fgcat_dscr.
  END CASE.
  GET FIRST fgcat-query.
  DO WHILE AVAILABLE(fgcat)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/fgcat_.i}
    DOWN.
    GET NEXT fgcat-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_procat.
  IMPORT begin_fgcat_dscr.
  IMPORT end_procat.
  IMPORT end_fgcat_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_procat COLON 40
    begin_fgcat_dscr COLON 40
    end_procat COLON 40
    end_fgcat_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
