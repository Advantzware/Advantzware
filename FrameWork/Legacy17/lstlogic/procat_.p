/* procat_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME procat_.
&Scoped-define LISTORDER Category,Description
&Scoped-define WHERE-STATEMENT procat.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY procat-query FOR procat.

DEFINE VARIABLE begin_procat_procat AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Product Category" NO-UNDO.
DEFINE VARIABLE begin_procat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_procat_procat AS CHARACTER FORMAT "X(5)" LABEL "Ending Product Category" NO-UNDO.
DEFINE VARIABLE end_procat_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY procat-query FOR EACH procat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        procat.procat GE begin_procat_procat AND
        procat.procat LE end_procat_procat.
    WHEN 2 THEN
    OPEN QUERY procat-query FOR EACH procat NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        procat.dscr GE begin_procat_dscr AND
        procat.dscr LE end_procat_dscr.
  END CASE.
  GET FIRST procat-query.
  DO WHILE AVAILABLE(procat)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/procat_.i}
    DOWN.
    GET NEXT procat-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_procat_procat.
  IMPORT begin_procat_dscr.
  IMPORT end_procat_procat.
  IMPORT end_procat_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_procat_procat COLON 40
    begin_procat_dscr COLON 40
    end_procat_procat COLON 40
    end_procat_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
