/* carrier_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME carrier_.
&Scoped-define LISTORDER Carrier,Description
&Scoped-define WHERE-STATEMENT carrier.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY carrier-query FOR carrier.

DEFINE VARIABLE begin_carrier AS CHARACTER FORMAT "X(5)" LABEL "Beginning Carrier" NO-UNDO.
DEFINE VARIABLE begin_carrier_dscr AS CHARACTER FORMAT "X(20)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_carrier AS CHARACTER FORMAT "X(5)" LABEL "Ending Carrier" NO-UNDO.
DEFINE VARIABLE end_carrier_dscr AS CHARACTER FORMAT "X(20)" LABEL "Ending Description" NO-UNDO.
DEFINE VARIABLE show-matrix AS LOGICAL FORMAT "yes/no" LABEL "Show Product Matrix" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY carrier-query FOR EACH carrier NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        carrier.carrier GE begin_carrier AND
        carrier.carrier LE end_carrier.
    WHEN 2 THEN
    OPEN QUERY carrier-query FOR EACH carrier NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        carrier.dscr GE begin_carrier_dscr AND
        carrier.dscr LE end_carrier_dscr.
  END CASE.
  GET FIRST carrier-query.
  DO WHILE AVAILABLE(carrier)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/carrier_.i}
    DOWN.
    GET NEXT carrier-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_carrier.
  IMPORT begin_carrier_dscr.
  IMPORT end_carrier.
  IMPORT end_carrier_dscr.
  IMPORT show-matrix.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_carrier COLON 40
    begin_carrier_dscr COLON 40
    end_carrier COLON 40
    end_carrier_dscr COLON 40
    show-matrix COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
