/* uom_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME uom_.
&Scoped-define LISTORDER UOM,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY uom-query FOR uom.

DEFINE VARIABLE begin_uom AS CHARACTER FORMAT "X(4)" LABEL "Beginning  Unit of Measure" NO-UNDO.
DEFINE VARIABLE begin_uom_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_uom AS CHARACTER FORMAT "X(4)" LABEL "Ending Unit of Measure" NO-UNDO.
DEFINE VARIABLE end_uom_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY uom-query FOR EACH uom NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        uom.uom GE begin_uom AND
        uom.uom LE end_uom.
    WHEN 2 THEN
    OPEN QUERY uom-query FOR EACH uom NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        uom.dscr GE begin_uom_dscr AND
        uom.dscr LE end_uom_dscr.
  END CASE.
  GET FIRST uom-query.
  DO WHILE AVAILABLE(uom)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/uom_.i}
    DOWN.
    GET NEXT uom-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_uom.
  IMPORT begin_uom_dscr.
  IMPORT end_uom.
  IMPORT end_uom_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_uom COLON 40
    begin_uom_dscr COLON 40
    end_uom COLON 40
    end_uom_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
