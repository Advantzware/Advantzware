/* ventype_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME ventype_.
&Scoped-define LISTORDER Vendor Type,Description
&Scoped-define WHERE-STATEMENT ventype.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY ventype-query FOR ventype.

DEFINE VARIABLE begin_ventype AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Vendor Type" NO-UNDO.
DEFINE VARIABLE begin_ventype_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_ventype AS CHARACTER FORMAT "X(8)" LABEL "Ending Vendor Type" NO-UNDO.
DEFINE VARIABLE end_ventype_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY ventype-query FOR EACH ventype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        ventype.type GE begin_ventype AND
        ventype.type LE end_ventype.
    WHEN 2 THEN
    OPEN QUERY ventype-query FOR EACH ventype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        ventype.Dscr GE begin_ventype_Dscr AND
        ventype.Dscr LE end_ventype_Dscr.
  END CASE.
  GET FIRST ventype-query.
  DO WHILE AVAILABLE(ventype)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/ventype_.i}
    DOWN.
    GET NEXT ventype-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_ventype.
  IMPORT begin_ventype_dscr.
  IMPORT end_ventype.
  IMPORT end_ventype_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_ventype COLON 40
    begin_ventype_dscr COLON 40
    end_ventype COLON 40
    end_ventype_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
