/* vend_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME vend_.
&Scoped-define LISTORDER Vendor,Name
&Scoped-define WHERE-STATEMENT vend.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY vend-query FOR vend.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Vendor" NO-UNDO.
DEFINE VARIABLE begin_vend_name AS CHARACTER FORMAT "X(30)" LABEL "Beginning Vendor Name" NO-UNDO.
DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)" LABEL "Ending Vendor" NO-UNDO.
DEFINE VARIABLE end_vend_name AS CHARACTER FORMAT "X(30)" LABEL "Ending Vendor Name" NO-UNDO.
DEFINE VARIABLE show-totals AS LOGICAL FORMAT "yes/no" LABEL "Show Totals" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY vend-query FOR EACH vend NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        vend.vend-no GE begin_vend-no AND
        vend.vend-no LE end_vend-no.
    WHEN 2 THEN
    OPEN QUERY vend-query FOR EACH vend NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        vend.name GE begin_vend_name AND
        vend.name LE end_vend_name.
  END CASE.
  GET FIRST vend-query.
  DO WHILE AVAILABLE(vend)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/vend_.i}
    DOWN.
    GET NEXT vend-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_vend-no.
  IMPORT begin_vend_name.
  IMPORT end_vend-no.
  IMPORT end_vend_name.
  IMPORT show-totals.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_vend-no COLON 40
    begin_vend_name COLON 40
    end_vend-no COLON 40
    end_vend_name COLON 40
    show-totals COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
