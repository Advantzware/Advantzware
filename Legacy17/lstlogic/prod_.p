/* prod_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME prod_.
&Scoped-define LISTORDER Product Line,Description
&Scoped-define WHERE-STATEMENT prod.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY prod-query FOR prod.

DEFINE VARIABLE begin_prolin AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Product Line" NO-UNDO.
DEFINE VARIABLE begin_prod_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_prolin AS CHARACTER FORMAT "X(8)" LABEL "Ending Product Line" NO-UNDO.
DEFINE VARIABLE end_prod_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.
DEFINE VARIABLE show-catagories AS LOGICAL FORMAT "yes/no" LABEL "Show Catagories" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY prod-query FOR EACH prod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prod.prolin GE begin_prolin AND
        prod.prolin LE end_prolin.
    WHEN 2 THEN
    OPEN QUERY prod-query FOR EACH prod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        prod.dscr GE begin_prod_dscr AND
        prod.dscr LE end_prod_dscr.
  END CASE.
  GET FIRST prod-query.
  DO WHILE AVAILABLE(prod)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/prod_.i}
    DOWN.
    GET NEXT prod-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_prolin.
  IMPORT begin_prod_dscr.
  IMPORT end_prolin.
  IMPORT end_prod_dscr.
  IMPORT show-catagories.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_prolin COLON 40
    begin_prod_dscr COLON 40
    end_prolin COLON 40
    end_prod_dscr COLON 40
    show-catagories COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
