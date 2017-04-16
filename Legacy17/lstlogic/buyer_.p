/* buyer_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME buyer_.
&Scoped-define LISTORDER Buyer,Buyer's Name
&Scoped-define WHERE-STATEMENT buyer.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY buyer-query FOR buyer.

DEFINE VARIABLE begin_buyer AS CHARACTER FORMAT "X(3)" LABEL "Beginning Buyer" NO-UNDO.
DEFINE VARIABLE begin_buyer_buyer-n AS CHARACTER FORMAT "X(30)" LABEL "Beginning Buyer Name" NO-UNDO.
DEFINE VARIABLE end_buyer AS CHARACTER FORMAT "X(3)" LABEL "Ending Buyer" NO-UNDO.
DEFINE VARIABLE end_buyer_buyer-n AS CHARACTER FORMAT "X(30)" LABEL "Ending Buyer Name" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY buyer-query FOR EACH buyer NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        buyer.buyer GE begin_buyer AND
        buyer.buyer LE end_buyer.
    WHEN 2 THEN
    OPEN QUERY buyer-query FOR EACH buyer NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        buyer.buyer-n GE begin_buyer_buyer-n AND
        buyer.buyer-n LE end_buyer_buyer-n.
  END CASE.
  GET FIRST buyer-query.
  DO WHILE AVAILABLE(buyer)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/buyer_.i}
    DOWN.
    GET NEXT buyer-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_buyer.
  IMPORT begin_buyer_buyer-n.
  IMPORT end_buyer.
  IMPORT end_buyer_buyer-n.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_buyer COLON 40
    begin_buyer_buyer-n COLON 40
    end_buyer COLON 40
    end_buyer_buyer-n COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
