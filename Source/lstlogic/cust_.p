/* cust_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME cust_.
&Scoped-define LISTORDER Cust. #,Customer Name
&Scoped-define WHERE-STATEMENT cust.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY cust-query FOR cust.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Customer" NO-UNDO.
DEFINE VARIABLE begin_cust_name AS CHARACTER FORMAT "X(30)" LABEL "Beginning Customer Name" NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Ending Customer" NO-UNDO.
DEFINE VARIABLE end_cust_name AS CHARACTER FORMAT "X(30)" LABEL "Ending Customer Name" NO-UNDO.
DEFINE VARIABLE show-shipto AS LOGICAL FORMAT "yes/no" LABEL "Show Ship To Addresses" NO-UNDO.
DEFINE VARIABLE show-soldto AS LOGICAL FORMAT "yes/no" LABEL "Show Sold To Addresses" NO-UNDO.
DEFINE VARIABLE show-totals AS LOGICAL FORMAT "yes/no" LABEL "Show Totals" NO-UNDO.
DEFINE VARIABLE tb_phone AS LOGICAL FORMAT "yes/no" LABEL "Print Phone Contacts?" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY cust-query FOR EACH cust NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        cust.cust-no GE begin_cust-no AND
        cust.cust-no LE end_cust-no.
    WHEN 2 THEN
    OPEN QUERY cust-query FOR EACH cust NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        cust.name GE begin_cust_name AND
        cust.name LE end_cust_name.
  END CASE.
  GET FIRST cust-query.
  DO WHILE AVAILABLE(cust)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/cust_.i}
    DOWN.
    GET NEXT cust-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_cust-no.
  IMPORT begin_cust_name.
  IMPORT end_cust-no.
  IMPORT end_cust_name.
  IMPORT tb_phone.
  IMPORT show-shipto.
  IMPORT show-soldto.
  IMPORT show-totals.
  
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_cust-no COLON 40
    begin_cust_name COLON 40
    end_cust-no COLON 40
    end_cust_name COLON 40
    show-shipto COLON 40
    show-soldto COLON 40
    show-totals COLON 40
    tb_phone    COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
