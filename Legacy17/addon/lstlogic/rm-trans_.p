/* rm-trans_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME rm-trans_.
&Scoped-define LISTORDER Item No
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY item-query FOR item.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(10)" LABEL "From Item #" NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(10)" LABEL "To Item" NO-UNDO.
DEFINE VARIABLE Begin_procat AS CHARACTER FORMAT "X(10)" LABEL "From Category" NO-UNDO.
DEFINE VARIABLE End_procat AS CHARACTER FORMAT "X(10)" LABEL "To Category" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" LABEL "From Date" NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" LABEL "To Date" NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(10)" LABEL "From Warehouse" NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(10)" LABEL "To Warehouse" NO-UNDO.
DEFINE VARIABLE TOGGLE-1 AS LOGICAL FORMAT "yes/no" LABEL "Receipts" NO-UNDO.
DEFINE VARIABLE TOGGLE-2 AS LOGICAL FORMAT "yes/no" LABEL "Issue" NO-UNDO.
DEFINE VARIABLE TOGGLE-3 AS LOGICAL FORMAT "yes/no" LABEL "Transfer" NO-UNDO.
DEFINE VARIABLE TOGGLE-4 AS LOGICAL FORMAT "yes/no" LABEL "Adjustment" NO-UNDO.
DEFINE VARIABLE TOGGLE-5 AS LOGICAL FORMAT "yes/no" LABEL "Count" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  {methods/lstlogic/custom/rm-trans_.i}
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_i-no.
  IMPORT end_i-no.
  IMPORT Begin_procat.
  IMPORT End_procat.
  IMPORT begin_date.
  IMPORT end_date.
  IMPORT begin_whse.
  IMPORT end_whse.
  IMPORT TOGGLE-1.
  IMPORT TOGGLE-2.
  IMPORT TOGGLE-3.
  IMPORT TOGGLE-4.
  IMPORT TOGGLE-5.
END PROCEDURE.

PROCEDURE Show-Selections:
  IF output-option = 4 THEN
  DO:
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","From Item #:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_i-no).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","To Item:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_i-no).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","From Category:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",Begin_procat).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","To Category:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",End_procat).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","From Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","To Date:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_date).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","From Warehouse:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",begin_whse).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","To Warehouse:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",end_whse).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Receipts:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",TOGGLE-1).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Issue:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",TOGGLE-2).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Transfer:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",TOGGLE-3).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Adjustment:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",TOGGLE-4).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
    y-pos = y-pos + 80.
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"bold","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",1,y-pos,800,0).
    RUN SetCellText IN h_Viper("Cell","Parameter","Count:").
    RUN SetCellAlign IN h_Viper("Cell","Parameter",2).
    RUN FlushGroup IN h_Viper ("Parameter").
    RUN SetCellFont IN h_Viper("Cell","Parameter",12,0,"","Arial").
    RUN SetCellPos IN h_Viper("Cell","Parameter",870,y-pos,700,0).
    RUN SetCellText IN h_Viper("Cell","Parameter",TOGGLE-5).
    RUN SetCellAlign IN h_Viper("Cell","Parameter",1).
    RUN FlushGroup IN h_Viper ("Parameter").
  END.
  ELSE
  DISPLAY
    begin_i-no COLON 40
    end_i-no COLON 40
    Begin_procat COLON 40
    End_procat COLON 40
    begin_date COLON 40
    end_date COLON 40
    begin_whse COLON 40
    end_whse COLON 40
    TOGGLE-1 COLON 40
    TOGGLE-2 COLON 40
    TOGGLE-3 COLON 40
    TOGGLE-4 COLON 40
    TOGGLE-5 COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
