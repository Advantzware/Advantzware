&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pcCustFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcPartFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcPartTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER piOrderFrom AS INT NO-UNDO.
DEFINE INPUT PARAMETER piOrderTo   AS INT NO-UNDO.
DEFINE INPUT PARAMETER pdDateFrom  AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pdDateTo    AS DATE NO-UNDO.


/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
 
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.


DEF TEMP-TABLE tt-report NO-UNDO
    FIELD i-no    AS CHAR
    FIELD ord-no  LIKE oe-rel.ord-no
    FIELD vdate   LIKE oe-rel.rel-date
    FIELD carrier AS CHAR
    FIELD shipid  AS CHAR
    FIELD release# LIKE oe-relh.release#
    FIELD row-id AS ROWID
    FIELD qty AS INT
    FIELD cShipFrom LIKE oe-rel.spare-char-1.


ASSIGN cTextListToSelect  = "Order#,Customer#,Order Date,FG Item#,Cust Part#,Item Name,Cust PO#,Ordered Qty,Prod. Qty," +
                            "Shipped Qty,Job On Hand,Sell Price,UOM,Unit Count,Pallet Count,Skids,Status,Due Date," +
                            "Customer Name,Est#,Job#,CAD#,Invoice Qty,Act. Rel. Quantity,Production Balance,O/U%,Rep," +
                            "Rep Name,Release Date,Carrier,Ship To Code,FG On Hand,Orders Due,Items Due,Last User ID,Hold Reason Code,Hold/Approved Date," +
                            "Scheduled Rel. Qty,Ship From,Ship To Name"

       cFieldListToSelect = "oe-ordl.ord-no,oe-ordl.cust-no,oe-ord.ord-date,oe-ordl.i-no,oe-ordl.part-no,oe-ordl.i-name,oe-ordl.po-no,oe-ordl.qty,v-prod-qty," +
                            "oe-ordl.ship-qty,v-bal,oe-ordl.price,oe-ordl.pr-uom,case-count,pallet-count,skid-count,oe-ord.stat,oe-ordl.req-date," +
                            "oe-ord.cust-name,oe-ordl.est-no,job,cad-no,oe-ordl.inv-qty,act-rel-qty,wip-qty,pct,sman," +
                            "sname,reldate,carrier,shipid,fg-oh,oe-ord.due-date,oe-ordl.req-date,oe-ord.user-id,oe-ord.spare-char-2,approved-date," +
                            "sch-rel-qty,ship-from,ship-name"
                            
        cFieldLength = "15,15,15,20,15,30,15,15,20," + "15,15,15,20,15,30,15,15,20," + "15,15,15,20,15,30,15,15,20," + "15,15,15,20,15,30,15,8,16,18," + "18,9,30"
           cFieldType = "c,c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c,c,c,c," + "c,c,c"
       .

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 RS-open-closed ~
RS-ord-stat begin_cust-no end_cust-no begin_item end_item begin_part ~
end_part begin_order end_order end_date begin_date begin_reldt end_reldt ~
begin_shipfrom end_shipfrom tb_print-del sl_avail sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS RS-open-closed RS-ord-stat begin_cust-no ~
end_cust-no begin_item end_item begin_part end_part begin_order end_order ~
end_date begin_date begin_reldt end_reldt begin_shipfrom end_shipfrom ~
tb_print-del sl_avail sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue Dialog-Frame 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" 
     LABEL "From Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_order AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "From Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_part AS CHARACTER FORMAT "X(15)" 
     LABEL "From Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_reldt AS DATE FORMAT "99/99/9999" 
     LABEL "From Release Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_shipfrom AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Ship From WH" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" 
     LABEL "To Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_order AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "To Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_part AS CHARACTER FORMAT "X(15)" 
     LABEL "To Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_reldt AS DATE FORMAT "99/99/9999" 
     LABEL "To Release Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shipfrom AS CHARACTER FORMAT "X(5)":U INITIAL "zzzz" 
     LABEL "Ending Ship From WH" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-order.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE RS-open-closed AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"Both", "Both"
     SIZE 36.6 BY .95 NO-UNDO.

DEFINE VARIABLE RS-ord-stat AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Any", 1,
"On Hold", 2,
"Approved", 3
     SIZE 41.8 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 13.05.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 3.43.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_print-del AS LOGICAL INITIAL no 
     LABEL "Print Detail by Release Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RS-open-closed AT ROW 2 COL 41.2 NO-LABEL WIDGET-ID 2
     RS-ord-stat AT ROW 3.14 COL 41.2 NO-LABEL WIDGET-ID 120
     begin_cust-no AT ROW 4.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 4.48 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_item AT ROW 5.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 100
     end_item AT ROW 5.62 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 102
     begin_part AT ROW 6.76 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 104
     end_part AT ROW 6.76 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 106
     begin_order AT ROW 7.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 108
     end_order AT ROW 7.91 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 110
     end_date AT ROW 9 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 114
     begin_date AT ROW 9.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 112
     begin_reldt AT ROW 10.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 116
     end_reldt AT ROW 10.14 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 118
     begin_shipfrom AT ROW 11.19 COL 28 COLON-ALIGNED HELP
          "Enter starting ship from location." WIDGET-ID 158
     end_shipfrom AT ROW 11.24 COL 71 COLON-ALIGNED HELP
          "Enter ending ship from location." WIDGET-ID 160
     tb_print-del AT ROW 12.81 COL 35.8
     sl_avail AT ROW 15.29 COL 6.6 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 15.29 COL 62.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 15.76 COL 43.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 16.95 COL 43.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 18.14 COL 43.6 WIDGET-ID 40
     btn_down AT ROW 19.33 COL 43.6 WIDGET-ID 42
     tb_excel AT ROW 21.86 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 21.86 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 22.81 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 25.19 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 25.19 COL 60.2 WIDGET-ID 12
     "Order Status:" VIEW-AS TEXT
          SIZE 13.4 BY 1 AT ROW 3 COL 27 WIDGET-ID 124
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 14.29 COL 3 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 14.19 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.05 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 21.1 COL 2 WIDGET-ID 84
     SPACE(0.79) SKIP(2.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Maintenance Excel Export" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_order:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_part:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_reldt:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_shipfrom:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_order:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_part:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_reldt:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_shipfrom:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME Dialog-Frame
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Order Maintenance Excel Export */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
       when "end_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
        when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/
       when "begin_shipfrom" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-loc.w  (cocode,ls-cur-val, output char-val). 
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* Ship From*/
       when "end_shipfrom" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-loc.w  (cocode,ls-cur-val, output char-val). 
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* Ship From*/
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Maintenance Excel Export */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date Dialog-Frame
ON LEAVE OF begin_date IN FRAME Dialog-Frame /* From Order Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_order Dialog-Frame
ON LEAVE OF begin_order IN FRAME Dialog-Frame /* From Order# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_part Dialog-Frame
ON LEAVE OF begin_part IN FRAME Dialog-Frame /* From Customer Part# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_reldt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_reldt Dialog-Frame
ON LEAVE OF begin_reldt IN FRAME Dialog-Frame /* From Release Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shipfrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shipfrom Dialog-Frame
ON LEAVE OF begin_shipfrom IN FRAME Dialog-Frame /* From ShipTo */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN GetSelectionList. 
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add Dialog-Frame
ON CHOOSE OF Btn_Add IN FRAME Dialog-Frame /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.

  /*
  DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down Dialog-Frame
ON CHOOSE OF btn_down IN FRAME Dialog-Frame /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove Dialog-Frame
ON CHOOSE OF Btn_Remove IN FRAME Dialog-Frame /* << Remove */
DO:
 /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    ldummy = sl_selected:DELETE(i).
  END
  */
  APPLY "DEFAULT-ACTION" TO sl_selected  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up Dialog-Frame
ON CHOOSE OF btn_Up IN FRAME Dialog-Frame /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date Dialog-Frame
ON LEAVE OF end_date IN FRAME Dialog-Frame /* To Order Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_order Dialog-Frame
ON LEAVE OF end_order IN FRAME Dialog-Frame /* To Order# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_part Dialog-Frame
ON LEAVE OF end_part IN FRAME Dialog-Frame /* To Customer Part# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_reldt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_reldt Dialog-Frame
ON LEAVE OF end_reldt IN FRAME Dialog-Frame /* To Release Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shipfrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shipfrom Dialog-Frame
ON LEAVE OF end_shipfrom IN FRAME Dialog-Frame /* To ShipTo */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON LEAVE OF fi_file IN FRAME Dialog-Frame /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail Dialog-Frame
ON DEFAULT-ACTION OF sl_avail IN FRAME Dialog-Frame
DO:
  
   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .

  
/* for pairs
    DEF VAR cSelectedList AS cha NO-UNDO.
    cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
         sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
        sl_selected:NUM-ITEMS
        SKIP cSelectedList
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected Dialog-Frame
ON DEFAULT-ACTION OF sl_selected IN FRAME Dialog-Frame
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
              ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              .
    END.           
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    .
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel Dialog-Frame
ON VALUE-CHANGED OF tb_excel IN FRAME Dialog-Frame /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-del Dialog-Frame
ON VALUE-CHANGED OF tb_print-del IN FRAME Dialog-Frame /* Print Detail by Release Date? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel Dialog-Frame
ON VALUE-CHANGED OF tb_runExcel IN FRAME Dialog-Frame /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.    

    /* stacey */
    RUN Set-Sort-Data.

    ASSIGN RS-ord-stat:SCREEN-VALUE = "1" .

    APPLY "entry" TO begin_cust-no.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList Dialog-Frame 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:   
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 Dialog-Frame 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY RS-open-closed RS-ord-stat begin_cust-no end_cust-no begin_item 
          end_item begin_part end_part begin_order end_order end_date begin_date 
          begin_reldt end_reldt begin_shipfrom end_shipfrom tb_print-del 
          sl_avail sl_selected tb_excel tb_runExcel fi_file 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-6 RECT-7 RECT-8 RS-open-closed RS-ord-stat begin_cust-no 
         end_cust-no begin_item end_item begin_part end_part begin_order 
         end_order end_date begin_date begin_reldt end_reldt begin_shipfrom 
         end_shipfrom tb_print-del sl_avail sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList Dialog-Frame 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
    IF NOT AVAIL ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field Dialog-Frame 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    DO:
      IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
        .
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report Dialog-Frame 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL.f}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER boe-ord FOR oe-ord.

DEF VAR v-fcust LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

DEF VAR lv-tmp-string AS CHAR NO-UNDO.

DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

DEF VAR v-prod-qty AS INT NO-UNDO.
DEF VAR v-bal AS INT NO-UNDO.
DEF VAR v-act-rel-qty AS INT NO-UNDO.
DEF VAR v-wip-qty AS INT NO-UNDO.
DEF VAR v-pct     AS INT NO-UNDO.
DEF VAR v-fgitem AS CHAR NO-UNDO.
DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-qoh AS INT NO-UNDO.
DEF VAR v-open-closed AS LOGICAL NO-UNDO.
DEF VAR v-case-count AS DECI NO-UNDO.
DEF VAR v-pallet-count AS DECI NO-UNDO.
DEF VAR v-skid-count   AS DECI NO-UNDO.
DEF VAR v-cnt          AS INT NO-UNDO.
DEF VAR vcarrier AS CHAR NO-UNDO.
DEF VAR vshipid  AS CHAR NO-UNDO.
DEF VAR vreldate AS CHAR NO-UNDO.
DEF VAR vsman  AS CHAR NO-UNDO .
DEF VAR vsname  AS CHAR NO-UNDO .
DEFINE VARIABLE cShipFr LIKE oe-rel.spare-char-1 NO-UNDO.
DEFINE VARIABLE dSchRelQty AS DECIMAL NO-UNDO.
DEFINE VARIABLE cShipName AS CHARACTER NO-UNDO .
ASSIGN
   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-excelheader = "".

   
SESSION:SET-WAIT-STATE ("general").

/* {sys/inc/print1.i}                         */
/* {sys/inc/outprint.i value(lines-per-page)} */

IF tb_excel THEN
   OUTPUT STREAM excel TO VALUE(fi_file).
    
   DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".
 END.
   
 IF tb_excel THEN
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
 

   /*   IF v-excelheader NE "" THEN
         PUT STREAM excel UNFORMATTED v-excelheader SKIP. */
   FOR EACH oe-ordl NO-LOCK WHERE 
            oe-ordl.company EQ cocode AND 
           (oe-ordl.cust-no >= begin_cust-no AND oe-ordl.cust-no <= end_cust-no) AND 
            /* stacey */
           (oe-ordl.i-no >= begin_item AND oe-ordl.i-no <= END_item) AND
           (oe-ordl.part-no >= begin_part AND oe-ordl.part-no <= end_part) AND
           (oe-ordl.ord-no >= int(begin_order) AND oe-ordl.ord-no <= int(end_order)) AND
            /* end stacey */
            (RS-open-closed EQ "Both" OR
               (oe-ordl.opened AND RS-open-closed EQ "Open" )       OR
               (NOT oe-ordl.opened AND RS-open-closed EQ "Closed")),
      FIRST oe-ord OF oe-ordl WHERE oe-ord.stat NE "W" AND
           (oe-ord.ord-date >= date(begin_date) AND oe-ord.ord-date <= date(end_date)) AND
       ((oe-ord.stat NE "H" AND NOT oe-ord.priceHold AND RS-ord-stat = 3) OR 
           ((oe-ord.stat EQ "H" OR oe-ord.priceHold) AND RS-ord-stat = 2) OR oe-ord.stat EQ "" OR RS-ord-stat = 1)
/*                                 AND (oe-ord.type    NE "T" OR NOT NO) */
                          USE-INDEX ord-no NO-LOCK, 
      FIRST itemfg NO-LOCK WHERE itemfg.company EQ oe-ordl.company 
                             AND itemfg.i-no EQ oe-ordl.i-no:

      ASSIGN 
         v-prod-qty = 0
         v-bal = 0
         v-act-rel-qty = 0
         v-pct = 0
         v-fgitem = ""
         v-excel-detail-lines = ""
         v-case-count = 0
         v-pallet-count = 0
         v-skid-count = 0
         v-cnt = v-cnt + 1
         dSchRelQty = 0 .

        DEF VAR li AS INT NO-UNDO.

      IF AVAIL oe-ordl THEN DO:
         IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK WHERE fg-rcpth.company   EQ cocode
                                                               AND fg-rcpth.job-no    EQ oe-ordl.job-no
                                                               AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                                                               AND fg-rcpth.i-no      EQ oe-ordl.i-no
                                                               AND fg-rcpth.rita-code EQ "R"
                                                        USE-INDEX job,
               EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                                   AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
               v-prod-qty = v-prod-qty + fg-rdtlh.qty.
            END.
         ELSE DO:
            FOR EACH job-hdr FIELDS(job-no job-no2) WHERE job-hdr.company EQ cocode 
                                                      AND job-hdr.ord-no EQ oe-ordl.ord-no 
                                                      AND job-hdr.i-no EQ oe-ordl.i-no
                                                USE-INDEX ord-no NO-LOCK,
               EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK WHERE fg-rcpth.company   EQ cocode
                                                              AND fg-rcpth.job-no    EQ job-hdr.job-no
                                                              AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                                                              AND fg-rcpth.i-no      EQ oe-ordl.i-no
                                                              AND fg-rcpth.rita-code EQ "R"
                                                        USE-INDEX job,
               EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                                   AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                v-prod-qty = v-prod-qty + fg-rdtlh.qty.
            END.
         END.
                IF oe-ordl.po-no-po NE 0 THEN   /* Task 05221402 */
        FOR EACH fg-rcpth FIELDS(r-no rita-code) WHERE
            fg-rcpth.company   EQ cocode AND
            fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) AND
            fg-rcpth.i-no      EQ oe-ordl.i-no AND
            fg-rcpth.rita-code EQ "R"
            NO-LOCK,
            EACH fg-rdtlh FIELDS(qty) WHERE
                 fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                 fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 NO-LOCK:
                 v-prod-qty = v-prod-qty + fg-rdtlh.qty.
        END.
      END.

      IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
         FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                                   AND fg-bin.job-no  EQ oe-ordl.job-no
                                   AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                   AND fg-bin.i-no    EQ oe-ordl.i-no:
            v-bal = v-bal + fg-bin.qty.
         END.
      IF AVAIL oe-ordl THEN
       /*  FOR EACH oe-rel WHERE oe-rel.company EQ cocode 
                           AND oe-rel.ord-no  EQ oe-ordl.ord-no 
                           AND oe-rel.i-no    EQ oe-ordl.i-no 
                           AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

            IF INDEX("A,B,P",v-stat) > 0 THEN
               v-act-rel-qty = v-act-rel-qty + oe-rel.qty.
         END. */
         FOR EACH tt-report NO-LOCK:
             DELETE tt-report .
         END.
         ASSIGN
             vcarrier = ""
             vshipid  = ""
             vreldate = "" 
             vsman     = ""
             vsname     = ""
             cShipFr = "".

         
         IF AVAIL oe-ordl THEN
         FOR EACH oe-rel WHERE oe-rel.company EQ cocode 
                           AND oe-rel.ord-no  EQ oe-ordl.ord-no 
                           AND oe-rel.i-no    EQ oe-ordl.i-no 
                           AND oe-rel.line    EQ oe-ordl.LINE
                           NO-LOCK:
    
            IF AVAIL oe-rel THEN
             FIND FIRST oe-rell
                 WHERE oe-rell.company  EQ oe-rel.company
                 AND oe-rell.r-no     EQ oe-rel.link-no
                 AND oe-rell.ord-no   EQ oe-rel.ord-no
                 AND oe-rell.rel-no   EQ oe-rel.rel-no
                 AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                 AND oe-rell.i-no     EQ oe-rel.i-no
                 AND oe-rell.line     EQ oe-rel.LINE NO-LOCK NO-ERROR.
             IF AVAIL oe-rell THEN

                 FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no
                 USE-INDEX r-no NO-LOCK NO-ERROR.

             CREATE tt-report .
             IF AVAIL oe-rel THEN
             ASSIGN
                 tt-report.row-id = ROWID(oe-rel)
                 tt-report.qty  = oe-rel.qty
                 tt-report.i-no    = oe-rel.i-no   
                 tt-report.ord-no  = oe-rel.ord-no
                 tt-report.carrier  = oe-rel.carrier
                 tt-report.shipid   = oe-rel.ship-id 
                 tt-report.vdate = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date
                 tt-report.cShipFrom = oe-rel.spare-char-1. 
                
            RELEASE oe-rell .
            RELEASE oe-relh .
            IF NOT tb_print-del THEN LEAVE .  /* task 05281401 */

         END. /* for each oe-rel  */

         IF NOT tb_print-del  THEN DO:
            FIND FIRST tt-report WHERE tt-report.ord-no = oe-ordl.ord-no
                                    AND tt-report.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
            
            IF NOT AVAIL tt-report THEN DO:
                CREATE tt-report .
                     ASSIGN
                         tt-report.qty  = oe-ordl.qty
                         tt-report.i-no    = oe-ordl.i-no 
                         tt-report.ord-no  = oe-ordl.ord-no
                         tt-report.carrier  = oe-ordl.carrier
                         tt-report.shipid   = oe-ord.ship-id
                         tt-report.cShipFrom = oe-ord.loc .
            END.
         END.
    
         ASSIGN
             vsman = IF oe-ordl.s-man[1] <> "" THEN oe-ordl.s-man[1] ELSE oe-ordl.s-man[2] .
             FIND FIRST sman
                 WHERE sman.company EQ cocode
                 AND sman.sman    EQ vsman
                 NO-LOCK NO-ERROR.
             IF AVAIL sman THEN ASSIGN vsman = sman.sman 
                                       vsname = sman.sname.
         
      FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

      v-wip-qty = oe-ordl.qty - (v-bal + oe-ordl.ship-qty).
      IF v-wip-qty LT 0 OR
         v-wip-qty LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
         v-wip-qty = 0.

      v-case-count   = oe-ordl.cas-cnt.
      IF v-case-count NE 0 THEN
         {sys/inc/roundup.i v-case-count}

      ASSIGN
         v-pallet-count = IF oe-ordl.cases-unit = 0 THEN 1 ELSE oe-ordl.cases-unit
         v-pallet-count = v-pallet-count * v-case-count.
     
      IF v-case-count NE 0 AND oe-ordl.qty NE 0 THEN 
         v-skid-count   = oe-ordl.qty / v-pallet-count.
      IF v-skid-count NE 0 THEN 
          {sys/inc/roundup.i v-skid-count}

      IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
         v-pct = ((v-prod-qty / oe-ordl.qty) - 1) * 100.
         IF v-pct EQ 0 THEN v-pct = 100.
         IF v-pct EQ -100 THEN v-pct = 0.
      END.
      v-fgitem =  oe-ordl.i-no.
      
   FOR EACH tt-report
       WHERE tt-report.cShipFrom GE begin_shipfrom
       AND tt-report.cShipFrom LE end_shipfrom NO-LOCK BY tt-report.vdate DESC:
      
       IF tb_print-del THEN  /* task 06051405 */
           IF NOT CAN-FIND( FIRST tt-report WHERE tt-report.vdate GE begin_reldt AND tt-report.vdate LE end_reldt  ) THEN NEXT .


       ASSIGN
           vcarrier = tt-report.carrier
           vshipid  = tt-report.shipid
           vreldate = string(tt-report.vdate)
           cShipFr = tt-report.cShipFrom
           cShipName = "" .

       find first carrier where carrier.company = g_company and
           carrier.carrier = vcarrier 
           no-lock no-error.
       IF AVAIL carrier THEN
           ASSIGN vcarrier = carrier.dscr .
       find first shipto no-lock
            where shipto.company = g_company and
           shipto.ship-id = vshipid 
           no-error.  
       cShipName = IF AVAIL shipto THEN shipto.ship-name ELSE "" .

       RUN oe/rel-stat.p (tt-report.row-id, OUTPUT v-stat).
       FIND FIRST oe-rel NO-LOCK 
           WHERE oe-rel.company EQ g_company 
             AND ROWID(oe-rel) EQ  tt-report.row-id NO-ERROR.
       
       IF AVAIL oe-rel THEN
           ASSIGN
           v-act-rel-qty = oe-rel.qty
           dSchRelQty    = oe-rel.tot-qty .
       ELSE ASSIGN
           v-act-rel-qty = 0 
           dSchRelQty    = 0.


      ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     IF AVAIL oe-ordl THEN
     BUFFER b-oe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
     IF AVAIL oe-ord THEN
       BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord), NO-LOCK) .

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

       IF INDEX(cTmpField,".") > 0 THEN DO:
            cFieldName = cTmpField .
           cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
            
           IF cFieldName BEGINS "oe-ordl" THEN hField = BUFFER b-oe-ordl:BUFFER-FIELD(cTmpField) .
           ELSE IF cFieldName BEGINS "oe-ord" THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
          IF hField <> ? THEN DO:                      
           cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
               cDisplay = cDisplay + cTmpField + 
                   FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

           cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".    
          END.
          ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
        END.
          ELSE DO:         
             CASE cTmpField:
                  WHEN "cad-no" THEN cVarValue = string(itemfg.cad-no). 
                  WHEN "fg-oh" THEN cVarValue = string(itemfg.q-onh). 
                  WHEN "v-bal" THEN cVarValue = string(v-bal). 
                  WHEN "case-count" THEN cVarValue = string(v-case-count). 
                  WHEN "pallet-count" THEN cVarValue = string(v-pallet-count).
                  WHEN "skid-count" THEN cVarValue = string(v-skid-count).
                  WHEN "job" THEN cVarValue = STRING(TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")).
                  WHEN "act-rel-qty" THEN cVarValue = string(v-act-rel-qty).
                  WHEN "wip-qty" THEN cVarValue = string(v-wip-qty).
                  WHEN "pct" THEN cVarValue = string(v-pct).
                  WHEN "sman" THEN cVarValue = string(vsman).
                  WHEN "sname" THEN cVarValue = string(vsname).
                  WHEN "reldate" THEN cVarValue = string(vreldate).
                  WHEN "carrier" THEN cVarValue = string(vcarrier) .
                  WHEN "shipid" THEN cVarValue = string(vshipid) .
                  WHEN "v-prod-qty" THEN cVarValue = STRING(v-prod-qty) .
                  WHEN "approved-date" THEN cVarValue = IF oe-ord.approved-date NE ? THEN STRING(oe-ord.approved-date) ELSE ""    .
                  WHEN "sch-rel-qty" THEN cVarValue = string(dSchRelQty).
                  WHEN "ship-from" THEN cVarValue = string(cShipFr,"x(9)").
                  WHEN "ship-name" THEN cVarValue = string(cShipName,"x(30)").
             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                                   FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.
      END.
      
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.
         
    
    /*  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.*/
   END.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.

   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data Dialog-Frame 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
      IF pcCustFrom <> "" THEN ASSIGN begin_cust-no:SCREEN-VALUE = pcCustFrom.
      IF pcCustTo <> "" THEN ASSIGN end_cust-no:SCREEN-VALUE   = pcCustTo.

      IF pcItemFrom <> "" THEN ASSIGN begin_item:SCREEN-VALUE = pcItemFrom.
      IF pcItemTo <> "" THEN ASSIGN end_item:SCREEN-VALUE   = pcItemTo.

      IF pcPartFrom <> "" THEN ASSIGN begin_part:SCREEN-VALUE = pcPartFrom.
      IF pcPartTo <> "" THEN ASSIGN end_part:SCREEN-VALUE   = pcPartTo.

      IF piOrderFrom <> 0 THEN ASSIGN begin_order:SCREEN-VALUE = string(piOrderFrom).
      IF piOrderTo <> 0 THEN ASSIGN end_order:SCREEN-VALUE   = string(piOrderTo).

      IF pdDateFrom <> ? THEN ASSIGN begin_date:SCREEN-VALUE = string(pdDateFrom).
      IF pdDateTo <> ? THEN ASSIGN end_date:SCREEN-VALUE   = string(pdDateTo).
  END.


  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue Dialog-Frame 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

