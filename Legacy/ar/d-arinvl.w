&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asitest167       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-poordl.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER ip-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN cocode = g_company.
ASSIGN locode = g_loc.

DEFINE VARIABLE lv-item-recid   AS RECID         NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL       NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL       NO-UNDO.
DEF VAR lv-uom-list AS cha INIT "EA,MSF,M" NO-UNDO.
DEF {&NEW} SHARED VAR g_lookup-var AS cha NO-UNDO.
{oe/oe-sysct1.i NEW}

DEF VAR ll-inquiry AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-invl ar-inv

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ar-invl.line ar-invl.actnum ~
ar-invl.i-no ar-invl.part-no ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ~
ar-invl.inv-qty ar-invl.cons-uom ar-invl.sf-sht ar-invl.unit-pr ~
ar-invl.pr-qty-uom ar-invl.disc ar-invl.amt ar-invl.amt-msf ar-invl.cost ~
ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ~
ar-invl.po-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ar-invl.line ~
ar-invl.actnum ar-invl.i-no ar-invl.part-no ar-invl.i-name ar-invl.i-dscr ~
ar-invl.lot-no ar-invl.inv-qty ar-invl.cons-uom ar-invl.sf-sht ~
ar-invl.unit-pr ar-invl.pr-qty-uom ar-invl.disc ar-invl.amt ar-invl.amt-msf ~
ar-invl.cost ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ~
ar-invl.s-comm[1] ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ~
ar-invl.sman[3] ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ~
ar-invl.ord-no ar-invl.po-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ar-invl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ar-invl
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ar-invl ~
      WHERE ASI.ar-invl.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ar-invl ~
      WHERE ASI.ar-invl.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ar-invl ar-inv
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ar-invl
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame ar-inv


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-invl.line ar-invl.actnum ar-invl.i-no ~
ar-invl.part-no ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ~
ar-invl.inv-qty ar-invl.cons-uom ar-invl.sf-sht ar-invl.unit-pr ~
ar-invl.pr-qty-uom ar-invl.disc ar-invl.amt ar-invl.amt-msf ar-invl.cost ~
ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ~
ar-invl.po-no 
&Scoped-define ENABLED-TABLES ar-invl
&Scoped-define FIRST-ENABLED-TABLE ar-invl
&Scoped-Define ENABLED-OBJECTS fi_acc-desc Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS ar-invl.line ar-invl.actnum ar-invl.i-no ~
ar-invl.part-no ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ~
ar-invl.inv-qty ar-invl.cons-uom ar-invl.sf-sht ar-invl.unit-pr ~
ar-invl.pr-qty-uom ar-invl.disc ar-invl.amt ar-invl.amt-msf ar-invl.cost ~
ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ~
ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ~
ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ~
ar-invl.po-no 
&Scoped-define DISPLAYED-TABLES ar-invl
&Scoped-define FIRST-DISPLAYED-TABLE ar-invl
&Scoped-Define DISPLAYED-OBJECTS fi_acc-desc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-itemfg-cost d-oeitem 
FUNCTION get-itemfg-cost RETURNS DECIMAL
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_acc-desc AS CHARACTER FORMAT "X(15)":U 
     LABEL "Account Desc#" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137.5 BY 3.38.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138 BY 14.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ar-invl, 
      ar-inv SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ar-invl.line AT ROW 1.95 COL 29 COLON-ALIGNED
          LABEL "Line"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ar-invl.actnum AT ROW 2.95 COL 29 COLON-ALIGNED
          LABEL "Account Number"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.i-no AT ROW 4.95 COL 29 COLON-ALIGNED
          LABEL "Item No"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.part-no AT ROW 5.95 COL 29 COLON-ALIGNED HELP
          ""
          LABEL "Cust Part #"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.i-name AT ROW 6.95 COL 29 COLON-ALIGNED
          LABEL "Item Name"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.i-dscr AT ROW 7.95 COL 29 COLON-ALIGNED
          LABEL "Item Description"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.lot-no AT ROW 8.95 COL 29 COLON-ALIGNED
          LABEL "Customer Lot#" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     ar-invl.inv-qty AT ROW 1.95 COL 85 COLON-ALIGNED
          LABEL "Invoice Qty"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     ar-invl.cons-uom AT ROW 1.95 COL 118.8 COLON-ALIGNED
          LABEL "Cons UOM"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ar-invl.sf-sht AT ROW 2.95 COL 85 COLON-ALIGNED FORMAT "->>,>>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     ar-invl.unit-pr AT ROW 3.95 COL 85 COLON-ALIGNED
          LABEL "Price"
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     ar-invl.pr-qty-uom AT ROW 3.86 COL 120.2 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ar-invl.disc AT ROW 4.95 COL 85 COLON-ALIGNED
          LABEL "Dsct%"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     ar-invl.amt AT ROW 5.95 COL 85 COLON-ALIGNED
          LABEL "Amount"
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     ar-invl.amt-msf AT ROW 6.95 COL 85 COLON-ALIGNED
          LABEL "Amt MSF#"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     ar-invl.cost AT ROW 7.95 COL 85 COLON-ALIGNED
          LABEL "Cost" FORMAT "->>>,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 25.6 BY 1
     ar-invl.dscr[1] AT ROW 7.91 COL 125 COLON-ALIGNED
          LABEL "Cost UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ar-invl.sman[1] AT ROW 11.48 COL 8.4 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.s-pct[1] AT ROW 11.48 COL 33 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.s-comm[1] AT ROW 11.48 COL 51.6 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.sman[2] AT ROW 12.48 COL 8.4 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.s-pct[2] AT ROW 12.48 COL 33 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.s-comm[2] AT ROW 12.48 COL 51.6 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.sman[3] AT ROW 13.48 COL 8.4 COLON-ALIGNED NO-LABEL FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     ar-invl.s-pct[3] AT ROW 13.48 COL 33 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.s-comm[3] AT ROW 13.48 COL 51.6 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.bol-no AT ROW 9.71 COL 86.2 COLON-ALIGNED
          LABEL "BOL #" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.ord-no AT ROW 11.05 COL 86.2 COLON-ALIGNED
          LABEL "Order #" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ar-invl.po-no AT ROW 12.38 COL 86.2 COLON-ALIGNED
          LABEL "PO #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi_acc-desc AT ROW 3.95 COL 29 COLON-ALIGNED
     Btn_OK AT ROW 17.05 COL 37
     Btn_Done AT ROW 17 COL 57
     Btn_Cancel AT ROW 17 COL 77.2
     "Sales Rep" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 10.76 COL 11.6 WIDGET-ID 2
     "% of Sales" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 10.76 COL 36 WIDGET-ID 4
     "Comm%" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 10.76 COL 54 WIDGET-ID 6
     RECT-21 AT ROW 15.71 COL 1.6
     RECT-38 AT ROW 1.1 COL 1.4
     SPACE(0.39) SKIP(3.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Customer Invoice Item Update".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ar-invl.actnum IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.amt IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.amt-msf IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.bol-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.cons-uom IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.disc IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.dscr[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.i-dscr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.i-name IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.i-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.inv-qty IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.line IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-invl.lot-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.ord-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.part-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN ar-invl.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.pr-qty-uom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-comm[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-comm[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-comm[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-pct[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-pct[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.s-pct[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.sf-sht IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ar-invl.sman[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.sman[2] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.sman[3] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-invl.unit-pr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asitest167.ar-invl,asitest167.ar-inv "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.ar-invl.company eq cocode "
     _Where[2]         = "ASI.ar-invl.company eq ar-inv.company and ASI.ar-invl.inv-no eq ar-inv.inv-no
  and ar-invl.x-no eq ar-inv.x-no OUTER-JOIN NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
    DEFINE VARIABLE char-val  AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE hlp-recid AS RECID         NO-UNDO.
    DEFINE VARIABLE lw-focus  AS WIDGET-HANDLE NO-UNDO.
    DEF VAR lv-handle AS HANDLE NO-UNDO.
    DEFINE VARIABLE op-row-id AS ROWID          NO-UNDO.
    DEFINE VARIABLE op-rec-id AS RECID NO-UNDO .

    lw-focus = FOCUS.

    CASE lw-focus:NAME :
       when "actnum" then do:
           RUN windows/l-acct.w (g_company,"", ar-invl.actnum:SCREEN-VALUE , OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN ar-invl.actnum:SCREEN-VALUE  = ENTRY(1,char-val)
                               fi_acc-desc:SCREEN-VALUE  = ENTRY(2,char-val)  .
       END.
       when "i-no" then do:
           RUN windows/l-itemfg.w (g_company, ar-inv.cust-no,ar-invl.i-no:SCREEN-VALUE , OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN ar-invl.i-no:SCREEN-VALUE  = ENTRY(1,char-val).
           RUN get-iteminfo .
           APPLY "entry" TO ar-invl.i-no .
       END.
       when "part-no" then do:
           RUN windows/l-cpart.w (g_company,ar-inv.cust-no,ar-invl.part-no:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-row-id).
           IF char-val <> "" THEN ASSIGN ar-invl.part-no:SCREEN-VALUE = ENTRY(1,char-val).
           APPLY "entry" TO ar-invl.part-no .
       END.
       when "bol-no" then do:
           RUN windows/l-bolhsp.w (g_company, ?,ar-invl.bol-no:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-rec-id).
           IF char-val <> "" THEN ASSIGN ar-invl.bol-no:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "ord-no" then do:
           RUN windows/l-ordno.w (g_company,ar-inv.cust-no ,"",ar-invl.ord-no:SCREEN-VALUE, OUTPUT char-val,OUTPUT op-rec-id).
           IF char-val <> "" THEN ASSIGN ar-invl.ord-no:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "pr-qty-uom" then do:
           RUN windows/l-stduom.w (g_company, lv-uom-list,FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       when "dscr[1]" then do:
           RUN windows/l-stduom.w (g_company,"EA,M",FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       END.
       OTHERWISE DO:
          lv-handle = focus:handle.
          run applhelp.p.             
          if g_lookup-var <> "" then do:
             lv-handle:screen-value = g_lookup-var.             
          end.   /* g_lookup-var <> "" */
          g_lookup-var = "".
       END.

    END CASE.

    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Order Release Item Update */
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    DISABLE TRIGGERS FOR LOAD OF ar-invl .

    IF lv-item-recid NE ? THEN DO:

       FIND ar-invl EXCLUSIVE-LOCK
            WHERE RECID(ar-invl) EQ lv-item-recid  NO-ERROR.
       IF AVAILABLE ar-invl THEN DELETE ar-invl .
    END.
    APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
    DEFINE VARIABLE dOldAmount AS DECIMAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:

     IF ar-invl.actnum:MODIFIED  THEN DO:
       FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.actnum.
          RETURN NO-APPLY.
       END.
     END.

     IF ar-invl.bol-no:MODIFIED 
         AND ar-invl.bol-no:SCREEN-VALUE NE "0" THEN DO:
       /* Bol# check */ 
       FIND FIRST oe-boll NO-LOCK 
           WHERE oe-boll.company EQ g_company 
             AND oe-boll.bol-no  EQ INTEGER(ar-invl.bol-no:SCREEN-VALUE)
             AND oe-boll.i-no    EQ ar-invl.i-no:SCREEN-VALUE
           NO-ERROR.
       IF NOT AVAIL oe-boll THEN DO:
          MESSAGE "Invalid Bol # for item#." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.
       END.
       
       IF INTEGER(ar-invl.ord-no:SCREEN-VALUE) NE oe-boll.ord-no THEN DO:
          MESSAGE "Invalid Bol # for item#." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.       
       END.
       IF ar-invl.po-no:SCREEN-VALUE NE oe-boll.po-no THEN DO:
          MESSAGE "Invalid PO# for this BOL." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-invl.bol-no.
          RETURN NO-APPLY.       
       END.       
     END.

     IF LOOKUP(ar-invl.pr-qty-uom:SCREEN-VALUE  ,lv-uom-list) <= 0 THEN DO:
         MESSAGE "Invalid Unit of Measure." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ar-invl.pr-qty-uom.
         RETURN NO-APPLY.
     END.
     IF LOOKUP(ar-invl.dscr[1]:SCREEN-VALUE ,"EA,M") <= 0 THEN DO:
         MESSAGE "Invalid Unit of Measure. EA or M is valid." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ar-invl.pr-qty-uom.
         RETURN NO-APPLY.
      END.
    END.

 ASSIGN dOldAmount = IF AVAIL ar-invl THEN ar-invl.amt ELSE 0 .
  DO TRANSACTION:
      FIND CURRENT ar-invl EXCLUSIVE-LOCK NO-ERROR.
      
      DO WITH FRAME {&FRAME-NAME}:
          ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
      END.
      RUN update-ar-invl(dOldAmount) .
  END.

 FIND CURRENT ar-invl NO-LOCK NO-ERROR.
  ip-rowid = ROWID(ar-invl).

APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME ar-invl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.actnum Dialog-Frame
ON LEAVE OF ar-invl.actnum IN FRAME Dialog-Frame /* Account Number */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.actnum:SCREEN-VALUE GT ""  THEN DO:
       FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       fi_acc-desc:SCREEN-VALUE  = account.dscr.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.i-no Dialog-Frame
ON LEAVE OF ar-invl.i-no IN FRAME Dialog-Frame /* Item No */
DO:
      IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.i-no:MODIFIED THEN DO:
       FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company = g_company 
               AND itemfg.i-no = ar-invl.i-no:SCREEN-VALUE
             NO-ERROR.
       IF NOT AVAIL itemfg THEN DO:
          MESSAGE "Invalid FG Item Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       ASSIGN
         ar-invl.i-dscr:SCREEN-VALUE  = itemfg.part-dscr1
         ar-invl.i-name:SCREEN-VALUE  = itemfg.i-name .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ar-invl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.i-no Dialog-Frame
ON VALUE-CHANGED OF ar-invl.i-no IN FRAME Dialog-Frame /* Item No */
DO:
      IF LASTKEY = -1 THEN RETURN.
       FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company = g_company 
               AND itemfg.i-no = ar-invl.i-no:SCREEN-VALUE
             NO-ERROR.
       IF AVAIL itemfg THEN DO:
           ASSIGN
         ar-invl.i-dscr:SCREEN-VALUE  = itemfg.part-dscr1
         ar-invl.i-name:SCREEN-VALUE  = itemfg.i-name 
         ar-invl.unit-pr:SCREEN-VALUE  = string(itemfg.sell-price)
         ar-invl.cost:screen-value   = STRING(get-itemfg-cost(itemfg.i-no)).
           FIND FIRST fgcat NO-LOCK 
               WHERE fgcat.company EQ itemfg.company 
                 AND fgcat.procat EQ  itemfg.procat NO-ERROR .
           IF AVAIL fgcat THEN
               ASSIGN ar-invl.actnum:SCREEN-VALUE  = fgcat.glacc .

           FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
           IF AVAIL account  THEN
           fi_acc-desc:SCREEN-VALUE  = account.dscr.
           
       END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   


&Scoped-define SELF-NAME ar-invl.inv-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.inv-qty Dialog-Frame
ON VALUE-CHANGED OF ar-invl.inv-qty IN FRAME Dialog-Frame /* quantity */
DO:
      IF LASTKEY = -1 THEN RETURN.
       RUN pCalcAmtMsf .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ar-invl.unit-pr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROLar-invl.unit-pr Dialog-Frame
ON VALUE-CHANGED OF ar-invl.unit-pr IN FRAME Dialog-Frame /* price */
DO:
      IF LASTKEY = -1 THEN RETURN.
       RUN pCalcAmtMsf .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.pr-qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.pr-qty-uom Dialog-Frame
ON LEAVE OF ar-invl.pr-qty-uom IN FRAME Dialog-Frame /* UOM */
DO:
    IF LASTKEY = -1 THEN RETURN.
    IF LOOKUP(ar-invl.pr-qty-uom:SCREEN-VALUE ,lv-uom-list) <= 0 THEN DO:
       MESSAGE "Invalid Unit of Measure." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.dscr[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.dscr[1] Dialog-Frame
ON LEAVE OF ar-invl.dscr[1] IN FRAME Dialog-Frame /* Cost!UOM */
DO:
   IF LASTKEY = -1 THEN RETURN.
    IF LOOKUP(ar-invl.dscr[1]:SCREEN-VALUE ,"EA,M") <= 0 THEN DO:
       MESSAGE "Invalid Unit of Measure.  EA or M is valid." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[1] Dialog-Frame
ON HELP OF ar-invl.sman[1] IN FRAME Dialog-Frame /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[1]:SCREEN-VALUE  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[1] Dialog-Frame
ON LEAVE OF ar-invl.sman[1] IN FRAME Dialog-Frame /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[1]:MODIFIED  THEN DO:

      IF ar-invl.sman[1]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[1]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[2] Dialog-Frame
ON HELP OF ar-invl.sman[2] IN FRAME Dialog-Frame /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[2]:SCREEN-VALUE = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[2] Dialog-Frame
ON LEAVE OF ar-invl.sman[2] IN FRAME Dialog-Frame /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[2]:MODIFIED  THEN DO:

      IF ar-invl.sman[2]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[2]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.sman[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[3] Dialog-Frame
ON HELP OF ar-invl.sman[3] IN FRAME Dialog-Frame /* SlsRep */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-sman2.w (g_company, output char-val).
   if char-val ne "" THEN
      ar-invl.sman[3]:SCREEN-VALUE  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.sman[3] Dialog-Frame
ON LEAVE OF ar-invl.sman[3] IN FRAME Dialog-Frame /* SlsRep */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF ar-invl.sman[3]:MODIFIED  THEN DO:

      IF ar-invl.sman[3]:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST sman WHERE
         sman.company EQ g_company AND
         sman.sman EQ ar-invl.sman[3]:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.bol-no Dialog-Frame
ON LEAVE OF ar-invl.bol-no IN FRAME Dialog-Frame /* Bill Of Lading Number */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.bol-no:MODIFIED  
     AND ar-invl.bol-no:SCREEN-VALUE GT "" THEN DO:
       FIND FIRST oe-bolh WHERE oe-bolh.company = g_company AND                                
                                oe-bolh.bol-no = INTEGER(ar-invl.bol-no:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL oe-bolh THEN DO:
          MESSAGE "Invalid BOL Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       ELSE DO:
        find FIRST oe-boll no-lock 
         WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.bol-no EQ oe-bolh.bol-no
          and oe-boll.i-no EQ ar-invl.i-no:screen-value
         no-error.
        IF avail oe-boll and INTEGER(ar-invl.ord-no:SCREEN-VALUE) EQ 0 THEN
         assign
         ar-invl.ord-no:screen-value = string(oe-boll.ord-no)
         ar-invl.po-no:screen-value = string(oe-boll.po-no)
         .
        if avail oe-boll then
         find first oe-ordl no-lock 
          where oe-ordl.company eq oe-boll.company
            and oe-ordl.ord-no eq oe-boll.ord-no
            and oe-ordl.i-no eq oe-boll.i-no
          no-error.
          if avail oe-ordl then
         ar-invl.part-no:screen-value = oe-ordl.part-no
         .
       END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-invl.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-invl.ord-no Dialog-Frame
ON LEAVE OF ar-invl.ord-no IN FRAME Dialog-Frame /* Order# */
DO:
      IF LASTKEY = -1 THEN RETURN.

    IF ar-invl.ord-no:MODIFIED 
     AND ar-invl.ord-no:screen-value NE "0" THEN DO:
       FIND FIRST oe-ord WHERE oe-ord.company = g_company AND                                
                                oe-ord.ord-no = INTEGER(ar-invl.ord-no:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL oe-ord THEN DO:
          MESSAGE "Invalid Order Number." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND ar-inv NO-LOCK
        WHERE ar-inv.company EQ cocode
          AND RECID(ar-inv)  EQ ip-recid2 NO-ERROR .
    
    IF ip-recid EQ ? THEN DO:
        RUN create-item.
    END.
    ELSE FIND ar-invl NO-LOCK WHERE RECID(ar-invl) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN ll-order-warned                     = NO.
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
            btn_done:SENSITIVE                        = YES.
            btn_ok:HIDDEN                             = YES.
            btn_cancel:HIDDEN                         = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        
        RUN oe/oe-sysct.p.

        IF NOT v-oecomm-log THEN RUN show-comm (NO).

        DISABLE fi_acc-desc ar-invl.cons-uom ar-invl.disc ar-invl.amt ar-invl.amt-msf.

        IF ip-type EQ "add"  OR ip-type EQ "update" THEN DO:
            APPLY "entry" TO ar-invl.LINE  .
        END.
        
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ar-invl Dialog-Frame 
PROCEDURE update-ar-invl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipAmount AS DECIMAL NO-UNDO .
  DEF VAR out-qty LIKE ar-invl.qty NO-UNDO.
  DEF BUFFER bf-inv FOR ar-inv.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND bf-inv WHERE RECID(bf-inv) = RECID(ar-inv) .
  
  IF  ip-type EQ "update"  THEN do:  /* update */
  
    ASSIGN bf-inv.gross = bf-inv.gross - ipAmount
           bf-inv.net   = bf-inv.net - ipAmount .
  END.
  
  ar-invl.qty = ar-invl.inv-qty.

  run sys/ref/convsuom.p (ar-invl.cons-uom,
                          ar-invl.pr-qty-uom,
                          ar-invl.sf-sht,
                          ar-invl.qty,
                          OUTPUT out-qty).

  assign
   ar-invl.amt     = if   (out-qty * ar-invl.unit-pr) eq 0
                     then (ar-invl.qty * ar-invl.unit-pr)
                     else (out-qty * ar-invl.unit-pr)
   ar-invl.amt-msf = ((ar-invl.qty * ar-invl.sf-sht) / 1000.0)
   bf-inv.gross    = bf-inv.gross + ar-invl.amt
   bf-inv.net      = bf-inv.net + ar-invl.amt.
 
   find first cust where cust.company eq g_company
                      and cust.cust-no eq ar-inv.cust-no no-lock no-error.
   ar-invl.tax = if ar-inv.tax-code ne "" and cust.sort eq "Y" then YES ELSE NO.
 
  IF ar-invl.bol-no GT 0 
    AND ar-invl.b-no EQ 0 THEN DO:
    FIND FIRST oe-bolh NO-LOCK
      WHERE oe-bolh.company EQ ar-invl.company
        AND oe-bolh.bol-no EQ ar-invl.bol-no 
      NO-ERROR.
    IF AVAIL oe-bolh THEN DO:
         
      ar-invl.b-no = oe-bolh.b-no.
      
    END.
      
  END.
  {ar/ar-invk.i bf-inv}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    DEF BUFFER bf-invl FOR ar-invl.
    DEF VAR li-next-line AS INT NO-UNDO.

    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ cocode
        AND RECID(ar-inv) EQ ip-recid2 
        NO-ERROR.

    IF AVAILABLE ar-inv THEN 
    DO WITH FRAME {&FRAME-NAME}:

         FIND LAST bf-invl WHERE bf-invl.x-no = ar-inv.x-no USE-INDEX x-no NO-LOCK NO-ERROR.
         li-next-line = IF AVAIL bf-invl THEN bf-invl.LINE + 1 ELSE 1.

        
        CREATE ar-invl.

        FIND FIRST ar-ctrl WHERE ar-ctrl.company = ar-inv.company NO-LOCK NO-ERROR.

        FIND FIRST cust WHERE
            cust.company EQ ar-inv.company AND
            cust.cust-no EQ ar-inv.cust-no
            NO-LOCK NO-ERROR.
        
        ASSIGN ar-invl.x-no = ar-inv.x-no
            ar-invl.company = ar-inv.company
            ar-invl.cust-no = ar-inv.cust-no
            ar-invl.inv-no = ar-inv.inv-no
            ar-invl.LINE = li-next-line
            ar-invl.po-no = ar-inv.po-no
            ar-invl.pr-qty-uom = "EA"
            ar-invl.cons-uom = "EA"
            ar-invl.dscr[1] = "EA"
            /*ar-invl.actnum = IF AVAIL ar-ctrl THEN ar-ctrl.sales ELSE ""*/
            ar-invl.sman[1] = IF AVAIL cust THEN cust.sman ELSE ""
            ar-invl.s-pct[1] = IF ar-invl.sman[1] NE "" THEN 100 ELSE 0.

        ASSIGN lv-item-recid = RECID(ar-invl).
            ll-new-record = YES.
            FIND CURRENT ar-invl NO-LOCK NO-ERROR .
        
    END. /* avail ar-inv */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF AVAILABLE ar-invl  THEN 
    DO:
        FIND FIRST account WHERE account.company = g_company
                          AND account.actnum = ar-invl.actnum NO-LOCK NO-ERROR.
     
        IF AVAILABLE account THEN
            ASSIGN fi_acc-desc = account.dscr .

        DISPLAY  ar-invl.line ar-invl.actnum ar-invl.i-no ar-invl.part-no 
               ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ar-invl.inv-qty 
               ar-invl.sf-sht ar-invl.unit-pr ar-invl.pr-qty-uom ar-invl.disc 
               ar-invl.amt ar-invl.amt-msf ar-invl.cost ar-invl.cons-uom
               ar-invl.dscr[1] ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1]
               ar-invl.sman[2] ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] 
               ar-invl.s-pct[3] ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no
               ar-invl.po-no  fi_acc-desc 
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm Dialog-Frame 
PROCEDURE show-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-visible AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ar-invl.s-pct[1]:VISIBLE   = ip-visible
     ar-invl.s-pct[2]:VISIBLE   = ip-visible
     ar-invl.s-pct[3]:VISIBLE   = ip-visible
     ar-invl.s-comm[1]:VISIBLE  = ip-visible
     ar-invl.s-comm[2]:VISIBLE  = ip-visible
     ar-invl.s-comm[3]:VISIBLE  = ip-visible.
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
  DISPLAY fi_acc-desc 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ar-invl THEN 
    DISPLAY ar-invl.line ar-invl.actnum ar-invl.i-no ar-invl.part-no 
          ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ar-invl.inv-qty 
          ar-invl.cons-uom ar-invl.sf-sht ar-invl.unit-pr ar-invl.pr-qty-uom 
          ar-invl.disc ar-invl.amt ar-invl.amt-msf ar-invl.cost ar-invl.dscr[1] 
          ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ar-invl.sman[2] 
          ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ar-invl.s-pct[3] 
          ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ar-invl.po-no 
      WITH FRAME Dialog-Frame.
  ENABLE ar-invl.line ar-invl.actnum ar-invl.i-no ar-invl.part-no 
         ar-invl.i-name ar-invl.i-dscr ar-invl.lot-no ar-invl.inv-qty 
         ar-invl.cons-uom ar-invl.sf-sht ar-invl.unit-pr ar-invl.pr-qty-uom 
         ar-invl.disc ar-invl.amt ar-invl.amt-msf ar-invl.cost ar-invl.dscr[1] 
         ar-invl.sman[1] ar-invl.s-pct[1] ar-invl.s-comm[1] ar-invl.sman[2] 
         ar-invl.s-pct[2] ar-invl.s-comm[2] ar-invl.sman[3] ar-invl.s-pct[3] 
         ar-invl.s-comm[3] ar-invl.bol-no ar-invl.ord-no ar-invl.po-no 
         fi_acc-desc Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-iteminfo Dialog-Frame 
PROCEDURE get-iteminfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
   FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company = g_company 
               AND itemfg.i-no = ar-invl.i-no:SCREEN-VALUE
             NO-ERROR.
       IF AVAIL itemfg THEN DO:
           ASSIGN
         ar-invl.i-dscr:SCREEN-VALUE  = itemfg.part-dscr1
         ar-invl.i-name:SCREEN-VALUE  = itemfg.i-name 
         ar-invl.unit-pr:SCREEN-VALUE  = string(itemfg.sell-price)
         ar-invl.cost:screen-value   = STRING(get-itemfg-cost(itemfg.i-no)).
           FIND FIRST fgcat NO-LOCK 
               WHERE fgcat.company EQ itemfg.company 
                 AND fgcat.procat EQ  itemfg.procat NO-ERROR .
           IF AVAIL fgcat THEN
               ASSIGN ar-invl.actnum:SCREEN-VALUE  = fgcat.glacc .

           FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-invl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
           IF AVAIL account  THEN
           fi_acc-desc:SCREEN-VALUE  = account.dscr.
           
       END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcAmtMsf Dialog-Frame 
PROCEDURE pCalcAmtMsf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR out-qty LIKE ar-invl.qty NO-UNDO.
  
DO WITH FRAME {&FRAME-NAME}:

  run sys/ref/convsuom.p (ar-invl.cons-uom:SCREEN-VALUE,
                          ar-invl.pr-qty-uom:SCREEN-VALUE,
                          ar-invl.sf-sht:SCREEN-VALUE,
                          ar-invl.inv-qty:SCREEN-VALUE,
                          OUTPUT out-qty).

  assign
      ar-invl.amt:SCREEN-VALUE     = if   (out-qty * DECIMAL(ar-invl.unit-pr:SCREEN-VALUE)) eq 0
                                    then STRING(decimal(ar-invl.inv-qty:SCREEN-VALUE) * decimal(ar-invl.unit-pr:SCREEN-VALUE))
                                    else STRING(out-qty * decimal(ar-invl.unit-pr:SCREEN-VALUE)) .
      ar-invl.amt-msf:SCREEN-VALUE = STRING((integer(ar-invl.inv-qty:SCREEN-VALUE) * decimal(ar-invl.sf-sht:SCREEN-VALUE)) / 1000.0)
   .
 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-itemfg-cost d-oeitem 
FUNCTION get-itemfg-cost RETURNS DECIMAL
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER bfItemfg FOR itemfg.
  DEF VAR v-cost AS DEC NO-UNDO.
  v-cost = 0.
  FIND FIRST bfItemfg WHERE bfItemfg.company = cocode
                        AND bfItemfg.i-no    = ipv-item
                      NO-LOCK NO-ERROR.
  IF AVAIL(bfItemfg) THEN
    v-cost = bfItemfg.total-std-cost.
  FIND FIRST fg-ctrl WHERE fg-ctrl.company = cocode NO-LOCK NO-ERROR.
  IF AVAIL fg-ctrl THEN DO:
    IF fg-ctrl.inv-meth = "A" AND bfItemfg.avg-cost GT 0 THEN
      v-cost = bfItemfg.avg-cost.
    ELSE
      IF fg-ctrl.inv-meth = "L" AND bfItemfg.last-cost GT 0 THEN
          v-cost = bfItemfg.last-cost.
  END.
  RETURN v-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

