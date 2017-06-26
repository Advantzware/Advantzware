&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgexp 
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
DEFINE INPUT PARAMETER lcSearch   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}

 v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
 v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

assign
 cocode = g_company
 /*locode = gloc*/ .

DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Customer,Name,Status,Address1,Address2,City,State,Zip,Email,Group,Date Added,Type,Type Dscr,Contact,Sales Rep,Sales Rep Name," +
                           "Flat Comm%,Area Code,Phone#,Broker Comm%,Fax#,Prefix,Country,Terms,Terms Dscr,Cr Acct#,Grace Days,$,Credit Rating," +
                           "Price Level,Credit Limit,Credit Hold,Order Limit,Finance Charges,Discount%,Auto Reprice,Currency,EDI,Factored,Invoice Per," +
                           "Taxable,Tax Prep Code,Tax Code,Tax Dscr,Tax Resale#,Exp,Freight Payment,FOB,Partial Ship,Location,Location Dscr,Carrier,Carrier Dscr," +
                           "Delivery Zone,Delivery Dscr,Territory,Territory Dscr,Pallet ID,Underrun%,Pallet,Overrun%,Case/Bundle,Mark-up,No Load Tags,Whse Days," +
                           "PO# Mandatory,Pallet Positions,Show Set Parts,Sales PTD,Sales YDT,Sales LYear,Cost PTD,Cost YDT,Cost LYear,Profits PTD,Profits YDT,Profits LYear," +
                           "Profit Percent PTD,Profit Percent YDT,Profit Percent LYear,Commissions PTD,Commissions YDT,Commissions LYear,MSF PTD,MSF YDT,MSF LYear," +
                           "High Balance,On,Last Payment,On Date,Total# of Inv Paid,Avg# Days to Pay,Open Orders Balance,Account Balance,On Account,Title,CPhone,Ext"

      cFieldListToSelect = "cust-no,name,active,addr[1],addr[2],city,state,zip,email,spare-char-2,date-field[1],type,custype-dscr,contact,sman,sname," +
                           "flat-comm,area-code,phone,scomm,fax,fax-prefix,fax-country,terms,terms-dscr,cr-use,cr-hold-invdays,cr-hold-invdue,cr-rating," +
                           "cust-level,cr-lim,cr-hold,ord-lim,fin-chg,disc,auto-reprice,curr-code,an-edi-cust,factored,inv-meth," +
                           "sort,spare-char-1,tax-gr,tax-dscr,tax-id,date-field[2],frt-pay,fob-code,ship-part,loc,loc-dscr,carrier,carrier-dscr," +
                           "del-zone,del-dscr,terr,terr-dscr,spare-int-1,under-pct,pallet,over-pct,case-bundle,markup,int-field[1],ship-days," +
                           "po-mand,manf-day,show-set,ptd-sales,ytd-sales,lyr-sales,cost[1],cost[5],cost[6],ptd-profit,ytd-profit,lyr-profit," +
                           "ptd-profit-pct,ytd-profit-pct,lyr-profit-pct,comm[1],comm[5],comm[6],total-msf,ytd-msf,lyytd-msf," +
                           "hibal,hibal-date,lpay,lpay-date,num-inv,avg-pay,ord-bal,acc-bal,on-account,title,cphone,ext" .
{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "Customer,Name,Status,Address1,Address2,City,State,Zip,Email,Group,Date Added,Type,Type Dscr,Contact,Salesman,Salesman Name," +
                           "Flat Comm%,Area Code,Phone#,Broker Comm%,Fax#,Prefix,Country,Terms,Terms Dscr,Cr Acct#,Grace Days,$,Credit Rating," +
                           "Price Level,Credit Limit,Credit Hold,Order Limit,Finance Charges,Discount%,Auto Reprice,Currency,EDI,Factored,Invoice Per," +
                           "Taxable,Tax Prep Code,Tax Code,Tax Dscr,Tax Resale#,Exp,Freight Payment,FOB,Partial Ship,Location,Location Dscr,Carrier,Carrier Dscr," +
                           "Delivery Zone,Delivery Dscr,Territory,Territory Dscr,Pallet ID,Underrun%,Pallet,Overrun%,Case/Bundle,Mark-up,No Load Tags,Whse Days," +
                           "PO# Mandatory,Pallet Positions,Show Set Parts,Sales PTD,Sales YDT,Sales LYear,Cost PTD,Cost YDT,Cost LYear,Profits PTD,Profits YDT,Profits LYear," +
                           "Profit Percent PTD,Profit Percent YDT,Profit Percent LYear,Commissions PTD,Commissions YDT,Commissions LYear,MSF PTD,MSF YDT,MSF LYear," +
                           "High Balance,On,Last Payment,On Date,Total# of Inv Paid,Avg# Days to Pay,Open Orders Balance,Account Balance,On Account,Title,CPhone,Ext" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust-type ~
end_cust-type tb_phone tb_notes Btn_Def sl_avail sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-type end_cust-type tb_phone ~
tb_notes sl_avail sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR )  FORWARD.

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

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
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

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "x(10)" 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-frmitm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 9.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_notes AS LOGICAL INITIAL no 
     LABEL "Print Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.2 BY 1.14 NO-UNDO.

DEFINE VARIABLE tb_phone AS LOGICAL INITIAL no 
     LABEL "Print Phone Contacts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.2 BY 1.14 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
     begin_cust-type AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 142
     end_cust-type AT ROW 3.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 144
     tb_phone AT ROW 6 COL 69.8 RIGHT-ALIGNED WIDGET-ID 146
     tb_notes AT ROW 7.38 COL 69.8 RIGHT-ALIGNED WIDGET-ID 148
     Btn_Def AT ROW 12.19 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13.33 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 14.52 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 15.71 COL 44 WIDGET-ID 136
     btn_down AT ROW 16.91 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.91 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.86 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 64.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.52 COL 3 WIDGET-ID 86
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.52 COL 9.4 WIDGET-ID 140
     RECT-6 AT ROW 10.76 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.62 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Customer to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgexp:SCROLLABLE       = FALSE
       FRAME rd-fgexp:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-type:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_notes IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_notes:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_phone IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_phone:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Customer to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-type" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-cust.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_cust-type" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Customer to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type rd-fgexp
ON LEAVE OF begin_cust-type IN FRAME rd-fgexp /* From Customer */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgexp
ON CHOOSE OF btn-ok IN FRAME rd-fgexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
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


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgexp
ON CHOOSE OF btn_down IN FRAME rd-fgexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgexp
ON CHOOSE OF btn_Up IN FRAME rd-fgexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type rd-fgexp
ON LEAVE OF end_cust-type IN FRAME rd-fgexp /* To Customer */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_notes rd-fgexp
ON VALUE-CHANGED OF tb_notes IN FRAME rd-fgexp /* Print Notes? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_phone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_phone rd-fgexp
ON VALUE-CHANGED OF tb_phone IN FRAME rd-fgexp /* Print Phone Contacts? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
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
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_cust-type.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgexp 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  
  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS:
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgexp  _DEFAULT-ENABLE
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
  DISPLAY begin_cust-type end_cust-type tb_phone tb_notes sl_avail sl_selected 
          tb_excel tb_runExcel fi_file 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_cust-type end_cust-type tb_phone tb_notes 
         Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME rd-fgexp.
  VIEW FRAME rd-fgexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgexp 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           /* ttRptSelected.FieldLength */
        .   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEFINE VARIABLE cexcelheader AS CHARACTER NO-UNDO .
DEF BUFFER b-cust FOR cust.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

FOR EACH b-cust WHERE b-cust.company = cocode
        AND b-cust.cust-no GE begin_cust-type
        AND b-cust.cust-no LE end_cust-type
        NO-LOCK:

    v-excel-detail-lines = "".

    FOR EACH ttRptSelected:
       
       v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
/*         CASE ttRptSelected.FieldList:                                                               */
/*             WHEN "itemfg.i-no" THEN                                                                 */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-no).            */
/*             WHEN "itemfg.procat" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.procat).          */
/*             WHEN "itemfg.i-name" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-name).          */
/*             WHEN "itemfg.part-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-no).         */
/*             WHEN "itemfg.est-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.est-no).          */
/*             WHEN "itemfg.item" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.item).           */
/*             WHEN "itemfg.cust-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cust-no).         */
/*             WHEN "itemfg.part-dscr1" THEN                                                           */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-dscr1).      */
/*             WHEN "itemfg.i-code" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-code).          */
/*             WHEN "itemfg.cad-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cad-no).          */
/*             WHEN "itemfg.spc-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.spc-no).          */
/*             WHEN "itemfg.stocked" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.stocked)). */
/*             WHEN "itemfg.q-onh" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.q-onh)).   */
/*         END CASE.                                                                                   */
    END.

    PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP. 
    
    IF tb_phone THEN
    FOR EACH phone WHERE phone.table_rec_key = b-cust.rec_key NO-LOCK:
        v-excel-detail-lines = "".
       FOR EACH ttRptSelected:
          IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email,CPhone,Ext,Area Code") <> 0    THEN do:
              IF ttRptSelected.TextList = "Contact" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.attention).
              IF ttRptSelected.TextList = "Title" THEN do:
                  FIND FIRST titlcode WHERE titlcode.titlcode = phone.titlcode NO-LOCK NO-ERROR.
                  IF AVAIL titlcode THEN
                      v-excel-detail-lines = v-excel-detail-lines + appendXLLine(titlcode.description).
                  ELSE
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine("").
              END.
              IF ttRptSelected.TextList = "Email" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.e_mail).
              IF ttRptSelected.TextList = "CPhone" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.phone).
              IF ttRptSelected.TextList = "Ext" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.phone_ext). 
              IF ttRptSelected.TextList = "Area Code" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.phone_city_code). 
          END.

          ELSE do:
              v-excel-detail-lines = v-excel-detail-lines + 
                  appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
          END.
        END.  /* each ttrptse */

       PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

    END. /* for each phone */
    
    IF tb_notes  THEN DO:
        FOR EACH notes NO-LOCK
            WHERE notes.rec_key EQ b-cust.rec_key 
              AND notes.note_type <> "o" BREAK BY notes.note_date  :

            cexcelheader = ",,Title,Note,Date,Time,User ID,Type,Group,Dept".
            IF FIRST(notes.note_date) THEN
            PUT STREAM excel UNFORMATTED '"' REPLACE(cexcelheader,',','","') '"' skip.

            PUT STREAM excel UNFORMATTED
               '"'              '",'
               '"'              '",'
               '"' notes.note_title '",' 
               '"' notes.note_text '",' 
               '"' notes.note_date '",'
               '"' string(notes.note_time,"hh:mm am") '",'
               '"' notes.user_id '",'
               '"' notes.note_type '",' 
               '"' notes.note_group '",'
               '"' notes.note_code '",' 
               SKIP. 
        END.
    END.

END. /* cust */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
    IF begin_cust-type:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST cust WHERE cust.company EQ cocode NO-LOCK NO-ERROR.
        begin_cust-type:SCREEN-VALUE = cust.cust-no.
        FIND LAST cust WHERE cust.company EQ cocode NO-LOCK NO-ERROR.
        end_cust-type:SCREEN-VALUE   = cust.cust-no .
    END.

    IF lcSearch NE "" THEN 
        begin_cust-type:SCREEN-VALUE = lcSearch.
    IF lcsearchby NE "" THEN 
        end_cust-type:SCREEN-VALUE = lcsearchby.

END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

  RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:
    lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
END.
/*     lc-header = lc-header + appendXLLine ("PO #").      */
/*     lc-header = lc-header + appendXLLine ("Vendor #").  */
/*     lc-header = lc-header + appendXLLine ("Due Date").  */
/*     lc-header = lc-header + appendXLLine ("Ship ID").   */
/*     lc-header = lc-header + appendXLLine ("Ship Name"). */
/*     lc-header = lc-header + appendXLLine ("Job #").     */
/*     lc-header = lc-header + appendXLLine ("Item #").    */
/*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
{custom/getperd.i} 
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR ptd-profit1 AS CHAR NO-UNDO.
    DEF VAR ytd-profit1 AS CHAR NO-UNDO.
    DEF VAR lyr-profit1 AS CHAR NO-UNDO.
    DEF VAR ptd-sales1 AS DEC NO-UNDO.

    CASE ipc-field :
        WHEN "ptd-profit" THEN DO:
            
            IF gperiod NE 0 THEN
            lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
            ELSE lc-return = "".
        END.
        WHEN "ytd-profit" THEN DO:
            lc-return = string(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
        END.
        WHEN "lyr-profit" THEN DO:
            lc-return = string(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
        END.
        WHEN "ptd-profit-pct" THEN DO:
            IF gperiod NE 0 THEN do:
            ptd-profit1 = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
            ptd-sales1 = (ipb-itemfg.sales[gperiod]).
            END.
            ELSE
                ASSIGN
                    ptd-profit1 = "0"
                    ptd-sales1 =  0.

            lc-return = string(decimal(ptd-profit1) / DECIMAL(ptd-sales1) * 100) .
        END.
        WHEN "ytd-profit-pct" THEN DO:
            /*IF ytd-profit-pct NE ? THEN*/
            ytd-profit1 = string(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
            lc-return = string(decimal(ytd-profit1) / DECIMAL(ipb-itemfg.ytd-sales) * 100) .
        END.
        WHEN "lyr-profit-pct" THEN DO:
           /* IF lyr-profit-pct NE ? THEN*/
            lyr-profit1 = string(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
            lc-return = string(decimal(lyr-profit1) / DECIMAL(ipb-itemfg.lyr-sales) * 100) .
        END.
        WHEN "ptd-sales" THEN DO:
            IF gperiod NE 0 THEN
            lc-return = string(ipb-itemfg.sales[gperiod]).
            ELSE lc-return = "" .
        END.
        WHEN "total-msf" THEN DO:
            IF gperiod NE 0 THEN
            lc-return = string(ipb-itemfg.ptd-msf[gperiod]).
            ELSE lc-return = "" .
        END.
        WHEN "sname"  THEN DO:
            FIND sman WHERE sman.company EQ ipb-itemfg.company
                AND sman.sman EQ  ipb-itemfg.sman NO-LOCK NO-ERROR.
            IF AVAIL sman THEN
                lc-return = sman.sNAME.
            ELSE
                lc-return = "".
            
        END.
        WHEN "terms-dscr"  THEN DO:
            FIND terms WHERE terms.company EQ ipb-itemfg.company
                AND terms.t-code EQ  ipb-itemfg.terms NO-LOCK NO-ERROR.
            IF AVAIL terms THEN
                lc-return = terms.dscr.
            ELSE
                lc-return = "".
        END.
         WHEN "tax-dscr"  THEN DO:
            FIND stax WHERE stax.company EQ ipb-itemfg.company
                AND stax.tax-group EQ  ipb-itemfg.tax-gr NO-LOCK NO-ERROR.
            IF AVAIL stax THEN
                lc-return = stax.tax-dscr1[1].
            ELSE
                lc-return = "".
        END.
        WHEN "custype-dscr" THEN DO:
            FIND custype WHERE custype.company EQ ipb-itemfg.company
                AND custype.custype EQ ipb-itemfg.TYPE NO-LOCK NO-ERROR.
            IF AVAIL custype THEN
                 lc-return = custype.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "loc-dscr"  THEN DO:
            FIND loc WHERE loc.company EQ ipb-itemfg.company
                AND loc.loc EQ  ipb-itemfg.loc NO-LOCK NO-ERROR.
            IF AVAIL loc THEN
                lc-return = loc.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "carrier-dscr"  THEN DO:
            FIND FIRST carrier WHERE carrier.company EQ ipb-itemfg.company
                AND carrier.carrier EQ  ipb-itemfg.carrier  NO-LOCK NO-ERROR.
 
            IF AVAIL carrier THEN
                lc-return = carrier.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "del-dscr"  THEN DO:
            FIND FIRST carr-mtx WHERE carr-mtx.company EQ ipb-itemfg.company
                AND carr-mtx.del-zone EQ  ipb-itemfg.del-zone NO-LOCK NO-ERROR.
            IF AVAIL carr-mtx THEN
                lc-return = carr-mtx.del-dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "terr-dscr"  THEN DO:
            FIND terr WHERE terr.company EQ ipb-itemfg.company
                AND terr.terr EQ  ipb-itemfg.terr NO-LOCK NO-ERROR.
            IF AVAIL terr THEN
                lc-return = terr.dscr .
            ELSE
                lc-return = "".
        END.
        WHEN "po-mand" THEN DO:
                FIND FIRST reftable WHERE reftable.reftable EQ "cust.po-mand" 
                    AND reftable.company  EQ ipb-itemfg.company
                    AND reftable.loc      EQ ""          
                    AND reftable.code     EQ ipb-itemfg.cust-no NO-ERROR.
                IF NOT AVAIL reftable THEN DO:
                    CREATE reftable.
                    ASSIGN
                        reftable.reftable = "cust.po-mand"
                        reftable.company  = ipb-itemfg.company
                        reftable.loc      = ""
                        reftable.code     = ipb-itemfg.cust-no.
                END.
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = string(reftable.val[1] EQ 1).
                ELSE 
                    reftable.val[1] = INT(ipb-itemfg.po-mand) .

        END.
        WHEN "show-set" THEN DO:
                FIND FIRST reftable WHERE reftable.reftable EQ "cust.show-set" 
                    AND reftable.company  EQ ipb-itemfg.company
                    AND reftable.loc      EQ ""          
                    AND reftable.code     EQ ipb-itemfg.cust-no NO-ERROR .
                IF NOT AVAIL reftable THEN DO:
                    CREATE reftable.
                    ASSIGN
                        reftable.reftable = "cust.show-set"
                        reftable.company  = ipb-itemfg.company
                        reftable.loc      = ""
                        reftable.code     = ipb-itemfg.cust-no
                        reftable.val[1]   = 1.
                END.
                 IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = string(reftable.val[1] EQ 1).
              /*  ELSE 
                    reftable.val[1] = INT(ipb-itemfg.show-set) .*/
           
        END.
        WHEN "flat-comm" THEN DO:
                FIND FIRST reftable WHERE reftable.reftable EQ "cust.flat-comm"
                    AND reftable.company  EQ ipb-itemfg.company
                    AND reftable.loc      EQ ""          
                    AND reftable.code     EQ ipb-itemfg.cust-no
                      NO-ERROR.     
                IF NOT AVAIL reftable THEN DO:
                    CREATE reftable.
                    ASSIGN
                        reftable.reftable = "cust5.flat-comm"
                        reftable.company  = ipb-itemfg.company
                        reftable.loc      = ""
                        reftable.code     = ipb-itemfg.cust-no.
                END.                                                                         
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = string(reftable.val[1]).
              /*  ELSE 
                    reftable.val[1] = INT(ipb-itemfg.flat-comm) . */
        END.
        WHEN "inv-meth"  THEN DO:
            CASE ipb-itemfg.inv-meth :
                WHEN NO THEN
                    lc-return = "BOL".
                WHEN Yes THEN
                    lc-return = "PO".
                OTHERWISE
                    lc-return = "Group by Date".
            END CASE.
        END.
        WHEN "sort"  THEN DO:
            CASE ipb-itemfg.SORT :
                WHEN "N" THEN
                    lc-return = "No".
                WHEN "Y" THEN
                    lc-return = "Yes".
            END CASE.
        END.
        WHEN "fob-code"  THEN DO:
            CASE ipb-itemfg.fob-code :
                WHEN "DEST" THEN
                    lc-return = "Destination".
                WHEN "ORIG" THEN
                    lc-return = "Origin".
            END CASE.
        END.
        WHEN "active" THEN DO:
            CASE ipb-itemfg.active :
                WHEN "A" THEN
                    lc-return = "Active".
                WHEN "I" THEN
                    lc-return = "Inactive".
                WHEN "X" THEN
                    lc-return = "Inhouse".
                WHEN "S" THEN
                    lc-return = "Statement".
                WHEN "E" THEN
                    lc-return = "Service".
            END CASE.
        END.
        WHEN "frt-pay" THEN DO:
            CASE ipb-itemfg.frt-pay :
                WHEN "B" THEN
                    lc-return = "Bill".
                WHEN "C" THEN
                    lc-return = "Collect".
                WHEN "P" THEN
                    lc-return = "Prepaid".
                WHEN "T" THEN
                    lc-return = "3rd Party".
            END CASE.
        END.
        WHEN "title"  THEN DO:
            lc-return = "" .
        END.
        WHEN "cphone"  THEN DO:
            lc-return = "" .
        END.
        WHEN "ext"  THEN DO:
            lc-return = "" .
        END.
        WHEN "dfuncTotMSFPTD"  THEN DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipb-itemfg.ptd-msf[g_period]).*/
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

