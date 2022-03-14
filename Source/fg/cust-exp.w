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
DEFINE INPUT PARAMETER lcSearch   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
DEFINE {&NEW} SHARED VARIABLE g_batch       AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS ROWID   NO-UNDO.
DEFINE               VARIABLE v-prgmname    LIKE prgrms.prgmname NO-UNDO.
DEFINE               VARIABLE lActive       AS LOGICAL NO-UNDO .
DEFINE               VARIABLE ou-log        AS LOGICAL NO-UNDO .
{sys/inc/var.i new shared}

v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

ASSIGN
    cocode = g_company
    /*locode = gloc*/ .

DO TRANSACTION:
    {sys/ref/CustList.i NEW}
END.

DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
     
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/* Removed Prep Tax Group - 16.8.9 - Tkt#48289 - MYT - 4/30/19 */
ASSIGN 
    cTextListToSelect  = "Customer,Name,Status,Address1,Address2,City,State,Zip,Email,Group,Date Added,Type,Type Dscr,Contact,Sales Rep,Sales Rep Name," +
                           "Flat Comm%,Area Code,Phone#,Broker Comm%,Fax#,Prefix,Country,Terms,Terms Dscr,Cr Acct#,Grace Days,$,Credit Rating," +
                           "Price Level,Credit Limit,Credit Hold,Order Limit,Finance Charges,Discount%,Auto Reprice,Currency,EDI,Factored,Invoice Per," +
                           "Taxable,Tax Prep Code,Tax Group,Tax Dscr,Tax Resale#,Exp,Freight Payment,FOB,Partial Ship,Location,Location Dscr,Carrier,Carrier Dscr," +
                           "Delivery Zone,Delivery Dscr,Territory,Territory Dscr,Pallet ID,Underrun%,Pallet,Overrun%,Case/Bundle,Mark-up,No Load Tags,Whse Days," +
                           "PO# Mandatory,Pallet Positions,Show Set Parts,Sales PTD,Sales YDT,Sales LYear,Cost PTD,Cost YDT,Cost LYear,Profits PTD,Profits YDT,Profits LYear," +
                           "Profit Percent PTD,Profit Percent YDT,Profit Percent LYear,Commissions PTD,Commissions YDT,Commissions LYear,MSF PTD,MSF YDT,MSF LYear," +
                           "High Balance,On,Last Payment,On Date,Total# of Inv Paid,Avg# Days to Pay,Open Orders Balance,Account Balance,On Account,Title,CPhone,Ext,CSR," +
                           "Note 1,Note 2,Note 3,Note 4,ShipTo Name,ShipTo Address 1,ShipTo Address 2,ShipTo City,ShipTo State,ShipTo Zip,Paperless Invoice?,Contract Pricing," +
                           "Bank Account,Swift Code,Routing,Account Type,Split Type,Parent Cust,Market segment,NAICS Code,AR ClassId,Accountant,Matrix Precision,Matrix Rounding," +
                           "Industry,Tag Status,Internal"

    cFieldListToSelect = "cust-no,name,active,addr[1],addr[2],city,state,zip,email,spare-char-2,date-field[1],type,custype-dscr,contact,sman,sname," +
                           "flat-comm,area-code,phone,scomm,fax,fax-prefix,fax-country,terms,terms-dscr,cr-use,cr-hold-invdays,cr-hold-invdue,cr-rating," +
                           "cust-level,cr-lim,cr-hold,ord-lim,fin-chg,disc,auto-reprice,curr-code,an-edi-cust,factored,inv-meth," +
                           "sort,spare-char-1,tax-gr,tax-dscr,tax-id,date-field[2],frt-pay,fob-code,ship-part,loc,loc-dscr,carrier,carrier-dscr," +
                           "del-zone,del-dscr,terr,terr-dscr,spare-int-1,under-pct,pallet,over-pct,case-bundle,markup,int-field[1],ship-days," +
                           "po-mand,manf-day,show-set,ptd-sales,ytd-sales,lyr-sales,cost[1],cost[5],cost[6],ptd-profit,ytd-profit,lyr-profit," +
                           "ptd-profit-pct,ytd-profit-pct,lyr-profit-pct,comm[1],comm[5],comm[6],total-msf,ytd-msf,lyytd-msf," +
                           "hibal,hibal-date,lpay,lpay-date,num-inv,avg-pay,ord-bal,acc-bal,on-account,title,cphone,ext,csrUser_id," +
                           "note1,note2,note3,note4,ship-name,ship-addr1,ship-addr2,ship-city,ship-state,ship-zip,log-field[1],cnt-price," +
                           "bank-acct,SwiftBIC,Bank-RTN,accountType,splitType,parentCust,marketSegment,naicsCode,classId,accountant,matrixPrecision,matrixRounding," +
                           "industryID,tag-status,Internal" .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Customer,Name,Address1,Address2,City,State,Country,Zip,Sales Rep,Area Code,Phone#," +
                                 "Fax#,Credit Limit,Status,Credit Hold,Type,Terms,Tax Resale#,Note 1,Note 2,Note 3,Note 4," +
                                 "ShipTo Name,ShipTo Address 1,ShipTo Address 2,ShipTo City,ShipTo State,ShipTo Zip," +
                                 "Contact,Date Added,CSR,Cr Acct#,Credit Rating,Order Limit,Discount%,Currency,Finance Charges," +
                                 "Auto Reprice,EDI,Factored,Grace Days,$,Invoice Per,Freight Payment,FOB,Location,Carrier,Delivery Zone," + 
                                 "Territory,Pallet ID,Overrun%,Underrun%,Pallet,Case/Bundle,Mark-up,No Load Tags,Whse Days,Pallet Positions," +
                                 "PO# Mandatory,Show Set Parts,Paperless Invoice?,Partial Ship,Taxable,Tax Prep Code,Tax Group,Tax Resale#,Exp," +
                                 "Email,Group,Broker Comm%,Flat Comm%,Prefix,Contract Pricing,Bank Account,Swift Code,Routing,Account Type," + 
                                 "Split Type,Parent Cust,Market segment,NAICS Code,AR ClassId,Accountant,Matrix Precision,Matrix Rounding," +
                                 "Industry,Tag Status,Internal" .

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
end_cust-type tb_phone tb_notes sl_avail sl_selected Btn_Def Btn_Add ~
Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-type end_cust-type tb_phone ~
tb_notes sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "x(10)" 
    LABEL "From Customer" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_cust-type   AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
    LABEL "To Customer" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\CustomerExport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 52 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 6.38.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_notes    AS LOGICAL   INITIAL NO 
    LABEL "Print Notes?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29.2 BY 1.14 NO-UNDO.

DEFINE VARIABLE tb_phone    AS LOGICAL   INITIAL NO 
    LABEL "Print Phone Contacts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29.2 BY 1.14 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
    begin_cust-type AT ROW 2.91 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 142
    end_cust-type AT ROW 2.91 COL 68 COLON-ALIGNED HELP
    "Enter Ending Customer" WIDGET-ID 144
    tb_phone AT ROW 4.95 COL 66.8 RIGHT-ALIGNED WIDGET-ID 146
    tb_notes AT ROW 6.33 COL 66.8 RIGHT-ALIGNED WIDGET-ID 148
    sl_avail AT ROW 9.43 COL 7 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 9.43 COL 62.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 9.57 COL 42.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 10.71 COL 42.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 11.91 COL 42.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 13.1 COL 42.2 WIDGET-ID 136
    btn_down AT ROW 14.29 COL 42.2 WIDGET-ID 132
    fi_file AT ROW 17.1 COL 18.8 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 17.19 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 19 COL 42 WIDGET-ID 60
    tb_excel AT ROW 19.1 COL 4 WIDGET-ID 32
    btn-ok AT ROW 19.81 COL 33.4 WIDGET-ID 14
    btn-cancel AT ROW 19.81 COL 53.4 WIDGET-ID 12
    "Selected Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8.71 COL 69.4 WIDGET-ID 138
    "Available Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8.71 COL 13.4 WIDGET-ID 140
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 8.05 COL 5 WIDGET-ID 86
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    RECT-6 AT ROW 8.38 COL 4 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 16.48 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(2.98)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
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
    FRAME rd-fgexp:SCROLLABLE = FALSE
    FRAME rd-fgexp:HIDDEN     = TRUE.

ASSIGN 
    begin_cust-type:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    end_cust-type:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-fgexp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_notes IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_notes:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_phone IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_phone:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Customer to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

            WHEN "begin_cust-type" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg */
            WHEN "end_cust-type" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg*/

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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
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
  
        ASSIGN 
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
        RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
        fi_file:SCREEN-VALUE =  cFileName.
  
        RUN GetSelectionList.  
  
        RUN run-report.
  
        IF NOT tb_OpenCSV THEN 
        DO:        
            MESSAGE "CSV file have been created." SKIP(1)
                "~"OK"~" to open CSV file?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                TITLE "" UPDATE lChoice AS LOGICAL.
            IF lChoice THEN
            DO:
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
            END.
        END.  /* IF NOT tb_OpenCSV THEN */
  
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON HELP OF fi_file IN FRAME rd-fgexp /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* Name */
    DO:
        ASSIGN {&self-name}.
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
    DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_notes rd-fgexp
ON VALUE-CHANGED OF tb_notes IN FRAME rd-fgexp /* Print Notes? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_phone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_phone rd-fgexp
ON VALUE-CHANGED OF tb_phone IN FRAME rd-fgexp /* Print Phone Contacts? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgexp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

 
    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'AF1',
        INPUT YES,
        OUTPUT lActive).
    {sys/inc/chblankcust.i ""AF1""} 

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
RUN Set-Sort-Data.

APPLY "entry" TO begin_cust-type.
fi_file:SCREEN-VALUE = "c:\tmp\CustomerExport.csv".
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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
     
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
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmpList      AS CHARACTER NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
     
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
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
        fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME rd-fgexp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_cust-type end_cust-type tb_phone tb_notes 
        sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList  = ENTRY(i,cTmpList)
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
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
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
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cexcelheader         AS CHARACTER NO-UNDO .
    DEFINE BUFFER b-cust FOR cust.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FOR EACH b-cust WHERE b-cust.company = cocode
        AND b-cust.cust-no GE begin_cust-type
        AND b-cust.cust-no LE end_cust-type
        AND ( LOOKUP(b-cust.cust-no,custcount) <> 0 OR custcount = "")
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
                    IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email,CPhone,Ext,Area Code") <> 0    THEN 
                    DO:
                        IF ttRptSelected.TextList = "Contact" THEN
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(phone.attention).
                        IF ttRptSelected.TextList = "Title" THEN 
                        DO:
                            FIND FIRST titlcode WHERE titlcode.titlcode = phone.titlcode NO-LOCK NO-ERROR.
                            IF AVAILABLE titlcode THEN
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

                    ELSE 
                    DO:
                        v-excel-detail-lines = v-excel-detail-lines + 
                            appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
                    END.
                END.  /* each ttrptse */

                PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

            END. /* for each phone */
    
        IF tb_notes  THEN 
        DO:
            FOR EACH notes NO-LOCK
                WHERE notes.rec_key EQ b-cust.rec_key 
                AND notes.note_type <> "o" BREAK BY notes.note_date  :

                cexcelheader = ",,Title,Note,Date,Time,User ID,Type,Group,Dept".
                IF FIRST(notes.note_date) THEN
                    PUT STREAM excel UNFORMATTED '"' REPLACE(cexcelheader,',','","') '"' SKIP.

                PUT STREAM excel UNFORMATTED
                    '"'              '",'
                    '"'              '",'
                    '"' notes.note_title '",' 
                    '"' notes.note_text '",' 
                    '"' notes.note_date '",'
                    '"' STRING(notes.note_time,"hh:mm am") '",'
                    '"' notes.user_id '",'
                    '"' notes.note_type '",' 
                    '"' notes.note_group '",'
                    '"' notes.note_code '",' 
                    SKIP. 
            END.
        END.

    END. /* cust */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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
        IF begin_cust-type:SCREEN-VALUE EQ "" THEN 
        DO:
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
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.
    
    ipc-append = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, ipc-append).
    
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER.

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
    DEFINE VARIABLE lc-header AS CHARACTER NO-UNDO.

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
    ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    {custom/getperd.i} 
    DEFINE VARIABLE h-field     AS HANDLE.
    DEFINE VARIABLE li-extent   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return   AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE VARIABLE ptd-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ytd-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lyr-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ptd-sales1  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cShipNote   AS CHARACTER FORMAT "x(80)" EXTENT 4 NO-UNDO.

    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipb-itemfg.company
        AND shipto.cust-no EQ ipb-itemfg.cust-no
        AND shipto.ship-id EQ ipb-itemfg.cust-no
        NO-ERROR.
    ASSIGN 
        i = 1.
    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ ipb-itemfg.rec_key 
        AND notes.note_type <> "o" BREAK BY notes.note_date  :
        cShipNote[i] = IF notes.note_text <> "" THEN notes.note_text ELSE "" .
        i = i + 1.
        IF i EQ 5 THEN LEAVE.
    END.
    
    CASE ipc-field :
        WHEN "ptd-profit" THEN 
            DO:
            
                IF gperiod NE 0 THEN
                    lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
                ELSE lc-return = "".
            END.
        WHEN "ytd-profit" THEN 
            DO:
                lc-return = STRING(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
            END.
        WHEN "lyr-profit" THEN 
            DO:
                lc-return = STRING(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
            END.
        WHEN "ptd-profit-pct" THEN 
            DO:
                IF gperiod NE 0 THEN 
                DO:
                    ptd-profit1 = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
                    ptd-sales1 = (ipb-itemfg.sales[gperiod]).
                END.
                ELSE
                    ASSIGN
                        ptd-profit1 = "0"
                        ptd-sales1  = 0.

                lc-return = STRING(DECIMAL(ptd-profit1) / DECIMAL(ptd-sales1) * 100) .
            END.
        WHEN "ytd-profit-pct" THEN 
            DO:
                /*IF ytd-profit-pct NE ? THEN*/
                ytd-profit1 = STRING(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
                lc-return = STRING(DECIMAL(ytd-profit1) / DECIMAL(ipb-itemfg.ytd-sales) * 100) .
            END.
        WHEN "lyr-profit-pct" THEN 
            DO:
                /* IF lyr-profit-pct NE ? THEN*/
                lyr-profit1 = STRING(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
                lc-return = STRING(DECIMAL(lyr-profit1) / DECIMAL(ipb-itemfg.lyr-sales) * 100) .
            END.
        WHEN "ptd-sales" THEN 
            DO:
                IF gperiod NE 0 THEN
                    lc-return = STRING(ipb-itemfg.sales[gperiod]).
                ELSE lc-return = "" .
            END.
        WHEN "total-msf" THEN 
            DO:
                IF gperiod NE 0 THEN
                    lc-return = STRING(ipb-itemfg.ptd-msf[gperiod]).
                ELSE lc-return = "" .
            END.
        WHEN "sname"  THEN 
            DO:
                FIND sman WHERE sman.company EQ ipb-itemfg.company
                    AND sman.sman EQ  ipb-itemfg.sman NO-LOCK NO-ERROR.
                IF AVAILABLE sman THEN
                    lc-return = sman.sNAME.
                ELSE
                    lc-return = "".
            
            END.
        WHEN "terms-dscr"  THEN 
            DO:
                FIND terms WHERE terms.company EQ ipb-itemfg.company
                    AND terms.t-code EQ  ipb-itemfg.terms NO-LOCK NO-ERROR.
                IF AVAILABLE terms THEN
                    lc-return = terms.dscr.
                ELSE
                    lc-return = "".
            END.
        WHEN "tax-dscr"  THEN 
            DO:
                FIND stax WHERE stax.company EQ ipb-itemfg.company
                    AND stax.tax-group EQ  ipb-itemfg.tax-gr NO-LOCK NO-ERROR.
                IF AVAILABLE stax THEN
                    lc-return = stax.tax-dscr1[1].
                ELSE
                    lc-return = "".
            END.
        WHEN "custype-dscr" THEN 
            DO:
                FIND custype WHERE custype.company EQ ipb-itemfg.company
                    AND custype.custype EQ ipb-itemfg.TYPE NO-LOCK NO-ERROR.
                IF AVAILABLE custype THEN
                    lc-return = custype.dscr.
                ELSE
                    lc-return = "".
            END.
        WHEN "loc-dscr"  THEN 
            DO:
                FIND loc WHERE loc.company EQ ipb-itemfg.company
                    AND loc.loc EQ  ipb-itemfg.loc NO-LOCK NO-ERROR.
                IF AVAILABLE loc THEN
                    lc-return = loc.dscr.
                ELSE
                    lc-return = "".
            END.
        WHEN "carrier-dscr"  THEN 
            DO:
                FIND FIRST carrier WHERE carrier.company EQ ipb-itemfg.company
                    AND carrier.carrier EQ  ipb-itemfg.carrier  NO-LOCK NO-ERROR.
 
                IF AVAILABLE carrier THEN
                    lc-return = carrier.dscr.
                ELSE
                    lc-return = "".
            END.
        WHEN "del-dscr"  THEN 
            DO:
                FIND FIRST carr-mtx WHERE carr-mtx.company EQ ipb-itemfg.company
                    AND carr-mtx.del-zone EQ  ipb-itemfg.del-zone NO-LOCK NO-ERROR.
                IF AVAILABLE carr-mtx THEN
                    lc-return = carr-mtx.del-dscr.
                ELSE
                    lc-return = "".
            END.
        WHEN "terr-dscr"  THEN 
            DO:
                FIND terr WHERE terr.company EQ ipb-itemfg.company
                    AND terr.terr EQ  ipb-itemfg.terr NO-LOCK NO-ERROR.
                IF AVAILABLE terr THEN
                    lc-return = terr.dscr .
                ELSE
                    lc-return = "".
            END.
        WHEN "po-mand" THEN 
            DO:
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = STRING(ipb-itemfg.po-mandatory). 

            END.
        WHEN "show-set" THEN 
            DO:
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = STRING(ipb-itemfg.show-set).
           
            END.
        WHEN "flat-comm" THEN 
            DO:
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = STRING(ipb-itemfg.flatCommPct).
            END.
        WHEN "inv-meth"  THEN 
            DO:
                CASE ipb-itemfg.inv-meth :
                    WHEN NO THEN
                        lc-return = "BOL".
                    WHEN YES THEN
                        lc-return = "PO".
                    OTHERWISE
                    lc-return = "Group by Date".
                END CASE.
            END.
        WHEN "accountType"  THEN 
            DO:
                CASE ipb-itemfg.accountType :
                    WHEN "" THEN
                        lc-return = "None".                 
                    OTHERWISE
                    lc-return = ipb-itemfg.accountType.
                END CASE.
            END.
        WHEN "ship-name"  THEN 
            DO:
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-name .
                ELSE
                    lc-return = "".
            END.
        WHEN "ship-addr1"  THEN 
            DO:
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-addr[1] .
                ELSE
                    lc-return = "".
            END.
        WHEN "ship-addr2"  THEN 
            DO:
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-addr[2] .
                ELSE
                    lc-return = "".
            END.
        WHEN "ship-city"  THEN 
            DO:
            
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-city .
                ELSE
                    lc-return = "".
            END.
        WHEN "ship-state"  THEN 
            DO:
            
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-state .
                ELSE
                    lc-return = "".
            END.
        WHEN "ship-zip"  THEN 
            DO:
            
                IF AVAILABLE shipto THEN
                    lc-return = shipto.ship-zip .
                ELSE
                    lc-return = "".
            END.
        WHEN "note1"  THEN 
            DO:
                lc-return = cShipNote[1] .
            END.
        WHEN "note2"  THEN 
            DO:
                lc-return = cShipNote[2] .
            END.
        WHEN "note3"  THEN 
            DO:
                lc-return = cShipNote[3] .
            END.
        WHEN "note4"  THEN 
            DO:
                lc-return = cShipNote[4] .
            END.
        WHEN "sort"  THEN 
            DO:
                CASE ipb-itemfg.SORT :
                    WHEN "N" THEN
                        lc-return = "No".
                    WHEN "Y" THEN
                        lc-return = "Yes".
                END CASE.
            END.
        WHEN "fob-code"  THEN 
            DO:
                CASE ipb-itemfg.fob-code :
                    WHEN "DEST" THEN
                        lc-return = "Destination".
                    WHEN "ORIG" THEN
                        lc-return = "Origin".
                END CASE.
            END.
        WHEN "tag-status"  THEN 
            DO:
                CASE ipb-itemfg.tagStatus :
                    WHEN "" THEN
                        lc-return = "Only tags that are not on hold".
                    WHEN "H" THEN
                        lc-return = "Only on Hold tags".
                    WHEN "A" THEN
                        lc-return = "Any tag status".     
                END CASE.
            END.
        
        WHEN "active" THEN 
            DO:
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
        WHEN "frt-pay" THEN 
            DO:
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
        WHEN "title"  THEN 
            DO:
                lc-return = "" .
            END.
        WHEN "cphone"  THEN 
            DO:
                lc-return = "" .
            END.
        WHEN "ext"  THEN 
            DO:
                lc-return = "" .
            END. 
        WHEN "cnt-price"  THEN 
            DO:
                lc-return = IF ipb-itemfg.imported EQ TRUE THEN "Yes" ELSE "No" .
            END.
        WHEN "Bank-RTN"  THEN 
            DO:
                lc-return = STRING(ipb-itemfg.Bank-RTN,"999999999").
            END.
        WHEN "dfuncTotMSFPTD"  THEN 
            DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipb-itemfg.ptd-msf[g_period]).*/
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
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

