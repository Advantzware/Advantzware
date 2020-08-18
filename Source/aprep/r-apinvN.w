&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE tt-ap-inv NO-UNDO LIKE ap-inv    
    FIELD name LIKE vend.name. 

DEFINE VARIABLE is-xprint-form AS LOG NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha NO-UNDO.
DEFINE VARIABLE ll-secure      AS LOG NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha NO-UNDO.


ASSIGN 
    cTextListToSelect  = "AP Inv#,AP Vendor,AP Vend Name,AP Date,AP Due Date,AP Tax Code,AP Discount%,AP Disc Days,AP Status," +   /* 9*/
                           "AP Tax Amount,AP Amount,AP Paid Amount,AP Freight,AP Blanace Due,AP Currency," +  /* 6*/
                           "APLine PO#,APLine Account,APLine Dscription,APLine Qty,APLine Qty Uom,APLine Price,APLine Price Uom," +  /* 7*/
                           "APLine Taxable,APLine SqFt,APLine Amount,APLine Tot Msf,APLine Item,APLine Job,APLine Sheet,APLine Blank," +  /* 8*/
                           "APLine Line,PO Receipt Date,PO Order Qty,PO Qty UOM,PO Order Price,PO Price Uom,PO Receipts Qty," +    /* 7*/
    "PO Receipts Amt,PO Variance Amt,PO Variance Qty"  /* 3*/                           
    cFieldListToSelect = "inv,vend,ven-nam,date,due-date,Tax-code,discount,disc-days,status," +   /* 9*/
                             "tax-amt,amt,paid,freight,bal-due,curr-code," +  /* 6*/
                             "po-no,act,dscr,apQty,apQtyUom,apPrice,apPriceUom," +   /* 7*/
                             "apTax,apSqFt,apLineAmt,apTotMsf,apItem,apJob,apSNum,apBNum," +  /* 8*/
                             "line,PO_ReceiptDate,PO_OrderQty,PO_QuantityUOM,PO_OrderPrice,PO_OrderPriceUom,PO_ReceiptsQty," +   /* 7*/
    "PO_ReceiptsAmt,PO_VarianceAmt,PO_VarianceQty" /* 3*/
                           
    cFieldLength       = "10,9,30,10,11,11,12,12,9," + "13,15,15,11,14,11," + "10,20,30,13,14,13,16," + "14,11,14,14,15,11,12,12," +
                       "11,15,15,10,15,12,15," + "15,15,15"     
                      
    cFieldType         = "c,c,c,c,c,c,i,i,c," + "i,i,i,i,i,c," + "i,c,c,i,c,i,c," + "c,i,i,i,c,c,i,i," +
                     "i,c,i,i,i,c,i," + "i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "AP Inv#,APLine Account,AP Vendor,AP Vend Name,APLine Dscription,AP Date,AP Amount,APLine Amount".


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend begin_date ~
end_date begin_account end_account rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_date end_date ~
begin_account end_account rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file sl_avail sl_selected

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE begin_account  AS CHARACTER FORMAT "X(20)":U 
    LABEL "Beginning GL Account" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Inv Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_account    AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzz" 
    LABEL "Ending GL Account" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Inv Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-veninv.csv" 
    LABEL "If Yes, File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To File", 3,
    "To Fax", 4,
    "To Email", 5,
    "To Port Directly", 6
    SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 9.19.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 7.86.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.


DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel  AS LOGICAL   INITIAL NO 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend AT ROW 3.38 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 3.38 COL 66 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_date AT ROW 4.81 COL 24 COLON-ALIGNED HELP
    "Enter Beginning AP Invoice Date"
    end_date AT ROW 4.81 COL 66 COLON-ALIGNED HELP
    "Enter Ending AP Invoice Date"     
    begin_account AT ROW 6.30 COL 24.2 COLON-ALIGNED HELP
    "Enter Beginning General Ledger Account Number" WIDGET-ID 4
    end_account AT ROW 6.30 COL 66 COLON-ALIGNED HELP
    "Enter Ending General Ledger Account Number" WIDGET-ID 6
    sl_avail AT ROW 9.76 COL 4.8 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 9.76 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 9.76 COL 60.2 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.76 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 11.76 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 12.81 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 13.81 COL 40.8 WIDGET-ID 42
    rd-dest AT ROW 16.76 COL 7 NO-LABELS
    lv-ornt AT ROW 17 COL 31 NO-LABELS
    lines-per-page AT ROW 17 COL 84 COLON-ALIGNED
    lv-font-no AT ROW 18.91 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 19.86 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 20.95 COL 30
    tb_excel AT ROW 22.33 COL 50 RIGHT-ALIGNED
    tb_runExcel AT ROW 22.33 COL 71 RIGHT-ALIGNED
    fi_file AT ROW 23.14 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 24.71 COL 26
    btn-cancel AT ROW 24.71 COL 56
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 9.05 COL 60.2 WIDGET-ID 44
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 15.81 COL 5
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 9.05 COL 5.6 WIDGET-ID 38
    RECT-6 AT ROW 15.29 COL 1
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 96 BY 25.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "AP Invoices by Vendor"
        HEIGHT             = 25.33
        WIDTH              = 96.6
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".


ASSIGN
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".


ASSIGN 
    begin_account:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_account:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_account:HIDDEN IN FRAME FRAME-A       = TRUE
    end_account:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AP Invoices by Vendor */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AP Invoices by Vendor */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_account C-Win
ON LEAVE OF begin_account IN FRAME FRAME-A /* Beginning GL Account */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Inv Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN GetSelectionList.
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=end_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        SESSION:SET-WAIT-STATE ("").

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_account C-Win
ON LEAVE OF end_account IN FRAME FRAME-A /* Ending GL Account */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Inv Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
                LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        ASSIGN lv-font-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
    DO:
        ASSIGN lv-ornt.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
    DO:
        {custom/chgfont.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN DisplaySelectionList.
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_vend.
    END.

    /*RUN showAccountRange.*/

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
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

    {sys/ref/SelColCorrect.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY begin_vend end_vend begin_date end_date   
        begin_account end_account rd-dest lv-ornt lines-per-page lv-font-no 
        lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend end_vend begin_date end_date  
        begin_account end_account rd-dest lv-ornt lines-per-page 
        lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
        sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .        

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
    /*                                                         */
    /*      if init-dir = "" then init-dir = "c:\temp" .       */
    /*      SYSTEM-DIALOG GET-FILE list-name                   */
    /*          TITLE      "Enter Listing Name to SAVE AS ..." */
    /*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
    /*                     "All Files (*.*)" "*.*"             */
    /*          INITIAL-DIR init-dir                           */
    /*          ASK-OVERWRITE                                  */
    /*     /*     CREATE-TEST-FILE*/                           */
    /*          SAVE-AS                                        */
    /*          USE-FILENAME                                   */
    /*                                                         */
    /*          UPDATE OKpressed.                              */
    /*                                                         */
    /*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
    {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
         DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
         DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
    */
    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :

    DEFINE VARIABLE lv-total       AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE lv-gtotal      AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE ll-total       AS LOG       EXTENT 2 NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(500)" NO-UNDO.
    DEFINE VARIABLE cFileName      LIKE fi_file NO-UNDO .
    DEFINE VARIABLE iReceiptCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dReceptQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dReceptAmount  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtRecDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE iLineCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dConvertQty    AS DECIMAL   NO-UNDO .


    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    {sys/form/r-topsw.f}
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}. 


    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".


        IF LOOKUP(ttRptSelected.TextList, "APLine Amount,AP Amount") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    PUT  str-tit4 FORMAT "x(650)"
        SKIP
        str-tit5 FORMAT "x(650)"
        SKIP
        .

    EMPTY TEMP-TABLE tt-ap-inv.

    FOR EACH ap-inv
        WHERE ap-inv.company  EQ cocode
        AND ap-inv.vend-no  GE begin_vend
        AND ap-inv.vend-no  LE end_vend
        AND ((ap-inv.inv-date GE begin_date 
        AND ap-inv.inv-date LE end_date))      
        AND ap-inv.posted   EQ YES
        NO-LOCK
        BREAK BY ap-inv.vend-no
        BY ap-inv.inv-date
        BY ap-inv.inv-no : 

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-inv.vend-no "}

        FIND FIRST vend
            WHERE vend.company EQ ap-inv.company
            AND vend.vend-no EQ ap-inv.vend-no
            NO-LOCK NO-ERROR.    

        CREATE tt-ap-inv NO-ERROR.
        BUFFER-COPY ap-inv TO tt-ap-inv
            ASSIGN 
            tt-ap-inv.NAME = vend.NAME.

    END.

    FOR EACH tt-ap-inv NO-LOCK,
        EACH ap-invl WHERE ap-invl.company = tt-ap-inv.company
        AND ap-invl.i-no EQ tt-ap-inv.i-no 
        AND ap-invl.actnum GE begin_account
        AND ap-invl.actnum LE end_account 
        NO-LOCK
        BREAK BY tt-ap-inv.vend-no
        BY tt-ap-inv.inv-date
        BY tt-ap-inv.inv-no
        WITH FRAME detail2 NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132:

        {custom/statusMsg.i " 'Processing Vendor#  '  + tt-ap-inv.vend-no "}
        
     
     
        iReceiptCount = 0.
        dReceptAmount = 0.
        dReceptQty = 0.
        dtRecDate = ? .
           
        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company EQ ap-invl.company 
            AND po-ordl.po-no EQ ap-invl.po-no
            AND po-ordl.LINE EQ  {ap/invlline.i -1}            
            NO-ERROR .
        
        IF AVAILABLE po-ordl THEN 
        DO:                           
            IF po-ordl.item-type THEN 
            DO:
        
                FIND FIRST item
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR.
          
                FOR EACH rm-rcpth FIELDS(trans-date r-no pur-uom)
                    WHERE rm-rcpth.company    EQ cocode
                    AND rm-rcpth.i-no       EQ po-ordl.i-no
                    AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                    AND rm-rcpth.job-no     EQ po-ordl.job-no
                    AND rm-rcpth.job-no2    EQ po-ordl.job-no2                 
                    AND rm-rcpth.rita-code  EQ "R"
                    USE-INDEX item-po NO-LOCK,

                    FIRST rm-rdtlh
                    WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no
                    AND rm-rdtlh.s-num  EQ po-ordl.s-num
                    NO-LOCK

                    BREAK BY rm-rcpth.trans-date DESCENDING:

                    IF FIRST(rm-rcpth.trans-date) THEN
                        dtRecDate = rm-rcpth.trans-date.
             
                    dReceptAmount = dReceptAmount + ( rm-rdtlh.cost * rm-rdtlh.qty).
             
                    IF po-ordl.pr-qty-uom NE rm-rcpth.pur-uom  THEN
                    DO:
                        dConvertQty = rm-rdtlh.qty .
                        RUN custom/convquom.p(cocode, rm-rcpth.pur-uom, po-ordl.pr-qty-uom,
                            (IF AVAILABLE item THEN item.basis-w ELSE 0),
                            (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                            ELSE po-ordl.s-len), po-ordl.s-wid,
                            (IF AVAILABLE item THEN item.s-dep ELSE 0),
                            dConvertQty, OUTPUT dConvertQty).                                  
                    END.
                    ELSE dConvertQty = rm-rdtlh.qty.
             
                    dReceptQty = dReceptQty + dConvertQty.             
                    iReceiptCount = iReceiptCount + 1. 
            
                END.
            END.

            ELSE 
            DO:          
                FOR EACH fg-rcpth FIELDS(trans-date r-no rita-code pur-uom)
                    WHERE fg-rcpth.company    EQ cocode
                    AND fg-rcpth.i-no       EQ po-ordl.i-no
                    AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                    AND fg-rcpth.rita-code  EQ "R"                  
                    NO-LOCK,
                    EACH fg-rdtlh 
                    WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
                    AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code                      
                    BREAK BY fg-rcpth.trans-date DESCENDING:
               
               
                    IF FIRST(fg-rcpth.trans-date) THEN
                        dtRecDate = fg-rcpth.trans-date.
               
                    dReceptAmount = dReceptAmount + ((fg-rdtlh.cost * fg-rdtlh.qty) / (IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1) ).
                    dReceptQty = dReceptQty + fg-rdtlh.qty.
                    iReceiptCount = iReceiptCount + 1.
                END.
            END.
        
        END.  
        
        IF iLineCount > (lines-per-page - 10) THEN 
        DO:
            PAGE.
            PUT str-tit4 FORMAT "x(650)" SKIP
                str-tit5 FORMAT "x(650)" SKIP .
            iLineCount = 0 .
        END.
        
        
        iLineCount = iLineCount + 1.
        IF dReceptQty EQ ? THEN dReceptQty = 0.
        IF dReceptAmount EQ ? THEN dReceptAmount = 0.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "inv"          THEN 
                    cVarValue = STRING(tt-ap-inv.inv-no).
                         
                WHEN "vend"         THEN 
                    cVarValue = STRING(tt-ap-inv.vend-no).
                WHEN "ven-nam"      THEN  
                    cVarValue = STRING(tt-ap-inv.NAME,"x(30)").                         
                WHEN "date"         THEN 
                    cVarValue = IF tt-ap-inv.inv-date NE ? THEN STRING(tt-ap-inv.inv-date) ELSE "".
                WHEN "due-date"     THEN 
                    cVarValue = IF tt-ap-inv.due-date NE ? THEN STRING(tt-ap-inv.due-date) ELSE "".
                WHEN "Tax-code"     THEN 
                    cVarValue = IF tt-ap-inv.tax-gr NE ? THEN STRING(tt-ap-inv.tax-gr) ELSE "".
                WHEN "discount"     THEN 
                    cVarValue = IF tt-ap-inv.disc-% NE ? THEN STRING(tt-ap-inv.disc-%) ELSE "".
                WHEN "disc-days"     THEN 
                    cVarValue = IF tt-ap-inv.disc-days NE ? THEN STRING(tt-ap-inv.disc-days) ELSE "".
                WHEN "status"     THEN 
                    cVarValue = IF tt-ap-inv.stat NE ? THEN STRING(tt-ap-inv.stat) ELSE "".
                WHEN "tax-amt"     THEN 
                    cVarValue = IF tt-ap-inv.tax-amt NE ? THEN STRING(tt-ap-inv.tax-amt,"->,>>>,>>9.99") ELSE "".
                WHEN "amt"     THEN 
                    cVarValue = IF tt-ap-inv.net NE ? THEN STRING(tt-ap-inv.net,"->>>,>>>,>>9.99") ELSE "".
                WHEN "paid"     THEN 
                    cVarValue = IF tt-ap-inv.paid NE ? THEN STRING(tt-ap-inv.paid,"->>>,>>>,>>9.99") ELSE "".
                WHEN "freight"     THEN 
                    cVarValue = IF tt-ap-inv.freight NE ? THEN STRING(tt-ap-inv.freight,"->>>,>>9.99") ELSE "".
                WHEN "bal-due"     THEN 
                    cVarValue = IF tt-ap-inv.due NE ? THEN  STRING(tt-ap-inv.due,"->>,>>>,>>9.99") ELSE "".
                WHEN "curr-code"    THEN 
                    cVarValue = IF tt-ap-inv.curr-code[1] NE ? THEN  STRING(tt-ap-inv.curr-code[1]) ELSE "".                           
                WHEN "po-no"          THEN 
                    cVarValue = STRING(ap-invl.po-no).
                WHEN "act"          THEN 
                    cVarValue = STRING(ap-invl.actnum).
                WHEN "dscr"         THEN 
                    cVarValue = STRING(ap-invl.dscr,"x(30)").
                WHEN "apQty"         THEN 
                    cVarValue = STRING(ap-invl.qty,"->>,>>>,>>9.9<<<<<").
                WHEN "apQtyUom"         THEN 
                    cVarValue = STRING(ap-invl.cons-uom,"x(3)").
                WHEN "apPrice"         THEN 
                    cVarValue = STRING(ap-invl.unit-pr,"->,>>>,>>9.99<<<<").
                WHEN "apPriceUom"         THEN 
                    cVarValue = STRING(ap-invl.pr-qty-uom,"x(3)"). 
                WHEN "apTax"         THEN 
                    cVarValue = STRING(ap-invl.Tax,"Yes/No"). 
                WHEN "apSqFt"         THEN 
                    cVarValue = STRING(ap-invl.sf-sht,">,>>>,>>9.9<<<").
                WHEN "apLineAmt"         THEN 
                    cVarValue = STRING(ap-invl.amt,"->>,>>>,>>9.99").
                WHEN "apTotMsf"         THEN 
                    cVarValue = STRING(ap-invl.amt-msf,"->>,>>9.99").                          
                WHEN "apItem"         THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.i-no,"x(15)") ELSE "".
                WHEN "apJob"         THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">9") ) ELSE "".
                WHEN "apSNum"         THEN 
                    cVarValue = IF AVAILABLE po-ordl AND po-ordl.s-num NE ? THEN STRING(po-ordl.s-num) ELSE "".
                WHEN "apBNum"         THEN 
                    cVarValue = IF AVAILABLE po-ordl AND po-ordl.b-num NE ? THEN STRING(po-ordl.b-num) ELSE "".                           
                WHEN "line"          THEN 
                    DO:                         
                        cVarValue = STRING((ap-invl.line + (ap-invl.po-no * 1000 * -1))).
                    END.                          
                WHEN "line-amt"     THEN 
                    cVarValue = STRING(ap-invl.amt,"->>>,>>>,>>>,>>9.99") .                         
                WHEN "PO_ReceiptDate"     THEN 
                    cVarValue = IF dtRecDate NE ? THEN STRING(dtRecDate,"99/99/9999") ELSE "" .
                WHEN "PO_OrderQty"     THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.ord-qty,"->>>,>>>,>>9.99") ELSE "" .
                WHEN "PO_QuantityUOM"     THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.pr-qty-uom,"x(3)") ELSE "" .
                WHEN "PO_OrderPrice"     THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.cost,"->>>,>>>,>>9.99") ELSE "" .
                WHEN "PO_OrderPriceUom"     THEN 
                    cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ordl.pr-uom,"x(3)") ELSE "" .                          
                WHEN "PO_ReceiptsQty"     THEN 
                    cVarValue = STRING(dReceptQty,"->>>,>>>,>>9.99") .
                WHEN "PO_ReceiptsAmt"     THEN 
                    cVarValue = STRING(dReceptAmount,"->>>,>>>,>>9.99") .
                WHEN "PO_VarianceAmt"     THEN 
                    cVarValue = STRING(dReceptAmount - ap-invl.amt  ,"->>>,>>>,>>9.99") .
                WHEN "PO_VarianceQty"     THEN 
                    cVarValue = STRING(dReceptQty - ap-invl.qty  ,"->>>,>>>,>>9.99") .                                                  
                         
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.


        lv-total[2] = lv-total[2] + ap-invl.amt.
        IF FIRST-OF(tt-ap-inv.inv-no) THEN
            lv-gtotal[2] = lv-gtotal[2] + tt-ap-inv.net.           
          

        IF LAST(tt-ap-inv.vend-no) THEN 
        DO:

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:                          
                    WHEN "amt"              THEN 
                        cVarValue =  STRING(lv-gtotal[2],"->>>,>>>,>>9.99") .
                    WHEN "apLineAmt"           THEN 
                        cVarValue = STRING(lv-total[2],"->>>,>>>,>>9.99") .                        
                    OTHERWISE 
                    DO:
                        cVarValue = "".
                    END.

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            IF str-line NE "" THEN 
            DO:
                PUT UNFORMATTED 
                    "   Grand Total" SUBSTRING(cDisplay,15,600) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        "Grand Total " + substring(cExcelDisplay,3,600) SKIP.
                END.
            END.
        END.
    END. /*each tt-ap-inv*/
    /*END. /*detail*/ */
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                    END.
                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END.       
            END.                 
        END.            
        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
    END.

    PUT SPACE(28)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showAccountRange C-Win 
PROCEDURE showAccountRange :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}: 
    /* ASSIGN tb_detail.
     IF tb_detail THEN
       ASSIGN 
             begin_account:HIDDEN = NO
             end_account:HIDDEN = NO.
     ELSE
       ASSIGN
             begin_account:HIDDEN = YES
             end_account:HIDDEN = YES.*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


