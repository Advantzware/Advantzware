&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgnq-exp 
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
{sys/inc/var.i new shared}
v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

ASSIGN
    cocode = g_company
    /*locode = gloc*/ .

DEFINE STREAM excel.

DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value        AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = LOGICAL(cNK1Value) NO-ERROR.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Invoice#,Vendor#,Invoice Date,Due Date,Net,Paid,Discount,Balance Due,Tax Group,Discount%," +
                                               "Status,Tax,Freight,User,PO Number,Line,Account Number,Account Description,Quantity,UOM," +
                                               "Price,UOM Price,Tax2,SqFt,Amount,Total MSF,Item#,Description,Job#,Form#,Blank#"

    cFieldListToSelect = "inv,vend,inv-dt,due-dt,net,paid,disc,bal-due,tx-cd,disc%,stat,tax,freight,usr,po,line,act-no,act-dsc," +
                                        "qty,uom,price,uom-pric,tax2,sqft,amt,ttl-msf,ino,dscr,job,sht,blnk" .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Invoice#,Vendor#,Invoice Date,Due Date,Net,Paid,Discount,Balance Due,Tax Group,Discount%," +
                                               "Status,Tax,Freight,User,PO Number,Line,Account Number,Account Description,Quantity,UOM," +
                                               "Price,UOM Price,Tax2,SqFt,Amount,Total MSF,Item#,Description,Job#,Form#,Blank#".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgnq-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_invoice ~
end_invoice begin_vendor end_vendor begin_date end_date sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_invoice end_invoice begin_vendor ~
end_vendor begin_date end_date sl_avail sl_selected fi_file tb_OpenCSV ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgnq-exp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgnq-exp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgnq-exp 
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

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "From Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_invoice AS CHARACTER FORMAT "x(20)" 
    LABEL "From Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_vendor  AS CHARACTER FORMAT "x(8)" 
    LABEL "From Vendor" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "To Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_invoice   AS CHARACTER FORMAT "x(20)" INITIAL "zzzzzzzzzzzzzzzzzzzz" 
    LABEL "To Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_vendor    AS CHARACTER FORMAT "x(8)" INITIAL "zzzzzzzz" 
    LABEL "To Vendor" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-frmitm.csv" 
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

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgnq-exp
    begin_invoice AT ROW 2.67 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Invoice#" WIDGET-ID 142
    end_invoice AT ROW 2.67 COL 64 COLON-ALIGNED HELP
    "Enter Ending Invoice#" WIDGET-ID 144
    begin_vendor AT ROW 3.95 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Invoice#" WIDGET-ID 146
    end_vendor AT ROW 3.95 COL 64 COLON-ALIGNED HELP
    "Enter Ending Invoice#" WIDGET-ID 148
    begin_date AT ROW 5.24 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Invoice#" WIDGET-ID 150
    end_date AT ROW 5.24 COL 64 COLON-ALIGNED HELP
    "Enter Ending Invoice#" WIDGET-ID 152
    sl_avail AT ROW 9.43 COL 7 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 9.43 COL 42.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 9.43 COL 62.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.62 COL 42.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 11.81 COL 42.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 13 COL 42.8 WIDGET-ID 136
    btn_down AT ROW 14.19 COL 42.8 WIDGET-ID 132
    fi_file AT ROW 17.1 COL 18.8 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 17.19 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
    tb_excel AT ROW 19.05 COL 4 WIDGET-ID 32
    tbAutoClose AT ROW 19.1 COL 43 WIDGET-ID 58
    btn-ok AT ROW 20.05 COL 32.6 WIDGET-ID 14
    btn-cancel AT ROW 20.05 COL 52.6 WIDGET-ID 12
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
    TITLE "Export Vendor to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgnq-exp
   FRAME-NAME                                                           */
ASSIGN 
    FRAME rd-fgnq-exp:SCROLLABLE = FALSE
    FRAME rd-fgnq-exp:HIDDEN     = TRUE.

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    begin_invoice:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    begin_vendor:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_invoice:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_vendor:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgnq-exp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-fgnq-exp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON WINDOW-CLOSE OF FRAME rd-fgnq-exp /* Export Vendor to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-fgnq-exp
ON LEAVE OF begin_date IN FRAME rd-fgnq-exp /* From Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_invoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_invoice rd-fgnq-exp
ON LEAVE OF begin_invoice IN FRAME rd-fgnq-exp /* From Invoice# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vendor rd-fgnq-exp
ON LEAVE OF begin_vendor IN FRAME rd-fgnq-exp /* From Vendor */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgnq-exp
ON CHOOSE OF btn-cancel IN FRAME rd-fgnq-exp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgnq-exp
ON CHOOSE OF btn-ok IN FRAME rd-fgnq-exp /* OK */
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
        END. /* IF NOT tb_OpenCSV THEN  */
        ELSE DO:
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgnq-exp
ON CHOOSE OF Btn_Add IN FRAME rd-fgnq-exp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgnq-exp
ON CHOOSE OF Btn_Def IN FRAME rd-fgnq-exp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgnq-exp
ON CHOOSE OF btn_down IN FRAME rd-fgnq-exp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgnq-exp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgnq-exp /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgnq-exp
ON CHOOSE OF btn_Up IN FRAME rd-fgnq-exp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-fgnq-exp
ON LEAVE OF end_date IN FRAME rd-fgnq-exp /* To Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_invoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_invoice rd-fgnq-exp
ON LEAVE OF end_invoice IN FRAME rd-fgnq-exp /* To Invoice# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vendor rd-fgnq-exp
ON LEAVE OF end_vendor IN FRAME rd-fgnq-exp /* To Vendor */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON HELP OF fi_file IN FRAME rd-fgnq-exp /* Name */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON LEAVE OF fi_file IN FRAME rd-fgnq-exp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgnq-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgnq-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgnq-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgnq-exp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgnq-exp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgnq-exp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgnq-exp 


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
        RUN pAPInvoiceLength.
        {custom/usrprint.i}
RUN DisplaySelectionList2.
RUN Set-Sort-Data.

APPLY "entry" TO begin_invoice.
fi_file:SCREEN-VALUE = "c:\tmp\VendorExport.csv".
END. 
WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgnq-exp  _DEFAULT-DISABLE
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
    HIDE FRAME rd-fgnq-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgnq-exp 
PROCEDURE DisplaySelectionList2 :
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

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgnq-exp  _DEFAULT-ENABLE
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
    DISPLAY begin_invoice end_invoice begin_vendor end_vendor begin_date end_date 
        sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME rd-fgnq-exp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_invoice end_invoice begin_vendor end_vendor 
        begin_date end_date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME rd-fgnq-exp.
    VIEW FRAME rd-fgnq-exp.
    {&OPEN-BROWSERS-IN-QUERY-rd-fgnq-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAPInvoiceLength rd-fgnq-exp 
PROCEDURE pAPInvoiceLength :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF lAPInvoiceLength THEN 
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                begin_invoice:FORMAT      = "x(20)"
                end_invoice:FORMAT        = "x(20)"
                begin_invoice:WIDTH-CHARS = 30
                end_invoice:WIDTH-CHARS   = 30
                .
        END.
    END.
    ELSE 
    DO: 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                begin_invoice:FORMAT      = "x(12)"
                end_invoice:FORMAT        = "x(12)"
                begin_invoice:WIDTH-CHARS = 20
                end_invoice:WIDTH-CHARS   = 20
                .
        END.    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgnq-exp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-cust FOR cust.
    DEFINE VARIABLE li-pallets AS INTEGER NO-UNDO.
    DEFINE VARIABLE op-qty-pal AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-lineno   AS INTEGER NO-UNDO.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    v-excel-detail-lines = "".
    op-qty-pal = 0 .
    li-pallets = 0 .
    v-lineno = 0 .

    FOR EACH ap-inv WHERE ap-inv.company EQ cocode 
        AND ap-inv.posted   EQ YES 
        AND ap-inv.inv-no   GE begin_invoice
        AND ap-inv.inv-no   LE end_invoice
        AND ap-inv.vend-no  GE begin_vendor
        AND ap-inv.vend-no  LE end_vendor 
        AND ap-inv.inv-date GE begin_date
        AND ap-inv.inv-date LE end_date 
        NO-LOCK,
        EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK:

        v-excel-detail-lines = "".

        FOR EACH ttRptSelected:
            /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
            IF ttRptSelected.TextList = "Invoice#" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.inv-no)).
            IF ttRptSelected.TextList = "Vendor#" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.vend-no)).
            IF ttRptSelected.TextList = "Invoice Date" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.inv-date)).
            IF ttRptSelected.TextList = "Due Date" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.due-date)).
            IF ttRptSelected.TextList = "Net" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.net)).
            IF ttRptSelected.TextList = "Paid" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.paid)).
            IF ttRptSelected.TextList = "Discount" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.disc-taken)).
            IF ttRptSelected.TextList = "Balance Due" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.due)).
            IF ttRptSelected.TextList = "Tax Group" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.tax-gr)).
            IF ttRptSelected.TextList = "Discount%" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.disc-%,">>9.99%")).
            IF ttRptSelected.TextList = "Status" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.stat)).
            IF ttRptSelected.TextList = "Tax" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.tax-amt)).
            IF ttRptSelected.TextList = "Freight" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.freight)).
            IF ttRptSelected.TextList = "User" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-inv.user-id)).

            IF ttRptSelected.TextList = "PO Number" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.po-no)).
            IF ttRptSelected.TextList = "Line" THEN 
            DO:
                IF AVAILABLE ap-invl THEN
                    ASSIGN v-lineno = (ap-invl.line + (ap-invl.po-no * 1000 * -1)) .
                ELSE v-lineno =  0.

                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-lineno)).
            END.
            IF ttRptSelected.TextList = "Account Number" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.actnum)).
            IF ttRptSelected.TextList = "Account Description" THEN 
            DO:
                IF AVAILABLE ap-invl THEN 
                DO:
                    FIND FIRST account WHERE account.company = cocode
                        AND account.actnum = ap-invl.actnum NO-LOCK NO-ERROR.
                    IF AVAILABLE account THEN 
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(account.dscr)).
                    ELSE
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("")).
                END.
            END.
            IF ttRptSelected.TextList = "Quantity" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.qty)).
            IF ttRptSelected.TextList = "UOM" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.cons-uom)).
            IF ttRptSelected.TextList = "Price" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.unit-pr)).
            IF ttRptSelected.TextList = "UOM Price" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.pr-qty-uom)).
            IF ttRptSelected.TextList = "Tax2" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.tax)).
            IF ttRptSelected.TextList = "SqFt" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.sf-sht)).
            IF ttRptSelected.TextList = "Amount" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.amt)).
            IF ttRptSelected.TextList = "Total MSF" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.amt-msf)).
            IF ttRptSelected.TextList = "Item#" THEN 
            DO:
                IF AVAILABLE ap-invl THEN
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no = ap-invl.po-no
                        AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                        NO-LOCK NO-ERROR.
                   
                IF AVAILABLE po-ordl THEN 
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(po-ordl.i-no)).
                ELSE
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("")).
            END.
            IF ttRptSelected.TextList = "Description" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(ap-invl.dscr)).
            IF ttRptSelected.TextList = "Job#" THEN 
            DO:
                IF AVAILABLE ap-invl THEN
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no = ap-invl.po-no
                        AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                        NO-LOCK NO-ERROR.
                   
                IF AVAILABLE po-ordl and po-ordl.job-no ne "" THEN 
                    v-excel-detail-lines = v-excel-detail-lines + 
                                           appendXLLine(TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', po-ordl.job-no, po-ordl.job-no2)))).
                ELSE
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("")).
            END.
            IF ttRptSelected.TextList = "Form#" THEN 
            DO:
                IF AVAILABLE ap-invl THEN
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no = ap-invl.po-no
                        AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                        NO-LOCK NO-ERROR.
                   
                IF AVAILABLE po-ordl THEN 
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(po-ordl.s-num)).
                ELSE
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("")).
            END.
            IF ttRptSelected.TextList = "Blank#" THEN 
            DO:
                IF AVAILABLE ap-invl THEN
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no = ap-invl.po-no
                        AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                        NO-LOCK NO-ERROR.
                   
                IF AVAILABLE po-ordl THEN 
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(po-ordl.s-num)).
                ELSE
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("")).
            END.
              

              
              
        /*  END.

          ELSE do:
              v-excel-detail-lines = v-excel-detail-lines + 
                  appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
          END.  */
        END.  /* each ttrptse */

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

    END. /* for each phone */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgnq-exp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

    /* If a customer number was entered, find first and last matching customers. */
    /*   IF begin_i-no:SCREEN-VALUE EQ "" THEN DO:
           FIND FIRST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
           begin_i-no:SCREEN-VALUE = fg-rcpth.i-no.
           FIND LAST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
           end_i-no:SCREEN-VALUE   = fg-rcpth.i-no .
       END. */
    /*  IF begin_title-cod:SCREEN-VALUE EQ "" THEN DO:
          FIND FIRST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
          begin_title-cod:SCREEN-VALUE = phone.titlcode .
          FIND LAST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
          end_title-cod:SCREEN-VALUE   = phone.titlcode .
      END. */

    /*  IF lcSearch NE "" THEN 
          begin_cust-type:SCREEN-VALUE = lcSearch.
      IF lcsearchby NE "" THEN 
          end_cust-type:SCREEN-VALUE = lcsearchby.*/

    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgnq-exp 
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

    CASE ipc-field :
        WHEN "ptd-profit" THEN 
            DO:
                lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
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
                /*IF ptd-profit-pct NE ? THEN*/
                ptd-profit1 = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
                ptd-sales1 = (ipb-itemfg.sales[gperiod]).
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
            END.
        WHEN "total-msf" THEN 
            DO:
                IF gperiod NE 0 THEN
                    lc-return = STRING(ipb-itemfg.ptd-msf[gperiod]).
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
            /*  ELSE 
                  reftable.val[1] = INT(ipb-itemfg.flat-comm) . */
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

