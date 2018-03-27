&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\invLnChck.w

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

{methods/defines/hndldefs.i}
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-process    AS LOG       NO-UNDO.
DEFINE VARIABLE calendarDate AS CHARACTER.

DEFINE VARIABLE t-inv AS DECIMAL.
DEFINE VARIABLE t-bol AS DECIMAL.
DEFINE BUFFER bf-inv-head FOR inv-head.
DEFINE TEMP-TABLE ttBOLLineProblems
    FIELD company         LIKE company.company
    FIELD BolNo           LIKE oe-boll.bol-no COLUMN-LABEL 'BOL#'
    FIELD ItemNo          LIKE oe-boll.i-no FORMAT "X(22)" 
    FIELD ord-no          LIKE oe-boll.ord-no COLUMN-LABEL 'Order #'
    FIELD CustNo          LIKE oe-bolh.cust-no
    FIELD InvDate         LIKE inv-head.inv-date    
    FIELD ErrorType       AS CHARACTER
    FIELD TableType       AS CHARACTER
    FIELD CalcQty         AS DECIMAL
    FIELD ActQty          AS DECIMAL
    FIELD UnpostedInvoice AS INTEGER   FORMAT ">>>>>>>>"
    FIELD PostedInvoice   AS INTEGER   FORMAT ">>>>>>>>"
    .
DEFINE STREAM sReport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiFromBolDate btCalendar fiToBolDate ~
btCalendar-2 tgExcludeShipOnly fi_file_path btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiFromBolDate fiToBolDate ~
tgExcludeShipOnly fi_file_path 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCalendar 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    IMAGE-DOWN FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 5 BY 1.19 TOOLTIP "Popup Calendar".

DEFINE BUTTON btCalendar-2 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    IMAGE-DOWN FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 5 BY 1.19 TOOLTIP "Popup Calendar".

DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE VARIABLE fiFromBolDate AS DATE      FORMAT "99/99/99":U 
    LABEL "From Bol Date" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiToBolDate   AS DATE      FORMAT "99/99/99":U 
    LABEL "To Bol Date" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file_path  AS CHARACTER FORMAT "X(75)" INITIAL "c:~\tmp~\missingInvoiceLines.txt" 
    LABEL "Report Output File" 
    VIEW-AS FILL-IN 
    SIZE 39 BY 1.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 11.48.

DEFINE VARIABLE tgExcludeShipOnly AS LOGICAL INITIAL YES 
    LABEL "Exclude Ship/Bill Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fiFromBolDate AT ROW 3.86 COL 24 COLON-ALIGNED WIDGET-ID 8
    btCalendar AT ROW 3.86 COL 40.4 WIDGET-ID 16
    fiToBolDate AT ROW 3.86 COL 59 COLON-ALIGNED WIDGET-ID 10
    btCalendar-2 AT ROW 3.86 COL 75.8 WIDGET-ID 18
    tgExcludeShipOnly AT ROW 5.76 COL 26 WIDGET-ID 12
    fi_file_path AT ROW 8.14 COL 22 COLON-ALIGNED HELP
    "Enter File Path" WIDGET-ID 6
    btn-process AT ROW 13.67 COL 21
    btn-cancel AT ROW 13.67 COL 53
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 1.62 COL 3
    RECT-17 AT ROW 1.86 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.6 BY 14.33.


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
        TITLE              = "Check for Missing Invoice Lines"
        HEIGHT             = 15.29
        WIDTH              = 90.4
        MAX-HEIGHT         = 23.86
        MAX-WIDTH          = 103.4
        VIRTUAL-HEIGHT     = 23.86
        VIRTUAL-WIDTH      = 103.4
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
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    fi_file_path:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Check for Missing Invoice Lines */
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
ON WINDOW-CLOSE OF C-Win /* Check for Missing Invoice Lines */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalendar C-Win
ON CHOOSE OF btCalendar IN FRAME FRAME-A
    DO:
  
        RUN nosweat/popupcal.w (OUTPUT calendarDate).
  
        IF calendarDate NE '' THEN
            fiFromBolDate:SCREEN-VALUE  = STRING(DATE(calendarDate),"99/99/9999").

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCalendar-2 C-Win
ON CHOOSE OF btCalendar-2 IN FRAME FRAME-A
    DO:
  
        RUN nosweat/popupcal.w (OUTPUT calendarDate).
  
        IF calendarDate NE '' THEN
            fiToBolDate:SCREEN-VALUE  = STRING(DATE(calendarDate),"99/99/9999").

        RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        ASSIGN tgExcludeShipOnly
            fiFromBolDate 
            fiToBolDate
            .
        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromBolDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromBolDate C-Win
ON HELP OF fiFromBolDate IN FRAME FRAME-A /* From Bol Date */
    DO:
        {methods\calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiToBolDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiToBolDate C-Win
ON HELP OF fiToBolDate IN FRAME FRAME-A /* To Bol Date */
    DO:
   {methods\calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file_path
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON HELP OF fi_file_path IN FRAME FRAME-A /* Report Output File */
    DO:
        DEFINE VARIABLE v-file-path AS CHARACTER NO-UNDO.

        SYSTEM-DIALOG GET-DIR v-file-path
            TITLE "Select Archive Files Path".

        fi_file_path:SCREEN-VALUE = v-file-path + "\".

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file_path C-Win
ON LEAVE OF fi_file_path IN FRAME FRAME-A /* Report Output File */
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

    RUN enable_UI.
    fiFromBolDate:screen-value = STRING(TODAY - 3).
    fiToBolDate:screen-value = STRING(TODAY).
    {methods/nowait.i}
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
    DISPLAY fiFromBolDate fiToBolDate tgExcludeShipOnly fi_file_path 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 fiFromBolDate btCalendar fiToBolDate btCalendar-2 
        tgExcludeShipOnly fi_file_path btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE VARIABLE ll            AS LOG       NO-UNDO.
    DEFINE VARIABLE dFromBolDate  AS DATE      NO-UNDO.
    DEFINE VARIABLE dtoBolDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE dTotQty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvQty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cRecFound     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE linvoiceFound AS LOG       NO-UNDO.
    DEFINE VARIABLE cExcludeList  AS CHARACTER NO-UNDO.

    ASSIGN 
        dFromBolDate = fiFromBolDate
        dToBolDate   = fiToBolDate
        dTotQty      = 0
        cExcludeList = "T"
        .
       
    IF tgExcludeShipOnly THEN 
        cExcludeList = cExcludeList + ",S,I".
  
    OUTPUT STREAM sReport TO VALUE(fi_file_path).    
    STATUS DEFAULT 'Running...'.
    DEFINE VARIABLE iInvFound         LIKE inv-head.inv-no NO-UNDO.
    DEFINE VARIABLE iArFound          LIKE ar-invl.inv-no NO-UNDO.
    DEFINE VARIABLE lSkipforSetHeader AS LOGICAL NO-UNDO.
    FOR EACH company NO-LOCK,
        EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ company.company
        AND oe-bolh.bol-date GE dFromBolDate
        AND oe-bolh.bol-date LE dToBolDate
        AND oe-bolh.posted,
        FIRST cust NO-LOCK
        WHERE cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no
        AND cust.active NE "X" /* exclude transfers */,    
        EACH oe-boll 
        WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no                    
        AND oe-boll.qty NE 0
        /* AND oe-boll.posted */
        AND /* oe-boll.s-code NE "T" */ LOOKUP(oe-boll.s-code, "T,S,I") EQ 0
        NO-LOCK /*,
                   
    FIRST oe-bolh OF oe-boll NO-LOCK */
        BREAK BY oe-boll.b-no BY oe-boll.i-no BY oe-boll.po-no
        :
    
        dtotQty = dTotQty + oe-boll.qty.
        IF LAST-OF(oe-boll.po-no) THEN 
        DO:    
       
            ASSIGN 
                lInvoiceFound     = NO
                dinvQty           = 0
                iinvFound         = 0
                iArFound          = 0
                lSkipForSetHeader = FALSE
                .
               
            FOR EACH bf-inv-line NO-LOCK 
                WHERE bf-inv-line.company EQ oe-boll.company
                AND bf-inv-line.i-no    EQ oe-boll.i-no
                AND bf-inv-line.po-no   EQ oe-boll.po-no
                AND bf-inv-line.b-no    EQ oe-boll.b-no
                :
                ASSIGN 
                    lInvoiceFound = YES  
                    iInvFound     = bf-inv-line.inv-no
                    dInvQty       = dInvQty + bf-inv-line.inv-qty
                    .
            END.
            FOR EACH  ar-invl NO-LOCK
                WHERE ar-invl.company EQ oe-boll.company
                AND ar-invl.i-no    EQ oe-boll.i-no
                AND ar-invl.po-no   EQ oe-boll.po-no
                AND ar-invl.bol-no    EQ oe-boll.bol-no
                AND ar-invl.b-no      EQ oe-boll.b-no                 
                USE-INDEX bol-no
                :
                ASSIGN
                    lInvoiceFound = YES
                    iArFound      = ar-invl.inv-no
                    dInvQty       = dInvQty + ar-invl.inv-qty
                    .
            END.
        
            IF NOT lInvoiceFound THEN 
            DO: 
                FIND FIRST ttBOLLineProblems NO-LOCK
                    WHERE ttBOLLineProblems.BolNo EQ oe-boll.bol-no
                    AND ttBOLLineProblems.ItemNo EQ oe-boll.i-no
                    NO-ERROR.
                IF NOT AVAILABLE ttBOLLineProblems THEN 
                DO:
                    CREATE ttBOLLineProblems.
                    ASSIGN 
                        ttBOLLineProblems.BolNo           = oe-boll.bol-no
                        ttBOLLineProblems.ItemNo          = oe-boll.i-no
                        ttBOLLineProblems.CustNo          = oe-bolh.cust-no
                        ttBOLLineProblems.InvDate         = oe-bolh.bol-date
                        ttBOLLineProblems.company         = oe-bolh.company
                        ttBOLLineProblems.ord-no          = oe-boll.ord-no
                        ttBOLLineProblems.errorType       = "No Inv"
                        ttBOLLineProblems.CalcQty         = dTotQty
                        ttBOLLineProblems.ActQty          = dInvQty         
                        ttBOLLineProblems.PostedInvoice   = iArFound
                        ttBOLLineProblems.UnPostedInvoice = iInvFound                        
                        .
                END.
            END. /* invoice was not found */
            ELSE 
            DO:

               
                IF dInvQty NE dTotQty THEN 
                DO:
                    FOR EACH fg-set NO-LOCK WHERE fg-set.company EQ oe-boll.company 
                        AND fg-set.part-no EQ oe-boll.i-no:
                        IF (iInvFound GT 0 AND  CAN-FIND(FIRST bf-inv-line NO-LOCK 
                            WHERE bf-inv-line.company EQ oe-boll.company
                            AND bf-inv-line.b-no    EQ oe-boll.b-no
                            AND bf-inv-line.i-no    EQ fg-set.set-no) )
                            OR 
                            (iArFound GT 0 AND  CAN-FIND(FIRST ar-invl NO-LOCK 
                            WHERE ar-invl.company EQ oe-boll.company
                            AND ar-invl.bol-no    EQ oe-boll.bol-no
                            AND ar-invl.b-no      EQ oe-boll.b-no                                                         
                            AND ar-invl.i-no    EQ fg-set.set-no) ) THEN
                            lSkipForSetHeader = TRUE.
                                                             
                    END.
                
                    IF NOT lSkipForSetHeader THEN 
                    DO:                                                    
                        CREATE ttBOLLineProblems.
                        ASSIGN 
                            ttBOLLineProblems.BolNo           = oe-boll.bol-no
                            ttBOLLineProblems.ItemNo          = oe-boll.i-no
                            ttBOLLineProblems.CustNo          = oe-bolh.cust-no
                            ttBOLLineProblems.InvDate         = oe-bolh.bol-date
                            ttBOLLineProblems.company         = oe-bolh.company
                            ttBOLLineProblems.ord-no          = oe-boll.ord-no
                            ttBOLLineProblems.errorType       = "Qty Err"
                            ttBOLLineProblems.CalcQty         = dTotQty
                            ttBOLLineProblems.ActQty          = dInvQty         
                            ttBOLLineProblems.PostedInvoice   = iArFound
                            ttBOLLineProblems.UnPostedInvoice = iInvFound                               
                            .           
                    END.
                END.
            END. /* else invoice was found */
        
            ASSIGN 
                dTotQty = 0
                dInvQty = 0
                .
        END. /* LAST PO-NO*/
    
    END.


    /* output to "clipboard". */
    FOR EACH bf-inv-head NO-LOCK WHERE bf-inv-head.multi-invoice = TRUE
        AND bf-inv-head.inv-no GT 0 ,
        EACH inv-head NO-LOCK WHERE inv-head.company EQ bf-inv-head.company  AND 
        inv-head.inv-no EQ bf-inv-head.inv-no
        AND inv-head.multi-invoice EQ NO,
        EACH inv-line OF inv-head NO-LOCK
        BREAK BY inv-head.bol-no:

        t-inv = t-inv + inv-line.inv-qty.
        IF LAST-OF (inv-head.bol-no) THEN 
        DO:
            t-bol = 0.
            FOR EACH oe-boll WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no EQ inv-line.b-no
                NO-LOCK.
                t-bol = t-bol + oe-boll.qty.
            END.
            
            IF t-bol NE t-inv THEN DO: 

                CREATE ttBOLLineProblems.
                ASSIGN 
                    ttBOLLineProblems.BolNo           = inv-head.bol-no
                    ttBOLLineProblems.ItemNo          = ""
                    ttBOLLineProblems.CustNo          = inv-head.cust-no
                    ttBOLLineProblems.InvDate         = inv-head.inv-date
                    ttBOLLineProblems.company         = inv-head.company                
                    ttBOLLineProblems.errorType       = "Multi Inv"
                    ttBOLLineProblems.CalcQty         = t-bol
                    ttBOLLineProblems.ActQty          = t-inv                             
                    ttBOLLineProblems.UnPostedInvoice = inv-head.inv-no                               
                    .                        
            END.                     
            t-inv = 0.
    
        END.
    END.
    FOR EACH ttBOLLineProblems:
        DISPLAY STREAM sReport ttBOLLineProblems.company ttBOLLineProblems.CustNo 
            ttBOLLineProblems.BolNo ttBOLLineProblems.ord-no ttBOLLineProblems.ItemNo 
            ttBOLLineProblems.InvDate COLUMN-LABEL 'Inv/BOL!Date' ttBOLLineProblems.errorType 
            ttBOLLineProblems.calcQty COLUMN-LABEL "BOL!Qty"ttBOLLineProblems.actQty
            COLUMN-LABEL "Inv!Qty"
            ttBOLLineProblems.PostedInvoice COLUMN-LABEL "Posted!Invoice!Found"
            ttBOLLineProblems.UnPostedInvoice COLUMN-LABEL "UnPosted!Invoice!Found"
            WITH STREAM-IO WIDTH 132.
    END.

    OUTPUT STREAM sReport CLOSE.
    STATUS DEFAULT "DONE.".

    MESSAGE "Report finished. Do you want to view it?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
    IF ll THEN 
        OS-COMMAND NO-WAIT NOTEPAD VALUE(fi_file_path).
   
    STATUS DEFAULT ''.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

