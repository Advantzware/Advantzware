&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\r-pstranN.w
  
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

{sys/inc/var.i new shared}
ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE NEW SHARED VARIABLE choice            AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form    AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-print-fmt       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ls-fax-file       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-list-name      LIKE list-name EXTENT 2 NO-UNDO.
DEFINE            VARIABLE gv-fgemail        AS LOGICAL   NO-UNDO INIT ?.
DEFINE NEW SHARED VARIABLE cShipTO           AS CHARACTER NO-UNDO .

DEFINE            VARIABLE retcode           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cRtnChar          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lBussFormModle    AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{fg/ttTransBins.i new shared}  
    
DEFINE TEMP-TABLE tt-email NO-UNDO 
    FIELD tt-recid AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .    


DEFINE TEMP-TABLE tt-fgemail NO-UNDO
    FIELD i-no      LIKE itemfg.i-no
    FIELD po-no     LIKE oe-ordl.po-no
    FIELD ord-no    LIKE oe-ordl.ord-no
    FIELD qty-rec   AS DECIMAL
    FIELD recipient AS CHARACTER.  

DEFINE STREAM st-email.
DEFINE STREAM logFile.
DEFINE STREAM before.
DEFINE STREAM after. 

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE lvReturnChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvFound       AS LOG       NO-UNDO.
DEFINE VARIABLE lv-pdf-file   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-font-no    AS CHARACTER FORMAT "X(256)":U INITIAL "15" NO-UNDO.
DEFINE VARIABLE lv-ornt       AS CHARACTER INITIAL "P" NO-UNDO .
DEFINE VARIABLE v-program     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-prt-bypass AS LOG       NO-UNDO.  /* bypass window's printer driver */
DEFINE VARIABLE ip-run-what   AS CHARACTER NO-UNDO .
{custom/xprint.i}  

v-print-fmt = "xprint" .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 begin_whse end_whse begin_loc-bin ~
end_loc-bin begins_ship-no rd-dest Btn_OK Btn_select ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_whse end_whse begin_loc-bin ~
end_loc-bin begins_ship-no rd-dest  

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Ca&ncel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "&OK" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_select AUTO-GO 
    LABEL "&Select" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE begins_ship-no AS CHARACTER FORMAT "X(15)":U 
    LABEL "Create Bol To Ship To" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc-bin  AS CHARACTER FORMAT "X(8)" 
    LABEL "From Bin" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE begin_whse     AS CHARACTER FORMAT "X(5)" 
    LABEL "From Location" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE end_loc-bin    AS CHARACTER FORMAT "X(8)" 
    LABEL "To Bin" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE end_whse       AS CHARACTER FORMAT "X(5)" 
    LABEL "To Location" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 3
    SIZE 20 BY 5.05 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 71 BY 6.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_whse AT ROW 2.14 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Warehouse" WIDGET-ID 42
    end_whse AT ROW 2.14 COL 58.6 COLON-ALIGNED HELP
    "Enter ending Warehouse" WIDGET-ID 46
    begin_loc-bin AT ROW 3.24 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Bin" WIDGET-ID 40
    end_loc-bin AT ROW 3.24 COL 58.6 COLON-ALIGNED HELP
    "Enter Ending Bin" WIDGET-ID 48
    begins_ship-no AT ROW 5.1 COL 26.8 COLON-ALIGNED HELP
    "Enter the From FG Item Number"
    rd-dest AT ROW 7.95 COL 10.2 NO-LABELS
    Btn_OK AT ROW 14.76 COL 18.4
    Btn_select AT ROW 14.76 COL 36.2 WIDGET-ID 44
    Btn_Cancel AT ROW 14.81 COL 53.4
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.14 COL 4.4 WIDGET-ID 50
    BGCOLOR 2 
    "Output Options" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 7.05 COL 10
    RECT-6 AT ROW 7.24 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 80.2 BY 16.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "On-Hand Transfer BOL"
        HEIGHT             = 16.76
        WIDTH              = 81.8
        MAX-HEIGHT         = 45.05
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 45.05
        VIRTUAL-WIDTH      = 256
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
    begins_ship-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_loc-bin:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_loc-bin:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* On-Hand Transfer BOL */
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
ON WINDOW-CLOSE OF C-Win /* On-Hand Transfer BOL */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begins_ship-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begins_ship-no C-Win
ON HELP OF begins_ship-no IN FRAME FRAME-A /* Create Bol To Ship To */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        FIND FIRST cust WHERE cust.company = cocode AND
            cust.active = "X" NO-LOCK NO-ERROR.

        RUN WINDOWS/l-shipto.w (cocode,"",cust.cust-no, begins_ship-no:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN begins_ship-no:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begins_ship-no C-Win
ON LEAVE OF begins_ship-no IN FRAME FRAME-A /* Create Bol To Ship To */
    DO:
        ASSIGN {&self-name}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc-bin C-Win
ON LEAVE OF begin_loc-bin IN FRAME FRAME-A /* From Bin */
    DO:
        ASSIGN {&self-name}.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc-bin C-Win
ON VALUE-CHANGED OF begin_loc-bin IN FRAME FRAME-A /* From Bin */
    DO:
        EMPTY TEMP-TABLE ttTransBin.       
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* From Location */
    DO:             
        ASSIGN {&self-name}.           
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON VALUE-CHANGED OF begin_whse IN FRAME FRAME-A /* From Location */
    DO:             
        EMPTY TEMP-TABLE ttTransBin.           
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-A /* OK */
    DO:
        DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iCount      AS INTEGER NO-UNDO.
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.       

            FIND FIRST cust WHERE cust.company = cocode AND
                cust.active = "X" NO-LOCK NO-ERROR.
            IF AVAILABLE cust THEN
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ cocode
                    AND shipto.ship-id EQ begins_ship-no:SCREEN-VALUE 
                    AND shipto.cust-no EQ cust.cust-no NO-ERROR .
            IF NOT AVAILABLE shipto THEN 
            DO:
                MESSAGE "Invalid ShipId, Try help..." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
                RETURN .
            END.
        END.
        
        iCount = 0. 
        FOR EACH ttTransBin :              
            iCount = iCount + 1.
        END.
        IF iCount EQ 0 THEN 
        DO:
            RUN build-tables.
        END.    
  
        RUN pPrintData.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_select C-Win
ON CHOOSE OF Btn_select IN FRAME FRAME-A /* Select */
    DO:
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
            IF end_whse EQ "" THEN
            DO:
                MESSAGE "Please enter To Location...,try help" 
                    VIEW-AS ALERT-BOX INFORMATION.
                APPLY "entry" TO end_whse.
                RETURN NO-APPLY.         
            END.
            IF end_loc-bin EQ "" THEN
            DO:
                MESSAGE "Please enter To Bin...,try help" 
                    VIEW-AS ALERT-BOX INFORMATION.
                APPLY "entry" TO end_loc-bin.
                RETURN NO-APPLY.         
            END.
            iCount = 0. 
            FOR EACH ttTransBin :              
                iCount = iCount + 1.
            END.
            IF iCount EQ 0 THEN 
            DO:
                RUN build-tables.
            END.    
            RUN fg/dLoadTagItem.w .
               
        END.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc-bin C-Win
ON LEAVE OF end_loc-bin IN FRAME FRAME-A /* To Bin */
    DO:
        ASSIGN {&self-name}.       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc-bin C-Win
ON VALUE-CHANGED OF end_loc-bin IN FRAME FRAME-A /* To Bin */
    DO:
        EMPTY TEMP-TABLE ttTransBin.       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* To Location */
    DO:
        
        ASSIGN {&self-name}.
             
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON VALUE-CHANGED OF end_whse IN FRAME FRAME-A /* To Location */
    DO:
        
        EMPTY TEMP-TABLE ttTransBin.              
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


IF ip-run-what EQ "" THEN 
DO:
PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEFINE INPUT PARAMETER mailTo AS CHARACTER.
    DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT PARAMETER mailText AS CHARACTER.
    DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
    DEFINE INPUT PARAMETER mailDialog AS LONG.
    DEFINE OUTPUT PARAMETER retCode AS LONG.
END.

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
END.

ELSE DELETE WIDGET {&WINDOW-NAME}.

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
 
    DO WITH FRAME {&FRAME-NAME}:
        RUN enable_UI.

        {custom/usrprint.i} 
   
        {methods/nowait.i}

        IF NOT THIS-PROCEDURE:PERSISTENT THEN
            WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tables C-Win 
PROCEDURE build-tables :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/  
    EMPTY TEMP-TABLE ttTransBin.
        
    FOR EACH fg-bin
        WHERE fg-bin.company   EQ gcompany               
        AND fg-bin.loc GE begin_whse 
        AND fg-bin.loc LE end_whse 
        AND fg-bin.loc-bin GE begin_loc-bin
        AND fg-bin.loc-bin LE end_loc-bin 
        AND fg-bin.tag NE "" 
        AND fg-bin.qty GT 0:       
            
        CREATE ttTransBin.
        BUFFER-COPY fg-bin TO ttTransBin .
        ASSIGN                 
            ttTransBin.cases       = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)                  
            ttTransBin.row-id      = ROWID(fg-bin)
            ttTransBin.has-rec     = YES
            ttTransBin.IS-SELECTED = TRUE.  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    DISPLAY begin_whse end_whse begin_loc-bin end_loc-bin begins_ship-no rd-dest            
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 begin_whse end_whse begin_loc-bin end_loc-bin begins_ship-no 
        rd-dest Btn_OK Btn_select Btn_Cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
    DEFINE VARIABLE v-trans-lbl AS CHARACTER NO-UNDO .
   
    CASE rd-dest:
        WHEN 1 THEN RUN output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
        WHEN 2 THEN RUN output-to-screen(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
        WHEN 3 THEN 
            DO:
                IF is-xprint-form THEN 
                DO:
               
                    RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                    {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust= v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=  lv-pdf-file }
                END.
                ELSE 
                DO:
                    {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust= v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }
    
                END.
            END. 
    END CASE.
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
    {custom\out2file.i}  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/

    RUN custom/d-print.w (list-name).

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

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.   
    ELSE
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

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE
        RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintData C-Win 
PROCEDURE pPrintData :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-r-no   LIKE rm-rctd.r-no NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
      
    CASE rd-dest:
        WHEN 1 THEN
            LvOutputSelection = "Printer".
        WHEN 2 THEN
            LvOutputSelection = "Screen". 
        WHEN 3 THEN
            LvOutputSelection = "Email".
     
    END CASE.
  
    RUN SetBolForm (v-print-fmt).
   
    RUN run-report.

    RUN GenerateReport("",NO).
 

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    {sys/form/r-top.i}

    {sys/inc/ctrtext.i str-tit 112}.

    DEFINE VARIABLE ext-cost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE type            AS ch        FORMAT "X" INITIAL "R".
    DEFINE VARIABLE type-prt        AS ch        FORMAT "X(11)" INIT "".
    DEFINE VARIABLE v-fg-qty        LIKE fg-rctd.t-qty.
    DEFINE VARIABLE v-fg-cost       AS DECIMAL   FORMAT "->,>>>,>>9.99<<".
    DEFINE VARIABLE v-tot-qty       AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-tot-cost      AS DECIMAL   FORMAT "->>>,>>9.99<<".
    DEFINE VARIABLE v-grd-tot-qty   AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-grd-tot-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
    DEFINE VARIABLE v-grd-tot-value AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
    DEFINE VARIABLE v-tot-value     AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-cum-tot       AS de.                                   
    DEFINE VARIABLE v-tran-type     AS CHARACTER FORMAT "x(1)".      
    DEFINE VARIABLE v-entrytype     AS CHARACTER INITIAL "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
    DEFINE VARIABLE v-on            LIKE eb.num-up.
    DEFINE VARIABLE v-qty-pallet    AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-whse          LIKE fg-rctd.loc.            
    DEFINE VARIABLE v-one           AS INTEGER   FORMAT "->>,>>9" INIT 1.
    DEFINE VARIABLE v-ftime         AS LOGICAL   INIT NO.
    DEFINE VARIABLE v-dscr          LIKE account.dscr.
    DEFINE VARIABLE v-disp-actnum   LIKE account.actnum.
    DEFINE VARIABLE v-disp-amt      AS DECIMAL   FORMAT ">>,>>>,>>9.99cr".
    DEFINE VARIABLE v-hdr           AS CHARACTER FORMAT "x(12)".
    DEFINE VARIABLE v-postlst       AS cha       NO-UNDO.
    DEFINE VARIABLE ll-wip          AS LOG       NO-UNDO.
    DEFINE VARIABLE li              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-loop         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-time          AS CHARACTER FORMAT "X(5)" NO-UNDO.

    DEFINE VARIABLE v-itm-lbl       AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-itm-dsh       AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-desc-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-Po-lbl        AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-vend-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-desc-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-Po-dsh        AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-vend-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-uom-lbl       AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-uom-dsh       AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-cstprt        AS CHARACTER FORMAT "x(15)" NO-UNDO.
    /*DEFINE VARIABLE v-pr-tots2      LIKE v-pr-tots NO-UNDO.      */

    ASSIGN 
        v-uom-lbl = "JOB #"
        v-uom-dsh = "----------".

    {ce/msfcalc.i}

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2       = CURRENT-WINDOW:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        str-tit3       = "Period Date: " /*+ string(v-post-date,"99/99/9999")*/ + "             Posted by: " + USERID('nosweat') + "  As of " + string(TODAY,"99/99/9999")
        {sys/inc/ctrtext.i str-tit3 132}

        v-postlst      = "T"  
        IS-xprint-form = YES 
        cShipTO        = begins_ship-no .

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(74)}    
    
       
    ASSIGN 
        lv-pdf-file = init-dir + "\TransBol" + string(TIME) + ".pdf".


    IF IS-xprint-form THEN 
    DO:
  
        CASE rd-dest:
            WHEN 1 THEN 
                PUT "<PRINTER?>".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<PREVIEW><MODAL=NO>". 
                    ELSE
                        PUT "<PREVIEW>".      
                END.
            WHEN 3 THEN 
                DO:
                    PUT "<PDF-OUTPUT=" + lv-pdf-file + ">" FORM "x(180)".
                END.
        END CASE.
    END.
            
    RUN VALUE(v-program). 

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-fgemail C-Win 
PROCEDURE send-fgemail :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-fgemail-file AS cha .

    DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-to-list     AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailto      AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailsubject AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailbody    AS cha       NO-UNDO.
    DEFINE VARIABLE lv-mailattach  AS cha       NO-UNDO.
    DEFINE VARIABLE v-fgemail-file AS cha       NO-UNDO.
    DEFINE VARIABLE v-dir          AS CHARACTER FORMAT "X(80)" NO-UNDO.

    /* gdm - 12170901 */
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        v-dir = users.user_program[2] + "\".
    ELSE
        v-dir = "c:\tmp\".

    FOR EACH tt-email,
        FIRST cust NO-LOCK WHERE cust.company = g_company
        AND cust.cust-no = tt-email.cust-no
        AND cust.active = "E" BREAK BY tt-email.cust-no:
        IF FIRST-OF(tt-email.cust-no) THEN 
        DO:
            v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
            OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
            PUT STREAM st-email 
                "      Qty      JOB#       FG Item#          Part #          PO #            Item Name                 " SKIP
                "============ ========== =============== =============== =============== ==============================" SKIP.
        END.

        RELEASE bf-oe-ordl.

        /* gdm - 12170901 */
        FIND FIRST bf-job-hdr WHERE
            bf-job-hdr.company EQ g_company AND
            bf-job-hdr.job-no EQ tt-email.job-no AND
            bf-job-hdr.job-no2 EQ tt-email.job-no2 AND
            bf-job-hdr.i-no EQ tt-email.i-no AND
            bf-job-hdr.ord-no NE 0
            NO-LOCK NO-ERROR.

        IF AVAILABLE bf-job-hdr THEN
            FIND FIRST bf-oe-ordl WHERE
                bf-oe-ordl.company EQ g_company AND
                bf-oe-ordl.ord-no EQ bf-job-hdr.ord-no
                NO-LOCK NO-ERROR.
       
        FIND FIRST bf-itemfg WHERE
            bf-itemfg.company EQ g_company AND
            bf-itemfg.i-no EQ tt-email.i-no
            NO-LOCK NO-ERROR.

        PUT STREAM st-email UNFORMATTED
            tt-email.qty FORM "->>>,>>>,>>9" " " 
            tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
            " " tt-email.i-no FORM "X(15)"
            " " (IF AVAILABLE bf-oe-ordl THEN bf-oe-ordl.part-no ELSE IF AVAILABLE bf-itemfg THEN bf-itemfg.part-no ELSE "") FORM "x(15)"
            " " (IF AVAILABLE bf-oe-ordl THEN bf-oe-ordl.po-no ELSE IF AVAILABLE bf-job-hdr THEN bf-job-hdr.po-no ELSE "") FORM "x(15)" 
            " " (IF AVAILABLE bf-oe-ordl THEN bf-oe-ordl.i-name ELSE IF AVAILABLE bf-itemfg THEN bf-itemfg.i-name ELSE "") FORM "x(30)"
            SKIP.

        IF LAST-OF(tt-email.cust-no) THEN 
        DO:
            OUTPUT STREAM st-email CLOSE.           
            {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
            IF ls-to-list NE '' THEN 
            DO:
                ASSIGN 
                    lv-mailto      = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Receipts have been posted"
                    lv-mailbody    = "Finished Goods Receipts have been posted"
                    lv-mailattach  = v-fgemail-file.
                RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
            END.
        END. /* last-of(tt-email.cust-no) */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBOLForm C-Win 
PROCEDURE SetBOLForm :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHARACTER NO-UNDO.
   
    CASE icFormName:       
        WHEN "xprint" THEN 
            ASSIGN 
                is-xprint-form = YES
                v-program      = "fg/rep/boltxten.p".
         
        OTHERWISE
        ASSIGN
            is-xprint-form = YES
            v-program      = "fg/rep/boltxten.p".
    END CASE.
  
   
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

    PUT STREAM before SPACE(28)
        "< Selection Parameters >"
        SKIP(1).
  
    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:
       
            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".
                 
            PUT STREAM before lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.
 
    PUT STREAM before FILL("-",80) FORMAT "x(80)" SKIP.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-report C-Win 
PROCEDURE show-report :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-trans-lbl AS CHARACTER NO-UNDO .

    list-name = lv-list-name[ip-int].

  
    DO WITH FRAME {&FRAME-NAME}:
        CASE rd-dest :
            WHEN 1 THEN RUN output-to-printer("",NO).
            WHEN 2 THEN RUN output-to-screen("",NO).
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                /*run output-to-fax.*/
                /* {custom/asifax.i &type= "Customer"
                                  &begin_cust=v-trans-lbl
                                  &END_cust= v-trans-lbl
                                  &fax-subject="FRAME {&FRAME-NAME}:TITLE"
                                  &fax-body="FRAME {&FRAME-NAME}:TITLE"
                                  &fax-file=list-name }*/
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust=v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust=v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }
    
                    END.
                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc C-Win 
PROCEDURE valid-loc :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cNoteMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdValidator  AS HANDLE    NO-UNDO.
    RUN util/Validate.p PERSISTENT SET hdValidator.

    {&methods/lValidateError.i YES}
    
    DO WITH FRAME {&FRAME-NAME}:
     
        RUN pIsValidWarehouse IN hdValidator (begin_whse:SCREEN-VALUE,YES,cocode, OUTPUT lValid, OUTPUT cNoteMessage).
        IF NOT lValid THEN 
        DO:          
            MESSAGE cNoteMessage VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO begin_whse.
            ASSIGN 
                oplReturnError = YES .
        END.
      
    END.
    {&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.
  
    IF AVAILABLE oe-ordl THEN
        FOR EACH oe-rel WHERE 
            oe-rel.company EQ cocode AND
            oe-rel.ord-no  EQ oe-ordl.ord-no AND
            oe-rel.i-no    EQ oe-ordl.i-no AND
            oe-rel.line    EQ oe-ordl.line
            NO-LOCK:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

            IF INDEX("A,B,P",lv-stat) > 0 THEN
                li = li + oe-rel.qty.
        END.
     
    RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    (  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li     AS INTEGER NO-UNDO.
    DEFINE VARIABLE op-bal AS INTEGER NO-UNDO.
    IF AVAILABLE oe-ordl THEN
    DO:
        IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company   EQ cocode
                AND fg-rcpth.job-no    EQ oe-ordl.job-no
                AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                AND fg-rcpth.i-no      EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ "R"
                USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                li = li + fg-rdtlh.qty.
            END.
        ELSE
        DO:
            FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
                job-hdr.company EQ cocode AND
                job-hdr.ord-no EQ oe-ordl.ord-no AND
                job-hdr.i-no EQ oe-ordl.i-no
                USE-INDEX ord-no
                NO-LOCK,
                EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company   EQ cocode
                AND fg-rcpth.job-no    EQ job-hdr.job-no
                AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                AND fg-rcpth.i-no      EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ "R"
                USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                li = li + fg-rdtlh.qty.
            END.
        END.
     
        IF oe-ordl.po-no-po NE 0 THEN
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
                li = li + fg-rdtlh.qty.
            END.
    END.

    op-bal = li.
    RETURN li.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-tot-qty AS INTEGER NO-UNDO.           
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company    EQ oe-ordl.company
        AND fg-rcpth.i-no       EQ oe-ordl.i-no
        AND fg-rcpth.job-no     EQ oe-ordl.job-no
        AND fg-rcpth.rita-code  EQ "R"
        USE-INDEX tran NO-LOCK,
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        NO-LOCK:
        v-tot-qty = v-tot-qty + fg-rdtlh.qty.
    END.
    RETURN v-tot-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE hProc     AS HANDLE NO-UNDO.
    DEFINE VARIABLE lWasFound AS LOG    NO-UNDO.
    lWasFound = NO.
    hProc = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(hProc):
        IF INDEX(hProc:FILE-NAME, "addon") GT 0 THEN 
        DO:
            lWasFound = YES.
            LEAVE. /* found it. */
        END.
    
        hProc = hProc:NEXT-SIBLING.
    END.

    RETURN lWasFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
    DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k            AS INTEGER   NO-UNDO.

    /*k = NUM-ENTRIES(invalidChars).
    DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
    /*END.*/
    RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

