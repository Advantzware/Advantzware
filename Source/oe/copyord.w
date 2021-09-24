&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE NEW SHARED BUFFER xeb  FOR eb.

DEFINE            BUFFER xop  FOR est-op.

DEFINE NEW SHARED VARIABLE xcal      AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid    AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len    AS de      NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id    AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE maxco     AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE qty       AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-qty-mod AS LOG     NO-UNDO.
DEFINE NEW SHARED VARIABLE nufile    AS LOG     INITIAL YES NO-UNDO.
DEFINE            VARIABLE giNextOrd AS INTEGER NO-UNDO.
ASSIGN
    cocode = gcompany
    locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 RECT-19 from_company ~
begin_ord-no to_company to_ord-no tbAutoClose btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS from_company from_c-name begin_ord-no ~
from_c-name2 to_company to_c-name to_ord-no tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_ord-no AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "From Order#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE from_c-name  AS CHARACTER FORMAT "x(30)" 
    VIEW-AS FILL-IN 
    SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE from_c-name2 AS CHARACTER FORMAT "x(30)" 
    VIEW-AS FILL-IN 
    SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE from_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
    LABEL "From Company" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE to_c-name    AS CHARACTER FORMAT "x(30)" 
    VIEW-AS FILL-IN 
    SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE to_company   AS CHARACTER FORMAT "XXX" INITIAL "001" 
    LABEL "To Company" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE to_ord-no    AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "To Order#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 8.76.

DEFINE RECTANGLE RECT-18
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 3.57.

DEFINE RECTANGLE RECT-19
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 3.1.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    from_company AT ROW 3.38 COL 25 COLON-ALIGNED HELP
    "Enter Company To Copy From"
    from_c-name AT ROW 3.38 COL 41 COLON-ALIGNED NO-LABELS
    begin_ord-no AT ROW 4.33 COL 25 COLON-ALIGNED HELP
    "Enter Order# to Be Copied From"
    from_c-name2 AT ROW 4.33 COL 41 COLON-ALIGNED NO-LABELS
    to_company AT ROW 7.43 COL 25 COLON-ALIGNED HELP
    "Copy To Company"
    to_c-name AT ROW 7.43 COL 41 COLON-ALIGNED NO-LABELS
    to_ord-no AT ROW 8.38 COL 25 COLON-ALIGNED HELP
    "Enter Order# to Be Copied To"
    tbAutoClose AT ROW 10.52 COL 42.8 WIDGET-ID 64
    btn-process AT ROW 11.48 COL 31
    btn-cancel AT ROW 11.48 COL 54     
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .95 AT ROW 1.05 COL 5
    FONT 4
    " C O P Y  T O" VIEW-AS TEXT
    SIZE 14 BY 1 AT ROW 6.24 COL 43
    BGCOLOR 8 FGCOLOR 15 
    " C O P Y  F R O M" VIEW-AS TEXT
    SIZE 18.2 BY 1 AT ROW 2.05 COL 41
    BGCOLOR 8 FGCOLOR 15 
    RECT-17 AT ROW 1.52 COL 4
    RECT-18 AT ROW 2.43 COL 5
    RECT-19 AT ROW 6.71 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 101 BY 12.43
    BGCOLOR 15 FONT 6.


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
        TITLE              = "Copy Customer Order"
        HEIGHT             = 12.43
        WIDTH              = 101
        MAX-HEIGHT         = 26.62
        MAX-WIDTH          = 160
        VIRTUAL-HEIGHT     = 26.62
        VIRTUAL-WIDTH      = 160
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        FONT               = 6
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
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN from_c-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN from_c-name2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    from_company:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN to_c-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    to_company:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    to_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Customer Order */
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
ON WINDOW-CLOSE OF C-Win /* Copy Customer Order */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* From Order# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-begin_ord-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON VALUE-CHANGED OF begin_ord-no IN FRAME FRAME-A /* From Order# */
    DO:
        RUN new-begin_ord-no.
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
        DEFINE VARIABLE v-process AS LOG INIT NO NO-UNDO.


        DO WITH FRAME {&FRAME-NAME}:    
            RUN valid-begin_ord-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            APPLY "entry" TO to_company.
            RUN valid-company NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            /*  Not relevant since using a sequence for order number */
            /*     RUN valid-to_ord-no NO-ERROR.               */
            /*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */

            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
            TRIM(STRING(begin_ord-no)) + " from Company " + TRIM(from_company) + " to " +
            " Company " + TRIM(to_company) + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON HELP OF to_company IN FRAME FRAME-A /* To Company */
    DO:  
        DEFINE VARIABLE c-char-val AS cha NO-UNDO.
        DEFINE VARIABLE op-error   AS cha NO-UNDO.

        RUN windows/l-chrval.w (OUTPUT c-char-val, OUTPUT op-error).

        IF c-char-val NE ""                        AND 
            c-char-val NE {&self-name}:SCREEN-VALUE THEN 
        DO:
            {&self-name}:SCREEN-VALUE = c-char-val.
            APPLY "value-changed" TO {&self-name}.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON LEAVE OF to_company IN FRAME FRAME-A /* To Company */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-company NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.

        FIND FIRST oe-ctrl WHERE 
            oe-ctrl.company = to_company:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR. 
        IF AVAILABLE oe-ctrl THEN
            ASSIGN to_ord-no:SCREEN-VALUE = STRING(oe-ctrl.n-ord).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON VALUE-CHANGED OF to_company IN FRAME FRAME-A /* To Company */
    DO:
        RUN new-company.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_ord-no C-Win
ON LEAVE OF to_ord-no IN FRAME FRAME-A /* To Order# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-to_ord-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
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

    ASSIGN
        from_company = cocode
        to_company   = cocode.
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}  

    DO WITH FRAME {&frame-name}:
        DISPLAY from_company to_company.
        {custom/usrprint.i}    
APPLY "entry" TO to_company.
RUN new-company.
APPLY "entry" TO from_company.
RUN new-company.
from_company:SENSITIVE = NO.
/* Replaced with sequence */
/*     find first oe-ctrl WHERE                                               */
/*         oe-ctrl.company = to_company:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR. */
/*                                                                            */
/*     to_ord-no:SCREEN-VALUE = string(oe-ctrl.n-ord).                        */
RUN sys/ref/asicurseq.p (INPUT cocode, INPUT "order_seq", OUTPUT giNextOrd) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
TO_ord-no:SCREEN-VALUE = STRING(giNextOrd + 1).
END.

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
    DISPLAY from_company from_c-name begin_ord-no from_c-name2 to_company 
        to_c-name to_ord-no tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 RECT-18 RECT-19 from_company begin_ord-no to_company to_ord-no 
        tbAutoClose btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_ord-no C-Win 
PROCEDURE new-begin_ord-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND oe-ord
            WHERE oe-ord.company EQ from_company:SCREEN-VALUE
            AND oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN 
        DO:
            FIND FIRST cust
                WHERE cust.company EQ oe-ord.company
                AND cust.cust-no EQ oe-ord.cust-no
                NO-LOCK NO-ERROR.
            ASSIGN
                from_c-name2:SCREEN-VALUE = IF AVAILABLE cust THEN cust.name ELSE "".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-company C-Win 
PROCEDURE new-company :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND company WHERE company.company BEGINS FOCUS:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE company THEN 
        DO:
            FOCUS:SCREEN-VALUE = company.company.

            IF FOCUS:NAME EQ "from_company" THEN from_c-name:SCREEN-VALUE = company.NAME.
            ELSE to_c-name:SCREEN-VALUE   = company.NAME. 

            APPLY "value-changed" TO begin_ord-no.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    /*------------------------------------------------------------------------------
      Purpose:     copy order record
      Parameters:  <none>
      Notes:       copyOrder procedure is contained in oe/copyOrder.i include
    ------------------------------------------------------------------------------*/  
    DEFINE VARIABLE viToOrdNo AS INTEGER NO-UNDO.
    SESSION:SET-WAIT-STATE("general").    

    /*  RUN copyOrder (from_company,to_company,begin_ord-no,oe-ctrl.n-ord). */
    FIND FIRST oe-ctrl NO-LOCK NO-ERROR.
    IF AVAILABLE oe-ctrl THEN
        viToOrdNo = oe-ctrl.n-ord.

    RUN oe/copyord2.p (INPUT From_Company,
        INPUT To_Company,
        INPUT begin_ord-no,
        INPUT-OUTPUT viToOrdNo,
        INPUT-OUTPUT fil_id,
        INPUT-OUTPUT v-qty-mod,
        INPUT-OUTPUT nufile,
        INPUT YES /* increment to next ord-no */).

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE("").
    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_ord-no C-Win 
PROCEDURE valid-begin_ord-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        IF v-msg EQ "" THEN
            IF NOT CAN-FIND(FIRST oe-ord
                WHERE oe-ord.company EQ from_company:SCREEN-VALUE
                AND oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)) THEN
                v-msg = TRIM(begin_ord-no:LABEL) + " doesn't exist, try help...".

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_ord-no.
            RETURN ERROR.
        END.
        IF INT(to_ord-no:SCREEN-VALUE) LE 0 THEN
            to_ord-no:SCREEN-VALUE = "1".

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company C-Win 
PROCEDURE valid-company :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msg AS CHARACTER INIT "" NO-UNDO.  

    DO WITH FRAME {&FRAME-NAME}:      
        IF NOT CAN-FIND(FIRST company WHERE company.company EQ FOCUS:SCREEN-VALUE) THEN
            lv-msg = "Invalid entry, try help".

        IF lv-msg EQ "" AND FOCUS:NAME EQ "from_company" AND
            NOT CAN-FIND(FIRST cust WHERE cust.company EQ FOCUS:SCREEN-VALUE) THEN
            lv-msg = "Sorry, no FG Items exist for this company".

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO FOCUS.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-to_ord-no C-Win 
PROCEDURE valid-to_ord-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        IF v-msg EQ "" THEN
            IF CAN-FIND(FIRST oe-ord
                WHERE oe-ord.company EQ to_company:SCREEN-VALUE
                AND oe-ord.ord-no  EQ INT(to_ord-no:SCREEN-VALUE)) THEN
                v-msg = TRIM(to_ord-no:LABEL) + " already exists, please re-enter...".

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO to_ord-no.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

