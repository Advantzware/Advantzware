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

  Created: 02/04/2019

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

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_date end_date begin_userid ~
end_userid begin_order end_order begin_fgitem end_fgitem begin_cust ~
end_cust btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_userid ~
end_userid begin_order end_order begin_fgitem end_fgitem begin_cust ~
end_cust 

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
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust   AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Customer" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Created Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fgitem AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_order  AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Entered By" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Created Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_fgitem   AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_order    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_userid   AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Entered By#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 6.43 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 6.43 COL 63 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_userid AT ROW 7.91 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Entered By"
    end_userid AT ROW 7.91 COL 63 COLON-ALIGNED HELP
    "Enter Ending Entered BY"
    begin_order AT ROW 9.33 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Order" WIDGET-ID 2
    end_order AT ROW 9.33 COL 63 COLON-ALIGNED HELP
    "Enter Ending Order" WIDGET-ID 4
    begin_fgitem AT ROW 10.71 COL 24 COLON-ALIGNED HELP
    "Enter Beginning FG Item" WIDGET-ID 6
    end_fgitem AT ROW 10.71 COL 63 COLON-ALIGNED HELP
    "Enter Ending FG Item" WIDGET-ID 8
    begin_cust AT ROW 12 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Customer" WIDGET-ID 10
    end_cust AT ROW 12 COL 63 COLON-ALIGNED HELP
    "Enter Ending Customer" WIDGET-ID 12
    btn-process AT ROW 15.76 COL 21
    btn-cancel AT ROW 15.76 COL 53
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 5.29 COL 5
    "" VIEW-AS TEXT
    SIZE 2.2 BY .95 AT ROW 1.95 COL 88
    BGCOLOR 11 
    RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
    "" VIEW-AS TEXT
    SIZE 88.8 BY .95 AT ROW 1 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 88.8 BY .95 AT ROW 3.76 COL 1
    BGCOLOR 11 
    "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
    SIZE 84 BY .95 AT ROW 1.95 COL 4
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
    SIZE 76 BY .95 AT ROW 2.91 COL 8
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "" VIEW-AS TEXT
    SIZE 7 BY .95 AT ROW 2.91 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 3 BY .95 AT ROW 1.95 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
    BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.2 BY 3.81
    BGCOLOR 11 .


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
        TITLE              = "Auto Close Order"
        HEIGHT             = 17.71
        WIDTH              = 90.2
        MAX-HEIGHT         = 19.76
        MAX-WIDTH          = 98.2
        VIRTUAL-HEIGHT     = 19.76
        VIRTUAL-WIDTH      = 98.2
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

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update AR/OE Invoice Cost  Comm */
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
ON WINDOW-CLOSE OF C-Win /* Update AR/OE Invoice Cost  Comm */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A 
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        CASE FOCUS:NAME:
            WHEN "begin_fgitem" THEN 
                DO:
                    RUN windows/l-itmfg2.w (cocode, "", INPUT begin_fgitem:SCREEN-VALUE, INPUT 1, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            begin_fgitem:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO begin_fgitem.
                    END.                           
                END.     
            WHEN "end_fgitem" THEN 
                DO:
                    RUN windows/l-itmfg2.w (cocode, "", INPUT end_fgitem:SCREEN-VALUE, INPUT 1, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            end_fgitem:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO end_fgitem.
                    END.                           
                END.     
            WHEN "begin_cust" THEN 
                DO:
                    RUN windows/l-cust2.w (cocode, INPUT begin_cust:SCREEN-VALUE, "", OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            begin_cust:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO begin_cust.
                    END.                           
                END. 
            WHEN "end_cust" THEN 
                DO:
                    RUN windows/l-cust2.w (cocode, INPUT end_cust:SCREEN-VALUE, "", OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            end_cust:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO end_cust.
                    END.                           
                END. 
            WHEN "begin_order" THEN 
                DO:
                    RUN windows/l-ordno2.w (cocode,"", INPUT begin_order:screen-value, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            begin_order:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO begin_order.
                    END.                           
                END.  
            WHEN "end_order" THEN 
                DO:
                    RUN windows/l-ordno2.w (cocode,"", INPUT end_order:screen-value, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            end_order:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO end_order.
                    END.                           
                END.  
            WHEN "begin_userid" THEN 
                DO:
                    RUN windows/l-users.w ( INPUT begin_userid:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            begin_userid:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO begin_userid.
                    END.                           
                END.  
            WHEN "end_userid" THEN 
                DO:
                    RUN windows/l-users.w (INPUT end_userid:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            end_userid:SCREEN-VALUE = ENTRY(1,char-val).
                        APPLY "entry" TO end_userid.
                    END.                           
                END.  
        END CASE.    
        RETURN NO-APPLY.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DEFINE VARIABLE v-process AS LOG NO-UNDO.


        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE)
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.
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
    DO WITH FRAME {&FRAME-NAME}:
        APPLY "entry" TO begin_date.
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
    DISPLAY begin_date end_date begin_userid end_userid begin_order end_order 
        begin_fgitem end_fgitem begin_cust end_cust 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_date end_date begin_userid end_userid begin_order 
        end_order begin_fgitem end_fgitem begin_cust end_cust btn-process 
        btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-B IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :

    DEFINE VARIABLE cStatus    AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cReason    AS CHARACTER NO-UNDO .
    DEFINE VARIABLE hPgmRunPro AS HANDLE    NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl .

    SESSION:SET-WAIT-STATE("General").

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            begin_userid 
            end_userid
            begin_order
            end_order
            begin_fgitem
            end_fgitem
            begin_cust 
            end_cust 
            begin_date
            end_date.
    END.

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company    EQ cocode
        AND oe-ordl.ord-no  GE begin_order
        AND oe-ordl.ord-no  LE end_order
        AND oe-ordl.cust-no GE begin_cust 
        AND oe-ordl.cust-no LE end_cust
        AND oe-ordl.i-no    GE begin_fgitem 
        AND oe-ordl.i-no    LE end_fgitem
        AND oe-ordl.opened  EQ YES ,
        FIRST oe-ord NO-LOCK               
        WHERE oe-ord.company    EQ oe-ordl.company  
        AND oe-ord.ord-no     EQ oe-ordl.ord-no     
        AND oe-ord.ord-date   GE begin_date 
        AND oe-ord.ord-date   LE end_date 
        AND oe-ord.entered-id GE begin_userid
        AND oe-ord.entered-id LE end_userid 
        AND oe-ord.opened EQ YES 
        USE-INDEX ord-no  :
        
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ordl.cust-no
            NO-ERROR .

        STATUS DEFAULT "Order Closing: " + TRIM(STRING(oe-ordl.ord-no)) + " FG Item " +
            STRING(oe-ordl.i-no).

        RUN oe/closelin.p (INPUT ROWID(oe-ordl),INPUT YES) .
                        
    
        IF cStatus EQ 'C' THEN 
        DO:
            IF NOT CAN-FIND(FIRST bf-oe-ordl WHERE 
                bf-oe-ordl.company = oe-ord.company AND
                bf-oe-ordl.ord-no = oe-ord.ord-no AND 
                bf-oe-ordl.stat NE "C") THEN
                RUN oe\close.p(RECID(oe-ord), YES).
        END.
    
    END.

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE("").

    MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

    RETURN NO-APPLY.
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

