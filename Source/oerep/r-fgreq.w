&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
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

DEFINE VARIABLE v-program      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG           NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_ord-no end_ord-no ~
begin_cust-no end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date ~
begin_prm-date end_prm-date tb_re-prnt rd-dest tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_ord-no end_ord-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date begin_prm-date ~
end_prm-date tb_re-prnt rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win          AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/01 
    LABEL "From Order Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_prm-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/01 
    LABEL "From Promise Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "To Order Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_prm-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "To Promise Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

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
    "To Email", 5
    SIZE 17 BY 4.48 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 8.33.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_re-prnt   AS LOGICAL INITIAL NO 
    LABEL "Reprint?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY 1.05 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_ord-no AT ROW 2.91 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_ord-no AT ROW 2.91 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    begin_cust-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_i-no AT ROW 4.81 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 4.81 COL 69 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_ord-date AT ROW 5.76 COL 28 COLON-ALIGNED
    end_ord-date AT ROW 5.76 COL 69 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    begin_prm-date AT ROW 6.71 COL 28 COLON-ALIGNED
    end_prm-date AT ROW 6.71 COL 69 COLON-ALIGNED HELP
    "Enter Ending Promise Date"
    tb_re-prnt AT ROW 8.62 COL 48
    lv-font-no AT ROW 10.76 COL 37 COLON-ALIGNED
    lv-ornt AT ROW 10.76 COL 46 NO-LABELS
    lines-per-page AT ROW 10.76 COL 87 COLON-ALIGNED
    rd-dest AT ROW 10.81 COL 5 NO-LABELS
    lv-font-name AT ROW 11.95 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14.29 COL 29.2
    tbAutoClose AT ROW 15.52 COL 29.2 WIDGET-ID 64
    btn-ok AT ROW 16.48 COL 29
    btn-cancel AT ROW 16.48 COL 53.4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 10.05 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    RECT-6 AT ROW 10.52 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.8 BY 17
    BGCOLOR 15 .


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
        TITLE              = "FG Purchase Requirements"
        HEIGHT             = 17
        WIDTH              = 94.8
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
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_prm-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_prm-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_re-prnt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Purchase Requirements */
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
ON WINDOW-CLOSE OF C-Win /* FG Purchase Requirements */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* From Order Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prm-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prm-date C-Win
ON LEAVE OF begin_prm-date IN FRAME FRAME-A /* From Promise Date */
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
        SESSION:SET-WAIT-STATE ("general").

        ASSIGN rd-dest.

        RUN run-report. 

        SESSION:SET-WAIT-STATE ("").

        CASE rd-dest:

            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject="FG Purchase Requirements"
                            &fax-body="FG Purchase Requirements"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject="FG Purchase Requirements"
                             &mail-body="FG Purchase Requirements"
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject="FG Purchase Requirements"
                                  &mail-body="FG Purchase Requirements"
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE("").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* To Order Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_prm-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prm-date C-Win
ON LEAVE OF end_prm-date IN FRAME FRAME-A /* To Promise Date */
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
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

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


&Scoped-define SELF-NAME tb_re-prnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_re-prnt C-Win
ON VALUE-CHANGED OF tb_re-prnt IN FRAME FRAME-A /* Reprint? */
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

    ASSIGN
        begin_ord-date = DATE(1,1,YEAR(TODAY))
        end_ord-date   = DATE(12,31,9999)
        begin_prm-date = DATE(1,1,YEAR(TODAY))
        end_prm-date   = DATE(12,31,9999).

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png"). 
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OU9" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i} 

    APPLY "entry" TO begin_ord-no IN FRAME {&FRAME-NAME}.

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
    DISPLAY begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no 
        begin_ord-date end_ord-date begin_prm-date end_prm-date tb_re-prnt 
        rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_ord-no end_ord-no begin_cust-no end_cust-no 
        begin_i-no end_i-no begin_ord-date end_ord-date begin_prm-date 
        end_prm-date tb_re-prnt rd-dest tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
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
    /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
   
        if init-dir = "" then init-dir = "c:\temp" .
        SYSTEM-DIALOG GET-FILE list-name
            TITLE      "Enter Listing Name to SAVE AS ..."
            FILTERS    "Listing Files (*.rpt)" "*.rpt",
                       "All Files (*.*)" "*.*"
            INITIAL-DIR init-dir
            ASK-OVERWRITE
       /*     CREATE-TEST-FILE*/
            SAVE-AS
            USE-FILENAME
   
            UPDATE OKpressed.
   
        IF NOT OKpressed THEN  RETURN NO-APPLY.
      */
    {custom/out2file.i}

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
    RUN custom\d-print.w (list-name).
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* -------------------------------------------------------------------------  */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    {sys/form/r-topw.f}

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE v-ord       LIKE oe-ord.ord-no FORMAT ">>>>>>>>"
        EXTENT 2 INIT [0,99999999].
    DEFINE VARIABLE v-cust      LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].
    DEFINE VARIABLE v-item      LIKE oe-ordl.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
    DEFINE VARIABLE v-odate     LIKE oe-ord.ord-date FORMAT "99/99/9999"
        EXTENT 2 INIT [01/01/0001,12/31/9999].
    DEFINE VARIABLE v-pdate     LIKE oe-ordl.prom-date FORMAT "99/99/9999"
        EXTENT 2 INIT [01/01/0001,12/31/9999].
    DEFINE VARIABLE v-rprnt     AS LOG INIT NO.

    DEFINE VARIABLE v-cost      LIKE oe-ordl.cost.
    DEFINE VARIABLE v-area-code LIKE vend.area-code EXTENT 2.
    DEFINE VARIABLE v-phone     LIKE vend.phone EXTENT 2.
    DEFINE VARIABLE v-fax-area  LIKE vend.fax-area EXTENT 2.
    DEFINE VARIABLE v-fax       LIKE vend.fax EXTENT 2.
    DEFINE VARIABLE v-contact   LIKE vend.contact EXTENT 2.

    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit2   = c-win:TITLE + " by Job#"
        {sys/inc/ctrtext.i str-tit2 112}

        v-ord[1]   = begin_ord-no
        v-ord[2]   = end_ord-no
        v-cust[1]  = begin_cust-no
        v-cust[2]  = end_cust-no 
        v-item[1]  = begin_i-no
        v-item[2]  = end_i-no
        v-odate[1] = begin_ord-date
        v-odate[2] = end_ord-date
        v-pdate[1] = begin_prm-date
        v-pdate[2] = end_prm-date
        v-rprnt    = tb_re-prnt.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    GE v-item[1]
        AND itemfg.i-no    LE v-item[2]
        AND itemfg.pur-man EQ YES
        USE-INDEX i-no NO-LOCK,

        EACH oe-ordl
        WHERE oe-ordl.company    EQ cocode
        AND oe-ordl.i-no       EQ itemfg.i-no
        AND oe-ordl.ftick-prnt EQ v-rprnt
        AND oe-ordl.ord-no     GE v-ord[1]
        AND oe-ordl.ord-no     LE v-ord[2]
        AND oe-ordl.prom-date  GE v-pdate[1]
        AND oe-ordl.prom-date  LE v-pdate[2]
        USE-INDEX item,

        FIRST oe-ord
        WHERE oe-ord.company           EQ cocode
        AND oe-ord.ord-no            EQ oe-ordl.ord-no
        AND index("CDZ",oe-ord.stat) EQ 0
        AND oe-ord.cust-no           GE v-cust[1]
        AND oe-ord.cust-no           LE v-cust[2]
        AND oe-ord.ord-date          GE v-odate[1]
        AND oe-ord.ord-date          LE v-odate[2]
        USE-INDEX ord-no NO-LOCK

        BY oe-ord.cust-no
        BY itemfg.i-no:

        PAGE.

        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            NO-LOCK NO-ERROR.

        ASSIGN
            v-cost      = 0
            v-area-code = ""
            v-phone     = ""
            v-fax-area  = ""
            v-fax       = ""
            v-contact   = "".

        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company  EQ cocode
            AND b-oe-ordl.i-no     EQ oe-ordl.i-no
            AND b-oe-ordl.po-no-po NE 0
            AND recid(b-oe-ordl)   NE recid(oe-ordl)
            NO-LOCK
            BY b-oe-ordl.po-no-po DESCENDING:

            v-cost = b-oe-ordl.cost / 1000.
            LEAVE.
        END.

        DO i = 1 TO 2:
            FIND FIRST vend
                WHERE vend.company EQ cocode
                AND vend.vend-no EQ (IF i EQ 1 THEN itemfg.vend-no
                ELSE itemfg.vend2-no)
                NO-LOCK NO-ERROR.
            IF AVAILABLE vend THEN
                ASSIGN
                    v-area-code[i] = vend.area-code
                    v-phone[i]     = vend.phone
                    v-fax-area[i]  = vend.fax-area
                    v-fax[i]       = vend.fax
                    v-contact[i]   = vend.contact.
        END.

        DISPLAY oe-ord.cust-no    COLON 40 LABEL "Customer Number"
            cust.name         COLON 40 LABEL "Name"           
            WHEN AVAILABLE cust
            SKIP(1)
            oe-ord.ord-no     COLON 40 LABEL "Order"
            SKIP(1)
            itemfg.i-no       COLON 40 LABEL "Item Number"
            itemfg.i-name     COLON 40 LABEL "Name"
            itemfg.part-dscr1 COLON 40 LABEL "Desc"
            itemfg.part-dscr2    AT 42 NO-LABELS
            itemfg.part-dscr3    AT 42 NO-LABELS
            oe-ordl.part-no   COLON 40 LABEL "Customer Part Number"
            SKIP(1)
            itemfg.q-onh      COLON 40
            oe-ordl.qty       COLON 40 LABEL "Customer Order Qty"
            v-cost            COLON 40 LABEL "Last Cost"
            SKIP(1)
            itemfg.vend-no    COLON 40 LABEL "Primary Vendor"
            v-area-code[1]    COLON 40 LABEL "Phone"
            v-phone[1]                 NO-LABELS
            v-fax-area[1]     COLON 40 LABEL "Fax"
            v-fax[1]                   NO-LABELS
            v-contact[1]      COLON 40 LABEL "Contact"
            SKIP(1)
            itemfg.vend2-no   COLON 40 LABEL "Secondary Vendor"
            v-area-code[2]    COLON 40 LABEL "Phone"
            v-phone[2]                 NO-LABELS
            v-fax-area[2]     COLON 40 LABEL "Fax"
            v-fax[2]                   NO-LABELS
            v-contact[2]      COLON 40 LABEL "Contact"

            WITH NO-BOX STREAM-IO WIDTH 132 NO-ATTR-SPACE NO-UNDERLINE SIDE-LABELS.

        oe-ordl.ftick-prnt = YES.
    END. /* each itemfg */
    SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
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

