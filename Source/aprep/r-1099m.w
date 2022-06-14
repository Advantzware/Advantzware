&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-1099m.w

  Description: 1099-MISC

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
DEFINE VARIABLE list-name      AS cha       NO-UNDO.
DEFINE VARIABLE init-dir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-program      AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.

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

{aprep\r-1099m.i NEW}
{custom/xprint.i}

DEFINE BUFFER xperiod FOR period.

DEFINE VARIABLE v-num-per       LIKE company.num-per NO-UNDO.
DEFINE VARIABLE v-print-fmt     AS CHARACTER NO-UNDO.

DEFINE VARIABLE retcode         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE d-print-fmt-dec AS DECIMAL   NO-UNDO.
DEFINE VARIABLE c1099Misc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE c1099MiscLeftMargin AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend rs-date ~
begin_date end_date lv-copies tb_zero-ven rd-dest tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend rs-date begin_date ~
end_date lv-copies tb_zero-ven rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/01 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-copies      AS INTEGER   FORMAT ">9":U INITIAL 1 
    LABEL "Copies/Set" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier NEW SIZE=10(12CPI)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "14" 
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
    "To File", 3
    SIZE 16 BY 3.33 NO-UNDO.

DEFINE VARIABLE rs-date        AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Invoice Date", "Invoice",
    "Check Date", "Check"
    SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 3.91.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 7.14.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_zero-ven AS LOGICAL INITIAL NO 
    LABEL "Include Vendors with Zero Amounts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 41 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend AT ROW 2.48 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 2.48 COL 67 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    rs-date AT ROW 3.76 COL 30.2 NO-LABELS
    begin_date AT ROW 5.1 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 5.1 COL 67 COLON-ALIGNED HELP
    "Enter Ending Date"
    lv-copies AT ROW 6.1 COL 28 COLON-ALIGNED
    tb_zero-ven AT ROW 7.33 COL 30.2
    rd-dest AT ROW 9.57 COL 5 NO-LABELS
    lv-font-no AT ROW 9.57 COL 35 COLON-ALIGNED
    lv-ornt AT ROW 9.57 COL 44 NO-LABELS
    lines-per-page AT ROW 9.57 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 10.76 COL 29 COLON-ALIGNED NO-LABELS
    tbAutoClose AT ROW 13.38 COL 28 WIDGET-ID 78
    btn-ok AT ROW 14.33 COL 28
    btn-cancel AT ROW 14.33 COL 56
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8.81 COL 4
    RECT-6 AT ROW 9.24 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 19.95
    BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "1099-MISC"
        HEIGHT             = 14.86
        WIDTH              = 94.6
        MAX-HEIGHT         = 20.48
        MAX-WIDTH          = 95.8
        VIRTUAL-HEIGHT     = 20.48
        VIRTUAL-WIDTH      = 95.8
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_zero-ven:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* 1099-MISC */
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
ON WINDOW-CLOSE OF C-Win /* 1099-MISC */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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
            ASSIGN rd-dest rs-date begin_vend end_vend begin_date end_date
                lv-copies tb_zero-ven.
        END.

        RUN run-report.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE. 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-copies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-copies C-Win
ON LEAVE OF lv-copies IN FRAME FRAME-A /* Copies/Set */
    DO:
        ASSIGN lv-copies.
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


&Scoped-define SELF-NAME rs-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-date C-Win
ON VALUE-CHANGED OF rs-date IN FRAME FRAME-A
    DO:
        DO WITH FRAME {&FRAME-NAME}:

            ASSIGN rs-date
                begin_date:LABEL = "Beginning " + rs-date + " Date"
                end_date:LABEL   = "Ending " + rs-date + " Date".

            DISPLAY begin_date end_date WITH FRAME {&FRAME-NAME}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero-ven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-ven C-Win
ON VALUE-CHANGED OF tb_zero-ven IN FRAME FRAME-A /* Include Vendors with Zero Amounts? */
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

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
    IF AVAILABLE company THEN v-num-per = company.num-per.

    ASSIGN
        begin_date = ?
        end_date   = ?.

    RUN spGetSettingByName ("1099Misc", OUTPUT c1099Misc).
    RUN spGetSettingByName ("1099MiscLeftMargin", OUTPUT c1099MiscLeftMargin).
    
    ASSIGN
        v-print-fmt = c1099Misc
        d-print-fmt-dec = DECIMAL(c1099MiscLeftMargin).
    
    CASE v-print-fmt:
        WHEN "Fibre" THEN
            ASSIGN 
                v-program      = "aprep/fibremsc.p"
                lines-per-page = 50
                is-xprint-form = YES.
        WHEN "1Up1099" THEN
            ASSIGN 
                v-program      = "aprep/fibremsc.p"
                lines-per-page = 50
                is-xprint-form = YES. 
        WHEN "2Up1099" THEN
            ASSIGN 
                v-program      = "aprep/MscTwoUp.p"
                lines-per-page = 50
                is-xprint-form = YES.          
    END.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    

    {methods/nowait.i}
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total-proc C-Win 
PROCEDURE calc-total-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iop-vend-tot AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-last-of-vend AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        iop-vend-tot = iop-vend-tot + (ap-payl.amt-paid ).

        IF ip-last-of-vend THEN 
        DO:
            IF tb_zero-ven:CHECKED THEN 
            DO:
                CREATE tt-1099-m.
                ASSIGN 
                    tt-1099-m.vend-no        = vend.vend-no
                    tt-1099-m.vend-name      = vend.NAME
                    tt-1099-m.vend-tax-id    = vend.tax-id
                    tt-1099-m.vend-add1      = vend.add1
                    tt-1099-m.vend-add2      = vend.add2
                    tt-1099-m.vend-city-line = vend.city + ","
                                          + " " + vend.state
                                          + " " + vend.zip
                    tt-1099-m.vend-total     = iop-vend-tot
                    tt-1099-m.vend-box       = vend.code-1099 .
                RELEASE tt-1099-m.
            END.
            ELSE 
            DO:
                IF iop-vend-tot NE 0 THEN 
                DO:
                    CREATE tt-1099-m.
                    ASSIGN 
                        tt-1099-m.vend-no        = vend.vend-no
                        tt-1099-m.vend-name      = vend.NAME
                        tt-1099-m.vend-tax-id    = vend.tax-id
                        tt-1099-m.vend-add1      = vend.add1
                        tt-1099-m.vend-add2      = vend.add2
                        tt-1099-m.vend-city-line = vend.city + ","
                                          + " " + vend.state
                                          + " " + vend.zip
                        tt-1099-m.vend-total     = iop-vend-tot
                        tt-1099-m.vend-box       = vend.code-1099.
                    RELEASE tt-1099-m.
                END.
            END.

            iop-vend-tot  = 0.
        END.
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
    DISPLAY begin_vend end_vend rs-date begin_date end_date lv-copies tb_zero-ven 
        rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend end_vend rs-date begin_date end_date 
        lv-copies tb_zero-ven rd-dest tbAutoClose btn-ok btn-cancel 
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

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DEFINE VARIABLE v-vend-tot AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-count    AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE tt-1099-m.

    {sys/form/r-top.i}
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    IF IS-xprint-form THEN
    DO:
        CASE rd-dest:
            WHEN 1 THEN 
                PUT  "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)" . 
                    ELSE
                        PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".        
                END.
        END CASE.
        PUT "</PROGRESS>".
    END.

    s-copies = lv-copies.

    SESSION:SET-WAIT-STATE ("general").

    IF rs-date EQ "Invoice" THEN
        FOR EACH vend FIELDS(company vend-no code-1099 NAME tax-id add1
            add2 city state zip) WHERE
            vend.company   EQ cocode AND
            vend.vend-no   GE begin_vend AND
            vend.vend-no   LE end_vend AND
            vend.code-1099 NE "N" AND
            vend.code-1099 NE ""
            NO-LOCK,
            EACH ap-inv FIELDS(company vend-no inv-date posted) WHERE
            ap-inv.company EQ cocode AND
            ap-inv.vend-no EQ vend.vend-no AND
            ap-inv.inv-date GE begin_date AND
            ap-inv.inv-date LE end_date AND
            ap-inv.posted EQ YES
            NO-LOCK,
            EACH ap-payl FIELDS(amt-paid amt-disc) WHERE
            ap-payl.inv-no   EQ ap-inv.inv-no AND
            ap-payl.vend-no  EQ ap-inv.vend-no AND
            ap-payl.posted   EQ YES
            NO-LOCK
            BREAK BY vend.vend-no:

            RUN calc-total-proc(INPUT-OUTPUT v-vend-tot,
                INPUT LAST-OF(vend.vend-no)).
        END.
    ELSE /* Check*/
        FOR EACH vend FIELDS(company vend-no code-1099 NAME tax-id add1
            add2 city state zip) WHERE
            vend.company   EQ cocode AND
            vend.vend-no   GE begin_vend AND
            vend.vend-no   LE end_vend AND
            vend.code-1099 NE "N" AND
            vend.code-1099 NE ""
            NO-LOCK,
            EACH ap-pay FIELDS(company vend-no check-date posted) WHERE
            ap-pay.company    EQ cocode AND
            ap-pay.vend-no    EQ vend.vend-no AND
            ap-pay.check-date GE begin_date AND
            ap-pay.check-date LE end_date AND
            ap-pay.posted     EQ YES
            NO-LOCK,
            EACH ap-payl FIELDS(amt-paid amt-disc) WHERE
            ap-payl.c-no EQ ap-pay.c-no AND
            ap-payl.memo EQ NO
            NO-LOCK
            BREAK BY vend.vend-no:

            RUN calc-total-proc(INPUT-OUTPUT v-vend-tot,
                INPUT LAST-OF(vend.vend-no)).
        END.

    RUN value(v-program).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

