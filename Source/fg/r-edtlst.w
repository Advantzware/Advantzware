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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

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
DEFINE VARIABLE v-types AS CHARACTER FORMAT "x(10)" NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-6 begin_date end_date begin_loc ~
end_loc tb_sort t-receipt t-issue t-trans t-adj t-phy tb_total rd-dest ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_loc end_loc ~
tb_sort t-receipt t-issue t-trans t-adj t-phy tb_total rd-dest tbAutoClose 

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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U 
    LABEL "Begining Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc      AS CHARACTER FORMAT "X(8)":U 
    LABEL "Begining Location" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc        AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ending Location" 
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
    "To File", 3
    SIZE 15 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 11.14.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.33.

DEFINE VARIABLE t-adj        AS LOGICAL INITIAL NO 
    LABEL "Adjustments" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY 1
    FONT 6 NO-UNDO.

DEFINE VARIABLE t-issue      AS LOGICAL INITIAL NO 
    LABEL "Shipments" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY 1
    FONT 6 NO-UNDO.

DEFINE VARIABLE t-phy        AS LOGICAL INITIAL NO 
    LABEL "Credit Returns" 
    VIEW-AS TOGGLE-BOX
    SIZE 39 BY 1
    FONT 6 NO-UNDO.

DEFINE VARIABLE t-receipt    AS LOGICAL INITIAL NO 
    LABEL "Receipts" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1
    FONT 6 NO-UNDO.

DEFINE VARIABLE t-trans      AS LOGICAL INITIAL NO 
    LABEL "Transfers" 
    VIEW-AS TOGGLE-BOX
    SIZE 17 BY .81
    FONT 6 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sort      AS LOGICAL INITIAL NO 
    LABEL "Sort By Job?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_total     AS LOGICAL INITIAL NO 
    LABEL "Show Totals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.38 COL 25.6 COLON-ALIGNED
    end_date AT ROW 2.38 COL 66.6 COLON-ALIGNED
    begin_loc AT ROW 3.57 COL 25.6 COLON-ALIGNED
    end_loc AT ROW 3.57 COL 66.6 COLON-ALIGNED
    tb_sort AT ROW 5 COL 37
    t-receipt AT ROW 6.67 COL 37
    t-issue AT ROW 7.62 COL 37
    t-trans AT ROW 8.57 COL 37
    t-adj AT ROW 9.29 COL 37
    t-phy AT ROW 10.24 COL 37
    tb_total AT ROW 11.43 COL 37
    lv-font-no AT ROW 13.29 COL 33 COLON-ALIGNED
    lines-per-page AT ROW 13.38 COL 86.8 COLON-ALIGNED
    lv-ornt AT ROW 13.43 COL 42.4 NO-LABELS
    rd-dest AT ROW 13.48 COL 5 NO-LABELS
    lv-font-name AT ROW 14.52 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 16.24 COL 29.4
    tbAutoClose AT ROW 17.52 COL 29.2 WIDGET-ID 64
    btn-ok AT ROW 18.48 COL 29
    btn-cancel AT ROW 18.48 COL 51.2
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 12.67 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    "Transaction Types" VIEW-AS TEXT
    SIZE 22 BY .62 AT ROW 5.95 COL 24
    FONT 6
    RECT-17 AT ROW 1.52 COL 3
    RECT-6 AT ROW 13.1 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 23
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
        TITLE              = "Finished Goods Edit List"
        HEIGHT             = 19.05
        WIDTH              = 95
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
    tb_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Finished Goods Edit List */
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
ON WINDOW-CLOSE OF C-Win /* Finished Goods Edit List */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
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


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-issue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-issue C-Win
ON VALUE-CHANGED OF t-issue IN FRAME FRAME-A /* Shipments */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
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
        begin_date = TODAY
        END_date   = TODAY
        END_loc    = "zzzzzzzz".

    /*for each fg-rctd where fg-rctd.company                  eq cocode
                       and fg-rctd.rita-code                ne "C"
                        no-lock
                        break by fg-rctd.rita-code:
        if first-of(fg-rctd.rita-code) then v-types = v-types + fg-rctd.rita-code.
    end.
    IF INDEX(v-types,"R") > 0 THEN t-receipt = YES.
    IF index(v-types,"S") > 0 THEN t-issue  = YES.
    IF INDEX(v-types,"T") > 0 THEN t-trans  = YES.
    IF index(v-types,"A") > 0 THEN t-adj = YES.
    IF index(v-types,"E") > 0 THEN t-phy    = YES.*/
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IU4" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
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
    DISPLAY begin_date end_date begin_loc end_loc tb_sort t-receipt t-issue 
        t-trans t-adj t-phy tb_total rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 RECT-6 begin_date end_date begin_loc end_loc tb_sort t-receipt 
        t-issue t-trans t-adj t-phy tb_total rd-dest tbAutoClose btn-ok 
        btn-cancel 
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
    /*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.       */
    /*                                                          */
    /*      if init-dir = "" then init-dir = "c:\temp" .        */
    /*      SYSTEM-DIALOG GET-FILE list-name                    */
    /*          TITLE      "Enter Listing Name to SAVE AS ..."  */
    /*          FILTERS    "Listing Files (*.rpt)" "*.rpt",     */
    /*                     "All Files (*.*)" "*.*"              */
    /*          INITIAL-DIR init-dir                            */
    /*          ASK-OVERWRITE                                   */
    /*     /*     CREATE-TEST-FILE*/                            */
    /*          SAVE-AS                                         */
    /*          USE-FILENAME                                    */
    /*                                                          */
    /*          UPDATE OKpressed.                               */
    /*                                                          */
    /*      IF NOT OKpressed THEN  RETURN NO-APPLY.             */

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
    /* ------------------------------------------------ fg/rep/fg-edlst.p 9/91 cd */
    /* finish goods transactions edit list                                        */
    /* -------------------------------------------------------------------------- */
    {sys/form/r-topw.f}

    DEFINE VARIABLE fdate        LIKE fg-rctd.rct-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.
    DEFINE VARIABLE tdate        LIKE fdate INIT TODAY NO-UNDO.
    DEFINE VARIABLE floc         AS CHARACTER INIT "" NO-UNDO.
    DEFINE VARIABLE tloc         LIKE floc INIT "zzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-sort       AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-trans-time AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-fg-qty     AS DECIMAL   FORMAT "->>,>>>,>>9.99<" NO-UNDO.
    DEFINE VARIABLE v-fg-value   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-tran-type  AS CHARACTER FORMAT "x(1)" NO-UNDO. 
    DEFINE VARIABLE v-totadj     AS DECIMAL   FORMAT "->>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-grd-totadj AS DECIMAL   FORMAT "->>>,>>>,>>9" NO-UNDO. 
    DEFINE VARIABLE v-price      AS DECIMAL   FORMAT "->>>>>>>9.9999999" NO-UNDO.
    DEFINE VARIABLE v-cum-tot    AS de        FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-one        AS CHARACTER FORMAT "x(1)" INIT "1" NO-UNDO.
    DEFINE VARIABLE v-tot-qty    LIKE fg-rctd.t-qty NO-UNDO.

    DEFINE VARIABLE v-whse       LIKE fg-rctd.loc NO-UNDO.                                        
    DEFINE VARIABLE v-pr-tots    AS LOG       FORMAT "Y/N" INIT FALSE NO-UNDO. 


    FORM HEADER                                                                                           
        "ITEM" AT 2 "DESCRIPTION" AT 17 "FROM" AT 70 "QTY" AT 108 "SALES" AT 131
        SKIP
        "DATE" AT 4 "TIME" AT 15   "TY" AT 20 "TAG #" AT 23 "JOB #" AT 45 
        "P.O. #" AT 54 "VENDOR" AT 61                                   
        "BIN" AT 70  "CASES" AT 81
        "QTY/CASE" AT 88  "UOM" AT 97 "TOTAL" AT 108 "COST" AT 117 "VALUE" AT 131
        FILL("=",135) FORMAT "x(135)"  
        WITH FRAME f-top PAGE-TOP NO-BOX NO-LABELS STREAM-IO WIDTH 140.

    FORM
        fg-rctd.rct-date AT 4 SPACE(1)
        v-trans-time      FORMAT "x(5)"
        v-tran-type
        fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
        fg-rctd.job-no SPACE(0) "-" SPACE(0)
        fg-rctd.job-no2 FORMAT "999" SPACE(2)
        fg-rctd.po-no        FORMAT "x(8)"  AT 55                                                            
        po-ord.vend-no       AT 61
        fg-rctd.loc-bin      AT 70       
        fg-rctd.cases        TO 85
        fg-rctd.qty-case     TO 95
        fg-rctd.pur-uom     FORMAT "x(3)" AT 97
        v-tot-qty            FORMAT "->>>,>>>,>>9"  AT 101
        /*
            fg-rctd.t-qty        format "->>>,>>>,>>9"  at 99
        */
        fg-rctd.std-cost         AT 114 FORMAT "->>,>>9"
        v-fg-value           FORMAT "->,>>>,>>9.99" TO 135
        WITH FRAME detail NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 140.

    FORM
        fg-rctd.rct-date AT 4 SPACE(1)
        v-trans-time      FORMAT "x(5)"
        v-tran-type
        fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
        fg-rctd.job-no SPACE(0) "-" SPACE(0)
        fg-rctd.job-no2 FORMAT "999" SPACE(2)
        fg-rctd.po-no        FORMAT "x(6)" AT 55                                                            
        po-ord.vend-no       AT 61                                                              
        fg-rctd.loc-bin      AT 70  
        v-one                TO 85
        fg-rctd.partial      TO 95
        fg-rctd.pur-uom     FORMAT "x(3)" AT 97                                                                    
        v-tot-qty            FORMAT "->>>,>>>,>>9"  AT 101
        /*
            fg-rctd.t-qty        format "->>>,>>>,>>9"  at 99
        */ 
        fg-rctd.std-cost         AT 114 FORMAT "->>,>>9"
        v-fg-value           FORMAT "->,>>>,>>9.99" TO 135
        WITH FRAME pdetail NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 140.

    ASSIGN 
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112} 
        fdate     = begin_date
        tdate     = end_date
        floc      = begin_loc
        tloc      = end_loc
        v-types   = (IF t-receipt THEN "R" ELSE "") +
                    (IF t-issue THEN "S" ELSE "") +
                    (IF t-trans THEN "T" ELSE "") +
                    (IF t-adj THEN "A" ELSE "") +
                    (IF t-phy THEN "E" ELSE "")                                 
        v-sort    = tb_sort              
        v-pr-tots = tb_total.

    {sa/sa-sls01.i}

    {sys/inc/print1.i}

    SESSION:SET-WAIT-STATE ("general").

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH fg-rctd
        WHERE fg-rctd.company                  EQ cocode
        AND fg-rctd.rct-date               GE fdate
        AND fg-rctd.rct-date               LE tdate
        AND index(v-types,fg-rctd.rita-code) GT 0
        AND fg-rctd.rita-code                NE "C"
        AND fg-rctd.loc GE floc 
        AND fg-rctd.loc LE tloc  NO-LOCK :
        CREATE report.
        ASSIGN
            report.term-id = v-term
            report.key-01  = IF v-sort THEN STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', fg-rctd.job-no, fg-rctd.job-no2))                          
                        ELSE fg-rctd.loc                                                               
            report.key-02  = fg-rctd.i-no
            report.key-03  = STRING( YEAR(fg-rctd.rct-date),"9999") +
                        string(MONTH(fg-rctd.rct-date),"99")   +
                        string(  DAY(fg-rctd.rct-date),"99")
            /*report.key-04  = string(recid(fg-rctd))*/
            report.rec-id  = RECID(fg-rctd).
    END.

    FOR EACH report    WHERE report.term-id  EQ v-term,
        FIRST fg-rctd WHERE RECID(fg-rctd) EQ report.rec-id NO-LOCK
        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        BY fg-rctd.r-no:

        /* if v-sort and first-of(report.key-01) then page. */                                /* begin ekwtest */
        IF FIRST-OF(report.key-01) THEN 
        DO:
            ASSIGN 
                v-whse = fg-rctd.loc.
            /* if first(report.key-01) then do:
                hide frame r-top.
                view frame r-top.
                page.
             end. /* if first(report.key-01) */
    
             else 
             */
            PUT SKIP(3) "WHSE:" v-whse SKIP.
            IF FIRST(report.key-01) THEN DISPLAY WITH FRAME f-top.
        END.   /* if first-of(report.key-01) */                                                     /* end ekwtest */


        IF FIRST-OF(report.key-02) THEN 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ fg-rctd.company
                AND itemfg.i-no    EQ fg-rctd.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:
                FIND FIRST uom
                    WHERE uom.uom  EQ itemfg.sell-uom
                    AND uom.mult NE 0
                    NO-LOCK NO-ERROR.
                v-price = IF itemfg.sell-uom EQ "L" THEN itemfg.sell-price
                ELSE (itemfg.sell-price /
                    IF AVAILABLE uom THEN uom.mult ELSE 1000).
                IF v-price LT 0 THEN v-price = 0.
            END.

            /*djk*/
            PUT fg-rctd.i-no FORMAT "x(16)" 
                fg-rctd.i-name SPACE(21)
                /*
                            fg-rctd.cases          format "->>>>9" to 75 /*space(2)*/
                           fg-rctd.qty-case       format "->>>>9" to 85 /*space(4)*/
                */          

                /*"SELLING PRICE:"*/  "" AT 122
                itemfg.sell-price itemfg.sell-uom SKIP.
        END.

        ASSIGN 
            v-fg-value = fg-rctd.t-qty * v-price.

        IF fg-rctd.rita-code EQ "R" THEN
            ASSIGN
                v-totadj    = v-totadj + fg-rctd.t-qty
                v-cum-tot   = v-cum-tot + v-fg-value
                v-tran-type = "REC".

        ELSE
            IF fg-rctd.rita-code EQ "T" THEN v-tran-type = "TRAN".

            ELSE
                IF fg-rctd.rita-code EQ "A" THEN
                    ASSIGN
                        v-totadj    = v-totadj + fg-rctd.t-qty
                        v-cum-tot   = v-cum-tot + v-fg-value
                        v-tran-type = "ADJ".

                ELSE
                    IF fg-rctd.rita-code EQ "S" THEN
                        ASSIGN
                            v-totadj    = v-totadj - fg-rctd.t-qty
                            v-cum-tot   = v-cum-tot - v-fg-value
                            v-tran-type = "SHIP".

                    ELSE
                        IF fg-rctd.rita-code EQ "E" THEN
                            ASSIGN
                                v-totadj    = v-totadj + fg-rctd.t-qty
                                v-cum-tot   = v-cum-tot + v-fg-value
                                v-tran-type = "CRED".

                        ELSE
                            ASSIGN
                                v-totadj    = v-totadj + fg-rctd.t-qty
                                v-tran-type = "UNKN".

        IF LINE-COUNTER GT 56 THEN PAGE.

        IF fg-rctd.po-no <> " " THEN
            FIND po-ord WHERE po-ord.po-no = int(fg-rctd.po-no) NO-LOCK NO-ERROR.
        ASSIGN
            v-tot-qty    = fg-rctd.cases * fg-rctd.qty-case
            v-fg-value   = v-tot-qty * v-price
            v-trans-time = STRING(fg-rctd.trans-time, "HH:MM").

        DISPLAY fg-rctd.rct-date 
            WHEN FIRST-OF(report.key-03)  
            v-trans-time     
            WHEN FIRST-OF(report.key-03) 
            v-tran-type 
            fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
            fg-rctd.job-no
            fg-rctd.job-no2
            fg-rctd.po-no
            po-ord.vend-no                     
            WHEN AVAILABLE po-ord
            fg-rctd.loc-bin 

            fg-rctd.cases        FORMAT "->>,>>9"

            fg-rctd.qty-case
            fg-rctd.pur-uom 
            v-tot-qty  
            fg-rctd.std-cost    FORMAT ">>>,>>9"
            v-fg-value 

            WITH FRAME detail.                                                       
        DOWN 2 WITH FRAME detail.

        IF fg-rctd.partial <> 0 THEN 
        DO:
            ASSIGN
                v-tot-qty    = fg-rctd.partial
                v-fg-value   = v-tot-qty * v-price
                v-trans-time = STRING(fg-rctd.trans-time, "HH:MM").

            DISPLAY fg-rctd.rct-date 
                WHEN FIRST-OF(report.key-03) 
                v-trans-time       
                WHEN FIRST-OF(report.key-03) 
                v-tran-type 
                fg-rctd.tag       FORMAT "x(20)" /* gdm - 12090821 */
                fg-rctd.job-no
                fg-rctd.job-no2
                fg-rctd.po-no
                po-ord.vend-no                     
                WHEN AVAILABLE po-ord
                fg-rctd.loc-bin  
                v-one
                fg-rctd.partial FORM "->>,>>9"
                fg-rctd.pur-uom 
                /*            
                              fg-rctd.t-qty 
                */
                v-tot-qty   
                fg-rctd.std-cost    FORMAT ">>>,>>9"
                v-fg-value

                WITH FRAME pdetail.                                                       
            DOWN WITH FRAME pdetail.
        END.

        IF v-tran-type BEGINS "T" THEN
            PUT "TO: " AT 66 fg-rctd.loc2 fg-rctd.loc-bin2 SKIP(1).

        IF LAST-OF(report.key-02) AND v-pr-tots THEN 
        DO:                                             
            PUT "Total Adjustment: " AT 83 v-totadj SKIP.                         
            v-grd-totadj = v-grd-totadj + v-totadj.                                   
            v-totadj = 0.
        END.

        IF LAST(fg-rctd.r-no) THEN DELETE report.
    END. /* each fg-rctd */

    IF v-pr-tots THEN                                                                         
        PUT SKIP "--------------" TO 115 SKIP "Grand Total Adjustment: " TO 100 v-grd-totadj  SKIP. 

    OUTPUT CLOSE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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

