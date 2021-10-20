&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\fgglobpr.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE tt-rowid NO-UNDO 
    FIELD row-id AS ROWID
    INDEX row-id row-id.

DEFINE VARIABLE v-process AS LOG           NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no begin_vend ~
end_vend percent_chg amount_chg tb_undo btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_vend end_vend ~
percent_chg amount_chg tb_undo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win     AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE amount_chg  AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "$Amt Change" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "Percent Change" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 7.81.

DEFINE VARIABLE tb_undo AS LOGICAL INITIAL NO 
    LABEL "UNDO a prior price change?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_i-no AT ROW 2.91 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 2.91 COL 67 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_vend AT ROW 4.1 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 4.1 COL 67 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    percent_chg AT ROW 5.29 COL 26 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    amount_chg AT ROW 6.48 COL 26 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    tb_undo AT ROW 7.91 COL 28
    btn-process AT ROW 9.95 COL 21
    btn-cancel AT ROW 9.95 COL 53
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .62 AT ROW 1.14 COL 5
    RECT-17 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.4 BY 11.38
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
        TITLE              = "Global Price Change"
        HEIGHT             = 11.38
        WIDTH              = 96.2
        MAX-HEIGHT         = 19.76
        MAX-WIDTH          = 110
        VIRTUAL-HEIGHT     = 19.76
        VIRTUAL-WIDTH      = 110
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
    amount_chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    percent_chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_undo:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Price Change */
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
ON WINDOW-CLOSE OF C-Win /* Global Price Change */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME amount_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amount_chg C-Win
ON LEAVE OF amount_chg IN FRAME FRAME-A /* $Amt Change */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amount_chg C-Win
ON VALUE-CHANGED OF amount_chg IN FRAME FRAME-A /* $Amt Change */
    DO:
        IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
            percent_chg:HIDDEN = NO.
        ELSE
            ASSIGN
                percent_chg:SCREEN-VALUE = "0"
                percent_chg:HIDDEN       = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning FG Item# */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        v-process = NO.

        MESSAGE "Are you sure you want to change the Price Matrix(es) within the " +
            "selection parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN
        DO:
            RUN run-process.
            MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
            APPLY "close" TO THIS-PROCEDURE.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending FG Item# */
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


&Scoped-define SELF-NAME percent_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON LEAVE OF percent_chg IN FRAME FRAME-A /* Percent Change */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON VALUE-CHANGED OF percent_chg IN FRAME FRAME-A /* Percent Change */
    DO:
        IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
            amount_chg:HIDDEN = NO.
        ELSE
            ASSIGN
                amount_chg:SCREEN-VALUE = "0"
                amount_chg:HIDDEN       = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.

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

   
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png"). 
    RUN enable_UI.

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
    DISPLAY begin_i-no end_i-no begin_vend end_vend percent_chg amount_chg tb_undo 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_i-no end_i-no begin_vend end_vend percent_chg amount_chg 
        tb_undo btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-params C-Win 
PROCEDURE get-params :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-fld-list AS cha NO-UNDO.

    DEFINE VARIABLE lv-frame-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.

        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0 THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                op-fld-list = TRIM(op-fld-list) + " " +
                    lv-field-hdl:LABEL + ":" +
                    lv-field-hdl:SCREEN-VALUE + ",".

            ELSE 
            DO:  /* radio set */
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE.

                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                        op-fld-list = TRIM(op-fld-list) + " " +
                            lv-field2-hdl:SCREEN-VALUE + ":".

                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END. 

                op-fld-list = TRIM(op-fld-list) +
                    lv-field-hdl:SCREEN-VALUE + ",".      
            END.                 
        END.

        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    /* --------------------------------------------------- rm/gprcchg.p 05/96 JLF */
    /* Global Price Change by Mat-type --- for est mat only                       */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-i-no    LIKE item.i-no EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-vend-no LIKE item.vend-no EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-pct     AS DECIMAL FORMAT "->>9.99" NO-UNDO.

    DEFINE VARIABLE li        AS INTEGER NO-UNDO.


    ASSIGN
        v-i-no[1]    = begin_i-no
        v-i-no[2]    = end_i-no
        v-vend-no[1] = begin_vend
        v-vend-no[2] = end_vend
        v-pct        = percent_chg.

    SESSION:SET-WAIT-STATE("General").

    EMPTY TEMP-TABLE tt-rowid.

    CREATE reftable.
    ASSIGN
        reftable.reftable = "fg/fgglobpr.w"
        reftable.company  = cocode
        reftable.loc      = STRING(YEAR(TODAY),"9999") +
                     STRING(MONTH(TODAY),"99")  +
                     STRING(DAY(TODAY),"99")
        reftable.code     = STRING(TIME,"99999")
        reftable.code2    = USERID("nosweat").

    RUN get-params (OUTPUT reftable.dscr).

    FOR EACH itemfg
    {sys/look/itemfgW.i}
      AND itemfg.i-no GE v-i-no[1]
      AND itemfg.i-no LE v-i-no[2]   

    {rm/gprcchg1.i "itemfg"}
    END.

    RELEASE e-itemfg-vend.
    RELEASE itemfg.
    RELEASE reftable.
    RELEASE e-itemfg.

    SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

