&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_upd-code begin_rm-no end_rm-no ~
begin_vend end_vend percent_chg amount_chg tb_undo select-mat tbAutoClose ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_upd-code begin_rm-no end_rm-no ~
begin_vend end_vend percent_chg amount_chg tb_undo select-mat tbAutoClose 

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

DEFINE VARIABLE amount_chg     AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "$Amt Change" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no    AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_upd-code AS CHARACTER FORMAT "xx":U 
    LABEL "Updated Code" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no      AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg    AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "Percent Change" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 86 BY 15.19.

DEFINE VARIABLE select-mat  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 44 BY 5.71 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_undo     AS LOGICAL   INITIAL NO 
    LABEL "UNDO a prior price change?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_upd-code AT ROW 2.91 COL 25.6 COLON-ALIGNED HELP
    "Enter an Update Code"
    begin_rm-no AT ROW 3.86 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_rm-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_vend AT ROW 4.81 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 4.81 COL 66 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    percent_chg AT ROW 5.76 COL 25.6 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    amount_chg AT ROW 6.71 COL 25.6 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    tb_undo AT ROW 8.14 COL 27.6
    select-mat AT ROW 10.52 COL 27.6 HELP
    "Enter description of this Material Type." NO-LABELS
    mat-types AT ROW 13.14 COL 19.4 COLON-ALIGNED
    tbAutoClose AT ROW 16.86 COL 40 WIDGET-ID 62
    btn-process AT ROW 17.81 COL 29.6
    btn-cancel AT ROW 17.81 COL 49.6
    "Select/Deselect Material Types" VIEW-AS TEXT
    SIZE 38 BY 1 AT ROW 9.57 COL 30.6
    FONT 6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .62 AT ROW 1.14 COL 5
    RECT-17 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 92 BY 19.14
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
        HEIGHT             = 19.14
        WIDTH              = 92
        MAX-HEIGHT         = 20.86
        MAX-WIDTH          = 98.2
        VIRTUAL-HEIGHT     = 20.86
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
                                                                        */
ASSIGN 
    amount_chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_upd-code:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    mat-types:HIDDEN IN FRAME FRAME-A = TRUE.

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


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_upd-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_upd-code C-Win
ON LEAVE OF begin_upd-code IN FRAME FRAME-A /* Updated Code */
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
        v-process = NO.

        MESSAGE "Are you sure you want to change the Price Matrix(es) within the " +
            "selection parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.
  
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
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


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
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


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
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

    FOR EACH mat:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

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
    DISPLAY begin_upd-code begin_rm-no end_rm-no begin_vend end_vend percent_chg 
        amount_chg tb_undo select-mat tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 begin_upd-code begin_rm-no end_rm-no begin_vend end_vend 
        percent_chg amount_chg tb_undo select-mat tbAutoClose btn-process 
        btn-cancel 
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


    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

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

    DEFINE VARIABLE v-i-no       LIKE item.i-no EXTENT 2.
    DEFINE VARIABLE v-vend-no    LIKE item.vend-no EXTENT 2.
    DEFINE VARIABLE v-mat-type   LIKE item.mat-type INIT "*".
    DEFINE VARIABLE v-price-code LIKE item.price-code.
    DEFINE VARIABLE v-pct        AS DECIMAL   FORMAT "->>9.99".
    DEFINE VARIABLE v-mtype      AS CHARACTER FORMAT "x(47)".

    DEFINE VARIABLE li           AS INTEGER   NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    ASSIGN
        v-i-no[1]    = begin_rm-no
        v-i-no[2]    = end_rm-no
        v-vend-no[1] = begin_vend
        v-vend-no[2] = end_vend
        v-price-code = begin_upd-code
        v-pct        = percent_chg.

    DO WITH FRAME {&frame-name}:
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF v-mtype NE "" AND substr(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            substr(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

        mat-types = v-mtype.

        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.
    END.

    SESSION:SET-WAIT-STATE("General").

    EMPTY TEMP-TABLE tt-rowid.

    DO li = 1 TO LENGTH(v-mtype):
        FOR EACH item
        {sys/look/itemW.i}   
        AND item.mat-type EQ SUBSTR(v-mtype,li,1)
        AND item.i-no     GE v-i-no[1]
        AND item.i-no     LE v-i-no[2]

        {rm/gprcchg1.i item}

            item.price-code = v-price-code.
        END.
    END.

    SESSION:SET-WAIT-STATE("").

    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

