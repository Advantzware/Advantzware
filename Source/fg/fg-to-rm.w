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

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-process AS LOG NO-UNDO.

{oe/d-selbin.i NEW}

DEFINE VARIABLE ll-rm-warning AS LOG NO-UNDO.
DEFINE BUFFER bf-item FOR ITEM.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 RECT-19 begin_i-no ~
fi_fg-name begin_rm-no rd_type fi_rm-name rd_um cUom roll_rm btn-process ~
btn-cancel label-1 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no fi_fg-name begin_rm-no rd_type ~
fi_rm-name rd_um cUom fi_uom-dscr roll_rm label-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetFgBin C-Win 
FUNCTION fGetFgBin RETURNS LOGICAL
    ( ipcFgItem AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "From FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 41.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
    LABEL "To RM Item#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cUom        AS CHARACTER FORMAT "X(3)":U 
    LABEL "UOM" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 6.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_fg-name  AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN NATIVE 
    SIZE 74 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_rm-name  AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 74 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_uom-dscr AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN NATIVE 
    SIZE 32.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE label-1     AS CHARACTER FORMAT "X(10)":U INITIAL "RM U/M:" 
    VIEW-AS TEXT 
    SIZE 11 BY .62
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE roll_rm     AS INTEGER   FORMAT "->>>,>>9":U INITIAL 0 
    LABEL "Roll RM" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE rd_type     AS CHARACTER INITIAL "Board" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Board", "Board",
    "Misc", "Misc"
    SIZE 23.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE rd_um       AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "FG U/M", "FG",
    "RM U/M", "RM"
    SIZE 25 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 9.29.

DEFINE RECTANGLE RECT-18
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 26 BY 1.19.

DEFINE RECTANGLE RECT-19
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 27 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_i-no AT ROW 2.33 COL 20.4 COLON-ALIGNED HELP
    "Enter FG Item#"
    fi_fg-name AT ROW 3.52 COL 20.4 COLON-ALIGNED NO-LABELS
    begin_rm-no AT ROW 4.71 COL 20.4 COLON-ALIGNED HELP
    "Enter RM Item#"
    rd_type AT ROW 4.71 COL 54.4 NO-LABELS
    fi_rm-name AT ROW 5.91 COL 20.4 COLON-ALIGNED NO-LABELS
    rd_um AT ROW 7.38 COL 23.4 NO-LABELS WIDGET-ID 2
    cUom AT ROW 7.38 COL 55.2 COLON-ALIGNED HELP
    "Enter RM Item#" WIDGET-ID 14
    fi_uom-dscr AT ROW 7.38 COL 62 COLON-ALIGNED NO-LABELS WIDGET-ID 16
    roll_rm AT ROW 9.1 COL 20.4 COLON-ALIGNED HELP
    "Enter the Roll length in feet or zero if not a roll"
    btn-process AT ROW 11.19 COL 32.2
    btn-cancel AT ROW 11.19 COL 55
    label-1 AT ROW 7.57 COL 7.2 COLON-ALIGNED NO-LABELS WIDGET-ID 12
    "Enter Roll length in feet or zero if not a roll" VIEW-AS TEXT
    SIZE 56 BY 1 AT ROW 9.1 COL 40.4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 26 BY .62 AT ROW 1 COL 4
    RECT-17 AT ROW 1.48 COL 3
    RECT-18 AT ROW 4.62 COL 52.8 WIDGET-ID 8
    RECT-19 AT ROW 7.33 COL 22.4 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 99.4 BY 11.95
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
        TITLE              = "Create Board/Misc RM from FG"
        HEIGHT             = 11.95
        WIDTH              = 99.4
        MAX-HEIGHT         = 32.86
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 32.86
        VIRTUAL-WIDTH      = 273.2
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

/* SETTINGS FOR FILL-IN fi_uom-dscr IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    roll_rm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create Board/Misc RM from FG */
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
ON WINDOW-CLOSE OF C-Win /* Create Board/Misc RM from FG */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* From FG Item# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON VALUE-CHANGED OF begin_i-no IN FRAME FRAME-A /* From FG Item# */
    DO:
        RUN new-i-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* To RM Item# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-rm-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.

        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON VALUE-CHANGED OF begin_rm-no IN FRAME FRAME-A /* To RM Item# */
    DO:
        RUN new-rm-no.
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
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        ASSIGN rd_type begin_i-no begin_rm-no roll_rm rd_um.

        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-rm-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        RUN valid-uom(OUTPUT lReturnError) NO-ERROR.
        IF lReturnError THEN RETURN NO-APPLY.

        v-process = NO.

        MESSAGE "Are you sure you want to" TRIM(c-win:TITLE) +
            " within the selection parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cUom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUom C-Win
ON HELP OF cUom IN FRAME FRAME-A /* UOM */
    DO:
        DEFINE VARIABLE char-val AS cha       NO-UNDO.
        DEFINE VARIABLE uom-list AS CHARACTER.
        RUN sys/ref/uom-rm.p  (SUBSTRING(rd_type,1,1), OUTPUT uom-list).
        RUN windows/l-stduom.w (cocode,uom-list, cUom:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val).
         
        IF char-val <> "" THEN 
            ASSIGN cUom:screen-value IN FRAME {&frame-name}        = ENTRY(1,char-val)
                fi_uom-dscr:screen-value IN FRAME {&frame-name} = ENTRY(2,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUom C-Win
ON LEAVE OF cUom IN FRAME FRAME-A /* UOM */
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
        END.

        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUom C-Win
ON VALUE-CHANGED OF cUom IN FRAME FRAME-A /* UOM */
    DO:
        RUN new-uom.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_type C-Win
ON VALUE-CHANGED OF rd_type IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.

        IF rd_type EQ "Misc" THEN
            ASSIGN
                rd_um:HIDDEN       = YES
                rect-19:HIDDEN     = YES
                label-1:HIDDEN     = YES
                cUom:HIDDEN        = YES
                fi_uom-dscr:HIDDEN = YES
                roll_rm:SENSITIVE  = NO .
        ELSE
            ASSIGN
                rd_um:HIDDEN       = NO
                rect-19:HIDDEN     = NO
                label-1:HIDDEN     = NO
                cUom:HIDDEN        = NO
                fi_uom-dscr:HIDDEN = NO
                roll_rm:SENSITIVE  = YES .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_um
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_um C-Win
ON VALUE-CHANGED OF rd_um IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.

        IF rd_um EQ "FG" THEN
            ASSIGN         
                cUom:SENSITIVE        = NO
                fi_uom-dscr:SENSITIVE = NO.
        ELSE
            ASSIGN          
                cUom:SENSITIVE = YES
                .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME roll_rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL roll_rm C-Win
ON LEAVE OF roll_rm IN FRAME FRAME-A /* Roll RM */
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
    /* check security */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cUom:SENSITIVE IN FRAME {&FRAME-NAME}        = NO
            fi_uom-dscr:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        DISABLE fi_fg-name . 
        APPLY "entry" TO begin_i-no.
    END.  
  
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
    DISPLAY begin_i-no fi_fg-name begin_rm-no rd_type fi_rm-name rd_um cUom 
        fi_uom-dscr roll_rm label-1 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 RECT-18 RECT-19 begin_i-no fi_fg-name begin_rm-no rd_type 
        fi_rm-name rd_um cUom roll_rm btn-process btn-cancel label-1 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no C-Win 
PROCEDURE new-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        FIND itemfg
            {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ begin_i-no:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            fi_fg-name:SCREEN-VALUE = itemfg.i-name.

            IF begin_rm-no:SCREEN-VALUE EQ "" THEN
                begin_rm-no:SCREEN-VALUE = CAPS(itemfg.i-no).
            cUom:SCREEN-VALUE = CAPS(itemfg.pur-uom).  
            RUN new-uom.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rm-no C-Win 
PROCEDURE new-rm-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        ll-rm-warning = NO.

        FIND item
            WHERE item.company EQ cocode
            AND item.i-no    EQ begin_rm-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN fi_rm-name:SCREEN-VALUE = item.i-name.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-uom C-Win 
PROCEDURE new-uom :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}: 
        FIND FIRST uom NO-LOCK
            WHERE uom.uom EQ cUom:SCREEN-VALUE          
            NO-ERROR.    
        IF AVAILABLE uom THEN fi_uom-dscr:SCREEN-VALUE = uom.dscr.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    /* -------------------------------------------------- fg/fg-to-rm.p 12/99 JLF */
    /*  Move a FG to a Board RM                                                   */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE save_id        AS RECID     NO-UNDO.
    DEFINE VARIABLE fil_id         AS RECID     NO-UNDO.
    DEFINE VARIABLE v-rcpth-no     AS INTEGER.
    DEFINE VARIABLE v-qty          AS DECIMAL.
    DEFINE VARIABLE v-cost         AS DECIMAL.
    DEFINE VARIABLE v-r-qty        AS DECIMAL.
    DEFINE VARIABLE v-i-qty        AS DECIMAL.
    DEFINE VARIABLE v-t-qty        AS DECIMAL.

    DEFINE VARIABLE fitm           LIKE itemfg.i-no.
    DEFINE VARIABLE titm           LIKE item.i-no.
    DEFINE VARIABLE troll          AS INTEGER   FORMAT ">>,>>9".
    DEFINE VARIABLE v-board        AS LOG       FORMAT "Board/Misc" INIT YES.
    DEFINE VARIABLE uom-list       AS CHARACTER.
    DEFINE VARIABLE lv-rowids      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidateFgBin AS LOGICAL   NO-UNDO.

    DEFINE BUFFER b-loadtag FOR loadtag.


    ASSIGN
        v-board = rd_type BEGINS "B"
        fitm    = begin_i-no
        titm    = begin_rm-no
        troll   = roll_rm.

    SESSION:SET-WAIT-STATE("General").

    FIND FIRST item WHERE item.company EQ cocode
        AND item.i-no    EQ titm NO-LOCK NO-ERROR.

    new-item: DO ON ENDKEY UNDO, RETRY.
        IF NOT AVAILABLE item THEN CREATE item.

        fil_id = RECID(item).

        IF NEW item THEN 
        DO:
            ASSIGN
                item.company  = cocode
                item.loc      = locode
                item.i-no     = titm
                item.i-name   = itemfg.i-name
                item.i-dscr   = itemfg.i-dscr
                item.est-dscr = itemfg.i-name
                item.i-code   = "R"
                item.mat-type = IF v-board THEN "B" ELSE "M"
                item.beg-date = TODAY
                item.s-wid    = IF troll EQ 0 THEN itemfg.t-wid ELSE 0
                item.s-len    = IF troll EQ 0 THEN itemfg.t-len ELSE 0
                item.r-wid    = IF troll EQ 0 THEN 0 ELSE itemfg.t-wid
                item.pur-man  = itemfg.pur-man
                item.reg-no   = itemfg.test
                item.flute    = itemfg.flute
                item.basis-w  = itemfg.weight-100 / (itemfg.t-sqft * .1)
                item.pur-uom  = IF rd_um = "FG" THEN itemfg.pur-uom ELSE cUom
                item.cons-uom = IF rd_um = "FG" THEN itemfg.pur-uom ELSE cUom.

            IF item.basis-w EQ ? THEN item.basis-w = 0.


            FIND FIRST flute WHERE flute.company EQ item.company
                AND flute.code EQ item.flute NO-LOCK NO-ERROR.
            IF AVAILABLE flute THEN
                item.cal = flute.thickness.                  

            IF NOT v-board THEN 
            DO:
                ASSIGN
                    item.cost-type = "MIS"
                    item.procat    = "MISC".

                RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT uom-list).

                DO WHILE TRUE ON ENDKEY UNDO, RETRY:
                    MESSAGE "RM Purchase UOM:" UPDATE item.pur-uom.
                    IF LOOKUP(item.pur-uom,uom-list) EQ 0 THEN
                        MESSAGE "Must be one the following: " + TRIM(uom-list)
                            VIEW-AS ALERT-BOX ERROR.
                    ELSE 
                        LEAVE.
                END.

                item.pur-uom = CAPS(item.pur-uom).

                DO WHILE TRUE ON ENDKEY UNDO, RETRY:
                    MESSAGE "RM Consumption UOM:" UPDATE item.cons-uom.
                    IF LOOKUP(item.cons-uom,uom-list) EQ 0 THEN
                        MESSAGE "Must be one the following: " + TRIM(uom-list)
                            VIEW-AS ALERT-BOX ERROR.
                    ELSE 
                        LEAVE.
                END.

                item.cons-uom = CAPS(item.cons-uom).
            END.

            SESSION:SET-WAIT-STATE("").


            RUN rm/updaterm.p (ROWID(item)).
      
            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.company EQ cocode 
                AND bf-item.industry EQ item.industry
                AND bf-item.mat-type  EQ item.mat-type NO-ERROR.
          
            IF AVAILABLE bf-item THEN      
                ASSIGN
                    item.cost-type = bf-item.cost-type
                    item.procat    = bf-item.procat.

            SESSION:SET-WAIT-STATE("General").

        END.
    END.

    IF troll EQ 0 THEN troll = 1.

    FIND ITEM WHERE RECID(item) EQ fil_id.

    SESSION:SET-WAIT-STATE("").

    /* IF NOT v-board THEN RUN oe/d-selbin.w (3, ?, "ALL", itemfg.i-no, */
    /*                                        OUTPUT lv-rowids).        */
    lValidateFgBin = fGetFgBin(itemfg.i-no) .    
    IF lValidateFgBin THEN
        RUN oe/d-selbin.w (INPUT 3,
            INPUT ?,
            INPUT "ALL",
            INPUT itemfg.i-no,
            OUTPUT lv-rowids).

    SESSION:SET-WAIT-STATE("General").

    FOR EACH w-bin WHERE w-bin.selekt EQ "X",
        FIRST fg-bin WHERE RECID(fg-bin) EQ w-bin.rec-id:

        v-rcpth-no = 0.

        FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rctd AND fg-rctd.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rctd.r-no.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rcpth.r-no.

        CREATE fg-rcpth.
        ASSIGN
            fg-rcpth.r-no       = v-rcpth-no + 1
            fg-rcpth.company    = cocode
            fg-rcpth.loc        = locode
            fg-rcpth.trans-date = TODAY
            fg-rcpth.post-date  = TODAY
            fg-rcpth.i-no       = itemfg.i-no
            fg-rcpth.i-name     = itemfg.i-name
            fg-rcpth.job-no     = fg-bin.job-no
            fg-rcpth.job-no2    = fg-bin.job-no2
            fg-rcpth.pur-uom    = IF AVAILABLE itemfg THEN itemfg.pur-uom ELSE "M"
            fg-rcpth.rita-code  = "A".

        CREATE fg-rdtlh.
        ASSIGN
            fg-rdtlh.r-no       = fg-rcpth.r-no
            fg-rdtlh.company    = cocode
            fg-rdtlh.loc        = fg-bin.loc
            fg-rdtlh.loc-bin    = fg-bin.loc-bin
            fg-rdtlh.tag        = fg-bin.tag
            fg-rdtlh.qty        = - fg-bin.qty
            fg-rdtlh.rita-code  = fg-rcpth.rita-code
            fg-rdtlh.cost       = fg-bin.std-tot-cost
            fg-rdtlh.trans-time = TIME.

        /** Find Bin & if not avail then create it **/
        FIND FIRST rm-bin WHERE rm-bin.company EQ cocode    
            AND rm-bin.loc     EQ fg-bin.loc
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc-bin EQ fg-bin.loc-bin NO-LOCK NO-ERROR.

        IF NOT AVAILABLE rm-bin THEN 
        DO:
            CREATE rm-bin.
            ASSIGN  
                rm-bin.company = cocode
                rm-bin.loc     = fg-bin.loc   
                rm-bin.i-no    = "" 
                rm-bin.loc-bin = fg-bin.loc-bin.
        END.

        FIND FIRST rm-bin WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ item.i-no
            AND rm-bin.loc     EQ fg-bin.loc
            AND rm-bin.loc-bin EQ fg-bin.loc-bin
            AND rm-bin.tag     EQ fg-bin.tag NO-ERROR.
        IF NOT AVAILABLE rm-bin THEN 
        DO:
            CREATE rm-bin.
            ASSIGN
                rm-bin.company = cocode
                rm-bin.i-no    = item.i-no
                rm-bin.loc     = fg-bin.loc
                rm-bin.loc-bin = fg-bin.loc-bin
                rm-bin.tag     = fg-bin.tag.
        END. /* not avail rm-bin */ 
   
        RUN Conv_QuantityFromUOMToUOM (
            INPUT  cocode,
            INPUT  ITEM.i-no,
            INPUT  "RM",
            INPUT  fg-bin.qty * troll,
            INPUT  "EA", 
            INPUT  item.cons-uom,
            INPUT  0,
            INPUT  (IF item.s-len EQ 0 THEN 12 ELSE item.s-len), 
            INPUT  (IF item.s-wid EQ 0 THEN item.r-wid ELSE item.s-wid), 
            INPUT  item.s-dep,
            INPUT  0,
            OUTPUT v-qty,
            OUTPUT lError,
            OUTPUT cMessage
            ).    
  
        RUN Conv_ValueFromUOMtoUOM(cocode, 
            INPUT ITEM.i-no, 
            INPUT "RM", 
            INPUT (fg-bin.std-tot-cost / troll),
            INPUT fg-bin.pur-uom,
            INPUT (IF AVAILABLE ITEM THEN item.cons-uom ELSE "M"), 
            INPUT 0,
            INPUT DECIMAL(item.s-len),
            INPUT DECIMAL(item.s-wid),
            INPUT DECIMAL(item.s-dep),
            INPUT 0, 
            OUTPUT v-cost,
            OUTPUT lError, 
            OUTPUT cMessage).    

        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "v-qty" "v-cost"}

        ASSIGN
            rm-bin.qty     = rm-bin.qty + v-qty
            item.last-cost = v-cost.

        v-rcpth-no = 0.

        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT v-rcpth-no) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        CREATE rm-rcpth.
        ASSIGN
            rm-rcpth.r-no       = v-rcpth-no
            rm-rcpth.company    = cocode
            rm-rcpth.loc        = locode
            rm-rcpth.trans-date = TODAY
            rm-rcpth.i-no       = item.i-no
            rm-rcpth.i-name     = item.i-name
            rm-rcpth.pur-uom    = item.cons-uom
            rm-rcpth.rita-code  = "R"
            rm-rcpth.post-date  = TODAY.

        CREATE rm-rdtlh.
        ASSIGN
            rm-rdtlh.r-no       = rm-rcpth.r-no
            rm-rdtlh.company    = cocode
            rm-rdtlh.loc        = rm-bin.loc
            rm-rdtlh.loc-bin    = rm-bin.loc-bin
            rm-rdtlh.tag        = rm-bin.tag
            rm-rdtlh.qty        = v-qty
            rm-rdtlh.cost       = v-cost
            rm-rdtlh.rita-code  = rm-rcpth.rita-code
            rm-rdtlh.trans-time = TIME.

        fg-bin.qty = 0.

        /* delete fg loadtag */

        FIND FIRST loadtag WHERE loadtag.company   EQ cocode
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ w-bin.tag NO-ERROR. 
        IF AVAILABLE(loadtag) THEN 
        DO:
            ASSIGN
                loadtag.item-type = YES
                loadtag.po-no     = 0
                loadtag.line      = 0
                loadtag.job-no    = ""
                loadtag.job-no2   = 0
                loadtag.form-no   = 0
                loadtag.blank-no  = 0
                loadtag.ord-no    = 0
                loadtag.i-no      = CAPS(item.i-no)
                loadtag.i-name    = ITEM.i-name
                loadtag.sts       = "Printed"
                loadtag.tag-date  = TODAY
                loadtag.tag-time  = TIME.

        END. 
        DELETE w-bin.
    END.

    RUN fg/fg-reset.p (RECID(itemfg)).

    RUN rm/rm-reset.p (RECID(item)).

    FOR EACH rm-bin NO-LOCK WHERE rm-bin.company EQ cocode
        AND rm-bin.i-no    EQ item.i-no
        USE-INDEX i-no
        BREAK BY rm-bin.i-no:

        IF FIRST(rm-bin.i-no) THEN
            ASSIGN
                v-i-qty = 0
                v-cost  = 0.

        v-r-qty = rm-bin.qty.

        IF v-r-qty LT 0 THEN v-r-qty = v-r-qty * -1.

        ASSIGN
            v-i-qty = v-i-qty + v-r-qty
            v-cost  = v-cost  + (v-r-qty * rm-bin.cost).

        IF LAST(rm-bin.i-no) AND v-i-qty NE 0 THEN item.avg-cost = v-cost / v-i-qty.
    END. /* each rm-bin */

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE("General").

    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no C-Win 
PROCEDURE valid-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        begin_i-no:SCREEN-VALUE = CAPS(begin_i-no:SCREEN-VALUE).

        FIND FIRST itemfg
            {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ begin_i-no:SCREEN-VALUE NO-LOCK NO-ERROR.

        IF NOT AVAILABLE itemfg OR begin_i-no:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE "ERROR: Must enter a valid FG Item#"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_i-no.
            RETURN ERROR.
        END.
        ELSE 
        DO:
            fi_fg-name:SCREEN-VALUE = itemfg.i-name.

            IF begin_rm-no:SCREEN-VALUE EQ "" THEN
                begin_rm-no:SCREEN-VALUE = CAPS(itemfg.i-no).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-no C-Win 
PROCEDURE valid-rm-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF begin_i-no:SCREEN-VALUE <> "" THEN 
        DO:
            begin_rm-no:SCREEN-VALUE = CAPS(begin_rm-no:SCREEN-VALUE).

            IF begin_rm-no:SCREEN-VALUE EQ "" THEN 
            DO:
                MESSAGE TRIM(begin_rm-no:LABEL) + " may not be spaces..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO begin_rm-no.
                RETURN ERROR.
            END.

            FIND FIRST item WHERE item.company EQ cocode
                AND item.i-no    EQ begin_rm-no:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN 
            DO:
                IF item.i-code EQ "R" AND ((item.mat-type EQ "B" AND rd_type EQ "Board") 
                    OR (INDEX("MOXY789",ITEM.mat-type) GT 0 AND rd_type NE "Board")) THEN 
                DO:
                    IF NOT ll-rm-warning THEN
                        MESSAGE "This RM already exists, do you wish to update?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                            UPDATE ll-rm-warning.

                    IF NOT ll-rm-warning THEN 
                    DO:
                        APPLY "entry" TO begin_rm-no.
                        RETURN ERROR.
                    END.
                END.
                ELSE 
                DO:
                    MESSAGE "ERROR: Must enter a Real " + TRIM(rd_type) + " RM...".
                    APPLY "entry" TO begin_rm-no.
                    RETURN ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom C-Win 
PROCEDURE valid-uom :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        cUom:SCREEN-VALUE = CAPS(cUom:SCREEN-VALUE).
   
        IF rd_um:SCREEN-VALUE EQ "RM" THEN
        DO:       
            lCheckError = DYNAMIC-FUNCTION("fConv_ValidUomForRMItem", SUBSTRING(rd_type:SCREEN-VALUE,1,1), cUom:SCREEN-VALUE) .
            IF lCheckError THEN
            DO:
                MESSAGE "ERROR: Must enter a valid UOM, Try help"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO cUom.
                oplReturnError = YES.
            END.        
        END.
   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetFgBin C-Win 
FUNCTION fGetFgBin RETURNS LOGICAL
    ( ipcFgItem AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lReturnValue AS LOGICAL NO-UNDO.

    FOR EACH fg-bin
        WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ ipcFgItem
        AND fg-bin.qty     GT 0
        AND fg-bin.cust-no EQ '' /* ticket 23164 */
        NO-LOCK:

        IF NOT((TRIM(fg-bin.job-no) EQ "" OR
            NOT CAN-FIND(FIRST job
            WHERE job.company EQ fg-bin.company
            AND job.job-no  EQ fg-bin.job-no
            AND job.job-no2 EQ fg-bin.job-no2
            AND job.stat    EQ "H"))) THEN
            NEXT.   
        lReturnValue = YES .       
    END. 
    RETURN lReturnValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

