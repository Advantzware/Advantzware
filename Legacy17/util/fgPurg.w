&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgPurg.w

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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---  
                                     */

DEF    VAR      list-name AS cha       NO-UNDO.
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

DEF VAR v-print-fmt      AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form   AS LOGICAL   NO-UNDO.
DEF VAR ls-fax-file      AS CHAR      NO-UNDO.
DEF VAR ll-do-deactivate AS LOG       NO-UNDO.
DEF VAR hStatus          AS HANDLE    NO-UNDO.
DEF VAR llCancel         AS LOG       NO-UNDO.
DEF TEMP-TABLE tt-inactive-list 
    FIELD tt-i-no AS CHAR.
DEF VAR gviCnt AS INT NO-UNDO.
DEF STREAM excel.

DEF    VAR      v-i-no        LIKE itemfg.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEF    VAR      v-cust        LIKE itemfg.cust-no EXTENT 2 INIT ["","zzzzzzzz"].
DEF    VAR      v-procat      LIKE itemfg.procat EXTENT 2 INIT ["","zzzzz"].
DEF    VAR      v-break       AS CHAR      FORMAT "!" INIT "N".
DEF    VAR      v-prt-cost    AS LOG       FORMAT "Cost/Value" INIT NO.
DEF    VAR      v-custown     AS LOG       FORMAT "Y/N" INIT NO.
DEF    VAR      v-sort-by     AS LOG       FORMAT "Y/N" INIT NO.
DEF    VAR      v-zero        AS LOG       FORMAT "Y/N" INIT NO.
DEF    VAR      v-sho-cost    AS LOG       FORMAT "Y/N" INIT NO.

DEF    VAR      v-first       AS LOG       EXTENT 2 INIT YES.
DEF    VAR      v-page-break  AS CHAR.
DEF    VAR      v-label1      AS CHAR      FORMAT "x(14)" EXTENT 3.
DEF    VAR      v-label2      AS CHAR      FORMAT "x(14)".
DEF    VAR      v-price       AS DEC.
DEF    VAR      v-cost        AS DEC.
DEF    VAR      v-tq-onh      AS DEC       EXTENT 2.
DEF    VAR      v-tq-ono      LIKE v-tq-onh.
DEF    VAR      v-tq-alloc    LIKE v-tq-onh.
DEF    VAR      v-tq-avail    LIKE v-tq-onh.
DEF    VAR      v-tprice      LIKE v-tq-onh.
DEF    VAR      v-qty-onh     LIKE itemfg.q-onh.
DEF    VAR      iCnt          AS INT       NO-UNDO.
DEF    VAR      jCnt          AS INT       NO-UNDO.
DEF    VAR      v-status      AS CHAR      NO-UNDO FORMAT "x(1)".
DEF    VAR      lvdCutOffDate AS DATE      NO-UNDO.
DEF    VAR      excelheader   AS CHAR      NO-UNDO.
DEF    VAR      cMasterItemfg AS CHAR      NO-UNDO.

DEFINE VARIABLE lActive       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, 
    INPUT "FGMASTER",
    INPUT "C",
    INPUT NO, 
    INPUT NO, 
    INPUT "",
    INPUT "", 
    OUTPUT cReturn, 
    OUTPUT lFound).
IF lFound THEN
    cMasterItemfg = cReturn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCutOffDate begin_i-no end_i-no btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCutOffDate begin_i-no end_i-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCanDel C-Win 
FUNCTION getCanDel RETURNS LOGICAL
    ( ipcItem AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE begin_i-no   AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no     AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiCutOffDate AS DATE      FORMAT "99/99/9999":U 
    LABEL "Purge Transactions Prior to  this Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 11 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fiCutOffDate AT ROW 2.19 COL 49 COLON-ALIGNED WIDGET-ID 6
    begin_i-no AT ROW 4.57 COL 24 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 4.57 COL 67 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    btn-ok AT ROW 7.91 COL 21
    btn-cancel AT ROW 8 COL 61
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 95.4 BY 10.95.


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
        TITLE              = "Finished Goods Set Inactive Utility"
        HEIGHT             = 11.19
        WIDTH              = 97.2
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
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Set Inactive Utility */
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
ON WINDOW-CLOSE OF C-Win /* Finished Goods Set Inactive Utility */
    DO:
        IF VALID-HANDLE(hStatus) THEN
            DELETE OBJECT hStatus.

        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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


        RUN windows/w-message.w PERSISTENT SET hStatus.
        RUN setWindowTitle IN hStatus (INPUT "Searching for Items").


        RUN run-report. 
        
        IF VALID-HANDLE(hStatus) THEN
            DELETE OBJECT hStatus.
        MESSAGE "Please post physical count records that were created."
        VIEW-AS ALERT-BOX.
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
    APPLY "entry" TO fiCutOffDate .

    RUN enable_UI.

    SUBSCRIBE TO "CancelIt" ANYWHERE.
    SUBSCRIBE TO "NumDel" ANYWHERE.
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        /* {custom/usrprint.i} */
        APPLY "entry" TO fiCutOffDate .
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
PROCEDURE LockWindowUpdate EXTERNAL "user32.dll": 
    DEFINE INPUT PARAMETER hWndLock AS LONG NO-UNDO. 
END PROCEDURE. /* LockWindowUpdate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelIt C-Win 
PROCEDURE cancelIt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /* respond to cancel event */
    llCancel = TRUE.

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
    DISPLAY fiCutOffDate begin_i-no end_i-no 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE fiCutOffDate begin_i-no end_i-no btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :

  RUN util/inventoryArchive.p (INPUT cocode, ficutoffdate, INPUT begin_i-no, INPUT end_i-no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numDel C-Win 
PROCEDURE numDel :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiCnt AS INT NO-UNDO.
    RUN LockWindowUpdate(INPUT CURRENT-WINDOW:HWND). 
    IF VALID-HANDLE(hStatus) THEN
        RUN process-message IN hStatus (INPUT ipcTable + ": " + STRING(ipiCnt)).
    RUN LockWindowUpdate(INPUT 0).

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
    DEF VAR lv-frame-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-group-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
    DEF VAR parm-fld-list AS cha    NO-UNDO.
    DEF VAR parm-lbl-list AS cha    NO-UNDO.
    DEF VAR i             AS INT    NO-UNDO.
    DEF VAR lv-label      AS cha.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCanDel C-Win 
FUNCTION getCanDel RETURNS LOGICAL
    ( ipcItem AS CHAR ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF BUFFER bf-itemfg FOR itemfg.
    DEF VAR lCanDel AS LOG NO-UNDO.

    FIND bf-itemfg WHERE bf-itemfg.company = cocode
        AND bf-itemfg.i-no = ipcItem NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-itemfg THEN
        RETURN FALSE.

    lCanDel = TRUE.
    /* Check active status. */
    /*      FIND FIRST reftable NO-LOCK WHERE                */
    /*          reftable.reftable = "FGSTATUS" AND           */
    /*          reftable.company  = bf-itemfg.company AND    */
    /*          reftable.loc      = "" AND                   */
    /*          reftable.code     = bf-itemfg.i-no NO-ERROR. */

    /* If no status record found and user selected Active or Inactive,
       then skip.  (If all selected, then print it) */
    IF bf-itemfg.stat = "I" THEN lCanDel = FALSE.
    v-status = bf-itemfg.stat.
    /*      IF AVAILABLE reftable AND reftable.code2 EQ "I" THEN lCanDel = FALSE. */
    /*                                                                            */
    /*                                                                            */
    /*     IF AVAIL reftable THEN                                                 */
    /*         ASSIGN v-status = reftable.code2.                                  */
    /*     ELSE                                                                   */
    /*         ASSIGN v-status = "".                                              */

    /* Make sure last activity for this item is before cutoff date */
    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
        AND fg-rcpth.i-no  EQ bf-itemfg.i-no
        AND fg-rcpth.trans-date GT lvdCutOffDate
        AND fg-rcpth.rita-code EQ "R"
        USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
        AND fg-rcpth.i-no  EQ bf-itemfg.i-no
        AND fg-rcpth.trans-date GT lvdCutOffDate
        AND fg-rcpth.rita-code EQ "S"
        USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
        AND fg-rcpth.i-no  EQ bf-itemfg.i-no
        AND fg-rcpth.trans-date GT lvdCutOffDate
        AND fg-rcpth.rita-code EQ "T"
        USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    v-qty-onh  = 0.     
    FOR EACH fg-bin
        WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ bf-itemfg.i-no
        USE-INDEX i-no NO-LOCK:

        IF v-custown OR (fg-bin.loc NE "CUST" AND fg-bin.cust-no EQ "") THEN
            v-qty-onh = v-qty-onh + fg-bin.qty.
    END. /* each bin */

    /* Has on hand, then don't include */
    IF v-qty-onh GT 0 THEN
        lCanDel = FALSE.

    /* Has on order, then don't include */
    IF bf-itemfg.q-ono GT 0 THEN
        lCanDel = FALSE.

    /* Has allocated, then don't include */
    IF bf-itemfg.q-alloc GT 0 THEN
        lCanDel = FALSE.

    RETURN lCanDel.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

