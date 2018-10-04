&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\crtinv.w

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
DEFINE INPUT PARAMETER ipcFileMatch AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFileName AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE NEW SHARED VARIABLE miscflds_reckey   AS CHARACTER.
DEFINE NEW SHARED VARIABLE table_reckey      AS CHARACTER.
DEFINE NEW SHARED VARIABLE Persistent-Handle AS HANDLE.
DEFINE NEW SHARED VARIABLE ListLogic-Handle  AS HANDLE.
DEFINE            VARIABLE run-proc          AS CHARACTER.
DEFINE            VARIABLE hsignature        AS CHARACTER     NO-UNDO.
DEFINE            VARIABLE char-hdl          AS CHARACTER     NO-UNDO.
DEFINE            VARIABLE phandle           AS WIDGET-HANDLE NO-UNDO.
DEFINE            VARIABLE is-running        AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE help-page         AS INTEGER       NO-UNDO.


RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
/*{methods/prgsecur.i}*/
/*
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/invdate.i}
   {sys/inc/invlotline.i}
END.

{fg/fullset.i NEW}
{oe/oe-bolpi.i NEW}
*/
DEFINE VARIABLE v-process          AS LOG     NO-UNDO.
DEFINE VARIABLE fg-uom-list        AS cha     NO-UNDO.
DEFINE VARIABLE ll-calc-disc-FIRST AS LOG     NO-UNDO.
DEFINE VARIABLE v-cost             AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-basis            LIKE sman.commbasis INIT "" NO-UNDO.
DEFINE VARIABLE v-u-inv            LIKE oe-ctrl.u-inv INIT NO.

DEFINE NEW SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEFINE NEW SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

DEFINE TEMP-TABLE tt-missing-inv
    FIELD missingDate AS DATE
    FIELD missingTime AS CHARACTER
    FIELD missingUser AS CHARACTER 
    FIELD missingFile AS CHARACTER FORMAT "x(20)"
    FIELD missingFileFull AS CHARACTER FORMAT "x(40)" 
    FIELD r-oe-boll   AS ROWID.

DEFINE NEW SHARED TEMP-TABLE tt-relbol NO-UNDO 
    FIELD release#   LIKE oe-relh.release#
    FIELD tag#       AS cha
    FIELD i-no       AS cha     FORM "x(15)"
    FIELD i-name     AS cha     FORM "x(30)"
    FIELD ord-no     LIKE oe-ord.ord-no
    FIELD job-no     LIKE oe-rell.job-no
    FIELD job-no2    LIKE oe-rell.job-no2
    FIELD loc        LIKE oe-rell.loc
    FIELD loc-bin    LIKE oe-rell.loc-bin
    FIELD cust-no    LIKE oe-rell.cust-no
    FIELD cases      LIKE oe-rell.cases
    FIELD qty-case   LIKE oe-rell.qty-case
    FIELD cases-unit LIKE fg-rctd.cases-unit
    FIELD partial    LIKE oe-rell.partial
    FIELD qty        LIKE oe-rell.qty
    FIELD t-qty      LIKE oe-rell.qty
    FIELD line       LIKE oe-rell.line
    FIELD oerell-row AS ROWID
    FIELD seq        AS INTEGER
    FIELD warned     AS LOG
    FIELD po-no      LIKE oe-boll.po-no
    /* gdm - 10160906 */
    FIELD trailer#   LIKE oe-relh.trailer
    INDEX release# release# ord-no i-no po-no.


DEFINE BUFFER b-reftable3 FOR reftable.
DEFINE BUFFER b-reftable  FOR reftable.
DEFINE BUFFER b-oe-ordl   FOR oe-ordl.

/* RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list). */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-missing-inv

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tt-missing-inv.missingDate tt-missing-inv.missingTime tt-missing-inv.missingFile   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tt-missing-inv NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH tt-missing-inv NO-LOCK BY tt-missing-inv.missingFile.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tt-missing-inv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tt-missing-inv


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-4 btn-process btn-cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Select" 
     SIZE 18 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 15.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      tt-missing-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      tt-missing-inv.missingDate COLUMN-LABEL "Date"
    tt-missing-inv.missingTime COLUMN-LABEL "Time"
    tt-missing-inv.missingFile COLUMN-LABEL "File"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 14.52
         TITLE "Saved Release Entries" ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-4 AT ROW 1.71 COL 3 WIDGET-ID 200
     btn-process AT ROW 17.43 COL 23
     btn-cancel AT ROW 17.43 COL 55
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 20
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 18.91.


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
         TITLE              = "Create New Invoice from BOL Lines"
         HEIGHT             = 18.48
         WIDTH              = 90.8
         MAX-HEIGHT         = 25.52
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 25.52
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 RECT-1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-missing-inv NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create New Invoice from BOL Lines */
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
ON WINDOW-CLOSE OF C-Win /* Create New Invoice from BOL Lines */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-4 IN FRAME FRAME-A /* Saved Release Entries */
DO:
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
        {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Select */
DO:
        RUN run-process.
        {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
DEFINE STREAM sinput.  
DEFINE VARIABLE cFileInput AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cLogDir    AS CHARACTER NO-UNDO. 

FILE-INFO:FILE-NAME = ".\logs".  
cLogDir = FILE-INFO:FULL-PATHNAME.

INPUT STREAM sInput FROM OS-DIR(cLogDir). 
REPEAT:
    /* create temp-table */
    IMPORT STREAM sInput cFileInput.  
    FILE-INFO:FILE-NAME = cLogDir + "/" + cFileInput. 

    IF NOT cFileInput MATCHES ipcFileMatch THEN NEXT.
    CREATE tt-missing-inv.
    ASSIGN 
    tt-missing-inv.missingFile = cFileInput 
    tt-missing-inv.missingDate = FILE-INFO:FILE-MOD-DATE 
    tt-missing-inv.missingTime = STRING(FILE-INFO:FILE-MOD-TIME, "hh:mm")
    tt-missing-inv.missingFileFull = FILE-INFO:FULL-PATHNAME 
    .
END. 
INPUT STREAM sInput CLOSE.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* check security */

    RUN enable_UI.
    /*{methods/nowait.i} */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-range C-Win 
PROCEDURE check-range :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vi-cnt AS INTEGER NO-UNDO.
    EMPTY TEMP-TABLE tt-missing-inv.


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
  ENABLE RECT-1 BROWSE-4 btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE v-index      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-ref-no     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-fob-code   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-line-count AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-start-pos  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

    SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME frame-a:
   DO v-index = 1 TO browse-4:NUM-SELECTED-ROWS:
      browse-4:FETCH-SELECTED-ROW(v-index).

      IF v-index EQ 1 THEN
      DO:
          ASSIGN 
            opcFileName = tt-missing-inv.missingFileFull.
      END.
   END.

  END.
  APPLY 'close' TO THIS-PROCEDURE. 
    SESSION:SET-WAIT-STATE("").


/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

