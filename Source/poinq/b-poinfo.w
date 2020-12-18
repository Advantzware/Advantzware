&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: poinq/d-po-inq.w

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
&SCOPED-DEFINE yellowColumnsName d-po-inq#

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER ipLocation AS CHARACTER NO-UNDO .
/* Local Variable Definitions ---                                       */

/*{methods/prgsecur.i}    */           
{system/sysconst.i}
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}               
{sys/inc/VAR.i NEW SHARED}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE NEW SHARED VARIABLE uperiod       AS INTEGER   NO-UNDO.  /* for gl-open.p */

DEFINE            VARIABLE v-show-disc   AS LOG       NO-UNDO.
DEFINE            VARIABLE choice        AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-pre-disc   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-pre-paid   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-first      AS LOG       INIT YES NO-UNDO.
DEFINE            VARIABLE lv-num-rec    AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-in-add     AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-in-update  AS LOG       NO-UNDO.

DEFINE            VARIABLE v-paidflg     AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-t-rec-qty  LIKE po-ordl.t-rec-qty NO-UNDO.
DEFINE            VARIABLE cPoStatus     AS CHARACTER NO-UNDO .
DEFINE            VARIABLE cPoLineStatus AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE factor#       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQtyDue AS DECIMAL NO-UNDO .
&SCOPED-DEFINE sortby BY po-ordl.po-no DESC BY po-ordl.LINE BY po-ordl.i-no 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no


/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES po-ordl po-ord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 po-ordl.po-no po-ord.vend-no ~
po-ordl.due-date po-ord.ship-id po-ord.ship-name po-ordl.job-no po-ordl.job-no2 ~
po-ordl.s-num po-ordl.i-no po-ordl.i-name dim-in-16 (po-ordl.s-wid) @ po-ordl.s-wid po-ordl.s-wid ~
dim-in-16 (po-ordl.s-len) @ po-ordl.s-len po-ordl.s-len po-ordl.vend-i-no po-ordl.ord-qty ~
qty-in-ord-uom () @ lv-t-rec-qty po-ordl.pr-qty-uom po-ordl.t-rec-qty po-ordl.cons-uom po-ordl.cost ~
po-ordl.pr-uom po-ord.buyer is-it-polinestat() @ cPoLineStatus is-it-postat() @ cPoStatus is-it-paid() @ v-paidflg ~
po-ordl.cust-no po-ordl.LINE po-ord.Loc  pGetQtyDue() @ dQtyDue
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH po-ordl WHERE po-ordl.company = itemfg.company  ~
  AND po-ordl.i-no = itemfg.i-no ~
  AND lookup(po-ordl.stat, "o,p,u,a") > 0  ~
  AND po-ordl.item-type  = no ~
  NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company eq po-ordl.company and ~
po-ord.po-no eq po-ordl.po-no  AND (po-ord.loc EQ ipLocation OR ipLocation EQ "*All" ) ~
      AND lookup(po-ord.stat, "N,O,R,U,H") > 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH po-ordl WHERE po-ordl.company = itemfg.company  ~
  AND po-ordl.i-no = itemfg.i-no ~
  AND lookup(po-ordl.stat, "o,p,u,a") > 0  ~
  AND po-ordl.item-type  = no ~
  NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company eq po-ordl.company and ~
po-ord.po-no eq po-ordl.po-no  AND (po-ord.loc EQ ipLocation OR ipLocation EQ "*All" ) ~
      AND lookup(po-ord.stat, "N,O,R,U,H") > 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 po-ordl po-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 po-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 po-ord


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS citem cLoc cItemName fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckForNegative C-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
    ( ipcCompany AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dim-in-16 C-Win 
FUNCTION dim-in-16 RETURNS DECIMAL
    ( ip-dim AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getcurrentpo C-Win 
FUNCTION getcurrentpo RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-paid C-Win 
FUNCTION is-it-paid RETURNS LOGICAL
    (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-polinestat C-Win 
FUNCTION is-it-polinestat RETURNS CHARACTER
    (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-postat C-Win 
FUNCTION is-it-postat RETURNS CHARACTER
    (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isPaidCheck C-Win 
FUNCTION isPaidCheck RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD qty-in-ord-uom C-Win 
FUNCTION qty-in-ord-uom RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetQtyDue C-Win 
FUNCTION pGetQtyDue RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-ok 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "OK" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE citem     AS CHARACTER FORMAT "X(15)":U 
    LABEL "FG Item #" 
    VIEW-AS FILL-IN 
    SIZE 23.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemName AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cLoc      AS CHARACTER FORMAT "X(10)":U 
    LABEL "Location" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 122.2 BY 2.14
    BGCOLOR 15 .

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 47 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.


/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    po-ordl, 
    po-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
    QUERY BROWSE-1 DISPLAY
    po-ordl.po-no COLUMN-LABEL "PO#" FORMAT ">>>>>>>":U LABEL-BGCOLOR 14
    po-ord.vend-no COLUMN-LABEL "Vendor#" FORMAT "x(8)":U LABEL-BGCOLOR 14
    po-ordl.due-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
    po-ord.ship-id FORMAT "x(8)":U LABEL-BGCOLOR 14
    po-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14
    po-ordl.job-no2 COLUMN-LABEL "" FORMAT "99":U 
    po-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
    po-ord.Loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
    po-ordl.ord-qty COLUMN-LABEL "Qty Ordered" FORMAT "->>>,>>>,>>9.9<<<<<":U
    WIDTH 15.8 LABEL-BGCOLOR 14
    pGetQtyDue() @ dQtyDue COLUMN-LABEL "Due Qty" FORMAT "->>>,>>>,>>9.9<<<<<":U
    WIDTH 15.8 LABEL-BGCOLOR 14
    po-ord.ship-name FORMAT "x(30)":U LABEL-BGCOLOR 14
    po-ordl.s-num COLUMN-LABEL "Form#" FORMAT ">>>":U LABEL-BGCOLOR 14
    po-ordl.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
    po-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
    LABEL-BGCOLOR 14
    dim-in-16 (po-ordl.s-wid) @ po-ordl.s-wid
    po-ordl.s-wid COLUMN-LABEL "Width" FORMAT ">>,>>9.99<<<":U
    LABEL-BGCOLOR 14
    dim-in-16 (po-ordl.s-len) @ po-ordl.s-len
    po-ordl.s-len COLUMN-LABEL "Length" FORMAT ">>,>>9.99<<<":U
    LABEL-BGCOLOR 14
    po-ordl.vend-i-no COLUMN-LABEL "Vendor Item#" FORMAT "x(15)":U
    LABEL-BGCOLOR 14
    qty-in-ord-uom () @ lv-t-rec-qty COLUMN-LABEL "PO Qty Received" FORMAT "->>>,>>>,>>9.9<<":U
    WIDTH 21.2
    po-ordl.pr-qty-uom COLUMN-LABEL "Ord UOM" FORMAT "x(4)":U
    WIDTH 10 LABEL-BGCOLOR 14
    po-ordl.t-rec-qty COLUMN-LABEL "Qty Received" FORMAT "->>>,>>>,>>9.9<<<<<":U
    WIDTH 23.8 LABEL-BGCOLOR 14
    po-ordl.cons-uom COLUMN-LABEL "Rec. UOM" FORMAT "x(4)":U
    LABEL-BGCOLOR 14
    po-ordl.cost FORMAT "->,>>>,>>9.99<<<<":U LABEL-BGCOLOR 14
    po-ordl.pr-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U LABEL-BGCOLOR 14
    po-ord.buyer FORMAT "x(10)":U LABEL-BGCOLOR 14
    is-it-polinestat() @ cPoLineStatus COLUMN-LABEL "Line Status" FORMAT "x(20)":U 
    is-it-postat() @ cPoStatus COLUMN-LABEL "PO Status" FORMAT "x(20)":U 
    is-it-paid() @ v-paidflg COLUMN-LABEL "Paid" FORMAT "YES / NO":U
    LABEL-BGCOLOR 14
    po-ordl.LINE COLUMN-LABEL "Line #" FORMAT ">>>9":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 132.8 BY 15.76
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    BROWSE-1 AT ROW 4.81 COL 2.2
    btn-ok AT ROW 1.90 COL 126
    citem AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
    cLoc AT ROW 2.14 COL 95.4 COLON-ALIGNED WIDGET-ID 198
    cItemName AT ROW 2.14 COL 39.6 COLON-ALIGNED NO-LABELS WIDGET-ID 318
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    fi_sortby AT ROW 3.76 COL 10 COLON-ALIGNED NO-LABELS
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.2 ROW 1
    SIZE 149 BY 23.71
    FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Query,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "View PO's'"
        HEIGHT             = 20.57
        WIDTH              = 135.8
        MAX-HEIGHT         = 24.71
        MAX-WIDTH          = 156
        VIRTUAL-HEIGHT     = 24.71
        VIRTUAL-WIDTH      = 156
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                         */
/* BROWSE-TAB BROWSE-1 1 F-Main */
ASSIGN 
    BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:HIDDEN IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN citem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cLoc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH po-ordl WHERE po-ordl.company = itemfg.company  ~
  AND po-ordl.i-no = itemfg.i-no ~
  AND lookup(po-ordl.stat, "o,p,u,a") > 0  ~
  AND po-ordl.item-type  = no ~
  NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company eq po-ordl.company and ~
po-ord.po-no eq po-ordl.po-no  AND (po-ord.loc EQ ipLocation OR ipLocation EQ "*All" ) ~
      AND lookup(po-ord.stat, "N,O,R,U,H") > 0 NO-LOCK   ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

                 
/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* New Miscellaneous Product Estimate - Releases */
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
ON WINDOW-CLOSE OF C-Win /* New Miscellaneous Product Estimate - Releases */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
  
        /*  APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
        */
        APPLY "choose" TO btn-ok IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME F-Main
    DO:
    /* DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
     IF AVAILABLE eb THEN 
     DO:
         IF lEnableButton AND AVAIL estRelease THEN do:
             RUN est/dNewMiscUpd.w (RECID(estRelease),ROWID(eb),"View", OUTPUT lv-rowid) .
         END.
         ELSE IF AVAILABLE estRelease THEN 
         DO:
             RUN est/dNewMiscUpd.w (RECID(estRelease),ROWID(eb),"Update", OUTPUT lv-rowid) . 
             RUN repo-query (ROWID(estRelease)).
         END.
     
     
     END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON ROW-ENTRY OF BROWSE-1 IN FRAME F-Main
    DO:
    /* This code displays initial values for newly added or copied rows. */
    /*{src/adm/template/brsentry.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME F-Main
    DO:
        RUN startSearch.  
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME F-Main /* OK */
    DO:
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-1
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

    FIND FIRST itemfg WHERE ROWID(itemfg) EQ iprRowid  NO-LOCK NO-ERROR.

    {custom/yellowColumns.i}
    RUN enable_UI.
    {methods/nowait.i}
    /* Ticket# : 92946
       Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
    fi_sortby:HIDDEN  = TRUE.
    fi_sortby:VISIBLE = FALSE.
    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE itemfg THEN 
            ASSIGN
                citem     = itemfg.i-no 
                cItemName = itemfg.i-name
                cLoc      = ipLocation
                .
        DISPLAY citem cItemName cLoc.

        OPEN QUERY BROWSE-1 FOR EACH po-ordl WHERE po-ordl.company = itemfg.company 
            AND po-ordl.i-no = itemfg.i-no 
            AND lookup(po-ordl.stat, "o,p,u,a") > 0  
            AND po-ordl.item-type  = NO 
            NO-LOCK, 
            FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND 
            po-ord.po-no EQ po-ordl.po-no AND (po-ord.loc EQ ipLocation OR ipLocation EQ "*All" )
            AND lookup(po-ord.stat, "N,O,R,U,H") > 0 NO-LOCK 
            BY po-ordl.po-no DESCENDING BY po-ordl.LINE BY po-ordl.i-no.
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
    DISPLAY btn-ok RECT-1 citem cLoc cItemName BROWSE-1 
        WITH FRAME F-Main IN WINDOW C-Win.
    ENABLE btn-ok RECT-1 BROWSE-1 
        WITH FRAME F-Main IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW FRAME F-Main IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
PROCEDURE local-exit :
    /* -----------------------------------------------------------
          Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
          Parameters:  <none>
          Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
        -------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckForNegative C-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
    ( ipcCompany AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNegativeFound AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cVendor        AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ap-chk FOR ap-chk.

    ASSIGN 
        lNegativeFound = NO
        cVendor        = "".
    FOR EACH bf-ap-chk
        WHERE bf-ap-chk.company EQ ipcCompany
        AND bf-ap-chk.man-check EQ NO
        AND bf-ap-chk.check-amt LT 0
        NO-LOCK:
        lNegativeFound = YES.
        IF cVendor NE "" THEN cVendor = cVendor + ",".
        cVendor = cVendor + bf-ap-chk.vend-no.
    END.

    IF lNegativeFound THEN 
        MESSAGE "Checks with a negative total have been found for vendor(s):" SKIP
            cVendor SKIP(1)
            "Would you like to return to the selection screen to correct this?"    
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lNegativeFound.
    RETURN lNegativeFound.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dim-in-16 C-Win 
FUNCTION dim-in-16 RETURNS DECIMAL
    ( ip-dim AS DECIMAL ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN (ip-dim - TRUNC(ip-dim, 0)) * factor# + TRUNC(ip-dim, 0).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getcurrentpo C-Win 
FUNCTION getcurrentpo RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    IF AVAILABLE po-ordl THEN
        RETURN po-ordl.po-no.
    ELSE RETURN -1.  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-paid C-Win 
FUNCTION is-it-paid RETURNS LOGICAL
    (  /* parameter-definitions */  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-flg AS LOG.

    FOR EACH  reftable NO-LOCK
        WHERE reftable.reftable EQ "AP-INVL" 
        AND reftable.company  EQ ""        
        AND reftable.loc      EQ ""        
        AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999"),
        EACH  ap-invl NO-LOCK
        WHERE ap-invl.company           EQ po-ordl.company
        AND ap-invl.i-no              EQ int(reftable.code2) 
        AND ap-invl.po-no             EQ po-ordl.po-no 
        AND (ap-invl.line + 
        (ap-invl.po-no * -1000)) EQ po-ordl.line,
        EACH  ap-inv NO-LOCK
        WHERE ap-inv.company EQ ap-invl.company 
        AND ap-inv.i-no EQ ap-invl.i-no 
        AND ap-inv.due  EQ 0:

        v-flg = TRUE.

    END.

    RETURN v-flg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-polinestat C-Win 
FUNCTION is-it-polinestat RETURNS CHARACTER
    (  /* parameter-definitions */  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult   AS CHARACTER NO-UNDO.
    
    IF AVAILABLE po-ordl THEN 
    DO: 
        lc-result = po-ordl.stat .
        RUN oe/getStatusDesc.p( INPUT po-ordl.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .
    END.
    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-postat C-Win 
FUNCTION is-it-postat RETURNS CHARACTER
    (  /* parameter-definitions */  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult   AS CHARACTER NO-UNDO.
    
    IF AVAILABLE po-ord THEN 
    DO: 
        lc-result = po-ord.stat .
        RUN oe/getStatusDesc.p( INPUT po-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .
    END.
    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isPaidCheck C-Win 
FUNCTION isPaidCheck RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-flg AS LOG NO-UNDO.


    v-flg = FALSE.
    FOR EACH  reftable NO-LOCK
        WHERE reftable.reftable EQ "AP-INVL" 
        AND reftable.company  EQ ""        
        AND reftable.loc      EQ ""        
        AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999"),
        EACH  ap-invl NO-LOCK
        WHERE ap-invl.company           EQ po-ordl.company
        AND ap-invl.i-no              EQ int(reftable.code2) 
        AND ap-invl.po-no             EQ po-ordl.po-no 
        AND (ap-invl.line + 
        (ap-invl.po-no * -1000)) EQ po-ordl.line,
        EACH  ap-inv NO-LOCK
        WHERE ap-inv.company EQ ap-invl.company 
        AND ap-inv.i-no EQ ap-invl.i-no 
        AND ap-inv.due  EQ 0:

        v-flg = TRUE.
    END.

    RETURN v-flg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION qty-in-ord-uom C-Win 
FUNCTION qty-in-ord-uom RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld AS DECIMAL DECIMALS 10 EXTENT 2 NO-UNDO.

    DEFINE BUFFER b-po-ordl FOR po-ordl.



    FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.

    ld[2] = b-po-ordl.t-rec-qty.

    IF b-po-ordl.item-type EQ YES                 AND
        b-po-ordl.pr-qty-uom NE b-po-ordl.cons-uom THEN 
    DO:
        ld[2] = 0.

        FOR EACH rm-rcpth
            WHERE rm-rcpth.company   EQ b-po-ordl.company
            AND rm-rcpth.po-no     EQ STRING(b-po-ordl.po-no)
            AND rm-rcpth.i-no      EQ b-po-ordl.i-no
            AND rm-rcpth.rita-code EQ "R" NO-LOCK,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
            AND rm-rdtlh.job-no  EQ b-po-ordl.job-no
            AND rm-rdtlh.job-no2 EQ b-po-ordl.job-no2
            AND rm-rdtlh.s-num   EQ b-po-ordl.s-num
            NO-LOCK:

            IF b-po-ordl.pr-qty-uom EQ "ROLL" AND rm-rdtlh.tag NE "" THEN ld[1] = 1.

            ELSE 
            DO:
                ld[1] = rm-rdtlh.qty.

                IF rm-rcpth.pur-uom NE b-po-ordl.pr-qty-uom THEN 
                DO:
                    FIND FIRST item
                        WHERE item.company EQ b-po-ordl.company
                        AND item.i-no    EQ b-po-ordl.i-no
                        NO-LOCK NO-ERROR.

                    RUN custom/convquom.p(cocode, rm-rcpth.pur-uom, b-po-ordl.pr-qty-uom,
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        (IF b-po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                        ELSE b-po-ordl.s-len), b-po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        ld[1], OUTPUT ld[1]).
                END.
            END.

            ld[2] = ld[2] + ld[1].
        END.
    END.

    IF b-po-ordl.pr-qty-uom EQ "EA" THEN 
    DO:
        {sys/inc/roundup.i ld[2]}
    END.

    RETURN ld[2].   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetQtyDue C-Win 
FUNCTION pGetQtyDue RETURNS DECIMAL
    (  /* parameter-definitions */  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE dResult   AS DECIMAL NO-UNDO.
    
    IF AVAILABLE po-ordl THEN 
    DO: 
        dResult = MAX( po-ordl.ord-qty - po-ordl.t-rec-qty,0) .
    END.
    RETURN dResult.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


