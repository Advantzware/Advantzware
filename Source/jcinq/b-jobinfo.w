&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: jcinq/d-jobinfo.w

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

SESSION:DEBUG-ALERT = FALSE.

&SCOPED-DEFINE yellowColumnsName d-jobinfo#

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


DEFINE VARIABLE custPart             AS CHARACTER NO-UNDO.
DEFINE VARIABLE orderQty             AS INTEGER   NO-UNDO.
DEFINE VARIABLE producedQty          AS INTEGER   NO-UNDO.
DEFINE VARIABLE onHandQty            AS INTEGER   NO-UNDO.
DEFINE VARIABLE qtyOnHand            AS INTEGER   NO-UNDO.
DEFINE VARIABLE shipQty              AS INTEGER   NO-UNDO.
DEFINE VARIABLE invoiceQty           AS INTEGER   NO-UNDO.
DEFINE VARIABLE wipQty               AS INTEGER   NO-UNDO.
DEFINE VARIABLE overUnderPct         AS INTEGER   NO-UNDO.
DEFINE VARIABLE fgItemNo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-job-rec-key        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-job-header         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-col-move           AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE VARIABLE ll-first             AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE VARIABLE ll-initial           AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE VARIABLE lv-frst-rowid        AS ROWID     NO-UNDO.
DEFINE VARIABLE lv-last-rowid        AS ROWID     NO-UNDO.
DEFINE VARIABLE lv-frst-rowid2       AS ROWID     NO-UNDO.
DEFINE VARIABLE lv-last-rowid2       AS ROWID     NO-UNDO.

DEFINE VARIABLE lv-sort-by           AS CHARACTER INITIAL "job-no" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab       AS CHARACTER INITIAL "Job#" NO-UNDO.
DEFINE VARIABLE ll-sort-asc          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-show-prev         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-show-next         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-first-show-job-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-last-show-job-no  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPrevJob            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lActive              AS LOGICAL   NO-UNDO.


DO TRANSACTION:
    {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""JQ1"" }
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-hdr job

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 job-hdr.job-no job-hdr.job-no2 ~
job-hdr.i-no job-hdr.est-no job-hdr.ord-no job-hdr.cust-no job.start-date ~
job.due-date job.close-date job.stat custPart() @ custPart job-hdr.qty ~
orderQty() @ orderQty producedQty(onHandQty) @ producedQty ~
onHandQty(qtyOnHand) @ onHandQty shipQty() @ shipQty ~
invoiceQty() @ invoiceQty wipQty() @ wipQty ~
overUnderPct(onHandQty) @ overUnderPct fgItemNo() @ fgItemNo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH job-hdr NO-LOCK                               ~
        WHERE job-hdr.company EQ cocode                                                     ~
        AND ( (lookup(job-hdr.cust-no,custcount) <> 0 AND job-hdr.cust-no <> "") OR custcount = "") ~
        AND job-hdr.i-no EQ itemfg.i-no                                                     ~
        AND job-hdr.opened EQ YES,                                                          ~
        FIRST job NO-LOCK                                                                   ~
        WHERE job.company EQ job-hdr.company                                                ~
          AND job.job     EQ job-hdr.job                                                    ~
          AND job.job-no  EQ job-hdr.job-no                                                 ~
          AND job.job-no2 EQ job-hdr.job-no2                                                ~
          AND job.opened EQ YES                                                             ~
          AND (job.loc EQ ipLocation OR ipLocation EQ "*All")                               ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH job-hdr NO-LOCK                               ~
        WHERE job-hdr.company EQ cocode                                                     ~
        AND ( (lookup(job-hdr.cust-no,custcount) <> 0 AND job-hdr.cust-no <> "") OR custcount = "") ~
        AND job-hdr.i-no EQ itemfg.i-no                                                     ~
        AND job-hdr.opened EQ YES,                                                          ~
        FIRST job NO-LOCK                                                                   ~
        WHERE job.company EQ job-hdr.company                                                ~
          AND job.job     EQ job-hdr.job                                                    ~
          AND job.job-no  EQ job-hdr.job-no                                                 ~
          AND job.job-no2 EQ job-hdr.job-no2                                                ~
          AND job.opened EQ YES                                                             ~
          AND (job.loc EQ ipLocation OR ipLocation EQ "*All")                               ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 job-hdr job
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 job-hdr 
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 job


/* Definitions for FRAME Dialog-Frame                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS citem cLoc cItemName fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD custPart Dialog-Frame 
FUNCTION custPart RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fgItemNo Dialog-Frame 
FUNCTION fgItemNo RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD invoiceQty Dialog-Frame 
FUNCTION invoiceQty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD onHandQty Dialog-Frame 
FUNCTION onHandQty RETURNS INTEGER
    (OUTPUT opQtyOnHand AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD orderQty Dialog-Frame 
FUNCTION orderQty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD overUnderPct Dialog-Frame 
FUNCTION overUnderPct RETURNS INTEGER
    (ipBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD producedQty Dialog-Frame 
FUNCTION producedQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shipQty Dialog-Frame 
FUNCTION shipQty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wipQty Dialog-Frame 
FUNCTION wipQty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-ok 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
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
    job-hdr, 
    job SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
    QUERY BROWSE-1 DISPLAY
    job-hdr.job-no COLUMN-LABEL "Job#" FORMAT "x(7)":U COLUMN-BGCOLOR 8
    LABEL-BGCOLOR 14
    job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
    job-hdr.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
    LABEL-BGCOLOR 14
    job-hdr.qty COLUMN-LABEL "Job Qty" FORMAT "->>>,>>>,>>9":U
    orderQty() @ orderQty COLUMN-LABEL "Ordered Qty" FORMAT "->>,>>>,>>>":U
    producedQty(onHandQty) @ producedQty COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
    shipQty() @ shipQty COLUMN-LABEL "Shipped Qty" FORMAT "->>,>>>,>>>":U
    invoiceQty() @ invoiceQty COLUMN-LABEL "Invoice Qty" FORMAT "->>,>>>,>>>":U
    job.stat COLUMN-LABEL "Status" FORMAT "x":U LABEL-BGCOLOR 14
    job-hdr.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
    job-hdr.est-no COLUMN-LABEL "Estimate#" FORMAT "x(8)":U WIDTH 14
    LABEL-BGCOLOR 14
    job-hdr.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
    job.start-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
    job.due-date   FORMAT "99/99/9999":U LABEL-BGCOLOR 14
    job.close-date COLUMN-LABEL "Close Date" FORMAT "99/99/9999":U
    LABEL-BGCOLOR 14
    custPart() @ custPart COLUMN-LABEL "Customer Part" FORMAT "X(15)":U
    onHandQty(qtyOnHand) @ onHandQty COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
    wipQty() @ wipQty COLUMN-LABEL "WIP Qty" FORMAT "->>,>>>,>>>":U
    overUnderPct(onHandQty) @ overUnderPct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
    fgItemNo() @ fgItemNo COLUMN-LABEL "FG Item" FORMAT "X(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 132.8 BY 15.76
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    BROWSE-1 AT ROW 4.81 COL 2.2
    btn-ok AT ROW 1.90 COL 126
    citem AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
    cLoc AT ROW 2.14 COL 95.4 COLON-ALIGNED WIDGET-ID 198
    cItemName AT ROW 2.14 COL 39.6 COLON-ALIGNED NO-LABELS WIDGET-ID 318
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    fi_sortby AT ROW 3.76 COL 10 COLON-ALIGNED NO-LABELS
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "View Jobs".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Dialog-Frame
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Dialog-Frame
   FRAME-NAME                                                         */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
    BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame     = 2.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:HIDDEN IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN citem IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cItemName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cLoc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH job-hdr NO-LOCK                               ~
        WHERE job-hdr.company EQ cocode                                                     ~
        AND ( (lookup(job-hdr.cust-no,custcount) <> 0 AND job-hdr.cust-no <> "") OR custcount = "") ~
        AND job-hdr.i-no EQ itemfg.i-no                                                     ~
        AND job-hdr.opened EQ YES,                                                          ~
        FIRST job NO-LOCK                                                                   ~
        WHERE job.company EQ job-hdr.company                                                ~
          AND job.job     EQ job-hdr.job                                                    ~
          AND job.job-no  EQ job-hdr.job-no                                                 ~
          AND job.job-no2 EQ job-hdr.job-no2                                                ~
          AND job.opened EQ YES                                                             ~
          AND (job.loc EQ ipLocation OR ipLocation EQ "*All")               ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

                 
/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* New Miscellaneous Product Estimate - Releases */
    DO:
    
       APPLY "END-ERROR":U TO SELF. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-ENTRY OF BROWSE-1 IN FRAME Dialog-Frame
    DO:
    /* This code displays initial values for newly added or copied rows. */
    /*{src/adm/template/brsentry.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
    DO:
        RUN startSearch.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
    DO:
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


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
    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'JQ1',
        INPUT YES,
        OUTPUT lActive).
    {sys/inc/chblankcust.i ""JQ1""}

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE itemfg THEN 
            ASSIGN
                citem     = itemfg.i-no 
                cItemName = itemfg.i-name
                cLoc      = ipLocation
                .
        DISPLAY citem cItemName cLoc.
 
        /*   IF AVAILABLE itemfg THEN*/
        OPEN QUERY BROWSE-1 FOR EACH job-hdr NO-LOCK                               
            WHERE job-hdr.company EQ cocode
            AND ( (LOOKUP(job-hdr.cust-no,custcount) <> 0 AND job-hdr.cust-no <> "") OR custcount = "") 
            AND job-hdr.i-no EQ itemfg.i-no 
            AND job-hdr.opened EQ YES,
            FIRST job                                
            WHERE job.company EQ job-hdr.company 
            AND job.job     EQ job-hdr.job    
            AND job.job-no  EQ job-hdr.job-no 
            AND job.job-no2 EQ job-hdr.job-no2
            AND job.opened EQ YES  
            AND (job.loc EQ ipLocation OR ipLocation EQ "*All")
            NO-LOCK .
    END.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Dialog-Frame  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
        WITH FRAME Dialog-Frame.
    ENABLE btn-ok RECT-1 BROWSE-1 
        WITH FRAME Dialog-Frame.    
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION custPart Dialog-Frame 
FUNCTION custPart RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RELEASE itemfg.

    IF AVAILABLE job-hdr THEN
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
            NO-ERROR.

    RETURN IF AVAILABLE itemfg THEN itemfg.part-no ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fgItemNo Dialog-Frame 
FUNCTION fgItemNo RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    RETURN IF AVAILABLE job-hdr THEN job-hdr.i-no ELSE ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION invoiceQty Dialog-Frame 
FUNCTION invoiceQty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN
            rtnValue = oe-ordl.inv-qty.
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION onHandQty Dialog-Frame 
FUNCTION onHandQty RETURNS INTEGER
    (OUTPUT opQtyOnHand AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        /*     FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company         */
        /*                                  AND oe-ordl.i-no EQ job-hdr.i-no               */
        /*                                  AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR. */
        /*     IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN                          */
        FOR EACH fg-bin FIELDS(qty) NO-LOCK
            WHERE fg-bin.company EQ job-hdr.company
            AND fg-bin.job-no EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.i-no EQ job-hdr.i-no:
            rtnValue = rtnValue + fg-bin.qty.
        END. /* each fg-bin */
    END. /* avail job-hdr */
    opQtyOnHand = rtnValue.
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION orderQty Dialog-Frame 
FUNCTION orderQty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN
            rtnValue = oe-ordl.qty.
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION overUnderPct Dialog-Frame 
FUNCTION overUnderPct RETURNS INTEGER
    (ipBalance AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN 
        DO:
            rtnValue = ((ipBalance / oe-ordl.qty) - 1) * 100.
            IF rtnValue EQ 0 THEN rtnValue = 100.
            IF rtnValue EQ -100 THEN rtnValue = 0.
        END. /* avail oe-ordl */
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION producedQty Dialog-Frame 
FUNCTION producedQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cINo     AS CHARACTER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        ASSIGN 
            cINo    = job-hdr.i-no
            cJobNo  = job-hdr.job-no
            iJobNo2 = job-hdr.job-no2
            .
        /*     FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company          */
        /*                                  AND oe-ordl.i-no EQ job-hdr.i-no                */
        /*                                  AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.  */
        /*     IF AVAILABLE oe-ordl THEN                                                    */
        /*     DO:                                                                          */
        /*        cINo = oe-ordl.i-no.                                                      */
        /*        IF oe-ordl.job-no NE '' THEN                                              */
        /*            ASSIGN                                                                */
        /*                 cJobNo = oe-ordl.job-no                                          */
        /*                 iJobNo2 = oe-ordl.job-no2                                        */
        /*                 .                                                                */
        /*           FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK   */
        /*              WHERE fg-rcpth.company EQ oe-ordl.company       */
        /*                AND fg-rcpth.job-no EQ oe-ordl.job-no         */
        /*                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2       */
        /*                AND fg-rcpth.i-no EQ oe-ordl.i-no             */
        /*                AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,  */
        /*               EACH fg-rdtlh FIELDS(qty) NO-LOCK              */
        /*              WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no            */
        /*                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
        /*               rtnValue = rtnValue + fg-rdtlh.qty.            */
        /*        END. */
        /*       ELSE */
        /*          FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
        /*              WHERE fg-rcpth.company   EQ cocode             */
        /*                AND fg-rcpth.job-no    EQ job-hdr.job-no     */
        /*                AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
        /*                AND fg-rcpth.i-no      EQ oe-ordl.i-no       */
        /*                AND fg-rcpth.rita-code EQ "R"                */
        /*                USE-INDEX job,                               */
        /*              EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE        */
        /*                   fg-rdtlh.r-no      EQ fg-rcpth.r-no AND   */
        /*                   fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
        /*                   rtnValue = rtnValue + fg-rdtlh.qty.       */
        /*          END.                                               */
        /*     END. /* avail oe-ordl */ */
        /*     ELSE DO: */
        /*                                                            */
        /*         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
        /*             WHERE fg-rcpth.company   EQ cocode             */
        /*               AND fg-rcpth.job-no    EQ job-hdr.job-no     */
        /*               AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
        /*               AND fg-rcpth.i-no      EQ job-hdr.i-no       */
        /*               AND fg-rcpth.rita-code EQ "R"                */
        /*               USE-INDEX job,                               */
        /*             EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE        */
        /*                  fg-rdtlh.r-no      EQ fg-rcpth.r-no AND   */
        /*                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
        /*                  rtnValue = rtnValue + fg-rdtlh.qty.       */
        /*         END.                                               */
        /*                                                            */
        /*     END. */
        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
            INPUT cJobNo,
            INPUT iJobNo2,
            INPUT cINo,
            INPUT NO,
            OUTPUT rtnValue).
    END. /* avail job-hdr */
    opBalance = rtnValue.
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shipQty Dialog-Frame 
FUNCTION shipQty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-inv-qty  LIKE oe-ordl.inv-qty NO-UNDO.
    DEFINE VARIABLE li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
            RUN oe/ordlsqty.p (ROWID(oe-ordl),
                OUTPUT li-inv-qty, OUTPUT li-ship-qty).

            rtnValue = li-ship-qty.
        END.
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wipQty Dialog-Frame 
FUNCTION wipQty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
            rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
            IF rtnValue LT 0 OR
                rtnValue LT oe-ordl.qty *
                (IF AVAILABLE oe-ord THEN oe-ordl.under-pct 
            ELSE 100) / 100 THEN
                rtnValue = 0.
        END. /* avail oe-ordl */
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


