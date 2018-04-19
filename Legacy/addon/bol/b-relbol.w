&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: addon\bol\b-relbol.w
  
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

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.
DEFINE VARIABLE lSaveToTempfile AS LOGICAL NO-UNDO.
DEFINE STREAM sTmpSaveInfo.
DEFINE VARIABLE cTmpSaveFile AS CHARACTER NO-UNDO.

{oe/oe-relp1.i NEW}

DEF NEW SHARED BUFFER xoe-relh FOR oe-relh.

DEF SHARED VAR g-sharpshooter AS LOG NO-UNDO.

DEF TEMP-TABLE tt-rell NO-UNDO LIKE oe-rell
                       FIELD release# LIKE oe-relh.release#
                       FIELD row-id   AS   ROWID
                       INDEX row-id row-id
                       INDEX ord-no company ord-no line i-no
                             po-no rel-no b-ord-no.

DEF TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll
                       FIELD release# LIKE oe-relh.release#
                       INDEX tt-boll b-no ord-no i-no po-no rel-no b-ord-no
                       INDEX release# release# ord-no i-no rel-no b-ord-no po-no.

DEF TEMP-TABLE tt-boll2 NO-UNDO LIKE tt-boll
                       INDEX b-no b-no.

/*update changes to tmep-table in addon/bol/saverel.p as well*/
DEF NEW SHARED TEMP-TABLE tt-relbol NO-UNDO 
    FIELD release# LIKE oe-relh.release#
    FIELD tag# AS cha
    FIELD i-no AS cha FORM "x(15)"
    FIELD i-name AS cha FORM "x(30)"
    FIELD ord-no LIKE oe-ord.ord-no
    FIELD job-no LIKE oe-rell.job-no
    FIELD job-no2 LIKE oe-rell.job-no2
    FIELD loc LIKE oe-rell.loc
    FIELD loc-bin LIKE oe-rell.loc-bin
    FIELD cust-no LIKE oe-rell.cust-no
    FIELD cases LIKE oe-rell.cases
    FIELD qty-case LIKE oe-rell.qty-case
    FIELD cases-unit LIKE fg-rctd.cases-unit
    FIELD partial LIKE oe-rell.partial
    FIELD qty LIKE oe-rell.qty
    FIELD t-qty LIKE oe-rell.qty
    FIELD line LIKE oe-rell.line
    FIELD oerell-row AS ROWID
    FIELD seq AS INT
    FIELD warned AS LOG
    FIELD po-no LIKE oe-boll.po-no
    /* gdm - 10160906 */
    FIELD trailer# LIKE oe-relh.trailer
    INDEX release# release# ord-no i-no po-no.

DEFINE TEMP-TABLE ttRelQtys
    FIELD release# LIKE oe-relh.release#
    FIELD i-no LIKE oe-rell.i-no
    FIELD ord-no LIKE oe-rell.ord-no
    FIELD qty-rel LIKE oe-rell.qty
    .
{addon/bol/tt-email.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

/* Just for updating oe-rel.qty */
DEF TEMP-TABLE tt-report-a NO-UNDO LIKE report.          

DEF BUFFER bf-tmp FOR tt-relbol.
DEF BUFFER b-tt-relbol FOR tt-relbol.
DEF BUFFER b-oe-ordl FOR oe-ordl.

DEFINE VARIABLE CHAR-hdl          AS cha              NO-UNDO.
DEFINE VARIABLE v-ship-lu         AS ch               INITIAL ["I,S,B"] NO-UNDO.
DEFINE VARIABLE v-ship-no         AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-s-code          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-no-post         AS INTEGER          FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE v-tot-post        AS INTEGER          FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE v-first-release   AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-r-no            LIKE inv-head.r-no    NO-UNDO.
DEFINE VARIABLE v-ext-price       LIKE inv-line.t-price NO-UNDO.
DEFINE VARIABLE v-nxt-r-no        AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-po-no           LIKE oe-rel.po-no     NO-UNDO.
DEFINE VARIABLE v-royal           AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-n-bol           LIKE oe-ctrl.n-bol    NO-UNDO.
DEFINE VARIABLE v-bol-qty         LIKE oe-boll.qty      NO-UNDO.
DEFINE VARIABLE temp-tax          AS DECIMAL          INIT 0 NO-UNDO.
DEFINE VARIABLE v-hold-list       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-release#        AS INTEGER          NO-UNDO.
DEFINE VARIABLE lv-do-leave-tag   AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lv-do-leave-rel   AS LOGICAL          NO-UNDO.
DEFINE VARIABLE is-bol-printed    AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-msgreturn       AS INTEGER          NO-UNDO.
DEFINE VARIABLE lv-scan-next      AS LOGICAL          NO-UNDO.
DEFINE VARIABLE ll                AS LOGICAL          NO-UNDO.
DEFINE VARIABLE li                AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-ord-no          AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-prgmname        AS CHARACTER        NO-UNDO INIT 'b-relbol.'.
DEFINE VARIABLE v-ran-need-scroll AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-prev-rowid      AS ROWID            NO-UNDO.
DEFINE VARIABLE vfrt-pay          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE vfob-code         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE vfrt-list         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE vfob-list         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rell-ctr          AS INTEGER          NO-UNDO.
DEFINE VARIABLE hBrowse           AS HANDLE           NO-UNDO.
DEFINE VARIABLE hColumn           AS HANDLE           NO-UNDO.
DEFINE VARIABLE iCounter          AS INTEGER          NO-UNDO.
DEFINE VARIABLE ssupdrelpmpt-log  AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lv-go-to-unit     AS LOGICAL          NO-UNDO.
DEFINE VARIABLE ss-bolscan        AS INTEGER          INIT 0 NO-UNDO.
DEFINE VARIABLE lv-po-cnt         AS INTEGER          NO-UNDO.
DEFINE VARIABLE lv-po-no          AS cha              NO-UNDO.
DEFINE VARIABLE lv-po-from-rell   AS LOGICAL          NO-UNDO.

DEFINE VARIABLE lvrOeRell         AS ROWID            NO-UNDO.
DEFINE VARIABLE lvrCust           AS ROWID            NO-UNDO.
DEFINE VARIABLE lvrOeOrd          AS ROWID            NO-UNDO.
DEFINE VARIABLE lvlReturnNoApply  AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lvlReturnCancel   AS LOGICAL          NO-UNDO.
DEFINE VARIABLE gvlCheckOrdStat   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lsecurity-flag AS LOGICAL NO-UNDO.
/* bol print/post */
DEF NEW SHARED VAR out-recid AS RECID NO-UNDO.
DEFINE VARIABLE BolPostLog AS LOGICAL NO-UNDO.
DEF STREAM logFile.
DEF VAR cRtnChar AS CHAR NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSSBOLPassword AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSSBOLPassword AS CHARACTER NO-UNDO.

v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

RUN sys/ref/nk1look.p (INPUT cocode, "SSBOLPassword", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
lSSBOLPassword = LOGICAL(cRtnChar) NO-ERROR .
RUN sys/ref/nk1look.p (INPUT cocode, "SSBOLPassword", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  cSSBOLPassword = cRtnChar NO-ERROR .

IF NOT lSSBOLPassword OR cSSBOLPassword EQ ""  THEN
 lsecurity-flag = YES .

/* Include file contains transaction keyword */
{sys/ref/relpost.i}

DO TRANSACTION:

  {sys/inc/ssbol.i}
  {sys/inc/relmerge.i}
  {sys/inc/ssbolprint.i}
  

  FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "BOLFMT"
        NO-LOCK NO-ERROR.
  v-royal = AVAIL sys-ctrl AND lookup(sys-ctrl.char-fld,v-hold-list) NE 0.

  /* gdm - 10160906 */
  {sys/inc/ssbolscan.i}

  FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "SSBOLSCAN"
        NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
  ASSIGN ss-bolscan = sys-ctrl.int-fld .


  FIND FIRST sys-ctrl WHERE
       sys-ctrl.company EQ cocode AND
       sys-ctrl.name    EQ "SSUPDRELPMPT"
       NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
     CREATE sys-ctrl.
     ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.module = "SS"
        sys-ctrl.name    = "SSUPDRELPMPT"
        sys-ctrl.descrip = "Sharpshooter Create BOL Prompt if Saved Release?"
        sys-ctrl.log-fld = YES.
  END.

  ssupdrelpmpt-log = sys-ctrl.log-fld.


END.

/* Check if hold status will be checked on order */
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
IF AVAIL oe-ctrl THEN
  gvlCheckOrdStat = (IF oe-ctrl.spare-int-1 EQ 0 THEN YES ELSE NO).

PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEF INPUT PARAM mailTo AS CHAR.
    DEF INPUT PARAM mailsubject AS CHAR.
    DEF INPUT PARAM mailText AS CHAR.
    DEF INPUT PARAM mailFiles AS CHAR.
    DEF INPUT PARAM mailDialog AS LONG.
    DEF OUTPUT PARAM retCode AS LONG.
END.

&SCOPED-DEFINE SORTBY-PHRASE BY tt-relbol.seq DESC

DO TRANSACTION:
    {sys\inc\BOLWeight.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-relbol

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-relbol.seq tt-relbol.release# tt-relbol.tag# tt-relbol.trailer tt-relbol.i-no tt-relbol.i-name tt-relbol.ord-no tt-relbol.loc tt-relbol.loc-bin tt-relbol.cust-no tt-relbol.cases tt-relbol.qty-case tt-relbol.cases-unit tt-relbol.partial tt-relbol.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-relbol.release#  tt-relbol.tag# ~
tt-relbol.trailer  tt-relbol.cases  tt-relbol.loc  tt-relbol.loc-bin  tt-relbol.cust-no  tt-relbol.qty-case  tt-relbol.qty  tt-relbol.partial   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-relbol
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-relbol
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-relbol WHERE ~{&KEY-PHRASE}       ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-relbol WHERE ~{&KEY-PHRASE}       ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-relbol
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-relbol


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS v-job-qty v-qoh v-rel-qty v-scan-qty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD comma B-table-Win 
FUNCTION comma RETURNS CHARACTER
  (ipValue AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-bal B-table-Win 
FUNCTION get-bal RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE v-job-qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Job Qty" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-qoh AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-rel-qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Released Qty" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-scan-qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Scanned Qty" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 10  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-relbol SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-relbol.seq COLUMN-LABEL "Seq#" FORMAT ">>>>>>>>>"
  tt-relbol.release# COLUMN-LABEL "Release#" FORMAT ">>>>>>>>>"
  tt-relbol.tag# COLUMN-LABEL "Tag#" FORMAT "x(20)":U
  tt-relbol.trailer COLUMN-LABEL "Trailer" FORMAT "x(20)":U
  tt-relbol.i-no COLUMN-LABEL "Item#" 
  tt-relbol.i-name COLUMN-LABEL "Name" 
  tt-relbol.ord-no COLUMN-LABEL "Order#" 
  tt-relbol.loc COLUMN-LABEL "Warehouse" 
  tt-relbol.loc-bin COLUMN-LABEL "Bin"
  tt-relbol.cust-no COLUMN-LABEL "Customer#"
  tt-relbol.cases   COLUMN-LABEL "Units"
  tt-relbol.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9"
  tt-relbol.cases-unit COLUMN-LABEL "Unit!per Pallet" FORMAT ">>9":U
  tt-relbol.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
  tt-relbol.qty COLUMN-LABEL "Quantity"
  ENABLE
   tt-relbol.release#
   tt-relbol.tag#   
   tt-relbol.trailer
   tt-relbol.cases
   tt-relbol.loc
   tt-relbol.loc-bin
   tt-relbol.cust-no
   tt-relbol.qty-case
   tt-relbol.qty
   tt-relbol.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 114 BY 9.05
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     v-job-qty AT ROW 1 COL 8 COLON-ALIGNED WIDGET-ID 2
     v-qoh AT ROW 1 COL 31 COLON-ALIGNED WIDGET-ID 4
     v-rel-qty AT ROW 1 COL 61 COLON-ALIGNED
     v-scan-qty AT ROW 1 COL 94 COLON-ALIGNED
     br_table AT ROW 1.95 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.24
         WIDTH              = 114.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table v-scan-qty F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-job-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-qoh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-rel-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-scan-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-relbol WHERE ~{&KEY-PHRASE}
      ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */    
    {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
    
  IF NOT adm-new-record THEN
  RUN display-qtys.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


ON 'end-error':U OF tt-relbol.release#
DO:
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
   RUN do-cancel IN WIDGET-HANDLE(char-hdl).
   RUN check-modified ('clear').

   RETURN NO-APPLY.

END.
ON 'end-error':U OF tt-relbol.tag#
DO:
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
   RUN do-cancel IN WIDGET-HANDLE(char-hdl).
   RUN check-modified ('clear').

   RETURN NO-APPLY.

END.
ON 'entry':U OF tt-relbol.release#
DO:
    IF lv-scan-next THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.
    END.
    RETURN.
END.
ON 'entry':U OF tt-relbol.tag#
DO:
    IF lv-go-to-unit THEN
    DO:
       ASSIGN lv-go-to-unit = NO.
       APPLY "ENTRY":U TO tt-relbol.cases IN BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'leave':U OF tt-relbol.release#
DO:
    DEF VAR lv-num-item AS INT NO-UNDO.
    DEF VAR v-release-in-process AS LOG NO-UNDO.
    DEF VAR lOrderOnHold AS LOG NO-UNDO.
    
    DEF BUFFER b-itemfg FOR itemfg.
    IF (LASTKEY = -1 OR LASTKEY = 27 /*ESC*/) AND NOT lv-do-leave-rel THEN RETURN .
    lv-do-leave-rel = NO.
    IF NOT CAN-FIND(FIRST oe-relh WHERE oe-relh.company = cocode
                                    AND oe-relh.release# = INT(SELF:SCREEN-VALUE)
                                    AND oe-relh.printed AND NOT oe-relh.posted )
    THEN DO:
        IF NOT g-sharpshooter THEN MESSAGE "Invalid Release#. Not printed? or Already posted?   Try help..." VIEW-AS ALERT-BOX ERROR. 
        ELSE RUN custom/d-msg.w ("Error","","Invalid Release#.  Not printed? or Already posted? ","",1,"OK", OUTPUT v-msgreturn).         
        RETURN NO-APPLY.
    END.

    IF CAN-FIND(FIRST asi._file WHERE
       asi._file._File-Name = "ssrelbol") THEN
       DO:
          RUN addon/bol/find-ssrelbol.p(INPUT cocode,
                                        INPUT INT(SELF:SCREEN-VALUE),
                                        OUTPUT v-release-in-process).

          IF v-release-in-process AND ssupdrelpmpt-log THEN
          DO:
             IF NOT g-sharpshooter THEN
                MESSAGE "Release In Process. Please Exit and Use Update Release Button."
                   VIEW-AS ALERT-BOX ERROR. 
             ELSE
                RUN custom/d-msg.w ("Error","Release In Process.","Please Exit and Use","Update Release Button.",1,"OK", OUTPUT v-msgreturn).
         
             RETURN NO-APPLY.
          END.
       END.

    ASSIGN
    tt-relbol.release# = INT(SELF:SCREEN-VALUE)
    v-release# = tt-relbol.release#
    lv-num-item = 0
    v-rel-qty = 0
    v-job-qty = 0
    v-qoh     = 0.

    RUN ordStatCheck (OUTPUT lOrderOnHold).
        
    IF lOrderOnHold THEN DO:
      RUN dispatch ("cancel-record").
      RETURN NO-APPLY.
    END.
      

    FOR EACH oe-relh NO-LOCK WHERE oe-relh.company = cocode AND
                           oe-relh.release# = INT(SELF:SCREEN-VALUE)
                       AND oe-relh.printed AND NOT oe-relh.posted ,
        EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK BREAK BY oe-rell.i-no BY oe-rell.LINE:
        IF FIRST-OF(oe-rell.i-no) THEN lv-num-item = lv-num-item + 1.
        v-rel-qty = v-rel-qty + oe-rell.qty.
        IF LAST-OF(oe-rell.LINE) THEN DO:
            FIND FIRST b-oe-ordl WHERE b-oe-ordl.company EQ oe-relh.company
                                   AND b-oe-ordl.ord-no  EQ oe-rell.ord-no
                                   AND b-oe-ordl.LINE    EQ oe-rell.LINE
                                 NO-LOCK NO-ERROR.
            IF AVAIL b-oe-ordl THEN
                v-job-qty = get-bal().
            FIND FIRST b-itemfg WHERE b-itemfg.company EQ oe-relh.company
                                  AND b-itemfg.i-no    EQ oe-rell.i-no
                              NO-LOCK NO-ERROR.
            IF AVAIL b-itemfg THEN
                v-qoh = b-itemfg.q-onh.
        END.
    END.
    IF lv-num-item = 1 THEN DISPLAY v-rel-qty v-job-qty v-qoh WITH FRAME {&FRAME-NAME}.
    RETURN .
END.

ON 'F6':U OF BROWSE {&browse-name} /* tt-relbol.release# */
DO:
    RUN restoreSavedRecs.
END.

ON 'leave':U OF tt-relbol.tag#
DO:
   /* check release qty for the item */
   DEF VAR lv-qty-rel AS INT NO-UNDO.
   DEF VAR lv-qty-tag AS INT NO-UNDO.
   DEF VAR ll AS LOG NO-UNDO.

   IF (LASTKEY = -1 OR LASTKEY = 27 /*ESC*/) AND NOT lv-do-leave-tag  THEN RETURN.
   lv-do-leave-tag = NO.

   FIND FIRST oe-relh
       WHERE oe-relh.company  EQ cocode
         AND oe-relh.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
       NO-LOCK NO-ERROR.
   
   FIND FIRST loadtag
       WHERE loadtag.company   EQ cocode
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ tt-relbol.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Invalid Loadtag for the Release..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Invalid Loadtag for the Release...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
/*    IF AVAIL oe-relh THEN                                                                                               */
/*        FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode                                                            */
/*            AND oe-rell.r-no      EQ oe-relh.r-no                                                                       */
/*            AND oe-rell.i-no      EQ loadtag.i-no                                                                       */
/*          USE-INDEX r-no NO-LOCK NO-ERROR.                                                                              */
/*    IF NOT AVAIL oe-rell THEN DO:                                                                                       */
/*         RUN custom/d-msg.w ("Error","","FG Item on Tag does not match Item on Release","",1,"OK", OUTPUT v-msgreturn). */
/*         RETURN NO-APPLY.                                                                                               */
/*    END.                                                                                                                */
   RUN get-v-ord (OUTPUT lvrOeRell, OUTPUT lvrCust,
                  OUTPUT lvrOeOrd,  OUTPUT lvlReturnNoApply,
                  OUTPUT lvlReturnCancel).
   IF lvlReturnCancel THEN DO:
       RUN local-cancel-record.
       RETURN NO-APPLY.
   END.
   IF lvlReturnNoApply THEN
     RETURN NO-APPLY.

   IF lvrOeRell NE ? THEN
       FIND oe-rell WHERE ROWID(oe-rell) EQ lvrOeRell NO-LOCK NO-ERROR.
   IF lvrCust NE ? THEN
       FIND cust WHERE ROWID(cust) EQ lvrCust NO-LOCK NO-ERROR.
   IF lvrOeOrd NE ? THEN
       FIND oe-ord WHERE ROWID(oe-ord) EQ lvrOeOrd NO-LOCK NO-ERROR.   

   /*== check multi po# ===*/

   ASSIGN lv-po-cnt = 0
          lv-po-no  = "".
   
   FOR EACH oe-rell
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND oe-rell.ord-no    EQ v-ord-no
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK BREAK BY oe-rell.po-no:
     IF FIRST-OF(oe-rell.po-no) THEN lv-po-cnt = lv-po-cnt + 1.
     ASSIGN
     lv-po-no = oe-rell.po-no
     lv-po-from-rell = YES.
   END.
   IF lv-po-cnt > 1 THEN RUN addon/bol/d-selpo.w (RECID(oe-relh),RECID(loadtag),v-ord-no, OUTPUT lv-po-no).
      /*task 10030502*/
   ELSE IF NOT lv-po-from-rell AND CAN-FIND (FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = loadtag.i-no AND itemfg.i-code = "C") THEN DO:
       {addon/bol/bolpoord.i}
   END.
   IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.tag# = tt-relbol.tag#:SCREEN-VALUE
                   AND RECID(bf-tmp) <> RECID(tt-relbol) ) 
   THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Tag# already scanned..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# already scanned...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
   IF NOT CAN-FIND(FIRST fg-bin
                   WHERE fg-bin.company EQ cocode
                     AND fg-bin.tag     EQ tt-relbol.tag#:SCREEN-VALUE
                     AND fg-bin.i-no    EQ loadtag.i-no
/*                      AND fg-bin.job-no  EQ loadtag.job-no  */
/*                      AND fg-bin.job-no2 EQ loadtag.job-no2 */
                     AND fg-bin.qty     GT 0)
   THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Tag# has no inventory..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# has no inventory...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
   IF relmerge-int EQ 0 AND
      NOT CAN-FIND(FIRST oe-ordl
                   WHERE oe-ordl.company  EQ cocode
                     AND oe-ordl.ord-no   EQ v-ord-no
                     AND oe-ordl.i-no     EQ loadtag.i-no
                     AND ((oe-ordl.job-no EQ loadtag.job-no AND
                           oe-ordl.job-no2 EQ loadtag.job-no2) OR
                          TRIM(oe-ordl.job-no) EQ ""))
   THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Job# not on Order..." v-ord-no VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Job# not on Order...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
   FOR EACH oe-ordl
       WHERE oe-ordl.company  EQ cocode
         AND oe-ordl.ord-no   EQ v-ord-no
         AND oe-ordl.i-no     EQ loadtag.i-no
       NO-LOCK
       BREAK BY oe-ordl.job-no  DESC
             BY oe-ordl.job-no2 DESC:
     IF LAST(oe-ordl.job-no) OR
        (oe-ordl.job-no EQ loadtag.job-no AND
         oe-ordl.job-no2 EQ loadtag.job-no2) THEN
       LEAVE.
   END.
   IF NOT AVAIL oe-ordl AND loadtag.ord-no <> 0 THEN DO:
     IF NOT  g-sharpshooter THEN MESSAGE "Tag# Order/FG# invalid..." VIEW-AS ALERT-BOX ERROR.
     ELSE RUN custom/d-msg.w ("Error","","Tag# Order#/FG# invalid...","",1,"OK", OUTPUT v-msgreturn).         
     RETURN NO-APPLY.
   END.
   IF NOT CAN-FIND(FIRST oe-rell
                   WHERE oe-rell.company EQ oe-relh.company
                     AND oe-rell.r-no    EQ oe-relh.r-no
                     AND oe-rell.ord-no  EQ v-ord-no
                     AND oe-rell.i-no    EQ loadtag.i-no
                   USE-INDEX r-no) THEN DO:
     ll = NO.
     IF ssbol-log OR ssbol-log = ? THEN  DO:
       IF NOT  g-sharpshooter THEN
          MESSAGE "This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " 
                + TRIM(loadtag.i-no) +
                " is not on release, do you want to ADD TO RELEASE? "
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. 
       ELSE DO:
            RUN custom/d-msg.w ("Question","This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG#" + TRIM(loadtag.i-no) +
               " is not on release." , "Do you want to ADD TO RELEASE? ","",2,"Yes,No",OUTPUT v-msgreturn).
            IF v-msgreturn = 1 THEN ll = YES.
       END.
     END.
     ELSE DO:
        IF NOT  g-sharpshooter THEN MESSAGE "Invalid Tag# for the Release#. Try again..." VIEW-AS ALERT-BOX ERROR.
        ELSE RUN custom/d-msg.w ("Error","","Invalid Tag# for this Release#.  Try again...","",1,"OK", OUTPUT v-msgreturn).
     END.
     IF NOT ll THEN RETURN NO-APPLY.
     tt-relbol.warned = YES.
   END.
   IF NOT tt-relbol.warned THEN DO:
     lv-qty-rel = 0.
     FOR EACH oe-rell FIELDS(qty)
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND oe-rell.ord-no   EQ v-ord-no
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK:
       lv-qty-rel = lv-qty-rel + oe-rell.qty.
     END.
     lv-qty-tag = loadtag.pallet-count.
     FOR EACH bf-tmp
         WHERE bf-tmp.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
           AND (bf-tmp.ord-no   EQ v-ord-no)
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND bf-tmp.warned   EQ NO
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       lv-qty-tag = lv-qty-tag + bf-tmp.qty.
     END.
   END.
   ll = NO.
   IF lv-qty-tag GT lv-qty-rel THEN DO:
      IF NOT g-sharpshooter THEN
         MESSAGE "Qty scanned exceeds qty released for Order# " + TRIM(STRING(v-ord-no),">>>>>>>>")
              + " FG# "  + TRIM(loadtag.i-no) +
             ", accept this tag anyway?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
     ELSE DO:
        RUN custom/d-msg.w ("Question","Qty scanned exceeds qty released for Order# " +
                      TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " +
                      TRIM(loadtag.i-no) + "." , "Accept this tag anyway? ","",2,"Yes,No",OUTPUT v-msgreturn).
        IF v-msgreturn = 1 THEN ll = YES.
     END.
     IF NOT ll THEN DO:
       APPLY "entry" TO tt-relbol.tag# .
       RETURN NO-APPLY.
     END.
     FOR FIRST bf-tmp 
         WHERE bf-tmp.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       bf-tmp.warned = YES.
     END.
   END.

   RELEASE oe-ord.
   IF v-ord-no NE 0 THEN
   FIND FIRST oe-ord NO-LOCK
       WHERE oe-ord.company EQ cocode
         AND oe-ord.ord-no  EQ v-ord-no
       NO-ERROR.

   RELEASE fg-bin.
   FOR EACH fg-bin NO-LOCK
       WHERE fg-bin.company  EQ cocode
         AND fg-bin.tag      EQ tt-relbol.tag#:SCREEN-VALUE
         AND fg-bin.i-no     EQ loadtag.i-no
         AND fg-bin.job-no   EQ loadtag.job-no
         AND fg-bin.job-no2  EQ loadtag.job-no2
         AND fg-bin.qty      GT 0
         AND ((AVAIL oe-ord AND fg-bin.cust-no EQ oe-ord.cust-no) OR
              fg-bin.cust-no EQ "")
       USE-INDEX tag
       BREAK BY fg-bin.cust-no DESC
             BY fg-bin.qty:

     IF (fg-bin.cust-no EQ oe-relh.cust-no AND LAST-OF(fg-bin.cust-no)) OR
        LAST(fg-bin.cust-no) THEN LEAVE.
   END.
   
   /* Code moved since with section was too long */
   RUN assign-ttrel.
   
   IF TRIM(ssbolscan-cha) NE "" THEN
   DO:       
      DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
      APPLY "entry" TO tt-relbol.trailer IN BROWSE {&browse-name}.
      DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
      RETURN NO-APPLY.
   END.

   FIND FIRST itemfg WHERE
        itemfg.company = cocode AND
        itemfg.i-no = tt-relbol.i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.

   IF AVAIL ITEMfg AND itemfg.ship-meth THEN DO: /* case */
      APPLY "entry" TO tt-relbol.cases IN BROWSE {&browse-name}. 
      DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
      RETURN NO-APPLY.
   END.
   ELSE DO:

      IF int(tt-relbol.qty:screen-value) = 0 THEN DO:
         APPLY "entry" TO tt-relbol.qty.
         DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
         RETURN NO-APPLY.
      END.
      ELSE DO: 
          APPLY "row-leave" TO BROWSE {&browse-name}.
      END.
   END.
   RETURN NO-APPLY.
END.

ON 'value-changed':U OF tt-relbol.cases
DO:
   RUN calc-qty.
END.
ON 'value-changed':U OF tt-relbol.qty-case
DO:
   RUN calc-qty.
END.
ON 'value-changed':U OF tt-relbol.partial
DO:
   RUN calc-qty.
END.
ON 'help':U OF tt-relbol.release#
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.

    RUN windows/l-oerelh.w (cocode,FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
    IF char-val <> "" THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).  
        lv-do-leave-rel = YES.
        APPLY "leave" TO tt-relbol.release# IN BROWSE {&browse-name}.
        APPLY "entry" TO tt-relbol.tag# IN BROWSE {&browse-name}.
    END.
   
    RETURN NO-APPLY.
END.
ON 'help':U OF tt-relbol.tag#
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
                      /*was l-ldtag2.w */
    IF ss-bolscan = 0 THEN DO:
    RUN windows/l-ldtag9.w (cocode,NO, tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name},FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).

    IF char-val <> "" THEN DO:
        tt-relbol.tag#:SCREEN-VALUE = ENTRY(1,char-val).
        lv-do-leave-tag = YES.
        APPLY "leave" TO tt-relbol.tag#.
    END.
    RETURN NO-APPLY.
    END.

END.
ON 'help':U OF tt-relbol.trailer#
DO:
   DEF VAR char-val AS cha NO-UNDO.

   RUN browsers/l-truck.w (cocode,
                           tt-relbol.trailer#:SCREEN-VALUE IN BROWSE {&browse-NAME}, 
                           OUTPUT char-val).
   IF char-val <> "" THEN
   DO:
      ASSIGN tt-relbol.trailer:SCREEN-VALUE = ENTRY(1,char-val).
      APPLY "leave" TO tt-relbol.trailer#.
   END.
END.

ON 'leave':U OF tt-relbol.trailer
DO:
    IF LASTKEY = -1 OR LASTKEY = 27 /*ESC*/ THEN
       RETURN.

    IF TRIM(tt-relbol.trailer#:SCREEN-VALUE IN BROWSE {&browse-name}) EQ "" OR
       NOT CAN-FIND(FIRST truck WHERE
           truck.company EQ cocode AND
           truck.truck-code EQ tt-relbol.trailer#:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
       DO:
          IF NOT g-sharpshooter THEN MESSAGE "Invalid Trailer#." VIEW-AS ALERT-BOX ERROR. 
          ELSE RUN custom/d-msg.w ("Error","","Invalid Trailer#.","",1,"OK", OUTPUT v-msgreturn).         
          RETURN NO-APPLY.
       END.

    FIND FIRST oe-relh WHERE
         oe-relh.company  EQ cocode AND
         oe-relh.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
         NO-LOCK NO-ERROR.

    IF AVAIL oe-relh THEN DO:

       IF TRIM(oe-relh.trailer) NE "" AND
          oe-relh.trailer NE tt-relbol.trailer:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:

          ll = NO.

          IF NOT g-sharpshooter THEN
             MESSAGE "Trailer # does not match Release Trailer#.  Is this the Correct Trailer#?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
          ELSE
          DO:
             RUN custom/d-msg.w ("Question","","Is this the Correct Trailer#?","",2,"YES,NO", OUTPUT v-msgreturn).
             IF v-msgreturn = 1 THEN ll = YES.
          END.

          IF NOT ll THEN
             RETURN NO-APPLY.
       END.
    END.

    FIND FIRST itemfg WHERE
         itemfg.company = cocode AND
         itemfg.i-no = tt-relbol.i-no:SCREEN-VALUE
         NO-LOCK NO-ERROR.
    
    IF AVAIL itemfg AND itemfg.ship-meth THEN
    DO:
       APPLY "entry" TO tt-relbol.cases IN BROWSE {&browse-name}.
       DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
       RETURN.
    END.
    ELSE IF int(tt-relbol.qty:screen-value) = 0 THEN
    DO:
       APPLY "entry" TO tt-relbol.qty IN BROWSE {&browse-name}.
       DISPLAY tt-relbol.tag# WITH BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
    ELSE
       APPLY "row-leave" TO BROWSE {&browse-name}.

    RETURN NO-APPLY.
    
END.
/* gdm - 10160906 end */

/* ***************************  Main Block  *************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* Save entered data to text file in case of crash */
cTmpSaveFile = "logs/Relbol" + STRING(TODAY, "999999") + STRING(TIME, "999999") + USERID("nosweat") + ".txt".
lSaveToTempfile = FALSE.
IF SEARCH("logs/RelBol.txt") NE ? THEN
  lSaveToTempFile = TRUE.
  
IF TRIM(ssbolscan-cha) EQ "" THEN
DO:
    hBrowse = BROWSE br_table:HANDLE.
   
     /* Get the handle of the selected field column */
    DO iCounter = 1 TO hBrowse:NUM-COLUMNS:
       hColumn = hBrowse:GET-BROWSE-COLUMN(iCounter).
   
       IF hColumn:NAME = "trailer#" THEN
       DO:
          hColumn:READ-ONLY = TRUE.
          LEAVE.
       END.
    END.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreSavedRecs B-table-Win
PROCEDURE restoreSavedRecs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* RUN addon\bol\relfileselect.w (INPUT "relbol*", OUTPUT cTmpSaveFile). */
/*  DEF VAR cTmpSaveFile AS CHAR NO-UNDO.*/
  DEF VAR lFileSelected AS LOG NO-UNDO.
  SYSTEM-DIALOG GET-FILE cTmpSaveFile   
    FILTERS "restore (relbol*)" "relbol*"
    INITIAL-DIR ".\logs"
    UPDATE lFileSelected
    MUST-EXIST
    TITLE "Select Restore File". 
     
IF lFileSelected THEN DO: 
    INPUT STREAM sTmpSaveInfo FROM VALUE(cTmpSaveFile).
    REPEAT:
        CREATE tt-relbol. 
        IMPORT STREAM sTmpSaveInfo tt-relbol EXCEPT tt-relbol.oerell-row . 
         
    END.
    INPUT STREAM sTmpSaveInfo CLOSE.
    
    RUN dispatch ('open-query').
END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-ttrel B-table-Win 
PROCEDURE assign-ttrel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

   ASSIGN tt-relbol.tag# = tt-relbol.tag#:SCREEN-VALUE IN BROWSE br_table
          tt-relbol.i-no = loadtag.i-no
          tt-relbol.i-name = loadtag.i-name          
          tt-relbol.ord-no = v-ord-no 
          tt-relbol.job-no = loadtag.job-no
          tt-relbol.job-no2 = loadtag.job-no2
          tt-relbol.loc = IF AVAIL fg-bin THEN fg-bin.loc ELSE loadtag.loc
          tt-relbol.loc-bin = IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE loadtag.loc-bin
          tt-relbol.cust-no = IF AVAIL fg-bin THEN fg-bin.cust-no ELSE ""  
          tt-relbol.qty = IF AVAIL fg-bin THEN fg-bin.qty ELSE loadtag.pallet-count  /* loadtag.qty */
          tt-relbol.cases  = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) ELSE loadtag.case-bundle
          tt-relbol.qty-case = IF AVAIL fg-bin THEN fg-bin.case-count ELSE loadtag.qty-case
          tt-relbol.cases-unit = IF AVAIL fg-bin THEN fg-bin.cases-unit ELSE loadtag.case-bundle
          tt-relbol.partial = IF AVAIL fg-bin THEN fg-bin.partial-count ELSE loadtag.partial
          tt-relbol.line = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
          tt-relbol.po-no = lv-po-no
          tt-relbol.warned = ll.
   
   DISPLAY tt-relbol.release#
           tt-relbol.tag# 
           tt-relbol.i-no
           tt-relbol.i-name
           tt-relbol.qty
           tt-relbol.ord-no
           tt-relbol.loc
           tt-relbol.loc-bin
           tt-relbol.cust-no
           tt-relbol.cases tt-relbol.qty-case
           tt-relbol.cases-unit tt-relbol.partial
           WITH BROWSE {&browse-name}.

   IF TRIM(ssbolscan-cha) NE "" AND AVAIL oe-relh THEN
   DO:
      tt-relbol.trailer# = oe-relh.trailer.
      DISPLAY tt-relbol.trailer# WITH BROWSE {&browse-name}.
   END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BolPostLog B-table-Win 
PROCEDURE BolPostLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

  PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
    STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-email B-table-Win 
PROCEDURE build-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-done-what AS cha NO-UNDO.
   DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
   DEF INPUT PARAM ipdRelQty AS DECIMAL NO-UNDO.
   DEF INPUT PARAM ipdScanQty AS DECIMAL NO-UNDO.

   DEF BUFFER bf-relbol FOR tt-relbol.
   DEF BUFFER bf-rell FOR oe-rell.

   IF ip-done-what = "Added" OR ip-done-what EQ "OverUnder" THEN DO:
      FIND bf-relbol WHERE RECID(bf-relbol) = ip-recid NO-LOCK NO-ERROR.   
      IF AVAIL bf-relbol THEN DO:
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                        AND oe-ordl.ord-no = bf-relbol.ord-no
                        AND oe-ordl.i-no = bf-relbol.i-no 
                        AND oe-ordl.LINE = bf-relbol.LINE
             USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT AVAIL oe-ordl THEN
            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                           AND oe-ordl.ord-no = bf-relbol.ord-no
                           AND oe-ordl.i-no = bf-relbol.i-no
                USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT CAN-FIND(FIRST tt-email WHERE tt-email.release# = bf-relbol.release#
                                          AND tt-email.ord-no = bf-relbol.ord-no
                                          AND tt-email.i-no = bf-relbol.i-no
                                          AND tt-email.done-what = ip-done-what)
         THEN DO:
            CREATE tt-email.
            ASSIGN tt-email.release# = bf-relbol.release#
                   tt-email.ord-no = bf-relbol.ord-no
                   tt-email.i-no = bf-relbol.i-no
                   tt-email.part-no = IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE ""
                   tt-email.i-name = bf-relbol.i-name
                   tt-email.done-what = ip-done-what    
                   tt-email.ord-no2 = bf-relbol.ord-no
                   tt-email.job-no = IF AVAIL oe-ordl THEN oe-ordl.job-no ELSE ''
                   tt-email.job-no2 = IF AVAIL oe-ordl THEN oe-ordl.job-no2 ELSE 0
                   tt-email.qty-scan = ipdScanQty
                   tt-email.qty-rel = ipdRelQty
                    .
         END.
      END.
   END.
   ELSE DO:  /* deleted */
      FIND bf-rell WHERE RECID(bf-rell) = ip-recid NO-LOCK NO-ERROR.
      IF AVAIL bf-rell THEN DO:
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                        AND oe-ordl.ord-no = bf-rell.ord-no
                        AND oe-ordl.i-no = bf-rell.i-no 
                        USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT CAN-FIND(FIRST tt-email WHERE tt-email.release# = tt-relbol.release#
                                          AND tt-email.ord-no = tt-relbol.ord-no
                                          AND tt-email.i-no = bf-rell.i-no
                                          AND tt-email.done-what = ip-done-what  )
         THEN DO:
            CREATE tt-email.
            ASSIGN tt-email.release# = tt-relbol.release#
                   tt-email.ord-no = tt-relbol.ord-no
                   tt-email.i-no = bf-rell.i-no
                   tt-email.part-no = IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE ""
                   tt-email.i-name = IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE ""
                   tt-email.done-what = ip-done-what   
                   tt-email.ord-no2 = bf-rell.ord-no
                   tt-email.job-no = IF AVAIL oe-ordl THEN oe-ordl.job-no ELSE ''
                   tt-email.job-no2 = IF AVAIL oe-ordl THEN oe-ordl.job-no2 ELSE 0
                   .
         END.
      END.
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty B-table-Win 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    tt-relbol.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
        STRING((DEC(tt-relbol.cases:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) *
                DEC(tt-relbol.qty-case:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) +
               DEC(tt-relbol.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-bol B-table-Win 
PROCEDURE check-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-bol-printed AS LOG NO-UNDO.

  FIND FIRST bf-tmp NO-ERROR.
  IF NOT AVAIL bf-tmp THEN op-bol-printed = YES.
  ELSE op-bol-printed = is-bol-printed.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE choose-order B-table-Win 
PROCEDURE choose-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprOeRell AS ROWID NO-UNDO.
DEF INPUT PARAMETER iprLoadTag AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcButtonList AS CHAR NO-UNDO.

DEFINE OUTPUT PARAMETER opiOrderChosen AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opcButtonChoice AS CHARACTER   NO-UNDO.

DEF VAR lCancel AS LOG.
DEF VAR lChoice AS LOG.
DEF VAR v-msg1 AS CHAR NO-UNDO.
DEF VAR v-msg1-1 AS CHAR NO-UNDO.
DEF VAR v-msg2 AS CHAR NO-UNDO.
DEF VAR v-msg3 AS CHAR NO-UNDO.
DEF VAR v-msg4 AS CHAR NO-UNDO.
DEF VAR choice AS CHAR NO-UNDO.
DEF VAR op-values AS CHAR NO-UNDO.
DEF VAR lValid AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR ip-parms AS CHAR NO-UNDO.
DEF VAR lcUserPrompt AS CHAR  NO-UNDO.
DEF VAR lcNewOrder AS CHAR NO-UNDO.
DEF VAR cOrderList AS CHAR NO-UNDO.
DEF VAR cDir AS CHAR NO-UNDO.

DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf2-oe-rell FOR oe-rell.
DEF BUFFER bf-loadtag FOR loadtag.
DEF BUFFER bf-oe-relh FOR oe-relh .

FIND bf2-oe-rell WHERE ROWID(bf2-oe-rell) = iprOeRell NO-LOCK NO-ERROR.
IF NOT AVAIL bf2-oe-rell THEN
    RETURN.
FIND bf-loadtag WHERE ROWID(bf-loadtag) = iprLoadTag NO-LOCK NO-ERROR.
IF NOT AVAIL bf-loadtag THEN
    RETURN.

  
/* There may be a mismatch between the order number on the release */
/* compared to the loadtag, so ask the user which to use           */       
FOR EACH bf-oe-rell WHERE bf-oe-rell.r-no EQ bf2-oe-rell.r-no
     AND bf-oe-rell.i-no EQ bf2-oe-rell.i-no
    NO-LOCK
    BREAK BY bf-oe-rell.ord-no.
    IF LAST-OF(bf-oe-rell.ord-no) THEN
      cOrderList = cOrderList + STRING(bf-oe-rell.ord-no) + "#".
END. /* Each bf-oe-rell */
    
cOrderList = TRIM(cOrderList, "#").
ASSIGN
   v-msg1-1 = "Release# for Release Ticket is for Order# " + STRING(bf2-oe-rell.ord-no)
   v-msg2 =   "Press CANCEL to process another release."
   v-msg3 =   "Press NO to scan another Tag for this Release."
   v-msg4 =   "Press YES to add tag to BOL but Retain Original Order# on Release".

IF NUM-ENTRIES(cOrderList, "#") = 1 THEN DO:
    /* Use message specifying the one order */              
    IF bf-loadtag.ord-no = 0 THEN
        v-msg1 =   "Tag# Scanned has No Order# but".
      ELSE
        v-msg1 =   "Tag# Scanned is for Order# " + string(bf-loadtag.ord-no) + " but".
      ASSIGN
       v-msg1-1 = "Release# for Release Ticket is for Order# " + STRING(bf2-oe-rell.ord-no)
       v-msg2 =   "Press CANCEL to process another release."
       v-msg3 =   "Press NO to scan another Tag for this Release."
       v-msg4 =   "Press YES to add tag to BOL but Retain Original Order# on Release".

    ip-parms = 
       "type=fill-in,name=fi1,row=2,col=22,enable=false,width=58,scrval=" + v-msg1 + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi5,row=2.9,col=22,enable=false,width=58,scrval=" + v-msg1-1 + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi6,row=10,col=22,enable=false,width=58,scrval=" + v-msg2 + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi7,row=10.8,col=22,enable=false,width=58,scrval=" + v-msg3 + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi8,row=11.6,col=22,enable=false,width=62,scrval=" + v-msg4 + ",FORMAT=X(62)"           
        + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
        + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=17".
END.
ELSE DO:
    IF bf-loadtag.ord-no = 0 THEN
        v-msg1 = "Tag# Scanned has No Order#.".
      ELSE
        v-msg1 = "Tag# Scanned is for Order# " + string(bf-loadtag.ord-no) + ".".

  ASSIGN v-msg1-1 = "Please Select Order# to Use:".
  ip-parms = 
     "type=fill-in,name=fi1,row=2,col=22,enable=false,width=58,scrval=" + v-msg1 + ",FORMAT=X(58)"
      + "|type=fill-in,name=fi5,row=2.9,col=22,enable=false,width=58,scrval=" + v-msg1-1 + ",FORMAT=X(58)"
      + "|type=fill-in,name=fi6,row=10,col=22,enable=false,width=58,scrval=" + v-msg2 + ",FORMAT=X(58)"
      + "|type=fill-in,name=fi7,row=10.8,col=22,enable=false,width=58,scrval=" + v-msg3 + ",FORMAT=X(58)"
      + "|type=fill-in,name=fi8,row=11.6,col=22,enable=false,width=62,scrval=" + v-msg4 + ",FORMAT=X(62)"
      + "|type=selection-list,name=tg2,row=4.5,col=25,enable=true,height=150,font=15,width=20,list=" + cOrderList
      + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
      + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=17".
  /* Load special ini file for larger font #15 */
 

END.

  /* Load font set with larger font #15 */
  cDir = SEARCH("custom/l-font.ini").
  cDir = SUBSTRING(cDir, 1, INDEX(cDir, "custom") + 5).
  LOAD "l-font.ini" DIR cDir BASE-KEY "INI".
  USE "l-font.ini".

IF NOT lsecurity-flag THEN RUN sys/ref/d-passwd.w (9, OUTPUT lsecurity-flag).
        
IF lsecurity-flag THEN
RUN custom/d-prompt.w (INPUT ipcButtonList, ip-parms, "", OUTPUT op-values).
ELSE  op-values =  "DEFAULT" + "," + "No" .

/* Load original ini for original font set */
UNLOAD "l-font.ini" NO-ERROR.

DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
    IF ENTRY(i, op-values) EQ "default" THEN
      choice = ENTRY(i + 1, op-values) NO-ERROR.
    IF ENTRY(i, op-values) EQ "tg2" THEN
      lcNewOrder = ENTRY(i + 1, op-values) NO-ERROR.            
END. /* Do i = 1... */

opcButtonChoice = choice.
opiOrderChosen  = INTEGER(lcNewOrder) NO-ERROR.        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connect-asinos B-table-Win 
PROCEDURE connect-asinos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* wfk - 20281 - could not find where this is called */
/* IF NOT CONNECTED("asinos") THEN CONNECT value("-pf ../asinos.pf"). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-temp-rel B-table-Win 
PROCEDURE create-temp-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-create-backorder AS LOG NO-UNDO.
    
  DEF VAR li-nxt-rel-no AS INT NO-UNDO.
  DEF VAR dScanQty LIKE oe-boll.qty NO-UNDO.
  DEF VAR v-s-code AS CHAR NO-UNDO.
  DEF VAR v-next-seq AS INT NO-UNDO.

  DEF BUFFER bf-rell FOR oe-rell.

  /* Create release line for each temp record without one */
  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#
      BREAK BY bf-tmp.release#
            BY bf-tmp.ord-no
            BY bf-tmp.i-no:
    

    FIND FIRST bf-rell NO-LOCK WHERE bf-rell.r-no EQ oe-relh.r-no NO-ERROR.
    v-s-code = IF AVAIL bf-rell THEN bf-rell.s-code ELSE "B".

    dScanQty = dScanQty + bf-tmp.qty.

    IF LAST-OF(bf-tmp.i-no) THEN DO:
      IF NOT CAN-FIND(FIRST oe-rell
                      WHERE oe-rell.company EQ oe-relh.company
                        AND oe-rell.r-no    EQ oe-relh.r-no
                        AND oe-rell.ord-no  EQ bf-tmp.ord-no
                        AND oe-rell.i-no    EQ bf-tmp.i-no
                      USE-INDEX r-no) THEN DO:

        li-nxt-rel-no = 0.
        FOR EACH bf-rell NO-LOCK
            WHERE bf-rell.company EQ cocode
              AND bf-rell.ord-no  EQ bf-tmp.ord-no
            USE-INDEX ord-no 
            BY bf-rell.rel-no DESC:
    
          li-nxt-rel-no = bf-rell.rel-no.
          LEAVE.  
        END.
        RELEASE bf-rell.

        CREATE oe-rell.
        BUFFER-COPY bf-tmp TO oe-rell
        ASSIGN
         oe-rell.company = oe-relh.company
         oe-rell.r-no    = oe-relh.r-no
         oe-rell.rel-no  = li-nxt-rel-no + 1
         oe-rell.tag     = bf-tmp.tag#
         oe-rell.s-code  = v-s-code
         oe-rell.qty     = dScanQty
         oe-rell.cases   = TRUNC((oe-rell.qty - oe-rell.partial) /
                                 oe-rell.qty-case,0)
         oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).

         /* e-mail logic */         
         RUN build-email ("ADDED", RECID(bf-tmp),0,0).
      END.
      ELSE DO:
          FIND FIRST ttRelQtys
              WHERE ttRelQtys.release# EQ bf-tmp.release#
                AND ttRelQtys.ord-no EQ bf-tmp.ord-no
                AND ttRelQtys.i-no EQ bf-tmp.i-no
              NO-LOCK NO-ERROR.
          IF AVAIL ttRelQtys THEN DO:
              IF dScanQty LT ttRelQtys.qty-rel 
                  OR dScanQty GT ttRelQtys.qty-rel * 1.1 THEN
                  RUN build-email(INPUT "OverUnder", 
                                  INPUT RECID(bf-tmp),
                                  INPUT ttRelQtys.qty-rel,
                                  INPUT dScanQty).
          END.
      END.
      dScanQty = 0.
    END.
  END.

  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#
      BREAK BY bf-tmp.release#
            BY bf-tmp.ord-no
            BY bf-tmp.i-no:

    IF FIRST-OF(bf-tmp.i-no) THEN
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
          AND oe-rell.ord-no  EQ bf-tmp.ord-no
          AND oe-rell.i-no    EQ bf-tmp.i-no
        USE-INDEX r-no:

        CREATE tt-rell.
        BUFFER-COPY oe-rell TO tt-rell
        ASSIGN
           tt-rell.release# = oe-relh.release#
           tt-rell.row-id   = ROWID(oe-rell).
    END.
  END.

  /*create line item on bol with 0 ship qty*/
  IF ip-create-backorder THEN
     FOR EACH bf-tmp USE-INDEX release#,
         FIRST oe-relh NO-LOCK
         WHERE oe-relh.company  EQ cocode
           AND oe-relh.release# EQ bf-tmp.release#
         USE-INDEX release#,
         EACH oe-rell NO-LOCK
         WHERE oe-rell.company EQ oe-relh.company
           AND oe-rell.r-no    EQ oe-relh.r-no
         USE-INDEX r-no:
       
       IF NOT CAN-FIND(FIRST tt-rell WHERE tt-rell.row-id EQ ROWID(oe-rell)) THEN DO:

          CREATE tt-rell.
          BUFFER-COPY oe-rell EXCEPT qty TO tt-rell 
          ASSIGN
             tt-rell.release# = oe-relh.release#
             tt-rell.row-id   = ROWID(oe-rell).

          FIND FIRST oe-ordl WHERE
               oe-ordl.company EQ oe-rell.company AND
               oe-ordl.ord-no  EQ oe-relh.ord-no AND
               oe-ordl.line    EQ oe-rell.line
               NO-LOCK NO-ERROR.

          FOR EACH b-tt-relbol BY b-tt-relbol.seq DESC:
              v-next-seq = b-tt-relbol.seq + 1.
              LEAVE.
          END.
          FIND FIRST tt-relbol WHERE tt-relbol.oerell-row = ROWID(oe-rell)
              NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-relbol THEN DO:

              CREATE tt-relbol.
              ASSIGN
              tt-relbol.release# = oe-relh.release#
              tt-relbol.tag# = oe-rell.tag
              tt-relbol.i-no = oe-rell.i-no
              tt-relbol.ord-no = oe-rell.ord-no
              tt-relbol.job-no = oe-rell.job-no
              tt-relbol.job-no2 = oe-rell.job-no2
              tt-relbol.loc = oe-rell.loc
              tt-relbol.loc-bin = oe-rell.loc-bin
              tt-relbol.cust-no = oe-rell.cust-no 
              tt-relbol.qty = 0
              tt-relbol.cases  = 0
              tt-relbol.qty-case = 0
              tt-relbol.cases-unit = 0
              tt-relbol.partial = 0
              tt-relbol.t-qty = 0
              tt-relbol.oerell-row = ROWID(oe-rell)
              tt-relbol.line = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
              tt-relbol.po-no = oe-rell.po-no
              tt-relbol.seq   = v-next-seq.
    
              RELEASE tt-relbol.
          END.
       END.
     END.

  IF ip-create-backorder EQ NO THEN
  /* Delete release lines that have no temp record */
  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#,
      EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no:
    
    IF NOT CAN-FIND(FIRST tt-rell WHERE tt-rell.row-id EQ ROWID(oe-rell)) THEN DO:
       /* e-mail logic */
      RUN build-email ("DELETED", RECID(oe-rell),0,0).
      DELETE oe-rell.
    END.    
  END.
  
  

  FOR EACH bf-tmp NO-LOCK BREAK BY bf-tmp.release#:
      IF LAST-OF(bf-tmp.release#) THEN RUN send-email (bf-tmp.release#).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-boll2 B-table-Win 
PROCEDURE create-tt-boll2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  CREATE tt-boll2.
  BUFFER-COPY tt-boll EXCEPT rec_key TO tt-boll2
  ASSIGN
   tt-boll2.job-no   = tt-relbol.job-no
   tt-boll2.job-no2  = tt-relbol.job-no2
   tt-boll2.loc      = tt-relbol.loc
   tt-boll2.loc-bin  = tt-relbol.loc-bin
   tt-boll2.cust-no  = tt-relbol.cust-no
   tt-boll2.tag      = tt-relbol.tag#
   tt-boll2.cases    = tt-relbol.cases
   tt-boll2.qty-case = tt-relbol.qty-case
   tt-boll2.partial  = tt-relbol.partial
   tt-boll2.qty      = tt-relbol.qty
   tt-boll2.po-no    = tt-relbol.po-no.

  DELETE tt-relbol.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-qtys B-table-Win 
PROCEDURE display-qtys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-release# LIKE oe-relh.release# NO-UNDO.
  DEF VAR lv-i-no LIKE oe-rell.i-no NO-UNDO.

  DEF BUFFER b-rell FOR oe-rell.
  DEF BUFFER b-relh FOR oe-relh.
  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-scan-qty  = 0
     v-rel-qty   = 0
     v-job-qty   = 0
     v-qoh       = 0
     lv-release# = INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-i-no     = tt-relbol.i-no:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF lv-release# NE 0 AND TRIM(lv-i-no) NE "" THEN DO:
      FOR EACH bf-tmp
          WHERE bf-tmp.release# EQ lv-release#
            AND bf-tmp.i-no     EQ lv-i-no:
        v-scan-qty = v-scan-qty + bf-tmp.qty.
      END.
      
      FOR EACH b-relh NO-LOCK
          WHERE b-relh.company  EQ cocode
            AND b-relh.RELEASE# EQ lv-release#,
          EACH b-rell NO-LOCK
          WHERE b-rell.r-no EQ b-relh.r-no
            AND b-rell.i-no EQ lv-i-no
          USE-INDEX r-no
          BREAK BY b-rell.ord-no
                BY b-rell.LINE:
        v-rel-qty = v-rel-qty + b-rell.qty.
        IF LAST-OF(b-rell.LINE) THEN DO:
            FIND FIRST b-oe-ordl WHERE b-oe-ordl.company EQ b-relh.company
                                   AND b-oe-ordl.ord-no  EQ b-rell.ord-no
                                   AND b-oe-ordl.LINE    EQ b-rell.LINE
                                 NO-LOCK NO-ERROR.
            IF AVAIL b-oe-ordl THEN
                v-job-qty = get-bal().
            FIND FIRST b-itemfg WHERE b-itemfg.company EQ b-relh.company
                                AND b-itemfg.i-no    EQ b-rell.i-no
                              NO-LOCK NO-ERROR.
            IF AVAIL b-itemfg THEN
                v-qoh = b-itemfg.q-onh.
        END.
      END.
    END.

    DISPLAY v-job-qty v-qoh v-rel-qty v-scan-qty.
    IF v-scan-qty <> v-rel-qty  THEN  
        v-scan-qty:BGCOLOR =  12 .
    ELSE v-scan-qty:BGCOLOR =  10 .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-v-ord B-table-Win 
PROCEDURE get-v-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oprOeRell AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER oprCust AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER oprOeOrd AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER oplReturnNoApply AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.

DEF VAR lCancel AS LOG.
DEF VAR lChoice AS LOG.
DEF VAR v-msg1 AS CHAR NO-UNDO.
DEF VAR v-msg1-1 AS CHAR NO-UNDO.
DEF VAR v-msg2 AS CHAR NO-UNDO.
DEF VAR v-msg3 AS CHAR NO-UNDO.
DEF VAR v-msg4 AS CHAR NO-UNDO.
DEF VAR choice AS CHAR NO-UNDO.
DEF VAR op-values AS CHAR NO-UNDO.
DEF VAR lValid AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR ip-parms AS CHAR NO-UNDO.
DEF VAR lcUserPrompt AS CHAR  NO-UNDO.
DEF VAR lcNewOrder AS CHAR NO-UNDO.
DEF VAR cOrderList AS CHAR NO-UNDO.
DEF VAR cDir AS CHAR NO-UNDO.
DEF VAR liCnt AS INT NO-UNDO.
DEF VAR liOrderChosen LIKE oe-rell.ord-no NO-UNDO.
DEF VAR lcUserButton AS CHAR NO-UNDO.

DEF BUFFER bf-oe-rell FOR oe-rell.

ASSIGN v-ord-no = 0
       oplReturnNoApply = NO.

IF loadtag.ord-no = 0 THEN DO: /* stock box */
  /* Find with specific tag number */
  FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                       AND oe-rell.r-no      EQ oe-relh.r-no
                       AND oe-rell.tag      EQ loadtag.tag-no                           
                       USE-INDEX r-no NO-LOCK NO-ERROR.

  IF NOT AVAIL oe-rell THEN DO:
     DEF VAR v-ord-no-list AS cha NO-UNDO.
     DEF VAR v-tag-no-list AS cha NO-UNDO.
     DEF VAR v-i-qty AS INT NO-UNDO.
     DEF VAR v-i-rel-qty AS INT NO-UNDO.

     ASSIGN
     v-ord-no-list = ""
     v-tag-no-list = "".

     /* Total bf-tmp.qty for item on loadtag and order list */
     FOR EACH bf-tmp NO-LOCK WHERE bf-tmp.i-no = loadtag.i-no 
                             BREAK BY bf-tmp.ord-no:
         IF FIRST-OF(bf-tmp.ord-no) THEN
            v-ord-no-list = v-ord-no-list + STRING(bf-tmp.ord-no) + ",".

         ASSIGN
         v-tag-no-list = v-tag-no-list + bf-tmp.tag + ","
         v-i-qty = v-i-qty + bf-tmp.qty.
     END.

     /* Total of oe-rell.qty for loadtag item */
     FOR EACH oe-rell FIELDS(qty) NO-LOCK WHERE
         oe-rell.company = cocode AND
         oe-rell.r-no  = oe-relh.r-no AND
         oe-rell.i-no = loadtag.i-no
         USE-INDEX r-no:
         v-i-rel-qty = v-i-rel-qty + oe-rell.qty.
     END.

     IF adm-new-record THEN
        FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                    AND oe-rell.r-no      EQ oe-relh.r-no
                    AND oe-rell.i-no      EQ loadtag.i-no 
                    AND (LOOKUP(STRING(oe-rell.ord-no),v-ord-no-list) <= 0 OR
                         lookup(oe-rell.tag,v-tag-no-list) <= 0 OR
                         v-i-rel-qty > v-i-qty)
                    USE-INDEX r-no NO-LOCK NO-ERROR.
     ELSE FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                    AND oe-rell.r-no      EQ oe-relh.r-no
                    AND oe-rell.i-no      EQ loadtag.i-no 
                    AND (LOOKUP(STRING(oe-rell.ord-no),v-ord-no-list) > 0 OR
                         lookup(oe-rell.tag,v-tag-no-list) > 0)
                    USE-INDEX r-no NO-LOCK NO-ERROR.
      /* New logic per Joe, task 11061303 */
      /* Check for ambiguity */
      liCnt = 0.
      FOR EACH bf-oe-rell WHERE bf-oe-rell.company  EQ cocode
                           AND bf-oe-rell.r-no      EQ oe-relh.r-no
                           AND bf-oe-rell.i-no      EQ loadtag.i-no
                           USE-INDEX r-no NO-LOCK.
          liCnt = liCnt + 1.
      END.

      IF liCnt GT 1 THEN DO:
          FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                               AND oe-rell.r-no      EQ oe-relh.r-no
                               AND oe-rell.i-no      EQ loadtag.i-no                           
                               USE-INDEX r-no NO-LOCK NO-ERROR.
          
          /* To resolve ambiguity, prompt user for order# to use */
          RUN choose-order (INPUT ROWID(oe-rell),
                            INPUT ROWID(loadtag),
                            INPUT "Yes-No-Cancel",
                            OUTPUT liOrderChosen,
                            OUTPUT lcUserButton).

          IF lcUserButton = "CANCEL" THEN
              lCancel = TRUE.
          IF lcUserButton = "YES" THEN
              lChoice = YES.
          IF lcUserButton = "NO" THEN
              lChoice = NO.
               
          IF lCancel THEN DO:
            oplCancel = TRUE.
            RETURN NO-APPLY.
          END.
          IF lChoice = NO THEN DO:
              oplReturnNoApply = TRUE.
              RETURN NO-APPLY.
          END.
          
          IF liOrderChosen GT 0 THEN
              FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                                   AND oe-rell.r-no      EQ oe-relh.r-no
                                   AND oe-rell.i-no      EQ loadtag.i-no                           
                                   AND oe-rell.ord-no   EQ liOrderChosen
                                   USE-INDEX r-no NO-LOCK NO-ERROR.

      END. /* More than one order on this release */
  END.

  IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
  ELSE DO:
/*       IF v-i-rel-qty >= v-i-qty THEN DO: */
         FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ cocode
                    AND oe-rell.r-no      EQ oe-relh.r-no
                    AND oe-rell.i-no      EQ loadtag.i-no
                    USE-INDEX r-no NO-ERROR.
         IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.     
/*       END.                                                                        */
/*       ELSE DO:                                                                    */
/*          MESSAGE "Loadtag has no order number and FG Item not on any order." SKIP */
/*               "FG Item must be added to order entry line with Sell Price"         */
/*               VIEW-AS ALERT-BOX ERROR.                                            */
/*          oplReturnNoApply = YES.                                                  */
/*          RETURN NO-APPLY.                                                         */
/*       END.                                                                        */
  END.
END. /* Loadtag order number is zero */
ELSE DO:
  /* If the loadtag order # is not zero, match it with oe-rell */
  FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                         AND oe-rell.r-no      EQ oe-relh.r-no
                         AND oe-rell.ord-no    EQ loadtag.ord-no
                         AND oe-rell.i-no EQ  loadtag.i-no
                       USE-INDEX r-no NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rell AND relmerge-int EQ 1 THEN
    FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                         AND oe-rell.r-no      EQ oe-relh.r-no
                         /* AND oe-rell.ord-no    EQ loadtag.ord-no      */
                         AND oe-rell.i-no EQ  loadtag.i-no
                       USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL oe-rell THEN DO: 
      /* There may be a mismatch between the order number on the release */
      /* compared to the loadtag, so ask the user which to use           */

      ASSIGN
       v-msg1 =   "Tag# Scanned is for Order# " + string(loadtag.ord-no) + " but"
       v-msg1-1 = "Release# for Release Ticket is for Order# " + STRING(oe-rell.ord-no)
       v-msg2 =   "Press CANCEL to process another release."
       v-msg3 =   "Press NO to scan another Tag for this Release."
       v-msg4 =   "Press YES to add tag to BOL but Retain Original Order# on Release".
  
      IF loadtag.ord-no NE oe-rell.ord-no THEN DO:

            FOR EACH bf-oe-rell WHERE bf-oe-rell.r-no EQ oe-rell.r-no
                AND bf-oe-rell.i-no EQ oe-rell.i-no
                NO-LOCK
                BREAK BY bf-oe-rell.ord-no.
                IF LAST-OF(bf-oe-rell.ord-no) THEN
                cOrderList = cOrderList + STRING(bf-oe-rell.ord-no) + "#".
            END. /* Each bf-oe-rell */
            
            cOrderList = TRIM(cOrderList, "#").

/*         MESSAGE v-msg1 SKIP                                        */
/*                 v-msg1-1 SKIP                                      */
/*                 "" SKIP                                            */
/*                 v-msg2 SKIP                                        */
/*                 v-msg3 SKIP                                        */
/*                 v-msg4 SKIP                                        */
/*         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE ll. */
        IF NUM-ENTRIES(cOrderList, "#") = 1 THEN DO:
            /* Use message specifying the one order */
              v-ord-no = oe-rell.ord-no.
            ip-parms = 
               "type=fill-in,name=fi1,row=2,col=22,enable=false,width=58,scrval=" + v-msg1 + ",FORMAT=X(58)"
                + "|type=fill-in,name=fi5,row=2.9,col=22,enable=false,width=58,scrval=" + v-msg1-1 + ",FORMAT=X(58)"
                + "|type=fill-in,name=fi6,row=10,col=22,enable=false,width=58,scrval=" + v-msg2 + ",FORMAT=X(58)"
                + "|type=fill-in,name=fi7,row=10.8,col=22,enable=false,width=58,scrval=" + v-msg3 + ",FORMAT=X(58)"
                + "|type=fill-in,name=fi8,row=11.6,col=22,enable=false,width=62,scrval=" + v-msg4 + ",FORMAT=X(62)"           
                + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
                + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=17".
        END.
        ELSE DO:
          ASSIGN v-msg1 =   "Tag# Scanned is for Order# " + string(loadtag.ord-no) + "."
                 v-msg1-1 = "Please Select Order# to Use:".
          ip-parms = 
             "type=fill-in,name=fi1,row=2,col=22,enable=false,width=58,scrval=" + v-msg1 + ",FORMAT=X(58)"
              + "|type=fill-in,name=fi5,row=2.9,col=22,enable=false,width=58,scrval=" + v-msg1-1 + ",FORMAT=X(58)"
              + "|type=fill-in,name=fi6,row=10,col=22,enable=false,width=58,scrval=" + v-msg2 + ",FORMAT=X(58)"
              + "|type=fill-in,name=fi7,row=10.8,col=22,enable=false,width=58,scrval=" + v-msg3 + ",FORMAT=X(58)"
              + "|type=fill-in,name=fi8,row=11.6,col=22,enable=false,width=62,scrval=" + v-msg4 + ",FORMAT=X(62)"
              + "|type=selection-list,name=tg2,row=4.5,col=25,enable=true,height=150,font=15,width=20,list=" + cOrderList
              + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
              + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=17".
          /* Load special ini file for larger font #15 */
         
          cDir = SEARCH("custom/l-font.ini").
          cDir = SUBSTRING(cDir, 1, INDEX(cDir, "custom") + 5).
          LOAD "l-font.ini" DIR cDir BASE-KEY "INI".
          USE "l-font.ini".

        END.

        IF NOT lsecurity-flag THEN RUN sys/ref/d-passwd.w (9, OUTPUT lsecurity-flag).
        
        IF lsecurity-flag THEN
        RUN custom/d-prompt.w (INPUT "yes-no-cancel", ip-parms, "", OUTPUT op-values). /* New Logic */
        ELSE  op-values =  "DEFAULT" + "," + "No" .
        
        /* Load original ini for original font set */
        UNLOAD "l-font.ini" NO-ERROR.

        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "default" THEN
              choice = ENTRY(i + 1, op-values) NO-ERROR.
            IF ENTRY(i, op-values) EQ "tg2" THEN
              lcNewOrder = ENTRY(i + 1, op-values) NO-ERROR.            
        END. /* Do i = 1... */

        IF choice = "CANCEL" THEN
            lCancel = TRUE.
        IF choice = "YES" THEN
            lChoice = YES.
        IF choice = "NO" THEN
            lChoice = NO.
               
        IF lCancel THEN DO:
            oplCancel = TRUE.
            RETURN NO-APPLY.
        END.
            
        IF lChoice = YES THEN DO: 
           
            IF NUM-ENTRIES(cOrderList, "#") = 1 THEN
              v-ord-no = oe-rell.ord-no.
            ELSE
              v-ord-no = INTEGER(lcNewOrder) NO-ERROR.

        END. /* If user chose 'yes' to change order number */
        ELSE DO:
            /* User chose 'NO' */
            oplReturnNoApply = TRUE.
            RETURN NO-APPLY.

        END. /* Else DO:*/

      END. /* If order numbers match */
      ELSE     
        v-ord-no = oe-rell.ord-no.
  END. /* If avail oe-rell */
  ELSE DO:
   {addon/bol/bolstock.i}
   IF v-ord-no = 0 THEN DO:
     FIND FIRST oe-ord WHERE oe-ord.company = cocode
                         AND oe-ord.ord-no = loadtag.ord-no
                         AND oe-ord.OPENed = YES NO-LOCK NO-ERROR.
     IF AVAIL oe-ord THEN DO:
        IF oe-ord.cust-no = oe-relh.cust-no THEN DO: /* have ord# from release*/
           FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                                AND oe-rell.r-no      EQ oe-relh.r-no
                                AND oe-rell.tag EQ  loadtag.tag-no
                              USE-INDEX r-no NO-LOCK NO-ERROR.
           IF NOT AVAIL oe-rell THEN DO:
              {addon/bol/loadcust.i}
              IF AVAIL cust AND cust.active = "X" THEN  DO: 
                 FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ cocode
                          AND oe-rell.r-no      EQ oe-relh.r-no
                          AND oe-rell.i-no      EQ loadtag.i-no                           
                          USE-INDEX r-no NO-ERROR.
              END.
           END.
           IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
           ELSE v-ord-no = loadtag.ord-no.
        END.
        ELSE DO:
          {addon/bol/loadcust.i}
          IF AVAIL cust AND cust.active = "X" THEN  DO: /* same as stock box ord-no = 0*/
             FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                           AND oe-rell.r-no      EQ oe-relh.r-no
                           AND oe-rell.tag      EQ loadtag.tag-no                           
                           USE-INDEX r-no NO-LOCK NO-ERROR.
             IF NOT AVAIL oe-rell THEN /* only one line for the item*/
                FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                          AND oe-rell.r-no      EQ oe-relh.r-no
                          AND oe-rell.i-no      EQ loadtag.i-no                           
                          USE-INDEX r-no NO-LOCK NO-ERROR.
             IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
          END.
        END.
     END. /* avail oe-ord */
     ELSE DO: /* loadtag.ord-no is invalid */
         FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                              AND oe-rell.r-no      EQ oe-relh.r-no
                              AND oe-rell.tag EQ  loadtag.tag-no
                              USE-INDEX r-no NO-LOCK NO-ERROR.
         IF NOT AVAIL oe-rell THEN
            FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                                 AND oe-rell.r-no      EQ oe-relh.r-no
                                 AND oe-rell.i-no EQ  loadtag.i-no
                                 USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL oe-rell THEN  v-ord-no = oe-rell.ord-no.
         ELSE DO:
            MESSAGE "Order " loadtag.ord-no " is Closed. Reopen it and try again."
                VIEW-AS ALERT-BOX ERROR.
            oplReturnNoApply = YES.
            RETURN NO-APPLY.                      
         END.
     END.
   END.  /* v-ord-no = 0*/
  END.  /* not avail oe-rell*/
END. /* loadtag.ord-no <> 0 */   

IF AVAIL oe-rell   THEN oprOeRell = ROWID(oe-rell).
IF AVAIL cust      THEN oprCust   = ROWID(cust).
IF AVAIL oe-ord    THEN oprOeOrd  = ROWID(oe-ord).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR fil_id AS RECID NO-UNDO.
  DEF VAR nufile AS LOG NO-UNDO.
  DEF BUFFER b-rell FOR oe-rell.
  

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* FIND FIRST oe-relh WHERE oe-relh.company = cocode
                        AND oe-relh.release# = tt-relbol.release# NO-LOCK NO-ERROR.
   FIND FIRST oe-rell
       WHERE oe-rell.company = oe-relh.company
         AND oe-rell.r-no = oe-relh.r-no
         AND oe-rell.i-no = tt-relbol.i-no
         AND oe-rell.tag = tt-relbol.tag#
       USE-INDEX r-no NO-ERROR.
   IF NOT AVAIL oe-rell THEN
      FIND FIRST oe-rell
          WHERE oe-rell.company = oe-relh.company
            AND oe-rell.r-no = oe-relh.r-no
            AND oe-rell.i-no = tt-relbol.i-no
            /*  AND oe-rell.tag = "" */
          USE-INDEX r-no NO-ERROR.
   IF AVAIL oe-rell THEN DO:
      IF oe-rell.loc <> tt-relbol.loc THEN oe-rell.loc = tt-relbol.loc.
      IF oe-rell.loc-bin <> tt-relbol.loc-bin THEN 
              oe-rell.loc-bin = tt-relbol.loc-bin.
      ASSIGN /* do on print bol
             oe-rell.cases = tt-relbol.cases
             oe-rell.qty-case = tt-relbol.qty-case
             oe-rell.partial  = tt-relbol.partial
             oe-rell.qty = tt-relbol.qty
             */
             oe-rell.tag = tt-relbol.tag
             .
/*
          tt-relbol.qty = loadtag.pallet-count
          tt-relbol.ord-no = oe-rell.ord-no
          tt-relbol.loc = loadtag.loc
          tt-relbol.loc-bin = loadtag.loc-bin 
          tt-relbol.cases  = loadtag.case-bundle
          tt-relbol.qty-case = loadtag.qty-case
          tt-relbol.cases-unit = loadtag.case-bundle
          tt-relbol.partial = loadtag.partial
*/
   END.
   /* do on print bol ===
   ELSE DO:
        FIND FIRST b-rell
            WHERE oe-rell.company = oe-relh.company
              AND oe-rell.r-no = oe-relh.r-no
              AND b-rell.i-no = tt-relbol.i-no
            USE-INDEX r-no NO-LOCK NO-ERROR.
        {oe/oe-rell.a}
        ASSIGN oe-rell.job-no   = b-rell.job-no
               oe-rell.job-no2  = b-rell.job-no2
               oe-rell.loc      = tt-relbol.loc
               oe-rell.loc-bin  = tt-relbol.loc-bin
               oe-rell.cust-no  = tt-relbol.cust-no
               oe-rell.tag      = tt-relbol.tag
               oe-rell.i-no     = tt-relbol.i-no
               oe-rell.line     = b-rell.line
               oe-rell.s-code   = b-rell.s-code
               oe-rell.po-no    = b-rell.po-no
               oe-rell.ord-no   = b-rell.ord-no
               oe-rell.deleted  = no
               oe-rell.posted   = no
               oe-rell.printed  = no
               oe-rell.qty-case = /*if tt-relbol.case-count gt 0 then fg-bin.case-count
                                  else itemfg.case-count*/ tt-relbol.qty-case
               oe-rell.qty      = /*min(v-qty,fg-bin.qty) */ tt-relbol.qty
               oe-rell.cases    = /*trunc(oe-rell.qty / oe-rell.qty-case,0)*/ tt-relbol.cases
               oe-rell.partial  = /*oe-rell.qty modulo oe-rell.qty-case*/ tt-relbol.partial
               /*v-qty         = v-qty - min(v-qty,fg-bin.qty)*/.

   END.
   ===========*/
   v-release# = tt-relbol.release#.*/

  /* gdm - 10160906*/
  IF TRIM(ssbolscan-cha) NE "" THEN
  DO:
     FIND FIRST oe-relh EXCLUSIVE-LOCK WHERE
          oe-relh.company  EQ cocode AND
          oe-relh.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-ERROR NO-WAIT.
     IF AVAIL oe-relh THEN
     DO:
        ASSIGN oe-relh.trailer = tt-relbol.trailer#.
        FIND CURRENT oe-relh NO-LOCK.
        RELEASE oe-relh.
     END.
  END.
   /* gdm - 10160906 end */
   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-scan-next = NO.

  RUN dispatch ('open-query').
  IF v-prev-rowid <> ? THEN
     REPOSITION {&browse-name} TO ROWID v-prev-rowid NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH bf-tmp BY bf-tmp.seq DESC:
    li = bf-tmp.seq.
    LEAVE.
  END.

  ASSIGN
   tt-relbol.release# = v-release#
   tt-relbol.seq      = li + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   /*{custom/askdel.i}  */

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN display-qtys.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF is-bol-printed THEN DO:
     MESSAGE "BOL is already printed!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-rel# NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN validate-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.
  
  RUN validate-item NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lSaveToTempFile THEN DO:
      OUTPUT STREAM sTmpSaveInfo TO VALUE(cTmpSaveFile) APPEND.
      EXPORT STREAM sTmpSaveInfo tt-relbol EXCEPT oerell-row.
      OUTPUT STREAM sTmpSaveInfo CLOSE.   
  END. 
  
  v-prev-rowid = ROWID(tt-relbol).

  RUN display-qtys.

  RUN need-scroll.
  
  IF ssbol-int EQ 0 OR v-scan-qty LT v-rel-qty THEN RUN scan-next.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE need-scroll B-table-Win 
PROCEDURE need-scroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-cnt AS INT NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

FOR EACH bf-tmp NO-LOCK WHERE bf-tmp.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name}):
    lv-cnt = lv-cnt + 1.             
END.

IF lv-cnt >= 8 THEN DO:
   IF v-ran-need-scroll THEN RETURN.
   lv-rowid = ROWID(tt-relbol).
   RUN dispatch ('open-query').
   REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
   /*BROWSE {&browse-name}:SCROLLBAR-VERTICAL = YES.*/
   v-ran-need-scroll = YES.
END.
ELSE v-ran-need-scroll = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ordStatCheck B-table-Win 
PROCEDURE ordStatCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplOnHold AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lIsOnHold AS LOGICAL     NO-UNDO.

lIsOnHold = FALSE.
IF gvlCheckOrdStat THEN DO:
  FOR FIRST oe-relh NO-LOCK WHERE oe-relh.company = cocode AND
                         oe-relh.release# = INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
                     AND oe-relh.printed AND NOT oe-relh.posted ,
      FIRST oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no NO-LOCK BREAK BY oe-rell.i-no BY oe-rell.LINE:
    FIND FIRST oe-ord 
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ oe-rell.ord-no
      NO-LOCK NO-ERROR.
   
    IF AVAIL oe-ord AND INDEX(oe-ord.stat, "H") GT 0 THEN
      lIsOnHold = TRUE.
  END.
END.
 
IF lIsOnHold THEN
  MESSAGE "Sorry Order/Release is on Hold, Bill of Lading cannot be created!" VIEW-AS ALERT-BOX
   WARNING .
oplOnHold = lIsOnHold.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-release B-table-Win 
PROCEDURE post-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER bf-rell FOR oe-rell.

DEF VAR v-ship-lu AS ch INIT ["I,S,B"].
DEF VAR v-ship-no AS INT.
DEF VAR save_id AS RECID.
DEF VAR time_stamp AS ch.
DEF VAR v-s-code AS CHAR.
DEF VAR v-first-release AS LOG.
DEF VAR v-r-no LIKE inv-head.r-no.
DEF VAR v-ext-price LIKE inv-line.t-price.
DEF VAR v-nxt-r-no AS INT.
DEF VAR v-po-no LIKE oe-rel.po-no.
DEF VAR v-n-bol LIKE oe-ctrl.n-bol.
DEF VAR v-bol-qty LIKE oe-boll.qty.
DEF VAR temp-tax AS DEC INIT 0 NO-UNDO.
DEF VAR v-relpost-hld LIKE relpost-chr NO-UNDO.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR ll-exception AS LOG NO-UNDO.

BolPostLog = SEARCH('logs/bolpstall.log') NE ?.
IF BolPostLog THEN
OUTPUT STREAM logFile TO VALUE('logs/bolpstall.' +
     STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').
IF BolPostLog THEN RUN BolPostLog ('Started ' + USERID("NOSWEAT")).
IF BolPostLog AND avail(tt-relbol) THEN RUN BolPostLog ('Current Release ' + STRING(tt-relbol.release#)).
{sa/sa-sls01.i}

DO TRANSACTION:
  {sys/inc/boldate.i}
END.

FOR EACH tt-except:
  DELETE tt-except.
END.
 
FOR EACH tt-fg-bin:
  DELETE tt-fg-bin.
END.

ASSIGN
 v-relpost-hld = relpost-chr
 lv-bol-no     = 0.


IF BolPostLog THEN RUN BolPostLog ('Each oe-relh oe-relp2.i').
headblok:
FOR EACH oe-relh NO-LOCK
    WHERE oe-relh.company EQ cocode
      AND oe-relh.posted  EQ NO
      AND oe-relh.printed EQ YES
      AND CAN-FIND(FIRST tt-rell WHERE tt-rell.release# EQ oe-relh.release#)
    USE-INDEX post
  {oe/oe-relp2.i}
  FOR EACH oe-rell WHERE oe-rell.company EQ oe-relh.company
                     AND oe-rell.r-no    EQ oe-relh.r-no
                       USE-INDEX r-no
                   NO-LOCK,
    EACH oe-ordl WHERE oe-ordl.company EQ oe-relh.company
                   AND oe-ordl.ord-no EQ oe-rell.ord-no
      EXCLUSIVE-LOCK:

      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                     OUTPUT oe-ordl.inv-qty,
                     OUTPUT oe-ordl.ship-qty).
  END.
END.

EMPTY TEMP-TABLE tt-boll.

FOR EACH report WHERE report.term-id EQ v-term NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id:
  IF BolPostLog THEN RUN BolPostLog ('Delete BOL' + STRING(oe-boll.bol-no)).
  CREATE tt-boll.
  BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
  DELETE oe-boll.
END.

/*ysk nothing printed need delete report for old oe-boll and 
         create new one from update-bol  */
FOR EACH report WHERE report.term-id EQ v-term:
  IF NOT CAN-FIND(FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id)
  THEN DELETE report.
END.

IF BolPostLog THEN RUN BolPostLog ('Run Update Bol').
RUN update-bol (v-term).

FOR EACH report WHERE report.term-id EQ v-term:
    CREATE tt-report-a.
    BUFFER-COPY report TO tt-report-a.
END.

EMPTY TEMP-TABLE tt-report.

/* Save BOL to be posted in temp-table */
FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
    FIRST oe-bolh NO-LOCK
    WHERE oe-bolh.b-no    EQ oe-boll.b-no
      AND oe-bolh.printed EQ YES:
  CREATE tt-report.
  BUFFER-COPY report TO tt-report.
  DELETE report.
END.
IF BolPostLog THEN RUN BolPostLog ('Start Printing').
FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
    FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no
    BREAK BY oe-bolh.bol-no:

  IF FIRST-OF(oe-bolh.bol-no) AND ssbolprint-log NE ? THEN DO:

    IF ssbolprint-log EQ YES THEN
    DO:
       RUN custom/setUserPrint.p (oe-bolh.company,'oe-boll_.',
                                  'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert',
                                  oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +
                                  STRING(oe-boll.bol-no) + ',' + STRING(oe-boll.bol-no) +
                                  ',,99999999,' + STRING(oe-bolh.printed) + ',' +
                                  STRING(oe-bolh.posted) + ',BOL').
      
       RUN listobjs/oe-boll_.w.
    END.
    ELSE
    DO:
       SESSION:SET-WAIT-STATE ("general").

       RUN bol/printBol.p(INPUT oe-bolh.company,
                          INPUT locode,
                          INPUT oe-bolh.cust-no,
                          INPUT oe-bolh.bol-no,
                          INPUT oe-bolh.printed,
                          INPUT oe-bolh.posted,
                          INPUT IF ssbolprint-int = 1 THEN YES ELSE NO).

       SESSION:SET-WAIT-STATE ("").
    END.
  END.
END.

FOR EACH report WHERE report.term-id EQ v-term:   
  DELETE report.
END.

/* Recreate BOL to be posted from temp-table */
FOR EACH tt-report:
  CREATE report.
  BUFFER-COPY tt-report TO report.
  DELETE tt-report.
END.
IF BolPostLog THEN RUN BolPostLog ('Run oe-bolp3').
RUN oe/oe-bolp3.p (v-term).    

IF BolPostLog THEN RUN BolPostLog ('Run upd-actual-rel').
RUN upd-actual-rel (v-term).
EMPTY TEMP-TABLE tt-report-a.

IF BolPostLog THEN RUN BolPostLog ('Delete Releases').
delete-blok:
FOR EACH oe-relh
    WHERE oe-relh.company EQ cocode
      AND oe-relh.deleted EQ YES
    USE-INDEX deleted:
  FOR EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no:       
    DELETE oe-rell.
  END. /* each oe-rell */
  DELETE oe-relh.
END. /* each oe-relh */

RELEASE oe-boll.
  IF BolPostLog THEN OUTPUT STREAM logFile CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-bol B-table-Win 
PROCEDURE print-bol :
/*------------------------------------------------------------------------------
  Purpose:   Post release and print bol.  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR fil_id AS RECID NO-UNDO.
  DEF VAR nufile AS LOG NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li AS LOG INIT YES NO-UNDO.
  DEF VAR v-msgreturn AS INT NO-UNDO.
  DEF VAR v-create-backorder AS LOG NO-UNDO.
  DEFINE VARIABLE v-scan-qty-c AS INTEGER  INITIAL 0 NO-UNDO.
  DEFINE VARIABLE v-rel-qty-c AS INTEGER  INITIAL 0 NO-UNDO .

  RUN validate-scan(OUTPUT v-create-backorder) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN RETURN .

  /*IF v-scan-qty LT v-rel-qty THEN
  DO:
     IF NOT g-sharpshooter THEN
        MESSAGE "The scanned qty is less than the release qty, Do you want to proceed?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
      ELSE DO:
        RUN custom/d-msg.w ("Question","Scanned Qty is less than Release Qty.","Continue?","",2,"Yes,No",OUTPUT v-msgreturn).
        IF v-msgreturn = 1 THEN ll = YES.
      END.

      IF NOT ll THEN RETURN.
  END.*/

   /* Ticket 13130 */
  FOR EACH tt-relbol NO-LOCK BREAK BY tt-relbol.release# 
                                   BY tt-relbol.i-no:
      IF FIRST-OF(tt-relbol.i-no) THEN DO:
          ASSIGN 
              v-scan-qty-c = 0
              v-rel-qty-c  = 0
              li           = YES .

        FOR EACH bf-tmp NO-LOCK
          WHERE bf-tmp.release# EQ tt-relbol.release#
            AND bf-tmp.i-no     EQ tt-relbol.i-no:
            v-scan-qty-c = v-scan-qty-c + bf-tmp.qty.
        END.

        FOR EACH oe-relh NO-LOCK WHERE oe-relh.company = cocode AND
                           oe-relh.release# = INT(tt-relbol.release#)
                       AND oe-relh.printed AND NOT oe-relh.posted ,
            EACH oe-rell
            WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no NO-LOCK BREAK BY oe-rell.i-no BY oe-rell.LINE:

            v-rel-qty-c = v-rel-qty-c + oe-rell.qty.
        END.
        IF v-rel-qty-c LT v-scan-qty-c AND (ssbolprint-char = "OverShipWarning" OR ssbolprint-char =  "OverUnderShipWarning") THEN
            MESSAGE "Release Qty for item# " + STRING(tt-relbol.i-no) + " :  " + string(v-rel-qty-c) + "   " +
            "Scanned Qty for item# " + STRING(tt-relbol.i-no) + " :  " + STRING(v-scan-qty-c) + "   " +
            "Continue with BOL Creation?" 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE li.

        ELSE IF v-rel-qty-c GT v-scan-qty-c AND (ssbolprint-char = "UnderShipWarning" OR ssbolprint-char =  "OverUnderShipWarning") THEN
            MESSAGE "Release Qty for item# " + STRING(tt-relbol.i-no) + " :  " + string(v-rel-qty-c) + "   " +
            "Scanned Qty for item# " + STRING(tt-relbol.i-no) + " :  " + STRING(v-scan-qty-c) + "   " +
            "Continue with BOL Creation?" 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE li.
        
        IF NOT li THEN RETURN.

      END.


  END.

  SESSION:SET-WAIT-STATE("general").

  is-bol-printed = YES.

  EMPTY TEMP-TABLE tt-rell.

  RUN create-temp-rel(INPUT v-create-backorder).

  RUN post-release.

  EMPTY TEMP-TABLE tt-relbol.

  RUN dispatch ('open-query').
 
  SESSION:SET-WAIT-STATE(""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prompt-for-order B-table-Win 
PROCEDURE prompt-for-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER opiOrder1 AS INT NO-UNDO.
DEF INPUT PARAMETER opiOrder2 AS INT NO-UNDO.
DEF OUTPUT PARAMETER oplCancel AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opiChoice AS INT NO-UNDO.


DEFINE VARIABLE lcUserPrompt1 AS CHARACTER INIT "".
DEFINE VARIABLE lcUserPrompt2 AS CHARACTER INIT "".
DEFINE VARIABLE lcUserPrompt3 AS CHARACTER INIT "".
DEFINE VARIABLE lcUserPrompt4 AS CHARACTER INIT "".

DEFINE VARIABLE llFirstOrder AS LOG NO-UNDO.
DEFINE VARIABLE llSecondOrder AS LOG NO-UNDO.

DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE choice       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOrder1      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOrder2      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lvErrMsg     AS CHARACTER   NO-UNDO.

ASSIGN
 lcUserPrompt1 = "Tag# Scanned is for Order# " + string(opiOrder1) + " but"
 lcUserPrompt2 = "Release# for Release Ticket is for Order# " + STRING(opiOrder2)
 lcUserPrompt3 = "Please select Order# to Process or"
 lcUserPrompt4 = "press CANCEL to scan new tag:".




ip-parms = 
   /* Box Title */
   "type=literal,name=fi4,row=2,col=28,enable=false,width=52,scrval=" + lcUserPrompt1 + ",FORMAT=X(52)"
   + "|type=literal,name=fi5,row=2.8,col=28,enable=false,width=52,scrval=" + lcUserPrompt2 + ",FORMAT=X(52)"
   + "|type=literal,name=fi6,row=4.2,col=28,enable=false,width=52,scrval=" + lcUserPrompt3 + ",FORMAT=X(52)"
   + "|type=literal,name=fi7,row=4.9,col=28,enable=false,width=52,scrval=" + lcUserPrompt4 + ",FORMAT=X(52)"
    
    /* Set Attributes */
   /* + "|type=attrib,name=company,row=1,col=1,enable=false,width=2,inpval=" + cocode */


    /* Replace existing releases toggle box */
    + "|type=literal,name=label1,row=6,col=35,enable=false,width=38,scrval=" + "Order # " + string(opiOrder1) + ",FORMAT=X(38)"
    + "|type=toggle,name=tb_order1,row=6,col=32,enable=true,width=2,data-type=logical"

    /* Replace existing releases toggle box */
    + "|type=literal,name=label2,row=7,col=35,enable=false,width=38,scrval=" + "Order # " + STRING(opiOrder2) + ",FORMAT=X(58)"
    + "|type=toggle,name=tb_order2,row=7,col=32,enable=true,width=2,data-type=logical"


    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 

    /* Box Title */
    + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=12".
  
prompt-loop:
DO WHILE TRUE:
                            
    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    
    /* Process values using names given above */
    DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
        IF ENTRY(i, op-values) EQ "default" THEN
          choice = ENTRY(i + 1, op-values) NO-ERROR.

        /* Replace Existing */
        IF ENTRY(i, op-values) EQ "tb_order1" THEN
          lOrder1 = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 

        /* Replace Existing */
        IF ENTRY(i, op-values) EQ "tb_order2" THEN
          lOrder2 = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 

    END.

    opiChoice = IF lOrder1 THEN 1 ELSE 2.

    lvErrMsg = "".
    IF choice NE "CANCEL" THEN DO:
    
        IF lOrder1 AND lOrder2 THEN
          lvErrMsg = "Please select one order or the other and press OK.".

    END.

    IF lvErrMsg GT "" THEN 
      MESSAGE lvErrMsg VIEW-AS ALERT-BOX.
    ELSE
        LEAVE.
END.

ASSIGN
 oplCancel = IF choice EQ "CANCEL" THEN TRUE ELSE FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

   RUN dispatch ('open-query').
   IF ip-rowid <> ? THEN
      REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-relbol B-table-Win 
PROCEDURE save-relbol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN addon/bol/saverelbol.p(INPUT cocode).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scan-next B-table-Win 
PROCEDURE scan-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  lv-scan-next = YES.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN auto-add IN WIDGET-HANDLE(char-hdl).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-email B-table-Win 
PROCEDURE send-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-release# AS INT NO-UNDO.

  RUN addon/bol/bolemail.p(INPUT ip-release#,
                           INPUT cocode,
                           INPUT v-prgmname).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-relbol"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-go-to-qty B-table-Win 
PROCEDURE set-go-to-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   lv-go-to-unit = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sharpsh-print B-table-Win 
PROCEDURE sharpsh-print :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR beg-cust AS CHAR NO-UNDO.
  DEF VAR end-cust AS CHAR NO-UNDO.
  DEF VAR beg-bol AS INT NO-UNDO.
  DEF VAR end-bol AS INT NO-UNDO.
  DEF VAR beg-ord AS INT NO-UNDO.
  DEF VAR end-ord AS INT NO-UNDO.
  DEF VAR v-reprint AS LOG NO-UNDO.

  ASSIGN beg-cust = ""
         end-cust = "zzzzzzzz"
         beg-bol = 0  
         end-bol = 9999999
         beg-ord = 0
         end-ord = 9999999
         v-reprint = NO /* for testing purpose*/
         .
  RUN addon/bol/bolprt.p (beg-cust,end-cust,beg-bol,end-bol,beg-ord,end-ord,v-release#,v-reprint).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-actual-rel B-table-Win 
PROCEDURE upd-actual-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER v-term AS CHAR NO-UNDO.

DEF VAR h_oerel-recalc-act AS HANDLE.
DEF VAR vActQty AS DEC NO-UNDO.

RUN sbo/oerel-recalc-act.p PERSISTENT SET h_oerel-recalc-act.

FOR EACH tt-report-a NO-LOCK WHERE ,

  FIRST oe-boll EXCLUSIVE-LOCK WHERE RECID(oe-boll) EQ tt-report-a.rec-id,       

  FIRST oe-rell NO-LOCK
  WHERE oe-rell.company EQ oe-boll.company
    AND oe-rell.ord-no  EQ oe-boll.ord-no
    AND oe-rell.r-no    EQ oe-boll.r-no
    AND oe-rell.i-no    EQ oe-boll.i-no
    AND oe-rell.line    EQ oe-boll.line,

  EACH oe-rel WHERE oe-rel.r-no = oe-rell.link-no NO-LOCK:

  IF VALID-HANDLE(h_oerel-recalc-act) THEN
      RUN recalc-act-qty IN h_oerel-recalc-act (INPUT ROWID(oe-rel), OUTPUT vActQty).

  IF oe-boll.p-c = YES THEN DO:
    {oe/oe-bolpc.i ALL}
  END.
  ELSE DO:
    RUN oe/oe-bolpc.p (INPUT ROWID(oe-boll), "ALL").
  END.

END.


IF VALID-HANDLE(h_oerel-recalc-act) THEN
    DELETE OBJECT h_oerel-recalc-act.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-bol B-table-Win 
PROCEDURE update-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-term AS cha NO-UNDO.

  DEF VAR lv-qty LIKE oe-boll.qty NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-weight LIKE oe-boll.weight NO-UNDO.
  DEF VAR lv-freight LIKE oe-boll.freight NO-UNDO.
  DEF VAR dTotFreight AS DEC NO-UNDO.

  FOR EACH tt-boll USE-INDEX tt-boll,

      FIRST tt-rell
      WHERE tt-rell.company  EQ tt-boll.company
        AND tt-rell.ord-no   EQ tt-boll.ord-no
        AND tt-rell.line     EQ tt-boll.line
        AND tt-rell.i-no     EQ tt-boll.i-no
        AND tt-rell.po-no    EQ tt-boll.po-no
        AND tt-rell.rel-no   EQ tt-boll.rel-no
        AND tt-rell.b-ord-no EQ tt-boll.b-ord-no
      USE-INDEX ord-no

      BREAK BY tt-boll.b-no
            BY tt-boll.ord-no
            BY tt-boll.i-no
            BY tt-boll.po-no
            BY tt-boll.rel-no
            BY tt-boll.b-ord-no:

    IF FIRST-OF(tt-boll.b-ord-no) THEN lv-qty = 0.

    lv-qty = lv-qty + tt-boll.qty.

    IF LAST-OF(tt-boll.b-ord-no) THEN
      ASSIGN
       tt-boll.release# = tt-rell.release#
       tt-boll.qty      = lv-qty
       tt-boll.job-no   = ""
       tt-boll.job-no2  = 0
       tt-boll.loc      = ""
       tt-boll.loc-bin  = ""
       tt-boll.tag      = "".
  END.

  EMPTY TEMP-TABLE tt-boll2.

  FOR EACH tt-boll USE-INDEX release#
      BY tt-boll.release#
      BY tt-boll.ord-no
      BY tt-boll.i-no
      BY tt-boll.rel-no
      BY tt-boll.b-ord-no
      BY tt-boll.po-no:

    FOR EACH tt-relbol
        WHERE tt-relbol.release# EQ tt-boll.release#
          AND tt-relbol.ord-no   EQ tt-boll.ord-no
          AND tt-relbol.i-no     EQ tt-boll.i-no
          AND tt-relbol.po-no    EQ tt-boll.po-no
        USE-INDEX release#:

      RUN create-tt-boll2.
    END.

    DELETE tt-boll.
  END.

  RELEASE tt-boll2.

  ASSIGN
   lv-weight  = 0
   lv-freight = 0.

  FOR EACH tt-boll2 USE-INDEX b-no BREAK BY tt-boll2.b-no:
    FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ tt-boll2.b-no NO-LOCK.

    CREATE oe-boll.
    BUFFER-COPY tt-boll2 EXCEPT rec_key TO oe-boll.
    RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT oe-boll.tot-pallets).


    /*ysk*/
    CREATE report.
    ASSIGN
     report.term-id  = ip-term
     report.key-01   = oe-boll.i-no
     report.key-02   = STRING(oe-boll.ord-no,"9999999999")
     report.rec-id   = RECID(oe-boll).

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-ERROR.
 
/*     RUN oe/getBolFrt.p (ROWID(oe-boll),         */
/*                        oe-bolh.cust-no,         */
/*                        oe-bolh.ship-id,         */
/*                        oe-bolh.carrier,         */
/*                        OUTPUT oe-boll.freight). */

    RUN oe/calcBolWeight.p (INPUT ROWID(oe-boll), OUTPUT oe-boll.weight).  
    
    ASSIGN
     lv-weight  = lv-weight  + oe-boll.weight
     lv-freight = lv-freight + oe-boll.freight.
     

    
    IF LAST-OF(tt-boll2.b-no) THEN DO:
        
      FIND CURRENT oe-bolh.
      
      RUN oe/calcBolFrt.p (ROWID(oe-bolh), OUTPUT dTotFreight).
      oe-bolh.freight = dTotFreight.
      
      ASSIGN
       oe-bolh.tot-wt  = lv-weight
/*        oe-bolh.freight = lv-freight */
       lv-weight       = 0
       lv-freight      = 0.

      RUN oe/palcalc.p (ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets).

      FIND CURRENT oe-bolh NO-LOCK.
      
    END.
  END.



  FOR EACH tt-boll2
     BREAK BY tt-boll2.ord-no
           BY tt-boll2.i-no:

    IF LAST-OF(tt-boll2.i-no) THEN DO:
      FIND FIRST oe-boll 
        WHERE oe-boll.company EQ tt-boll2.company
          AND oe-boll.ord-no  EQ tt-boll2.ord-no
          AND oe-boll.i-no    EQ tt-boll2.i-no
        EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL oe-boll THEN DO:
          {oe/oe-bolpc.i ALL}
      END.
      RELEASE oe-boll.
    END. /* last of */
  END. /* each tt-boll2 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-item B-table-Win 
PROCEDURE validate-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-iqty AS INT NO-UNDO.
  DEF VAR v-ttqty AS INT NO-UNDO.

  IF tt-relbol.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
     IF NOT g-sharpshooter THEN MESSAGE "Item must be entered. " VIEW-AS ALERT-BOX ERROR. 
     ELSE RUN custom/d-msg.w ("Error","","Item must be entered...","",1,"OK", OUTPUT v-msgreturn).         
     APPLY "entry" TO tt-relbol.tag#.
     RETURN ERROR.
  END.

  FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.release# = v-release# NO-LOCK NO-ERROR.
  /*v-iqty = 0.
  FOR EACH oe-rell
      WHERE oe-rell.company = oe-relh.company
        AND oe-rell.r-no = oe-relh.r-no
        AND oe-rell.i-no = TRIM(tt-relbol.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
      USE-INDEX r-no NO-LOCK:
      v-iqty = v-iqty + oe-rell.qty.
  END.
  v-ttqty = 0.
  FOR each bf-tmp WHERE bf-tmp.release# = int(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND bf-tmp.i-no = tt-relbol.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND RECID(bf-tmp) <> RECID(tt-relbol)
                      NO-LOCK :
      v-ttqty = v-ttqty + bf-tmp.qty.
  END.
  IF v-iqty < v-ttqty + INT(tt-relbol.qty:SCREEN-VALUE IN BROWSE {&browse-name} ) THEN DO:
     IF v-iqty > v-ttqty THEN DO:
        tt-relbol.qty:SCREEN-VALUE IN BROWSE {&browse-name} = string(v-iqty - v-ttqty).
        RETURN.
     END.
     /* not now
     RUN custom/d-msg.w ("Error","","Scanned Qty is more than release Qty...","",1,"OK", OUTPUT v-msgreturn).         
     APPLY "entry" TO tt-relbol.tag# .
     RETURN ERROR.
     */
  END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-rel# B-table-Win 
PROCEDURE validate-rel# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST oe-relh WHERE oe-relh.company = cocode
                                    AND oe-relh.release# = INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name} )
                                    AND oe-relh.printed AND NOT oe-relh.posted )
  THEN DO:
      IF NOT g-sharpshooter THEN
         MESSAGE "Invalid Release#. Not printed? or Already posted?   Try help..." VIEW-AS ALERT-BOX ERROR. 
      ELSE RUN custom/d-msg.w ("Error","","Invalid Release#. Not printed? or Already posted? ","",1,"OK", OUTPUT v-msgreturn).         
      APPLY "entry" TO tt-relbol.release#.
      RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-scan B-table-Win 
PROCEDURE validate-scan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-create-backorder AS LOG NO-UNDO.
    
  DEF BUFFER bf-relbol FOR tt-relbol.
  DEF VAR v-msg AS CHAR NO-UNDO.
  
  EMPTY TEMP-TABLE ttRelQtys.
  FOR EACH bf-tmp BREAK BY bf-tmp.release#:
    IF LAST-OF(bf-tmp.release#) THEN
    FOR EACH oe-relh
        WHERE oe-relh.company  EQ cocode
          AND oe-relh.release# EQ bf-tmp.release#
        NO-LOCK,
        EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK
        BREAK BY oe-rell.ord-no
              BY oe-rell.i-no:
      
      FIND FIRST ttRelQtys 
          WHERE ttRelQtys.release# EQ oe-relh.release#
            AND ttRelQtys.ord-no EQ oe-rell.ord-no
            AND ttRelQtys.i-no EQ oe-rell.i-no
          NO-LOCK NO-ERROR.
      IF NOT AVAIL ttRelQtys THEN DO:
        CREATE ttRelQtys.
        ASSIGN 
            ttRelQtys.release# = oe-relh.release#
            ttRelQtys.ord-no = oe-rell.ord-no
            ttRelQtys.i-no = oe-rell.i-no
            .
      END.
      ttRelQtys.qty-rel = ttRelQtys.qty-rel + oe-rell.qty.
      
      IF LAST-OF(oe-rell.i-no) AND
         NOT CAN-FIND(FIRST bf-relbol
                      WHERE bf-relbol.release# EQ oe-relh.release#
                        AND bf-relbol.ord-no   EQ oe-rell.ord-no
                        AND bf-relbol.i-no     EQ oe-rell.i-no) THEN
      DO:
         ASSIGN
            v-msg = "Release/Order#/FG#: " +
                    TRIM(STRING(oe-relh.release#,">>>>>>>>")) + "/" +
                    TRIM(STRING(oe-rell.ord-no),">>>>>>>>") + "/" +
                    TRIM(oe-rell.i-no).
            ll    = NO.
         IF ssbol-log OR ssbol-log = ? THEN DO: /*mod to suppress prompt*/
        
            IF NOT g-sharpshooter THEN
            DO:
               IF ssbol-log <> ? THEN
                MESSAGE TRIM(v-msg + " was not scanned") + ", do you want to DELETE it from Release?" SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE ll.
               IF ll EQ NO THEN
                  op-create-backorder = YES.
               IF ll NE ? THEN
                  ll = YES.
               ELSE
                  ll = NO.
            END.
            ELSE DO:
               IF ssbol-log <> ? THEN
                    RUN custom/d-msg.w ("Question",TRIM(v-msg), "was not scanned", "Do you want to Delete it from Release? ",3,"Yes,No,Cancel",OUTPUT v-msgreturn).
               ELSE
                   v-msgreturn = 2.
               IF v-msgreturn = 2 THEN
                  op-create-backorder = YES.
               IF v-msgreturn NE 3 THEN
                  ll = YES.
            END.
         END.
         ELSE DO:
            IF NOT g-sharpshooter THEN
               MESSAGE TRIM(v-msg + " was not scanned") + "," SKIP
                       "scan all items for the Release before printing BOL..."
               VIEW-AS ALERT-BOX ERROR.
            ELSE RUN custom/d-msg.w ("Error",TRIM(v-msg),"was not scanned","scan all items for the Release before printing BOL...",1,"OK",OUTPUT v-msgreturn).
         END.
         IF NOT ll THEN RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-tag B-table-Win 
PROCEDURE validate-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-qty-rel LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-qty-tag LIKE oe-rell.qty NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

      
  IF tt-relbol.tag#:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
    /* MESSAGE "Tag# must be entered. " VIEW-AS ALERT-BOX ERROR. */
    APPLY "entry" TO tt-relbol.tag#.
    RETURN ERROR.
  END.

  IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.tag# = tt-relbol.tag#:SCREEN-VALUE
                   AND RECID(bf-tmp) <> RECID(tt-relbol) ) 
  THEN DO:
     RUN custom/d-msg.w ("Error","","Tag# already scanned...","",1,"OK", OUTPUT v-msgreturn).         
     APPLY "entry" TO tt-relbol.tag# .
     RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION comma B-table-Win 
FUNCTION comma RETURNS CHARACTER
  (ipValue AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipValue NE '' THEN ',' ELSE ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-bal B-table-Win 
FUNCTION get-bal RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR li AS INT NO-UNDO.

  IF AVAIL b-oe-ordl AND b-oe-ordl.job-no NE "" THEN
  FOR EACH fg-bin NO-LOCK
      WHERE fg-bin.company EQ cocode
        AND fg-bin.job-no  EQ b-oe-ordl.job-no
        AND fg-bin.job-no2 EQ b-oe-ordl.job-no2
        AND fg-bin.i-no    EQ b-oe-ordl.i-no:
    li = li + fg-bin.qty.
  END.

  RETURN li.    /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

