&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: ce\b-estop.w

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

&SCOPED-DEFINE yellowColumnsName b-estop
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i "new shared"}
ASSIGN
 cocode = g_company
 locode = g_loc.
       
DEFINE NEW SHARED buffer xest FOR est.
DEFINE NEW SHARED buffer xef FOR ef.
DEFINE NEW SHARED buffer xeb FOR eb.

DEFINE BUFFER xop FOR est-op.
DEFINE BUFFER op-lock FOR reftable.

DEFINE NEW SHARED VARIABLE xcal    AS de NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid  AS de NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len  AS de NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-chk-qty AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE v-sht-qty AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE v-rc-seq AS INT INITIAL 9999 NO-UNDO.

{ce/mach-ink.i NEW}

DEFINE VARIABLE ll-import-stds AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-d-seq LIKE est-op.d-seq NO-UNDO.
DEFINE VARIABLE lv-dept LIKE est-op.dept NO-UNDO.
DEFINE VARIABLE lv-op-sb LIKE est-op.op-sb NO-UNDO.
DEFINE VARIABLE lv-b-num LIKE est-op.b-num NO-UNDO.
DEFINE VARIABLE lv-n-out LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE maxco AS INTEGER NO-UNDO.
DEFINE VARIABLE v-passes   AS   INTEGER NO-UNDO.
DEFINE VARIABLE ll-machine-modified AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-import-selected AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-import-all AS LOGICAL NO-UNDO.
DEFINE VARIABLE li-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-eqty LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE v-override-mode AS LOGICAL NO-UNDO.

&SCOPED-DEFINE sortby-phrase BY est-op.line

DO WITH TRANSACTION:
   {sys\inc\ceroute#out.i}
   {sys\inc\estopmch.i}
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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est est-qty
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, est-qty.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-op

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste ~
est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] ~
est-op.op-rate[1] est-op.op-rate[2] est-op.num-col est-op.num-coat ~
est-op.plates est-op.fountains est-op.n_out_div 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste ~
est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] ~
est-op.num-col est-op.num-coat est-op.plates est-op.fountains ~
est-op.n_out_div 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table est-op
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table est-op
&Scoped-define QUERY-STRING-br_table FOR EACH est-op WHERE est-op.company = est-qty.company ~
  AND est-op.est-no = est-qty.est-no ~
  AND est-op.line < 500 ~
      AND ((ASI.est-op.qty eq est-qty.eqty and est.est-type eq 1) or ~
 (ASI.est-op.qty eq lv-eqty and est.est-type ne 1))  ~
   NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH est-op WHERE est-op.company = est-qty.company ~
  AND est-op.est-no = est-qty.est-no ~
  AND est-op.line < 500 ~
      AND ((ASI.est-op.qty eq est-qty.eqty and est.est-type eq 1) or ~
 (ASI.est-op.qty eq lv-eqty and est.est-type ne 1))  ~
   NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table est-op
&Scoped-define FIRST-TABLE-IN-QUERY-br_table est-op


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS fi_sortby 

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
</FOREIGN-KEYS>
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      est-op SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      est-op.s-num COLUMN-LABEL "S" FORMAT ">>>":U LABEL-BGCOLOR 14
      est-op.b-num COLUMN-LABEL "B" FORMAT ">>>":U
      est-op.m-code COLUMN-LABEL "Machine" FORMAT "x(6)":U COLUMN-FONT 0
            LABEL-BGCOLOR 14
      est-op.m-dscr FORMAT "x(20)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      est-op.n-out COLUMN-LABEL "Out" FORMAT ">>>9":U
      est-op.op-mr COLUMN-LABEL "MR-Hrs." FORMAT ">>9.99":U
      est-op.op-waste FORMAT ">>>>>9":U
      est-op.op-speed FORMAT ">>>>9":U
      est-op.op-spoil FORMAT ">>9.99":U
      est-op.op-crew[1] COLUMN-LABEL "MRCrew" FORMAT "9.99":U WIDTH 9.2
      est-op.op-crew[2] COLUMN-LABEL "RunCrew" FORMAT "9.99":U
            WIDTH 9.6
      est-op.op-rate[1] COLUMN-LABEL "MRate" FORMAT ">>9.99":U
            WIDTH 7.6
      est-op.op-rate[2] COLUMN-LABEL "RRate" FORMAT ">>9.99":U
      est-op.num-col COLUMN-LABEL "Inks" FORMAT ">>>":U WIDTH 9
      est-op.num-coat COLUMN-LABEL "Varnish" FORMAT ">>>":U WIDTH 12
      est-op.plates FORMAT ">>>":U
      est-op.fountains FORMAT ">>>":U
      est-op.n_out_div COLUMN-LABEL "Run Qty Divisor" FORMAT "->>,>>9.99":U
  ENABLE
      est-op.s-num
      est-op.b-num
      est-op.m-code
      est-op.m-dscr
      est-op.n-out
      est-op.op-mr
      est-op.op-waste
      est-op.op-speed
      est-op.op-spoil
      est-op.op-crew[1]
      est-op.op-crew[2]
      est-op.num-col
      est-op.num-coat
      est-op.plates
      est-op.fountains
      est-op.n_out_div HELP "Enter Divisor for Out Reduction"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 128 BY 7.14
         FONT 0
         TITLE "Operations".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     fi_sortby AT ROW 8.14 COL 15 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.est,asi.est-qty
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
         HEIGHT             = 8.43
         WIDTH              = 129.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.est-op WHERE ASI.est-qty <external> ... ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.est-op.company = ASI.est-qty.company
  AND ASI.est-op.est-no = ASI.est-qty.est-no
  AND ASI.est-op.line < 500"
     _Where[1]         = "((ASI.est-op.qty eq est-qty.eqty and est.est-type eq 1) or
 (ASI.est-op.qty eq lv-eqty and est.est-type ne 1)) 
  "
     _FldNameList[1]   > ASI.est-op.s-num
"est-op.s-num" "S" ">>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.est-op.b-num
"est-op.b-num" "B" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.est-op.m-code
"est-op.m-code" "Machine" ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.est-op.m-dscr
"est-op.m-dscr" ? ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.est-op.n-out
"est-op.n-out" "Out" ">>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.est-op.op-mr
"est-op.op-mr" "MR-Hrs." ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.est-op.op-waste
"est-op.op-waste" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.est-op.op-speed
"est-op.op-speed" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.est-op.op-spoil
"est-op.op-spoil" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.est-op.op-crew[1]
"est-op.op-crew[1]" "MRCrew" ? "decimal" ? ? ? ? ? ? yes ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.est-op.op-crew[2]
"est-op.op-crew[2]" "RunCrew" ? "decimal" ? ? ? ? ? ? yes ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.est-op.op-rate[1]
"est-op.op-rate[1]" "MRate" ? "decimal" ? ? ? ? ? ? no ? no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.est-op.op-rate[2]
"est-op.op-rate[2]" "RRate" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.est-op.num-col
"est-op.num-col" "Inks" ">>>" "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.est-op.num-coat
"est-op.num-coat" "Varnish" ">>>" "integer" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.est-op.plates
"est-op.plates" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.est-op.fountains
"est-op.fountains" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.est-op.n_out_div
"est-op.n_out_div" "Run Qty Divisor" ? "decimal" ? ? ? ? ? ? yes "Enter Divisor for Out Reduction" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
    DO:
        DEFINE VARIABLE phandle  AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE char-hdl AS cha           NO-UNDO.   

        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
            THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Operations */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Operations */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i}*/
   {est/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Operations */
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Operations */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "est-qty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "est-qty"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-route B-table-Win 
PROCEDURE build-route :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ll AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-msg1 AS CHARACTER NO-UNDO.

  DEF BUFFER b-est-qty FOR est-qty.


  {est/checkuse.i}
        
  v-override-mode = NO.

  IF CAN-FIND(FIRST est-op
              WHERE est-op.company EQ est-qty.company
                AND est-op.est-no  EQ est-qty.est-no
                AND (est-op.qty    EQ est-qty.eqty OR est.est-type GT 1))
                THEN DO:
                
    lv-msg = IF CAN-FIND(FIRST est-op
                         WHERE est-op.company EQ est-qty.company
                           AND est-op.est-no  EQ est-qty.est-no
                           AND (est-op.qty    EQ est-qty.eqty OR est.est-type GT 1)
                           AND est-op.auto    EQ YES) THEN
               "(NOTE: This will DELETE any machines NOT manually added)"
             ELSE
               "(NOTE: This will NOT DELETE any machines manually added)".
             
    IF INDEX(PROGRAM-NAME(2),"run-goto") EQ 0 THEN
       lv-msg1 = "Are you sure you want to build a new routing?".
    ELSE
       lv-msg1 = "Do you want to build a new routing?". 

    MESSAGE lv-msg1 SKIP
            lv-msg
            VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE ll-ans AS LOGICAL.
    IF NOT ll-ans THEN RETURN NO-APPLY.
  END.

  SESSION:SET-WAIT-STATE("general").
  
  FIND xest WHERE RECID(xest) EQ RECID(est).

  IF xest.est-type EQ 1 THEN DO:
    ll = NO.

    FOR EACH b-est-qty NO-LOCK 
        WHERE b-est-qty.company EQ est-qty.company
          AND b-est-qty.est-no  EQ est-qty.est-no
        BREAK BY b-est-qty.eqty:

      IF FIRST(b-est-qty.eqty)    AND
         NOT LAST(b-est-qty.eqty) THEN
        MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

      IF ll OR ROWID(est-qty) EQ ROWID(b-est-qty) THEN DO:  
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ b-est-qty.company
              AND ef.est-no  EQ b-est-qty.est-no:
      
          RUN set-lock (ef.form-no, NO).
        END.    
    
        FIND FIRST xef NO-LOCK  WHERE xef.company EQ b-est-qty.company 
                         AND xef.est-no EQ b-est-qty.est-no
                       NO-ERROR.
        FIND FIRST xeb  NO-LOCK WHERE xeb.company EQ b-est-qty.company 
                         AND xeb.est-no EQ b-est-qty.est-no
                         AND xeb.form-no EQ xef.form-no
                       NO-ERROR.

        RUN ce/mach-seq.p (b-est-qty.eqty).
      END.
    END.
  END.

  ELSE DO:  
    FOR EACH ef NO-LOCK 
        WHERE ef.company EQ est-qty.company
          AND ef.est-no  EQ est-qty.est-no:
      
      RUN set-lock (ef.form-no, NO).
    END.    
    
    FIND FIRST xef NO-LOCK  WHERE xef.company EQ est-qty.company 
                     AND xef.est-no EQ est-qty.est-no
                   NO-ERROR.
    FIND FIRST xeb NO-LOCK  WHERE xeb.company EQ est-qty.company 
                     AND xeb.est-no EQ est-qty.est-no
                     AND xeb.form-no EQ xef.form-no
                   NO-ERROR.

    IF xest.est-type EQ 2 THEN RUN ce/box/mach-seq.p.

    ELSE
    IF xest.est-type EQ 3 THEN RUN ce/tan/mach-seq.p. 

    ELSE                       RUN ce/com/mach-seq.p (0).
  END.

  RUN release-shared-buffers.
  
  RUN dispatch ('open-query').

  SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-of-mach B-table-Win 
PROCEDURE first-of-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ip-m-code LIKE est-op.m-code NO-UNDO.
  DEFINE OUTPUT PARAMETER op-first  AS   LOGICAL       NO-UNDO.
  
  DEFINE BUFFER b-est-op FOR est-op.
  DEFINE BUFFER b-mach FOR mach.
  DEFINE BUFFER b2-mach FOR mach.

  DEFINE VARIABLE v-mach-found AS LOGICAL NO-UNDO.

  IF NOT AVAILABLE est-op THEN
     LEAVE.

  FOR EACH b-est-op FIELDS(m-code) NO-LOCK WHERE
      b-est-op.company EQ est-op.company AND
      b-est-op.est-no  EQ est-op.est-no AND
      b-est-op.qty     EQ est-op.qty AND
      b-est-op.line    LT est-op.line
      ,
      FIRST b-mach FIELDS(sch-m-code) NO-LOCK WHERE
            b-mach.company EQ est-op.company AND
            b-mach.m-code EQ est-op.m-code
            ,
      FIRST b2-mach FIELDS(sch-m-code) NO-LOCK WHERE
            b2-mach.company EQ est-op.company AND
            b2-mach.m-code EQ b-est-op.m-code
            :

      IF b-est-op.m-code EQ ip-m-code OR
         b-mach.sch-m-code EQ b2-mach.sch-m-code THEN
         DO:
            v-mach-found = YES.
            LEAVE.
         END.
  END.

  op-first = NOT v-mach-found.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-stds B-table-Win 
PROCEDURE get-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chr-handle AS CHARACTER NO-UNDO.
  

  ll-import-stds = NO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "tableio-source", OUTPUT chr-handle).
  RUN finish-new-record IN WIDGET-HANDLE (chr-handle).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportRouting B-table-Win 
PROCEDURE ImportRouting :
/*------------------------------------------------------------------------------
  Purpose:  Assigns machine data to routing (est-op)   
  Parameters:  estop Row ID
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-est-op FOR est-op.
DEFINE BUFFER bf-mach FOR mach.

FOR EACH bf-est-op
    WHERE bf-est-op.company EQ est-op.company
      AND bf-est-op.est-no EQ est-op.est-no
      AND bf-est-op.LINE LT 500
      AND ROWID(bf-est-op) NE ROWID(est-op)
    EXCLUSIVE-LOCK,
    FIRST bf-mach NO-LOCK
        WHERE bf-mach.company EQ bf-est-op.company
        AND bf-mach.m-code EQ bf-est-op.m-code:
    
    ASSIGN
        bf-est-op.d-seq      = bf-mach.d-seq
        bf-est-op.dept       = bf-mach.dept[1]
        bf-est-op.op-sb      = bf-mach.p-type NE "B"
        bf-est-op.m-code     = bf-mach.m-code
        bf-est-op.m-dscr     = bf-mach.m-dscr
        bf-est-op.op-rate[1] = bf-mach.mr-trate
        bf-est-op.op-rate[2] = bf-mach.run-trate
        bf-est-op.op-crew[1] = bf-mach.mr-crusiz
        bf-est-op.op-crew[2] = bf-mach.run-crusiz
        bf-est-op.op-spoil   = bf-mach.run-spoil
        .


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  v-override-mode = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER xop FOR est-op.

  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  v-override-mode = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF est.est-type EQ 1                              AND
     NOT CAN-FIND(FIRST eb
                  WHERE eb.company EQ est-qty.company
                    AND eb.est-no  EQ est-qty.est-no
                    AND eb.eqty    EQ est-qty.eqty) THEN DO:
    FIND FIRST xop NO-LOCK
        WHERE xop.company EQ est-qty.company
          AND xop.est-no  EQ est-qty.est-no
          AND xop.qty     EQ est-qty.eqty
         NO-ERROR.
    IF NOT AVAILABLE xop THEN DO:
      RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
      RUN dispatch IN WIDGET-HANDLE(char-hdl) ("delete-record").
    END.
  END.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


   ASSIGN 
      est-op.s-num:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.b-num:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.m-code:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.m-dscr:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.n-out:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-mr:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-waste:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-speed:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-spoil:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-crew[1]:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.op-crew[2]:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.num-col:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.num-coat:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.plates:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.fountains:READ-ONLY IN BROWSE {&browse-name} = YES
      est-op.n_out_div:READ-ONLY IN BROWSE {&browse-name} = YES
      .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-eqty = 0.

  IF AVAIL est THEN
    IF est.est-type EQ 1 THEN lv-eqty = est-qty.eqty.

    ELSE
    FOR EACH xop NO-LOCK
        WHERE xop.company EQ est-qty.company
          AND xop.est-no  EQ est-qty.est-no
          AND xop.line    LT 500
        BY xop.qty:
      lv-eqty = xop.qty.
      LEAVE.
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE op-copy B-table-Win 
PROCEDURE op-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  v-override-mode = NO.

  RUN est/d-opcopy.w (ROWID(est-op)).

  RUN repo-query (ROWID(est-op)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers B-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  
  RUN dispatch ("open-query").

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

  RUN dispatch ("row-changed").
  
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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "est-op"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-import-stds B-table-Win 
PROCEDURE set-import-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-add-update  AS   CHARACTER           NO-UNDO.
  DEFINE INPUT PARAMETER ip-import-stds LIKE ll-import-stds      NO-UNDO.

  IF ip-add-update = "Update" AND ip-import-stds = NO THEN
     v-override-mode = YES.
  ELSE
     v-override-mode = NO.

  IF ip-import-stds THEN
  FOR EACH xop NO-LOCK
      WHERE xop.company EQ est.company
        AND xop.est-no  EQ est.est-no
        AND xop.line    LT 500
        AND (NOT AVAIL est-op OR ROWID(xop) NE ROWID(est-op)),
      FIRST mach NO-LOCK
      {sys/look/machW.i}
        AND mach.m-code EQ xop.m-code:
   IF mach.obsolete THEN DO:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace or standards will not be imported"
        VIEW-AS ALERT-BOX ERROR.
    ip-import-stds = NO.
    LEAVE.
   END.
  END.
  
  ASSIGN
   ll-import-stds     = ip-import-stds
   ll-import-selected = ip-import-stds.

  IF ip-add-update EQ "update" THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO est-op.s-num IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-lock B-table-Win 
PROCEDURE set-lock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-form-no LIKE ef.form-no NO-UNDO.
  DEFINE INPUT PARAMETER ip-op-lock LIKE ef.op-lock NO-UNDO.
  
  FIND FIRST ef
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no
        AND ef.form-no EQ ip-form-no
      NO-ERROR.

  IF AVAIL ef THEN DO:

     /*task 020050908*/
     IF ip-op-lock EQ ef.op-lock THEN
     DO:

        {est/op-lock.i xest}
        
        ASSIGN
           op-lock.val[1] = INTEGER(NOT ip-op-lock)
           op-lock.val[2] = op-lock.val[1].

        RELEASE op-lock.
        
     END.

     ef.op-lock = ip-op-lock.

     RELEASE ef.
  END.
  
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

