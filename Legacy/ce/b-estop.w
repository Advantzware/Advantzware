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
      est-op.n-out COLUMN-LABEL "Out" FORMAT ">>9":U
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
"est-op.n-out" "Out" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON HELP OF br_table IN FRAME F-Main /* Operations */
DO:
   DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
   CASE FOCUS:NAME:
       WHEN "m-code" THEN DO:
          RUN windows/l-mach.w (est.company,est.loc, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
          IF char-val NE "" THEN DO:
             ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                    est-op.m-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
             APPLY "value-changed" TO FOCUS.
          END.          
       END.
   END CASE.
   RETURN NO-APPLY.
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


&Scoped-define SELF-NAME est-op.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.s-num IN BROWSE br_table /* S */
DO:
  IF est.est-type EQ 1 THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.s-num IN BROWSE br_table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-num NO-ERROR.
    if ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.b-num IN BROWSE br_table /* B */
DO:
  DEFINE VARIABLE ll-1-blank AS LOGICAL NO-UNDO.

  DEFINE BUFFER b-eb FOR eb.


  FOR EACH b-eb NO-LOCK
      WHERE b-eb.company EQ est-qty.company
        AND b-eb.est-no  EQ est-qty.est-no
        AND b-eb.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
      BREAK BY b-eb.blank-no:

    ll-1-blank = FIRST(b-eb.blank-no) AND LAST(b-eb.blank-no).

    LEAVE.
  END.

  IF ll-1-blank THEN DO:
    APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.b-num IN BROWSE br_table /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-b-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.m-code IN BROWSE br_table /* Machine */
DO:
   IF v-estopmch-log = NO AND v-override-mode THEN
   DO:
      APPLY "tab" TO est-op.m-code IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.m-code IN BROWSE br_table /* Machine */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mach NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ll-import-stds AND NOT CAN-DO("RC,GU",lv-dept) THEN
      IF CAN-DO("PR,CT",lv-dept) THEN DO:
        APPLY "entry" TO est-op.plates IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END.
      ELSE RUN get-stds.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-code br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF est-op.m-code IN BROWSE br_table /* Machine */
DO:
  DEFINE VARIABLE li AS INTEGER NO-UNDO.


  FIND mach
      {sys/look/machW.i}
        AND mach.m-code BEGINS est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL MACH THEN DO:
    ASSIGN
     est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(mach.m-code)
     est-op.m-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = mach.m-dscr.

    DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}):
      APPLY "cursor-right" TO {&self-name} IN BROWSE {&browse-name}.
    END.

    IF mach.p-type EQ "B"                                           AND
       INT(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

    IF mach.p-type NE "B" THEN
      est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.m-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.m-dscr br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.m-dscr IN BROWSE br_table /* Desc */
DO:
  APPLY "tab" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.n-out IN BROWSE br_table /* Out */
DO:
  IF NOT (v-ceroute#out-log EQ YES AND INDEX("GL,DC",lv-dept) GT 0) AND
     NOT CAN-DO("RC,GU",lv-dept) THEN DO:
     APPLY "tab" TO est-op.n-out IN BROWSE {&browse-name}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.n-out IN BROWSE br_table /* Out */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mach NO-ERROR.
    if ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ll-import-stds AND LOOKUP(lv-dept,"RC,GU") NE 0 THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.num-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-col br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.num-col IN BROWSE br_table /* Inks */
DO:
  IF lv-dept NE "PR" THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-col br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.num-col IN BROWSE br_table /* Inks */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds AND lv-dept NE "PR" AND lv-dept NE "CT" THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.num-coat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-coat br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.num-coat IN BROWSE br_table /* Varnish */
DO:
  IF lv-dept NE "PR" AND lv-dept NE "CT" THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.num-coat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.num-coat IN BROWSE br_table /* Varnish */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds AND lv-dept NE "PR" AND lv-dept NE "CT" THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.plates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.plates IN BROWSE br_table /* Plate Changes */
DO:
  DEFINE VARIABLE ll AS LOGICAL INITIAL YES NO-UNDO.


  IF lv-dept EQ "PR" THEN
    RUN first-of-mach (est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name},
                       OUTPUT ll).

  IF ll THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.plates IN BROWSE br_table /* Plate Changes */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds AND lv-dept NE "PR" THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.fountains
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.fountains IN BROWSE br_table /* Fountain Changes */
DO:
  DEFINE VARIABLE ll AS LOGICAL INITIAL YES NO-UNDO.


  IF lv-dept EQ "PR" THEN
    RUN first-of-mach (est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name},
                       OUTPUT ll).

  IF ll THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.fountains IN BROWSE br_table /* Fountain Changes */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n_out_div
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n_out_div br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.n_out_div IN BROWSE br_table /* Run Qty Divisor */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds THEN RUN get-stds.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  v-override-mode = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE j          AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-outw     LIKE xef.n-out NO-UNDO.
  DEFINE VARIABLE v-outl     LIKE xef.n-out-l NO-UNDO.
  DEFINE VARIABLE v-rate     LIKE est-op.op-rate NO-UNDO.
  DEFINE VARIABLE v-recid    AS   RECID NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  fil_id = RECID(est-op).  /* for sub-program */
         
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  FIND xest WHERE RECID(xest) EQ RECID(est).
  
  FIND FIRST mach NO-LOCK 
      {sys/look/machW.i}
        AND mach.m-code EQ est-op.m-code
      NO-ERROR.

  FIND FIRST xef NO-LOCK 
      WHERE xef.company EQ est-op.company
        AND xef.est-no  EQ est-op.est-no
        AND xef.form-no EQ est-op.s-num
       NO-ERROR.
  
  ASSIGN
   est-op.d-seq      = mach.d-seq
   est-op.dept       = mach.dept[1]
   est-op.op-sb      = mach.p-type NE "B"
   est-op.m-code     = mach.m-code
   est-op.m-dscr     = mach.m-dscr
   est-op.op-rate[1] = mach.mr-trate
   est-op.op-rate[2] = mach.run-trate.

  IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN
    est-op.op-crew[1] = mach.mr-crusiz.

  IF est-op.op-crew[2] EQ 0 OR ll-import-selected  THEN
    est-op.op-crew[2] = mach.run-crusiz.
         
  IF ll-import-selected THEN 
    ASSIGN
     est-op.op-spoil = mach.run-spoil
     est-op.NUM-COL  = 0
     est-op.num-coat = 0.

 IF ll-import-all THEN
     RUN ImportRouting.

  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line    LT 500
      BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
    IF FIRST-OF(xop.dept) THEN j = 0.
    
    ASSIGN
     j           = j + 1
     xop.op-pass = j.
  END.

  j = 0.
  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line   LT 500
      BY xop.qty BY xop.s-num BY xop.b-num BY xop.d-seq BY xop.op-pass:

    {sys/inc/outstrPL.i xop SHARE}
    ASSIGN
     j        = j + 1
     xop.line = j.
     
    IF AVAILABLE reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 
  END.  
  
  ASSIGN
   fil_id  = RECID(est-op)
   v-recid = fil_id.

  FOR EACH ef NO-LOCK
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef NO-LOCK
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no:
    RUN set-lock (ef.form-no, YES).
  END.

  fil_id = v-recid.

  RUN release-shared-buffers.

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

  v-override-mode = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
                          
  /* Code placed here will execute PRIOR to standard behavior. */

  i = 1.
  FOR EACH xop NO-LOCK
      WHERE xop.company EQ est-qty.company
        AND xop.est-no  EQ est-qty.est-no
        AND xop.line    LT 500
       BY xop.line DESCENDING:
    i = xop.line + 1.
    LEAVE.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   est-op.company = est-qty.company
   est-op.est-no  = est-qty.est-no
   est-op.auto    = NO
   est-op.line    = i
   est-op.s-num   = 1
   est-op.b-num   = 0
   est-op.op-pass = 1
   est-op.qty     = IF est.est-type EQ 1 THEN est-qty.eqty ELSE lv-eqty.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  IF NOT winReSize THEN
  DO WITH FRAME {&FRAME-NAME}:
    DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO est-op.s-num IN BROWSE {&browse-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-s-num.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-b-num.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-mach.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /*IF ll-import-stds THEN RUN get-stds.*/

  ll-import-all = NO.
  IF ll-import-selected THEN DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "NO = Import Standards for Selected Machine" SKIP
            "YES = Import Standards for All Machines on Routing"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-import-all.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN repo-query (ROWID(est-op)).

  DO WITH FRAME {&FRAME-NAME}:
     DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
        APPLY 'cursor-left' TO {&BROWSE-NAME}.
     END.
  END.

  ll-import-selected = NO.

  RUN release-shared-buffers.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-b-num B-table-Win 
PROCEDURE valid-b-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO est-op.m-code IN BROWSE {&browse-name}.

    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF ((AVAIL mach AND mach.p-type EQ "B") OR
        INTEGER(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0) AND
       NOT CAN-FIND(FIRST eb
                    WHERE eb.company  EQ est-qty.company
                      AND eb.est-no   EQ est-qty.est-no
                      AND eb.form-no  EQ int(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND eb.blank-no EQ int(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      MESSAGE "Must enter a valid Blank#" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.b-num IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mach B-table-Win 
PROCEDURE valid-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE chr-handle AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ls-tmp AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

  DEFINE BUFFER xop FOR est-op.  
  DEFINE BUFFER b-ef FOR ef.
  DEFINE BUFFER b-eb FOR eb.
  DEFINE BUFFER b-mach FOR mach.
  

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mach THEN v-msg = "Must enter a valid Machine Code, try help".

    IF v-msg EQ "" THEN
      IF mach.obsolete THEN DO:  
        v-msg = "Machine is obsolete, please choose a different machine".
      END.
    IF v-msg EQ "" THEN DO:
      FIND FIRST b-ef NO-LOCK
          WHERE b-ef.company EQ est-qty.company
            AND b-ef.est-no  EQ est-qty.est-no
            AND b-ef.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
           NO-ERROR.

      FIND FIRST b-eb NO-LOCK
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
            AND b-eb.blank-no EQ INT(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
           NO-ERROR.
      IF NOT AVAIL b-eb THEN
      FIND FIRST b-eb NO-LOCK
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
           NO-ERROR.

      FIND xest NO-LOCK WHERE RECID(xest) EQ RECID(est) .

      ASSIGN
         lv-d-seq = mach.d-seq
         lv-dept  = mach.dept[1].

      IF LOOKUP(lv-dept,"RC,GU") NE 0 AND AVAIL b-ef                     AND
         (adm-adding-record OR
          INTEGER(est-op.n-out:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0) THEN
        est-op.n-out:SCREEN-VALUE IN BROWSE {&browse-name} =
            IF lv-dept EQ "RC" THEN STRING(b-ef.n-out) ELSE STRING(b-ef.n-out-l).
  
      IF mach.p-type NE "B" THEN
        ASSIGN
         lv-op-sb = YES
         lv-b-num = 0.
       
      ELSE
        ASSIGN
         lv-op-sb = NO
         lv-b-num = IF INTEGER(est-op.b-num:SCREEN-VALUE) EQ 0 THEN 1 ELSE INTEGER(est-op.b-num:SCREEN-VALUE).
       
      IF LOOKUP(lv-dept,"RC,GU") EQ 0 THEN lv-n-out = 0.

      IF lv-dept EQ "PR" THEN DO:
        RUN ce/mach-ink.p.

        i = INT(adm-new-record).
        FOR EACH xop NO-LOCK
            WHERE xop.company EQ est-op.company
              AND xop.est-no  EQ est-op.est-no
              AND (xop.qty    EQ est-op.qty OR est.est-type GE 2)
              AND xop.s-num   EQ b-ef.form-no
              AND xop.line    LT 500
              AND (ROWID(xop) NE ROWID(est-op) OR NOT adm-new-record)
              AND CAN-FIND(FIRST b-mach
                           WHERE b-mach.company EQ xop.company
                             AND b-mach.m-code  EQ xop.m-code
                             AND (b-mach.dept[1] EQ "PR" OR
                                  b-mach.dept[2] EQ "PR" OR
                                  b-mach.dept[3] EQ "PR" OR
                                  b-mach.dept[4] EQ "PR" OR
                                  b-mach.dept[1] EQ "CT" OR
                                  b-mach.dept[2] EQ "CT" OR
                                  b-mach.dept[3] EQ "CT" OR
                                  b-mach.dept[4] EQ "CT"))
             BY xop.d-seq BY xop.line:

          i = i + 1.
          IF ROWID(xop) EQ ROWID(est-op) THEN LEAVE.
        END.
       
        FIND FIRST w-ink
            WHERE w-ink.form-no EQ b-ef.form-no
              AND w-ink.pass    EQ i
            NO-ERROR.

        IF AVAIL w-ink THEN DO:
          IF INTEGER(est-op.NUM-COL:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
            w-ink.inks = INTEGER(est-op.NUM-COL:SCREEN-VALUE IN BROWSE {&browse-name}).
          IF INTEGER(est-op.num-coat:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
            w-ink.varn = INTEGER(est-op.num-coat:SCREEN-VALUE IN BROWSE {&browse-name}).

          IF w-ink.press NE mach.pr-type THEN
            v-msg = "WRONG PRESS TYPE for selected Ink..".
          ELSE
          IF mach.max-color LT w-ink.inks + w-ink.varn THEN
            v-msg = "NOT ENOUGH COLORS on PRESS for selected Inks...".
        END.

        ELSE v-msg = "No Inks defined...".
      END.

      IF LOOKUP(lv-dept,"RS,RC") GT 0 OR mach.p-type EQ "R" THEN
        ASSIGN
         sh-len = IF b-ef.roll THEN b-ef.gsh-wid ELSE b-ef.nsh-wid
         sh-wid = IF b-ef.roll THEN b-ef.gsh-len ELSE b-ef.nsh-len.
      ELSE
      IF LOOKUP(lv-dept,"PR,GU,LM") GT 0 OR b-ef.n-out-l LE 1 THEN
        ASSIGN
         sh-len = b-ef.nsh-wid
         sh-wid = b-ef.nsh-len.
      ELSE
        ASSIGN
         sh-len = b-ef.trim-w
         sh-wid = b-ef.trim-l.

      IF mach.p-type EQ "B" THEN
        ASSIGN
         sh-len = b-eb.t-len
         sh-wid = b-eb.t-wid.

      IF v-msg EQ ""            AND
         mach.min-len GT sh-len THEN
        v-msg = "BOARD too small for Machine " +
                TRIM(IF mach.p-type EQ "R" THEN "Side-To-Side" ELSE "Front-To-Back") +
                "!".

      IF v-msg EQ ""            AND
         mach.max-len LT sh-len THEN
        v-msg = "BOARD too large for Machine " +
                TRIM(IF mach.p-type EQ "R" THEN "Side-To-Side" ELSE "Front-To-Back") +
                "!".

      IF v-msg EQ ""            AND
         mach.min-wid GT sh-wid THEN
        v-msg = "BOARD too small for Machine " +
                TRIM(IF mach.p-type EQ "R" THEN "Front-To-Back" ELSE "Side-To-Side") +
                "!".

      IF v-msg EQ ""            AND
         mach.max-wid LT sh-wid THEN
        v-msg = "BOARD too large for Machine " +
                TRIM(IF mach.p-type EQ "R" THEN "Front-To-Back" ELSE "Side-To-Side") +
                "!".

      IF v-msg EQ ""             AND
         mach.min-cal GT b-ef.cal THEN
        v-msg = "BOARD CALIPER too small for Machine!".

      IF v-msg EQ ""             AND
         mach.max-cal LT b-ef.cal THEN
        v-msg = "BOARD CALIPER too large for Machine!".

      qty = est-qty.eqty.

      FIND xef NO-LOCK WHERE  ROWID(xef) EQ ROWID(b-ef)  NO-ERROR.
      FIND xeb NO-LOCK WHERE  ROWID(xeb) EQ ROWID(b-eb)  NO-ERROR.
        
      run ce/mach-qty.p (ROWID(mach)).

      IF v-msg EQ ""               AND
         v-chk-qty LT mach.min-run THEN
        v-msg = "RUN QTY. too small for Machine!".

      IF v-msg EQ ""               AND
         v-chk-qty GT mach.max-run THEN
        v-msg = "RUN QTY. too large for Machine!".
    END.

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) VIEW-AS ALERT-BOX.
      APPLY "entry" TO est-op.m-code.
      RETURN ERROR.
    END.

    ll-machine-modified = est-op.m-code:MODIFIED IN BROWSE {&browse-name}.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num B-table-Win 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST ef
                    WHERE ef.company EQ est-qty.company
                      AND ef.est-no  EQ est-qty.est-no
                      AND ef.form-no EQ INTEGER(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      MESSAGE "Must enter a valid Form#" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.s-num IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

