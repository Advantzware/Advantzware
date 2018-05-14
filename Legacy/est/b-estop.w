&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: est\b-estop.w

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
       
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.

DEFINE BUFFER xop FOR est-op.

DEFINE NEW SHARED VARIABLE xcal    AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid  AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len  AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id  AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE maxco   AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE qty     AS INTEGER NO-UNDO.

DEFINE VARIABLE ll-import-stds AS LOG NO-UNDO.
DEFINE VARIABLE lv-d-seq LIKE est-op.d-seq NO-UNDO.
DEFINE VARIABLE lv-dept LIKE est-op.dept NO-UNDO.
DEFINE VARIABLE lv-op-sb LIKE est-op.op-sb NO-UNDO.
DEFINE VARIABLE lv-b-num LIKE est-op.b-num NO-UNDO.
DEFINE VARIABLE lv-n-out LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE v-passes   AS   INTEGER NO-UNDO.
DEFINE VARIABLE ll-machine-modified AS LOG NO-UNDO.
DEFINE VARIABLE ll-import-selected AS LOG NO-UNDO.
DEFINE VARIABLE ll-import-all AS LOG NO-UNDO.
DEFINE VARIABLE v-avail AS LOG NO-UNDO.
DEFINE VARIABLE lv-qty LIKE est-op.qty NO-UNDO.
DEFINE VARIABLE li-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-foam-depts AS CHARACTER INIT "DC,RC" NO-UNDO.
DEFINE VARIABLE lv-n-out-depts AS CHARACTER INIT "CR,RC" NO-UNDO.
DEFINE VARIABLE prev-m-code LIKE est-op.m-code NO-UNDO.
DEFINE VARIABLE ll-foam AS LOG NO-UNDO.
DEFINE VARIABLE lv-eqty LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE v-override-mode AS LOG NO-UNDO.
DEFINE VARIABLE ll-add-record AS LOG NO-UNDO.

{est/d-machex.i NEW}

&SCOPED-DEFINE sortby-phrase BY est-op.line

DO WITH TRANSACTION:
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
est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] ~
est-op.op-crew[2] est-op.op-rate[1] est-op.op-rate[2] est-op.plates ~
est-op.fountains est-op.att-type[1] est-op.att-qty[1] est-op.att-type[2] ~
est-op.att-qty[2] est-op.att-type[3] est-op.att-qty[3] est-op.spare-char-1 ~
est-op.n_out_div 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.op-pass est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil est-op.op-crew[1] ~
est-op.op-crew[2] est-op.plates est-op.fountains est-op.att-type[1] ~
est-op.att-qty[1] est-op.att-type[2] est-op.att-qty[2] est-op.att-type[3] ~
est-op.att-qty[3] est-op.spare-char-1 est-op.n_out_div 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table est-op
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table est-op
&Scoped-define QUERY-STRING-br_table FOR EACH est-op WHERE est-op.company = est-qty.company ~
  AND est-op.est-no = est-qty.est-no ~
      AND est-op.line LT 500 and ~
((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or ~
 (ASI.est-op.qty eq lv-eqty and est.est-type ge 7)) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH est-op WHERE est-op.company = est-qty.company ~
  AND est-op.est-no = est-qty.est-no ~
      AND est-op.line LT 500 and ~
((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or ~
 (ASI.est-op.qty eq lv-eqty and est.est-type ge 7)) NO-LOCK ~
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
      est-op.s-num COLUMN-LABEL "S" FORMAT ">9":U LABEL-BGCOLOR 14
      est-op.b-num COLUMN-LABEL "B" FORMAT ">9":U
      est-op.m-code COLUMN-LABEL "Machine" FORMAT "x(6)":U COLUMN-FONT 0
            LABEL-BGCOLOR 14
      est-op.m-dscr FORMAT "x(20)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      est-op.op-pass COLUMN-LABEL "Pass#" FORMAT ">9":U
      est-op.n-out COLUMN-LABEL "Out" FORMAT ">>9":U
      est-op.op-mr COLUMN-LABEL "MR-Hrs." FORMAT ">>9.99":U
      est-op.op-waste FORMAT ">>>>>9":U
      est-op.op-speed FORMAT ">>>>9":U
      est-op.op-spoil FORMAT ">>9.99":U
      est-op.op-crew[1] COLUMN-LABEL "MRCrew" FORMAT ">9.99":U WIDTH 9.2
      est-op.op-crew[2] COLUMN-LABEL "RunCrew" FORMAT ">9.99":U
            WIDTH 10.2
      est-op.op-rate[1] COLUMN-LABEL "MRate" FORMAT ">>>9.99":U
            WIDTH 7.8
      est-op.op-rate[2] COLUMN-LABEL "RRate" FORMAT ">>>9.99":U
      est-op.plates FORMAT ">>>":U
      est-op.fountains FORMAT ">>>":U
      est-op.att-type[1] COLUMN-LABEL "Adder 1" FORMAT "X(5)":U
            WIDTH 11.2
      est-op.att-qty[1] COLUMN-LABEL "Qty" FORMAT ">>,>>>":U
      est-op.att-type[2] COLUMN-LABEL "Adder 2" FORMAT "X(5)":U
            WIDTH 11.2
      est-op.att-qty[2] COLUMN-LABEL "Qty" FORMAT ">>,>>>":U
      est-op.att-type[3] COLUMN-LABEL "Adder 3" FORMAT "X(5)":U
            WIDTH 11.2
      est-op.att-qty[3] COLUMN-LABEL "Qty" FORMAT ">>,>>>":U
      est-op.spare-char-1 COLUMN-LABEL "Feed" FORMAT "x(1)":U
      est-op.n_out_div COLUMN-LABEL "Run Qty Divisor" FORMAT "->>,>>9.99":U
  ENABLE
      est-op.s-num
      est-op.b-num
      est-op.m-code
      est-op.m-dscr
      est-op.op-pass
      est-op.n-out
      est-op.op-mr
      est-op.op-waste
      est-op.op-speed
      est-op.op-spoil
      est-op.op-crew[1]
      est-op.op-crew[2]
      est-op.plates
      est-op.fountains
      est-op.att-type[1]
      est-op.att-qty[1]
      est-op.att-type[2]
      est-op.att-qty[2]
      est-op.att-type[3]
      est-op.att-qty[3]
      est-op.spare-char-1
      est-op.n_out_div HELP "Enter Divisor for Run Quantity Reduction"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 130 BY 7.14
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
         HEIGHT             = 11
         WIDTH              = 151.6.
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
     _TblList          = "ASI.est-op WHERE ASI.est <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.est-op.company = ASI.est-qty.company
  AND ASI.est-op.est-no = ASI.est-qty.est-no"
     _Where[1]         = "ASI.est-op.line LT 500 and
((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or
 (ASI.est-op.qty eq lv-eqty and est.est-type ge 7))"
     _FldNameList[1]   > ASI.est-op.s-num
"est-op.s-num" "S" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.est-op.b-num
"est-op.b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.est-op.m-code
"est-op.m-code" "Machine" ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.est-op.m-dscr
"est-op.m-dscr" ? ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.est-op.op-pass
"est-op.op-pass" "Pass#" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.est-op.n-out
"est-op.n-out" "Out" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.est-op.op-mr
"est-op.op-mr" "MR-Hrs." ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.est-op.op-waste
"est-op.op-waste" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.est-op.op-speed
"est-op.op-speed" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.est-op.op-spoil
"est-op.op-spoil" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.est-op.op-crew[1]
"est-op.op-crew[1]" "MRCrew" ? "decimal" ? ? ? ? ? ? yes ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.est-op.op-crew[2]
"est-op.op-crew[2]" "RunCrew" ? "decimal" ? ? ? ? ? ? yes ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.est-op.op-rate[1]
"est-op.op-rate[1]" "MRate" ">>>9.99" "decimal" ? ? ? ? ? ? no ? no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.est-op.op-rate[2]
"est-op.op-rate[2]" "RRate" ">>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.est-op.plates
"est-op.plates" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.est-op.fountains
"est-op.fountains" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.est-op.att-type[1]
"est-op.att-type[1]" "Adder 1" ? "character" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.est-op.att-qty[1]
"est-op.att-qty[1]" "Qty" ">>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.est-op.att-type[2]
"est-op.att-type[2]" "Adder 2" ? "character" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.est-op.att-qty[2]
"est-op.att-qty[2]" "Qty" ">>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.est-op.att-type[3]
"est-op.att-type[3]" "Adder 3" ? "character" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.est-op.att-qty[3]
"est-op.att-qty[3]" "Qty" ">>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.est-op.spare-char-1
"est-op.spare-char-1" "Re" "N/R" "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.est-op.n_out_div
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
  DEFINE VARIABLE lw-focus AS HANDLE NO-UNDO.


  lw-focus = FOCUS.

  CASE lw-focus:NAME:
    WHEN "m-code" THEN DO:
      RUN windows/l-mach.w (est.company, est.loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN
        ASSIGN
         lw-focus:SCREEN-VALUE                               = ENTRY(1,char-val)
         est-op.m-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
    END.
    WHEN "att-type" THEN DO:
      RUN windows/l-mchatt.w (est.company, est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-att-type (lw-focus).
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
  IF est.est-type EQ 5 THEN DO:
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
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.b-num IN BROWSE br_table /* B */
DO:
  DEFINE VARIABLE ll-1-blank AS LOG NO-UNDO.

  DEFINE BUFFER b-eb FOR eb.

  FIND mach WHERE mach.company = est.company 
              AND mach.m-code = est-op.m-code
            NO-LOCK NO-ERROR.
  
  FOR EACH b-eb
      WHERE b-eb.company EQ est.company
        AND b-eb.est-no  EQ est.est-no
        AND b-eb.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
      NO-LOCK
      BREAK BY b-eb.blank-no:

    ll-1-blank = FIRST(b-eb.blank-no) AND LAST(b-eb.blank-no).

    LEAVE.
  END.

  IF ll-1-blank AND NOT (avail(mach) AND LOOKUP(mach.p-type, "A,P") GT 0) THEN DO:
    APPLY "tab" TO {&self-name}.
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

    IF ll-import-stds AND NOT CAN-DO(lv-n-out-depts,lv-dept) THEN DO:
      IF lv-dept EQ "PR" THEN
        APPLY "entry" TO est-op.plates IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO est-op.att-type[1] IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
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
        AND mach.m-code EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAILABLE MACH THEN DO:
    ASSIGN
     {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(mach.m-code)
     est-op.m-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = mach.m-dscr
     lv-dept                                             = mach.dept[1].

    DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}):
      APPLY "cursor-right" TO {&self-name} IN BROWSE {&browse-name}.
    END.
 
    IF mach.p-type EQ "B"                                           AND
      INT(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

    IF mach.p-type NE "B" AND NOT LOOKUP(mach.p-type, "P,A") GT 0 THEN
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


&Scoped-define SELF-NAME est-op.op-pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.op-pass br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.op-pass IN BROWSE br_table /* Pass# */
DO:
  RUN is-it-foam.

  IF NOT ll-foam OR NOT CAN-DO(lv-foam-depts,lv-dept) THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.op-pass br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.op-pass IN BROWSE br_table /* Pass# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-op-pass NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.n-out IN BROWSE br_table /* Out */
DO:
  RUN is-it-foam.

  IF NOT CAN-DO(lv-n-out-depts,lv-dept)                  AND
     (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam)  THEN DO:
    APPLY "tab" TO SELF.
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
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ll-import-stds AND CAN-DO(lv-n-out-depts,lv-dept) THEN DO:
      IF lv-dept EQ "PR" THEN
        APPLY "entry" TO est-op.plates IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO est-op.att-type[1] IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.plates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.plates br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.plates IN BROWSE br_table /* Plate Changes */
DO:
  DEFINE VARIABLE ll AS LOG INIT YES NO-UNDO.


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
  /*IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds AND lv-dept NE "PR" THEN RUN get-stds.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.fountains  
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.fountains br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.fountains IN BROWSE br_table /* Fountain Changes */
DO:
  DEFINE VARIABLE ll AS LOG INIT YES NO-UNDO.


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
  /*IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds THEN RUN get-stds.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.att-type[1] IN BROWSE br_table /* Adder 1 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-att-type (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[1] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF est-op.att-type[1] IN BROWSE br_table /* Adder 1 */
DO:
  RUN new-att-type (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[2] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.att-type[2] IN BROWSE br_table /* Adder 2 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-att-type (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[2] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF est-op.att-type[2] IN BROWSE br_table /* Adder 2 */
DO:
  RUN new-att-type (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-type[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.att-type[3] IN BROWSE br_table /* Adder 3 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-att-type (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-type[3] br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF est-op.att-type[3] IN BROWSE br_table /* Adder 3 */
DO:
  RUN new-att-type (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.att-qty[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.att-qty[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.att-qty[3] IN BROWSE br_table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF ll-import-stds THEN RUN get-stds.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME est-op.spare-char-1    
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.spare-char-1 br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.spare-char-1 IN BROWSE br_table /* Spare char 1 Changes */
DO:
    
  /*DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.*/
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
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
  DEFINE VARIABLE ll AS LOG NO-UNDO.
  DEFINE VARIABLE lv-msg1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.

  DEFINE BUFFER b-est-qty FOR est-qty.

  {est/checkuse.i}
       
  v-override-mode = NO.

  IF CAN-FIND(FIRST est-op
              WHERE est-op.company EQ est.company
                AND est-op.est-no  EQ est.est-no
                AND (est-op.qty    EQ est-qty.eqty OR est.est-type GE 7)) THEN DO:
                
    lv-msg = IF CAN-FIND(FIRST est-op
                         WHERE est-op.company EQ est.company
                           AND est-op.est-no  EQ est.est-no
                           AND (est-op.qty    EQ est-qty.eqty OR est.est-type GE 7)
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
            BUTTON YES-NO UPDATE ll-ans AS LOG.

    IF NOT ll-ans THEN RETURN NO-APPLY.
  END.
  
  FIND xest WHERE RECID(xest) EQ recid(est).

  ll = NO.

  IF xest.est-type GE 7 THEN DO:
    FOR EACH ef
        WHERE ef.company EQ est-qty.company
          AND ef.est-no  EQ est-qty.est-no
        NO-LOCK:
      
      RUN set-lock (ef.form-no, NO).
    END.    
    
    FIND FIRST xef WHERE xef.company = est-qty.company 
                     AND xef.est-no = est-qty.est-no
                   NO-LOCK NO-ERROR.
    FIND FIRST xeb WHERE xeb.company = est-qty.company 
                     AND xeb.est-no = est-qty.est-no
                     AND xeb.form-no = xef.form-no
                   NO-LOCK NO-ERROR.

    RUN cec/mach-seq.p (0, 0, xest.est-type EQ 8).
  END.

  ELSE DO:
    FOR EACH b-est-qty
        WHERE b-est-qty.company EQ est-qty.company
          AND b-est-qty.est-no  EQ est-qty.est-no
        NO-LOCK BREAK BY b-est-qty.eqty:

      IF FIRST(b-est-qty.eqty)    AND
         NOT LAST(b-est-qty.eqty) THEN
        MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

      IF ll OR ROWID(est-qty) EQ ROWID(b-est-qty) THEN DO:  
        FOR EACH ef
            WHERE ef.company EQ b-est-qty.company
              AND ef.est-no  EQ b-est-qty.est-no
            NO-LOCK:
      
          RUN set-lock (ef.form-no, NO).
        END.    
    
        FIND FIRST xef WHERE xef.company = b-est-qty.company 
                         AND xef.est-no = b-est-qty.est-no
                       NO-LOCK NO-ERROR.
        FIND FIRST xeb WHERE xeb.company = b-est-qty.company 
                         AND xeb.est-no = b-est-qty.est-no
                         AND xeb.form-no = xef.form-no
                       NO-LOCK NO-ERROR.

        RUN cec/mach-seq.p (0, b-est-qty.eqty, NO).
      END.
    END.
  END.

  RUN release-shared-buffers.
  
  RUN dispatch ('open-query').

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-mach-attach B-table-Win 
PROCEDURE find-mach-attach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER io-mach-attach FOR mach-attach.

  DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST io-mach-attach NO-LOCK
        WHERE io-mach-attach.company  EQ cocode
          AND io-mach-attach.m-code   EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
          AND io-mach-attach.att-type EQ ip-focus:SCREEN-VALUE
        NO-ERROR.
  END.

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
  DEFINE OUTPUT PARAMETER op-first  AS   LOG           NO-UNDO.
  
  DEFINE BUFFER b-est-op FOR est-op.


  op-first = AVAILABLE est-op AND
             NOT CAN-FIND(FIRST b-est-op
                          WHERE b-est-op.company EQ est-op.company
                            AND b-est-op.est-no  EQ est-op.est-no
                            AND b-est-op.qty     EQ est-op.qty
                            AND b-est-op.m-code  EQ ip-m-code
                            AND b-est-op.line    LT est-op.line).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-n-out B-table-Win 
PROCEDURE get-n-out :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-it-foam B-table-Win 
PROCEDURE is-it-foam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-ef FOR ef.

  
  DO WITH FRAME {&FRAME-NAME}:
    ll-foam = NO.
    FIND FIRST b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ est.est-no
          AND b-ef.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    IF AVAILABLE b-ef THEN RUN cec/isitfoam.p (ROWID(b-ef), OUTPUT ll-foam).
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
  ll-add-record = YES.
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
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-outw     LIKE xef.n-out NO-UNDO.
  DEFINE VARIABLE v-outl     LIKE xef.n-out-l NO-UNDO.
  DEFINE VARIABLE v-rate     LIKE est-op.op-rate NO-UNDO.
  DEFINE VARIABLE v-recid    AS   RECID NO-UNDO.
  DEFINE BUFFER bf-est-op FOR est-op.
 
  /* Code placed here will execute PRIOR to standard behavior. */
  fil_id = RECID(est-op).  /* for sub-program */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  FIND xest WHERE RECID(xest) EQ RECID(est).

  FIND FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ est-op.m-code
      NO-LOCK NO-ERROR.

  FIND FIRST xef
      WHERE xef.company EQ est-op.company
        AND xef.est-no  EQ est-op.est-no
        AND xef.form-no EQ est-op.s-num
      NO-LOCK NO-ERROR.

  RELEASE xeb.
  IF AVAILABLE xef THEN
  FIND FIRST xeb
      WHERE xeb.company   EQ xef.company
        AND xeb.est-no    EQ xef.est-no
        AND xeb.form-no   EQ xef.form-no
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK NO-ERROR.

  ASSIGN
   est-op.d-seq  = mach.d-seq
   est-op.dept   = mach.dept[1]
   est-op.op-sb  = mach.p-type NE "B"
   est-op.m-code = mach.m-code
   est-op.m-dscr = mach.m-dscr.

  IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[1] = mach.mr-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                        INPUT-OUTPUT est-op.op-crew[1]).
  END.

  IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[2] = mach.run-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                        INPUT-OUTPUT est-op.op-crew[2]).
  END.

  ASSIGN
   est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
   est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
         
  IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
  IF LOOKUP(mach.p-type, "P,A") EQ 0 THEN DO:
      /* Allow override for types P and A (task 09281204) */
      IF mach.p-type NE "B" THEN
        est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
         
      ELSE
      IF est-op.b-num EQ 0 THEN est-op.b-num = 1.

  END.
  ELSE DO:
      /* If adding a P or A, make it the last machine */
      IF ll-add-record THEN DO:
         FOR EACH bf-est-op WHERE bf-est-op.company EQ est-op.company
                             AND bf-est-op.est-no  EQ est-op.est-no
                           NO-LOCK
                           BY bf-est-op.s-num DESCENDING.
             LEAVE.
         END.
         IF AVAILABLE bf-est-op THEN DO:
             ASSIGN est-op.s-num = bf-est-op.s-num 
                    est-op.b-num = 1.
         END.
      END.      
  END.

  RUN is-it-foam.
     
  IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
     (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.
    
  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
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
        AND xop.line    LT 500
      BY xop.qty
      BY xop.s-num
      BY xop.b-num
      BY xop.d-seq
      BY xop.op-pass
      BY xop.rec_key:
      
    {sys/inc/outstrPL.i xop share}  
    ASSIGN
     j        = j + 1
     xop.line = j.
     
    IF AVAILABLE reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 

  END.

  IF NOT xef.op-lock AND NOT ll-foam THEN DO:
     v-outw = xef.n-out.    
     IF v-outw GT 1 THEN
     FOR EACH xop
         WHERE xop.company EQ est-op.company
           AND xop.est-no  EQ est-op.est-no
           AND xop.qty     EQ est-op.qty
           AND xop.s-num   EQ est-op.s-num
           AND lookup(xop.dept,lv-n-out-depts) GT 0
           AND xop.line    LT 500
         NO-LOCK BY xop.d-seq BY xop.line:
        
       v-outw = v-outw - xop.n-out.  
       IF v-outw LE 0 THEN DO:
         v-recid = RECID(xop).
         LEAVE.
       END.
     END.
     
     v-outl = xef.n-out-l.    
     IF v-outl GT 1 THEN
     FOR EACH xop
         WHERE xop.company EQ est-op.company
           AND xop.est-no  EQ est-op.est-no
           AND xop.qty     EQ est-op.qty
           AND xop.s-num   EQ est-op.s-num
           AND lookup(xop.dept,lv-n-out-depts) GT 0
           AND xop.line    LT 500
         NO-LOCK BY xop.d-seq DESCENDING BY xop.line DESCENDING:
         
       IF RECID(xop) EQ v-recid THEN LEAVE.       
       v-outl = v-outl - xop.n-out.      
       IF v-outl LE 0 THEN LEAVE.
     END.
     
     IF v-outw + v-outl LT 0 THEN DO ON ENDKEY UNDO, RETRY:
       MESSAGE "Number Out for 'CR or RC' machine passes do not match layout..."
               VIEW-AS ALERT-BOX.
       RETURN ERROR.
     END.
  END.    
  
  ASSIGN
   fil_id  = RECID(est-op)
   v-recid = fil_id.

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, YES).
  END.
  ASSIGN  fil_id = v-recid
          ll-add-record = NO.
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
  DEFINE VARIABLE lv-qty LIKE est-op.qty NO-UNDO.

                          
  /* Code placed here will execute PRIOR to standard behavior. */
  i = 1.
  FOR EACH xop
      WHERE xop.company EQ est.company
        AND xop.est-no  EQ est.est-no
        AND xop.line    LT 500
      NO-LOCK
      BY xop.line DESCENDING:
    i = xop.line + 1.
    LEAVE.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   est-op.company = est.company
   est-op.est-no  = est.est-no
   est-op.auto    = FALSE
   est-op.line    = i
   est-op.s-num   = 1
   est-op.b-num   = IF est.est-type EQ 5 THEN 1 ELSE 0
   est-op.op-pass = 1
   est-op.qty     = IF est.est-type NE 8 THEN est-qty.eqty ELSE lv-eqty.

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
  IF est.est-type NE 8                              AND
     NOT CAN-FIND(FIRST eb
                  WHERE eb.company EQ est-qty.company
                    AND eb.est-no  EQ est-qty.est-no
                    AND eb.eqty    EQ est-qty.eqty) THEN DO:
    FIND FIRST xop
        WHERE xop.company EQ est-qty.company
          AND xop.est-no  EQ est-qty.est-no
          AND xop.qty     EQ est-qty.eqty
        NO-LOCK NO-ERROR.
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
  
  prev-m-code = est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}.

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

  RUN is-it-foam.

  IF ll-foam THEN lv-n-out-depts = lv-n-out-depts + ",DC". 

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

  IF AVAILABLE est THEN
    IF est.est-type NE 8 THEN lv-eqty = est-qty.eqty.

    ELSE
    FOR EACH xop
        WHERE xop.company EQ est-qty.company
          AND xop.est-no  EQ est-qty.est-no
          AND xop.line    LT 500
        NO-LOCK
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
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-s-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-b-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-mach NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-op-pass NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-att-type (est-op.att-type[1]:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-att-type (est-op.att-type[2]:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-att-type (est-op.att-type[3]:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  
  ll-import-all = NO.
  IF ll-import-selected THEN DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "NO = Import Standards for Only Machine Imported?" SKIP
            "YES = Import Standards for All Machines on Routing?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-import-all.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN repo-query (ROWID(est-op)).

  IF NOT winReSize THEN
  DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    APPLY 'cursor-left' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-att-type B-table-Win 
PROCEDURE new-att-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

  DEFINE VARIABLE li AS INTEGER NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    li = ip-focus:INDEX.

    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
       RUN find-mach-attach (BUFFER mach-attach, ip-focus).
       FIND FIRST eb WHERE eb.company = est-op.company
                 AND eb.est-no = est-op.est-no
                 /*AND eb.eqty = est-op.eqty*/
                 AND eb.form-no = est-op.s-num
                 AND eb.blank-no = est-op.b-num NO-LOCK NO-ERROR.
       IF AVAILABLE eb THEN
       FIND FIRST style WHERE style.company = eb.company
                 AND style.style = eb.style
                 AND style.flute = eb.flute
                 AND style.test = eb.test 
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE style THEN
       FIND FIRST style WHERE style.company = eb.company
                 AND style.style = eb.style
                 AND style.flute = ""
                 AND style.test = ""
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
       IF AVAILABLE mach-attach THEN
         IF li EQ 1 THEN
           est-op.att-qty[1]:SCREEN-VALUE IN BROWSE {&browse-name} = 
             IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
             ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
         ELSE
         IF li EQ 2 THEN
           est-op.att-qty[2]:SCREEN-VALUE IN BROWSE {&browse-name} = 
             IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
             ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
         ELSE
           est-op.att-qty[3]:SCREEN-VALUE IN BROWSE {&browse-name} = 
               IF mach-attach.qty > 0 THEN STRING(mach-attach.qty)
               ELSE IF AVAILABLE style THEN STRING(style.dim-df) ELSE "0".
    END.
  END.

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

  RUN dispatch ("copy-record").

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
  DEFINE INPUT PARAMETER ip-import-stds LIKE ll-import-stds NO-UNDO.

  IF ip-add-update = "Update" AND ip-import-stds = NO THEN
     v-override-mode = YES.
  ELSE
     v-override-mode = NO.

  IF ip-import-stds THEN
  FOR EACH xop NO-LOCK
      WHERE xop.company EQ est.company
        AND xop.est-no  EQ est.est-no
        AND xop.line    LT 500
        AND (NOT AVAILABLE est-op OR ROWID(xop) NE ROWID(est-op)),
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

  IF ip-add-update EQ "update" THEN DO WITH FRAME {&frame-name}:
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
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND ef.form-no EQ ip-form-no
      NO-ERROR.
  IF AVAILABLE ef THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-att-type B-table-Win 
PROCEDURE valid-att-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

  DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-type LIKE est-op.att-type NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      ASSIGN
       lv-type[1] = est-op.att-type[1]:SCREEN-VALUE IN BROWSE {&browse-name}
       lv-type[2] = est-op.att-type[2]:SCREEN-VALUE IN BROWSE {&browse-name}
       lv-type[3] = est-op.att-type[3]:SCREEN-VALUE IN BROWSE {&browse-name}.

      RUN find-mach-attach (BUFFER mach-attach, ip-focus).
    
      IF NOT AVAILABLE mach-attach THEN
        lv-msg = "Invalid Attachment Type for Machine, try help".

      ELSE
        ip-focus:SCREEN-VALUE = mach-attach.att-type.

      IF lv-msg EQ "" THEN
      DO li = 1 TO EXTENT(lv-type):
        IF ip-focus:INDEX NE li                 AND
           ip-focus:SCREEN-VALUE EQ lv-type[li] THEN DO:
          lv-msg = TRIM(ip-focus:LABEL) +
                   " may not be the same as one of the other two".
          LEAVE.
        END.
      END.
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

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

  DO WITH FRAME {&frame-name}:
    APPLY "value-changed" TO est-op.m-code IN BROWSE {&browse-name}.

    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF ((AVAILABLE mach AND mach.p-type EQ "B") OR
        int(est-op.b-num:screen-value IN BROWSE {&browse-name}) NE 0) AND
       NOT CAN-FIND(FIRST eb
                    WHERE eb.company  EQ est.company
                      AND eb.est-no   EQ est.est-no
                      AND eb.form-no  EQ int(est-op.s-num:screen-value IN BROWSE {&browse-name})
                      AND eb.blank-no EQ int(est-op.b-num:screen-value IN BROWSE {&browse-name}))
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
  DEFINE VARIABLE ls-tmp AS cha NO-UNDO.
  DEFINE VARIABLE v-run AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-on-f AS INTEGER NO-UNDO.
  DEFINE VARIABLE sh-dep AS DECIMAL NO-UNDO.
  DEFINE VARIABLE li-aqueous AS INTEGER NO-UNDO.
  DEFINE VARIABLE cMachType  AS CHARACTER NO-UNDO .
  DEFINE VARIABLE v-msgreturn AS INTEGER NO-UNDO .
  DEFINE VARIABLE dNshLen AS DECIMAL NO-UNDO .
  DEFINE VARIABLE dTrimL  AS DECIMAL NO-UNDO .
  DEFINE VARIABLE dMachMaxLen AS DECIMAL NO-UNDO .
  DEFINE VARIABLE dMachMaxWid AS DECIMAL NO-UNDO .

  DEFINE BUFFER b-est-op FOR est-op.
  DEFINE BUFFER b-mach FOR mach.
  DEFINE BUFFER bf-ef FOR ef.
  

/*  {sys/inc/cepanel.i} - deprecated with 17756*/

  RUN is-it-foam.

  DO WITH FRAME {&frame-name}:
    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:screen-value IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE mach THEN DO:
      MESSAGE "Must enter a valid Machine Code, try help"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.


     IF mach.obsolete THEN DO:
      MESSAGE "Machine is obsolete, please choose a different machine"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  
    ASSIGN
     est-op.m-code:screen-value IN BROWSE {&browse-name} = mach.m-code
     est-op.m-dscr:screen-value IN BROWSE {&browse-name} = mach.m-dscr
     lv-d-seq = mach.d-seq
     lv-dept  = mach.dept[1].

    FIND FIRST xef
        WHERE xef.company EQ est.company
          AND xef.est-no  EQ est.est-no
          AND xef.form-no EQ int(est-op.s-num:screen-value IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

    FIND FIRST xeb
        WHERE xeb.company EQ est.company
          AND xeb.est-no  EQ est.est-no
          AND xeb.form-no EQ xef.form-no
          AND (xeb.blank-no EQ int(est-op.b-num:screen-value IN BROWSE {&browse-name}) OR
               int(est-op.b-num:screen-value IN BROWSE {&browse-name}) EQ 0)
        NO-LOCK NO-ERROR.

    FIND FIRST style
        {sys/ref/styleW.i}
          AND style.style EQ xeb.style
        NO-LOCK NO-ERROR.

    FIND xest WHERE RECID(xest) EQ recid(est) NO-LOCK.

    IF ll-import-stds THEN RUN get-n-out.

    v-on-f = int(est-op.n-out:screen-value IN BROWSE {&browse-name}).

    IF v-on-f NE 0 OR NOT CAN-DO(lv-n-out-depts,lv-dept) THEN DO:
      IF xef.lam-dscr EQ "R"                         /*or
         (xef.lam-dscr ne "R" and xef.xgrain eq "S")*/ THEN
        ASSIGN
         sh-wid = xef.nsh-wid
         sh-len = xef.nsh-len.
      ELSE
        ASSIGN
         sh-wid = xef.nsh-len
         sh-len = xef.nsh-wid.

      DO WHILE TRUE:
        sh-dep = xef.cal.

        IF mach.p-type EQ "B" THEN
          ASSIGN
           sh-len = xeb.t-len
           sh-wid = xeb.t-wid.

        IF ll-foam THEN DO:
          sh-dep = xeb.t-dep.

          IF mach.p-type NE "B" THEN DO:
            {sys/inc/outstrPL.i est-op NO}
         
            IF AVAILABLE reftable THEN
            DO i = 1 TO 3:
              CASE reftable.val[i]:
                WHEN 1.0 THEN sh-len = reftable.val[i + 3].
                WHEN 2.0 THEN sh-wid = reftable.val[i + 3].
                WHEN 3.0 THEN sh-dep = reftable.val[i + 3].
              END CASE.
            END.
          END.
        END.

        ASSIGN cMachType = IF AVAILABLE mach THEN mach.p-type ELSE "" 
               dMachMaxLen = IF AVAILABLE mach THEN mach.max-len ELSE 0 
               dMachMaxWid = IF AVAILABLE mach THEN mach.max-wid ELSE 0
               qty = lv-eqty. /*20108 - qty variable needed for mach-seq.i->mach-qty.p setting equal to selected routing qty*/

        IF lv-dept EQ "RC" THEN DO:
          xcal = sh-dep.
          RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
          IF AVAILABLE mach THEN LEAVE.
        END.

        ELSE DO:
          {cec/mach-seq.i sh-len sh-wid sh-dep}
        END.
                      
        IF NOT AVAILABLE mach THEN DO:
            IF (cMachType = "S" AND ( (sh-wid < sh-len AND  (dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid) ) OR (sh-wid > sh-len AND ( dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid)  ) )) THEN DO:

                RUN custom/d-msg-mach.w ("Warning","","Sheet size outside machine limits ","",2,"Reverse Feed Direction,OK", OUTPUT v-msgreturn).         
                IF v-msgreturn = 1  THEN DO:
                    FIND FIRST bf-ef EXCLUSIVE-LOCK WHERE 
                               bf-ef.company EQ xef.company AND
                               RECID(bf-ef)  EQ RECID(xef) NO-ERROR.
                    IF AVAILABLE bf-ef THEN
                        ASSIGN
                        dNshLen       = xef.nsh-len  
                        dTrimL        = xef.trim-l
                        bf-ef.nsh-len = xef.nsh-wid
                        bf-ef.nsh-wid = dNshLen
                        bf-ef.trim-l  = xef.trim-w
                        bf-ef.trim-w  = dTrimL
                         .
                    FIND FIRST mach
                        {sys/look/machW.i}
                        AND mach.m-code EQ est-op.m-code:screen-value IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
                    LEAVE.
                    
                END.
                ELSE DO:
                    APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                    RETURN ERROR.
                END.

            END.  /* cMachType = "S" */   
            ELSE IF cMachType = "B" AND est-op.spare-char-1:screen-value IN BROWSE {&browse-name} NE "R" 
            AND ( (sh-wid < sh-len AND  (dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid) ) OR (sh-wid > sh-len AND ( dMachMaxLen LT sh-len OR dMachMaxWid LT sh-wid)  ) ) THEN DO: 
                  RUN custom/d-msg-mach.w ("Warning","","Blank size outside machine limits","",2,"Reverse Blank Feed,OK", OUTPUT v-msgreturn).        
                 IF v-msgreturn = 1  THEN DO:
                    est-op.spare-char-1:screen-value IN BROWSE {&browse-name} = "R" .
                    FIND FIRST mach
                        {sys/look/machW.i}
                        AND mach.m-code EQ est-op.m-code:screen-value IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
                     {cec/mach-seq.i sh-wid sh-len sh-dep}
                     IF NOT AVAILABLE mach THEN  DO:
                        MESSAGE "Blank size outside machine limits, when reversed" VIEW-AS ALERT-BOX ERROR.
                        APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                        RETURN ERROR.
                     END.
                    LEAVE.
                 END.
                 
                 ELSE DO:
                     APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                     RETURN ERROR.
                 END.

            END.
            ELSE IF cMachType = "B" AND est-op.spare-char-1:screen-value IN BROWSE {&browse-name} EQ "R" THEN DO:
                FIND FIRST mach
                    {sys/look/machW.i}
                    AND mach.m-code EQ est-op.m-code:screen-value IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
                    {cec/mach-seq.i sh-wid sh-len sh-dep}
                     IF NOT AVAILABLE mach THEN  DO:
                        MESSAGE "Blank size outside machine limits, when reversed" VIEW-AS ALERT-BOX ERROR.
                        APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                        RETURN ERROR.
                     END.
                    LEAVE.
            END. 
            ELSE DO:
                 MESSAGE "Estimate specifications outside machine limits" VIEW-AS ALERT-BOX ERROR.
                 APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                 RETURN ERROR.
            END.

         
        END.
      END.
    END.
        
    IF CAN-DO(lv-n-out-depts,lv-dept) AND v-on-f LE 0 AND 
       FOCUS:NAME IN BROWSE {&browse-name} NE "m-code" THEN DO:
      MESSAGE TRIM(est-op.n-out:LABEL IN BROWSE {&browse-name}) +
              " must not be zero..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.n-out IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF mach.p-type NE "B" THEN
      ASSIGN
       lv-op-sb = YES
       lv-b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
       
    ELSE
      ASSIGN
       lv-op-sb = NO
       lv-b-num = IF int(est-op.b-num:screen-value) EQ 0 THEN 1 ELSE int(est-op.b-num:screen-value).
       
    IF NOT CAN-DO(lv-n-out-depts,lv-dept) THEN lv-n-out = 0.

    IF xeb.i-pass GT 0 THEN DO:
          /* press, check ink */
          IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
             mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN DO:
                FIND FIRST item {sys/look/itemivW.i}
                                AND item.i-no EQ xeb.i-code[1]
                     NO-LOCK NO-ERROR.
                IF NOT AVAILABLE item AND mach.dept[2] EQ "" AND
                  mach.dept[3] EQ "" AND mach.dept[4] EQ "" 
                THEN DO:
                     MESSAGE "No Inks defined !" VIEW-AS ALERT-BOX.
                     APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                     RETURN ERROR.                   
                END.
                ELSE IF AVAILABLE item AND item.press-type NE mach.pr-type THEN DO:
                    MESSAGE "WRONG PRESS TYPE for selected Ink!" VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                    RETURN ERROR.
                END.
                ASSIGN
                  maxco    = 0
                  v-passes = INT(adm-new-record).

                FOR EACH b-est-op
                    WHERE b-est-op.company EQ est-op.company
                      AND b-est-op.est-no  EQ est-op.est-no
                      AND (b-est-op.eqty   EQ est-op.eqty OR est.est-type GE 7)
                      AND b-est-op.s-num   EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND b-est-op.line    LT 500
                      AND (ROWID(b-est-op) NE ROWID(est-op) OR NOT adm-new-record)
                      AND CAN-FIND(FIRST b-mach
                                   WHERE b-mach.company EQ b-est-op.company
                                     AND b-mach.m-code  EQ b-est-op.m-code
                                     AND (b-mach.dept[1] EQ "PR" OR
                                          b-mach.dept[2] EQ "PR" OR
                                          b-mach.dept[3] EQ "PR" OR
                                          b-mach.dept[4] EQ "PR" OR
                                          b-mach.dept[1] EQ "CT" OR
                                          b-mach.dept[2] EQ "CT" OR
                                          b-mach.dept[3] EQ "CT" OR
                                          b-mach.dept[4] EQ "CT"))
                    NO-LOCK BY b-est-op.d-seq BY b-est-op.line:

                   v-passes = v-passes + 1.
                   IF ROWID(b-est-op) EQ ROWID(est-op) THEN LEAVE.
                END.

                DO i = 1 TO 10:
                   IF xeb.i-ps[i] NE v-passes THEN NEXT.
                   FIND FIRST item NO-LOCK
                       {sys/look/itemW.i}
                         AND item.i-no     EQ xeb.i-code[i]
                         AND INDEX("IV",item.mat-type) GT 0
                         AND item.ink-type NE "A"
                       NO-ERROR.
                   IF AVAILABLE item THEN maxco = maxco + 1.
                END.

                IF mach.max-color LT maxco THEN DO:
                   MESSAGE "NOT ENOUGH COLORS on PRESS for selected Inks!" VIEW-AS ALERT-BOX ERROR.
                   APPLY "entry" TO est-op.m-code IN BROWSE {&browse-name}.
                   RETURN ERROR.
                END.
          END.  /* dept = "PR" */
    END.  /* x-eb.i-pass */

    /*RUN cec/mach-qty.p (ROWID(mach), ROWID(xeb), v-on-f, sh-len, OUTPUT v-run).

    if v-run lt mach.min-run then do:
            message "RUN QTY. too small for Machine!" view-as alert-box error.
            apply "entry" to est-op.m-code in browse {&browse-name}.
            return error.
    end.
    if (xest.est-qty[1] / xest.prod-runs) gt mach.max-run then do:
            message "RUN QTY. too large for Machine!" view-as alert-box error.
            apply "entry" to est-op.m-code in browse {&browse-name}.
            return error.
    end.
    if mach.min-cal gt xef.cal then do:
            message "BOARD CALIPER too small for Machine!" view-as alert-box.
            apply "entry" to est-op.m-code in browse {&browse-name}.
            return error.
    end.
    if mach.max-cal lt xef.cal then do:
            message "BOARD CALIPER too large for Machine!" view-as alert-box.
            apply "entry" to est-op.m-code in browse {&browse-name}.
            return error.
    end.*/
    
    ll-machine-modified = est-op.m-code:MODIFIED IN BROWSE {&browse-name}.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-op-pass B-table-Win 
PROCEDURE valid-op-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-est-op FOR est-op.


  DO WITH FRAME {&FRAME-NAME}:
    RUN is-it-foam.

    IF ll-foam AND CAN-DO(lv-foam-depts,lv-dept) THEN DO:
      /*IF CAN-FIND(FIRST b-est-op
                  WHERE b-est-op.company EQ est.company
                    AND b-est-op.est-no  EQ est.est-no
                    AND b-est-op.s-num   EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND b-est-op.op-pass EQ INT(est-op.op-pass:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND ROWID(b-est-op)  NE ROWID(est-op))
      THEN DO:
        MESSAGE TRIM(est-op.op-pass:LABEL IN BROWSE {&browse-name}) +
                " already exists for this Form#..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" to est-op.op-pass IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.*/

      IF INT(est-op.op-pass:SCREEN-VALUE IN BROWSE {&browse-name}) NE 1 AND
         NOT CAN-FIND(FIRST ef-nsh
                      WHERE ef-nsh.company EQ est.company
                        AND ef-nsh.est-no  EQ est.est-no
                        AND ef-nsh.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND ef-nsh.pass-no EQ INT(est-op.op-pass:SCREEN-VALUE IN BROWSE {&browse-name}))
      THEN DO:
        MESSAGE "Net Sheet does not exist for this "                +
                TRIM(est-op.op-pass:LABEL IN BROWSE {&browse-name}) +
                "..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO est-op.op-pass IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
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
  
  DO WITH FRAME {&frame-name}:
    IF NOT CAN-FIND(FIRST ef
                    WHERE ef.company EQ est.company
                      AND ef.est-no  EQ est.est-no
                      AND ef.form-no EQ int(est-op.s-num:screen-value IN BROWSE {&browse-name}))
    THEN DO:
      MESSAGE "Must enter a valid Form#" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO est-op.s-num IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

