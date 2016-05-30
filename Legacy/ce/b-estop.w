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
assign
 cocode = g_company
 locode = g_loc.
       
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

def buffer xop for est-op.
DEF BUFFER op-lock FOR reftable.

def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def new shared var fil_id as recid no-undo.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
def NEW shared var v-chk-qty as dec no-undo.
def NEW shared var v-sht-qty as dec no-undo.
def NEW shared var v-rc-seq as int init 9999 no-undo.

{ce/mach-ink.i new}

def var ll-import-stds as log no-undo.
def var lv-d-seq like est-op.d-seq no-undo.
def var lv-dept like est-op.dept no-undo.
def var lv-op-sb like est-op.op-sb no-undo.
def var lv-b-num like est-op.b-num no-undo.
def var lv-n-out like est-op.n-out no-undo.
def var maxco as int no-undo.
def var v-passes   as   int no-undo.
def var ll-machine-modified as log no-undo.
DEF VAR ll-import-selected AS LOG NO-UNDO.
DEF VAR ll-import-all AS LOG NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR lv-eqty LIKE est-qty.eqty NO-UNDO.
DEF VAR v-override-mode AS LOG NO-UNDO.

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
est-op.plates est-op.fountains 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table est-op.s-num est-op.b-num ~
est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr est-op.op-waste ~
est-op.op-speed est-op.op-spoil est-op.op-crew[1] est-op.op-crew[2] ~
est-op.num-col est-op.num-coat est-op.plates est-op.fountains 
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
      est-op.op-speed FORMAT ">>>>>9":U
      est-op.op-spoil FORMAT ">>9.99":U
      est-op.op-crew[1] COLUMN-LABEL "MRCrew" FORMAT ">9.99":U WIDTH 9.2
      est-op.op-crew[2] COLUMN-LABEL "RunCrew" FORMAT ">9.99":U
            WIDTH 9.6
      est-op.op-rate[1] COLUMN-LABEL "MRate" FORMAT ">>>9.99":U
            WIDTH 8.6
      est-op.op-rate[2] COLUMN-LABEL "RRate" FORMAT ">>>9.99":U
      est-op.num-col COLUMN-LABEL "Inks" FORMAT ">>>":U WIDTH 9
      est-op.num-coat COLUMN-LABEL "Varnish" FORMAT ">>>":U WIDTH 12
      est-op.plates FORMAT ">>>":U
      est-op.fountains FORMAT ">>>":U
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
   def var char-val as cha no-undo.
   case focus:name:
       when "m-code" then do:
          run windows/l-mach.w (est.company,est.loc, focus:screen-value in browse {&browse-name}, output char-val).
          if char-val <> "" then DO:
             assign focus:screen-value = entry(1,char-val)
                    est-op.m-dscr:screen-value in browse {&browse-name} = entry(2,char-val).
             APPLY "value-changed" TO FOCUS.
          END.          
       end.
   end case.
   return no-apply.
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
  if est.est-type eq 1 then do:
    apply "tab" to self.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.s-num br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-op.s-num IN BROWSE br_table /* S */
DO:
  if lastkey ne -1 then do:
    run valid-s-num NO-ERROR.
    if ERROR-STATUS:ERROR then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.b-num br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.b-num IN BROWSE br_table /* B */
DO:
  DEF VAR ll-1-blank AS LOG NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  FOR EACH b-eb
      WHERE b-eb.company EQ est-qty.company
        AND b-eb.est-no  EQ est-qty.est-no
        AND b-eb.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
      NO-LOCK
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
  if lastkey ne -1 then do:
    run valid-b-num NO-ERROR.
    if ERROR-STATUS:ERROR then return no-apply.
  end.
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
  DEF VAR li AS INT NO-UNDO.


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
  apply "tab" to self.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-op.n-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-op.n-out br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-op.n-out IN BROWSE br_table /* Out */
DO:
  IF NOT (v-ceroute#out-log = YES AND INDEX("GL,DC",lv-dept) > 0) AND
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
  if lastkey ne -1 then DO:
    run valid-mach NO-ERROR.
    if ERROR-STATUS:ERROR then return no-apply.

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
  DEF VAR ll AS LOG INIT YES NO-UNDO.


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
  DEF VAR ll AS LOG INIT YES NO-UNDO.


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
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR lv-msg1 AS CHAR NO-UNDO.

  DEF BUFFER b-est-qty FOR est-qty.


  {est/checkuse.i}
        
  v-override-mode = NO.

  if can-find(first est-op
              where est-op.company eq est-qty.company
                and est-op.est-no  eq est-qty.est-no
                and (est-op.qty    eq est-qty.eqty or est.est-type gt 1))
                then do:
                
    lv-msg = if can-find(first est-op
                         where est-op.company eq est-qty.company
                           and est-op.est-no  eq est-qty.est-no
                           and (est-op.qty    eq est-qty.eqty or est.est-type gt 1)
                           and est-op.auto    eq yes) then
               "(NOTE: This will DELETE any machines NOT manually added)"
             else
               "(NOTE: This will NOT DELETE any machines manually added)".
             
    IF INDEX(PROGRAM-NAME(2),"run-goto") EQ 0 THEN
       lv-msg1 = "Are you sure you want to build a new routing?".
    ELSE
       lv-msg1 = "Do you want to build a new routing?". 

    message lv-msg1 skip
            lv-msg
            view-as alert-box question
            button yes-no update ll-ans as log.
    if not ll-ans then return no-apply.
  end.

  SESSION:SET-WAIT-STATE("general").
  
  find xest where recid(xest) eq recid(est).

  IF xest.est-type EQ 1 THEN DO:
    ll = NO.

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

        RUN ce/mach-seq.p (b-est-qty.eqty).
      END.
    END.
  END.

  ELSE DO:  
    for each ef
        where ef.company eq est-qty.company
          and ef.est-no  eq est-qty.est-no
        no-lock:
      
      run set-lock (ef.form-no, no).
    end.    
    
    FIND FIRST xef WHERE xef.company = est-qty.company 
                     AND xef.est-no = est-qty.est-no
                   NO-LOCK NO-ERROR.
    FIND FIRST xeb WHERE xeb.company = est-qty.company 
                     AND xeb.est-no = est-qty.est-no
                     AND xeb.form-no = xef.form-no
                   NO-LOCK NO-ERROR.

    IF xest.est-type EQ 2 THEN RUN ce/box/mach-seq.p.

    ELSE
    IF xest.est-type EQ 3 THEN RUN ce/tan/mach-seq.p. 

    ELSE                       RUN ce/com/mach-seq.p (0).
  END.

  RUN release-shared-buffers.
  
  run dispatch ('open-query').

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
  DEF INPUT  PARAM ip-m-code LIKE est-op.m-code NO-UNDO.
  DEF OUTPUT PARAM op-first  AS   LOG           NO-UNDO.
  
  DEF BUFFER b-est-op FOR est-op.
  DEF BUFFER b-mach FOR mach.
  DEF BUFFER b2-mach FOR mach.

  DEF VAR v-mach-found AS LOG NO-UNDO.

  IF NOT AVAIL est-op THEN
     LEAVE.

  FOR EACH b-est-op FIELDS(m-code) WHERE
      b-est-op.company EQ est-op.company AND
      b-est-op.est-no  EQ est-op.est-no AND
      b-est-op.qty     EQ est-op.qty AND
      b-est-op.line    LT est-op.line
      NO-LOCK,
      FIRST b-mach FIELDS(sch-m-code) WHERE
            b-mach.company EQ est-op.company AND
            b-mach.m-code EQ est-op.m-code
            NO-LOCK,
      FIRST b2-mach FIELDS(sch-m-code) WHERE
            b2-mach.company EQ est-op.company AND
            b2-mach.m-code EQ b-est-op.m-code
            NO-LOCK:

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
  def var chr-handle as char no-undo.
  

  ll-import-stds = NO.

  run get-link-handle in adm-broker-hdl (this-procedure, "tableio-source", output chr-handle).
  run finish-new-record in widget-handle (chr-handle).
  
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
    FIRST bf-mach
        WHERE bf-mach.company EQ bf-est-op.company
        AND bf-mach.m-code eq bf-est-op.m-code
      NO-LOCK:
    
    ASSIGN
        bf-est-op.d-seq      = bf-mach.d-seq
        bf-est-op.dept       = bf-mach.dept[1]
        bf-est-op.op-sb      = bf-mach.p-type ne "B"
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
  def var j as int no-undo.
  def var v-outw     like xef.n-out no-undo.
  def var v-outl     like xef.n-out-l no-undo.
  def var v-rate     like est-op.op-rate no-undo.
  def var v-recid    as   recid no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  fil_id = recid(est-op).  /* for sub-program */
         
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  find xest where recid(xest) eq recid(est).
  
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq est-op.m-code
      no-lock no-error.

  find first xef
      where xef.company eq est-op.company
        and xef.est-no  eq est-op.est-no
        and xef.form-no eq est-op.s-num
      no-lock no-error.
  
  ASSIGN
   est-op.d-seq      = mach.d-seq
   est-op.dept       = mach.dept[1]
   est-op.op-sb      = mach.p-type ne "B"
   est-op.m-code     = mach.m-code
   est-op.m-dscr     = mach.m-dscr
   est-op.op-rate[1] = mach.mr-trate
   est-op.op-rate[2] = mach.run-trate.

  if est-op.op-crew[1] eq 0 or ll-import-selected then
    est-op.op-crew[1] = mach.mr-crusiz.

  if est-op.op-crew[2] eq 0 or ll-import-selected  then
    est-op.op-crew[2] = mach.run-crusiz.
         
  if ll-import-selected then 
    assign
     est-op.op-spoil = mach.run-spoil
     est-op.num-col  = 0
     est-op.num-coat = 0.

 IF ll-import-all THEN
     RUN ImportRouting.

  for each xop
      where xop.company eq est-op.company
        and xop.est-no  eq est-op.est-no
        and xop.line    lt 500
      break by xop.qty
            by xop.s-num
            by xop.b-num
            by xop.dept
            by xop.line:
            
    if first-of(xop.dept) then j = 0.
    
    assign
     j           = j + 1
     xop.op-pass = j.
  end.

  j = 0.
  for each xop
      where xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        and xop.line    lt 500
      by xop.qty by xop.s-num by xop.b-num by xop.d-seq by xop.op-pass:

    {sys/inc/outstrPL.i xop share}
    assign
     j        = j + 1
     xop.line = j.
     
    if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
  end.  
  
  assign
   fil_id  = recid(est-op)
   v-recid = fil_id.

  FOR EACH ef 
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef 
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no
      NO-LOCK:
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
  for each xop
      where xop.company eq est-qty.company
        and xop.est-no  eq est-qty.est-no
        and xop.line    lt 500
      NO-LOCK by xop.line descending:
    i = xop.line + 1.
    leave.
  end.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign
   est-op.company = est-qty.company
   est-op.est-no  = est-qty.est-no
   est-op.auto    = no
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
  DEF BUFFER xop FOR est-op.

  DEF VAR char-hdl AS CHAR NO-UNDO.


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
    FIND FIRST xop
        WHERE xop.company EQ est-qty.company
          AND xop.est-no  EQ est-qty.est-no
          AND xop.qty     EQ est-qty.eqty
        NO-LOCK NO-ERROR.
    IF NOT AVAIL xop THEN DO:
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
  run valid-s-num.
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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  
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
  def input parameter ip-add-update  as   char           no-undo.
  def input parameter ip-import-stds like ll-import-stds no-undo.

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
        AND mach.m-code EQ xop.m-code,
      FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "mach.obsolete"
        AND reftable.company  EQ mach.company
        AND reftable.loc      EQ mach.loc
        AND reftable.code     EQ mach.m-code
        AND reftable.val[1]   EQ 1:
    MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace or standards will not be imported"
        VIEW-AS ALERT-BOX ERROR.
    ip-import-stds = NO.
    LEAVE.
  END.
  
  ASSIGN
   ll-import-stds     = ip-import-stds
   ll-import-selected = ip-import-stds.

  if ip-add-update eq "update" then do with frame {&frame-name}:
    apply "entry" to est-op.s-num in browse {&browse-name}.
  end.

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
  def input parameter ip-form-no like ef.form-no no-undo.
  def input parameter ip-op-lock like ef.op-lock no-undo.
  
  find first ef
      where ef.company eq est-qty.company
        and ef.est-no  eq est-qty.est-no
        and ef.form-no eq ip-form-no
      no-error.

  if avail ef then do:

     /*task 020050908*/
     IF ip-op-lock EQ ef.op-lock THEN
     DO:
        {est/op-lock.i xest}
        
        ASSIGN
           op-lock.val[1] = INT(NOT ip-op-lock)
           op-lock.val[2] = op-lock.val[1].

        RELEASE op-lock.
     END.

     ef.op-lock = ip-op-lock.

     release ef.
  end.
  
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
  
  do with frame {&frame-name}:
    APPLY "value-changed" TO est-op.m-code IN BROWSE {&browse-name}.

    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF ((AVAIL mach AND mach.p-type EQ "B") OR
        int(est-op.b-num:screen-value in browse {&browse-name}) NE 0) and
       not can-find(first eb
                    where eb.company  eq est-qty.company
                      and eb.est-no   eq est-qty.est-no
                      and eb.form-no  eq int(est-op.s-num:screen-value in browse {&browse-name})
                      and eb.blank-no eq int(est-op.b-num:screen-value in browse {&browse-name}))
    then do:
      message "Must enter a valid Blank#" view-as alert-box error.
      apply "entry" to est-op.b-num in browse {&browse-name}.
      RETURN ERROR.
    end.
  end.

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
  def var i as int no-undo.
  def var chr-handle as char no-undo.
  def var ls-tmp as cha no-undo.
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER xop FOR est-op.  
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-mach FOR mach.
  

  do with frame {&frame-name}:
    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL mach THEN v-msg = "Must enter a valid Machine Code, try help".

    IF v-msg EQ "" THEN
      IF CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "mach.obsolete"
                    AND reftable.company  EQ mach.company
                    AND reftable.loc      EQ mach.loc
                    AND reftable.code     EQ mach.m-code
                    AND reftable.val[1]   EQ 1) THEN
        v-msg = "Machine is obsolete, please choose a different machine".

    IF v-msg EQ "" THEN DO:
      FIND FIRST b-ef
          WHERE b-ef.company EQ est-qty.company
            AND b-ef.est-no  EQ est-qty.est-no
            AND b-ef.form-no EQ INT(est-op.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      FIND FIRST b-eb
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
            AND b-eb.blank-no EQ INT(est-op.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL b-eb THEN
      FIND FIRST b-eb
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
          NO-LOCK NO-ERROR.

      find xest where recid(xest) eq recid(est) no-lock.

      ASSIGN
         lv-d-seq = mach.d-seq
         lv-dept  = mach.dept[1].

      IF LOOKUP(lv-dept,"RC,GU") NE 0 AND AVAIL b-ef                     AND
         (adm-adding-record OR
          INT(est-op.n-out:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0) THEN
        est-op.n-out:SCREEN-VALUE IN BROWSE {&browse-name} =
            IF lv-dept EQ "RC" THEN STRING(b-ef.n-out) ELSE STRING(b-ef.n-out-l).
  
      if mach.p-type ne "B" then
        assign
         lv-op-sb = yes
         lv-b-num = 0.
       
      else
        assign
         lv-op-sb = no
         lv-b-num = if int(est-op.b-num:screen-value) eq 0 then 1 else int(est-op.b-num:screen-value).
       
      if lookup(lv-dept,"RC,GU") eq 0 then lv-n-out = 0.

      IF lv-dept EQ "PR" THEN DO:
        RUN ce/mach-ink.p.

        i = INT(adm-new-record).
        FOR EACH xop
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
            NO-LOCK BY xop.d-seq BY xop.line:

          i = i + 1.
          IF ROWID(xop) EQ ROWID(est-op) THEN LEAVE.
        END.
       
        FIND FIRST w-ink
            WHERE w-ink.form-no EQ b-ef.form-no
              AND w-ink.pass    EQ i
            NO-ERROR.

        IF AVAIL w-ink THEN DO:
          IF INT(est-op.num-col:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
            w-ink.inks = INT(est-op.num-col:SCREEN-VALUE IN BROWSE {&browse-name}).
          IF INT(est-op.num-coat:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
            w-ink.varn = INT(est-op.num-coat:SCREEN-VALUE IN BROWSE {&browse-name}).

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
         mach.min-cal gt b-ef.cal THEN
        v-msg = "BOARD CALIPER too small for Machine!".

      IF v-msg EQ ""             AND
         mach.max-cal lt b-ef.cal THEN
        v-msg = "BOARD CALIPER too large for Machine!".

      qty = est-qty.eqty.

      FIND xef WHERE ROWID(xef) EQ ROWID(b-ef) NO-LOCK NO-ERROR.
      FIND xeb WHERE ROWID(xeb) EQ ROWID(b-eb) NO-LOCK NO-ERROR.
        
      run ce/mach-qty.p (ROWID(mach)).

      IF v-msg EQ ""               AND
         v-chk-qty lt mach.min-run THEN
        v-msg = "RUN QTY. too small for Machine!".

      IF v-msg EQ ""               AND
         v-chk-qty gt mach.max-run THEN
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
  
  do with frame {&frame-name}:
    if not can-find(first ef
                    where ef.company eq est-qty.company
                      and ef.est-no  eq est-qty.est-no
                      and ef.form-no eq int(est-op.s-num:screen-value in browse {&browse-name}))
    then do:
      message "Must enter a valid Form#" view-as alert-box error.
      apply "entry" to est-op.s-num in browse {&browse-name}.
      RETURN ERROR.
    end.
  end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

