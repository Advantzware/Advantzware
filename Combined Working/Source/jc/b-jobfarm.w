&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: jc/b-jobmat.w

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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR li-est-type LIKE est.est-type NO-UNDO.
DEF VAR ll-check-qty AS LOG NO-UNDO.
DEF VAR ll-commit AS LOG NO-UNDO.
DEF VAR lv-allocated AS LOG NO-UNDO.
DEF VAR prev-cost AS DEC NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR lv-sort-by AS CHAR NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR NO-UNDO.    
DEF VAR hHistory AS HANDLE NO-UNDO.
{cec/bestfitc.i NEW SHARED}

{windows/l-jobmt1.i NEW}

&SCOPED-DEFINE sortby-log ~
    IF lv-sort-by EQ "i-no" THEN job-farm.i-no ELSE ~
    IF lv-sort-by EQ "i-name" THEN itemfg.i-name ELSE ~
    IF lv-sort-by EQ "wid" THEN STRING(job-farm.wid,'>>9.99<<') ELSE ~
    IF lv-sort-by EQ "len" THEN STRING(job-farm.len,'>>9.99<<') ELSE ~
    STRING(job-farm.frm,'>>9')
&SCOPED-DEFINE sortby BY job-farm.frm BY job-farm.blank-no
&SCOPED-DEFINE sortby-phrase-asc BY ({&sortby-log}) {&sortby}
&SCOPED-DEFINE sortby-phrase-desc BY ({&sortby-log}) DESC {&sortby}

{sys/inc/rmissue.i}

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

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
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-farm itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-farm.frm job-farm.blank-no ~
job-farm.i-no itemfg.i-name job-farm.std-cost job-farm.sc-uom ~
job-farm.cost-m job-farm.qty job-farm.qty-uom job-farm.est-cost ~
job-farm.est-uom job-farm.est-setup job-farm.qty-mr job-farm.qty-iss ~
job-farm.vend-po-qty job-farm.po-no job-farm.pur-uom job-farm.po-setup ~
job-farm.std-tot-cost job-farm.act-tot-cost job-farm.all-flg ~
job-farm.act-cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table job-farm.frm ~
job-farm.blank-no job-farm.i-no job-farm.std-cost job-farm.sc-uom ~
job-farm.cost-m job-farm.qty job-farm.qty-uom job-farm.est-cost ~
job-farm.est-uom job-farm.est-setup job-farm.qty-mr job-farm.qty-iss ~
job-farm.vend-po-qty job-farm.po-no job-farm.pur-uom job-farm.po-setup ~
job-farm.std-tot-cost job-farm.act-tot-cost job-farm.all-flg 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-farm
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-farm
&Scoped-define QUERY-STRING-br_table FOR EACH job-farm WHERE job-farm.company = job.company ~
  AND job-farm.job = job.job ~
  AND job-farm.job-no = job.job-no ~
  AND job-farm.job-no2 = job.job-no2 ~
use-index seq-idx NO-LOCK, ~
      FIRST itemfg OF job-farm ~
      WHERE itemfg.company EQ job-farm.company AND ~
itemfg.i-no EQ job-farm.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job-farm WHERE job-farm.company = job.company ~
  AND job-farm.job = job.job ~
  AND job-farm.job-no = job.job-no ~
  AND job-farm.job-no2 = job.job-no2 ~
use-index seq-idx NO-LOCK, ~
      FIRST itemfg OF job-farm ~
      WHERE itemfg.company EQ job-farm.company AND ~
itemfg.i-no EQ job-farm.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-farm itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-farm
&Scoped-define SECOND-TABLE-IN-QUERY-br_table itemfg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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
company||y|ASI.job-farm.company
j-no||y|ASI.job-farm.j-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,j-no"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job-farm, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      job-farm.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 4 COLUMN-FONT 0
            LABEL-BGCOLOR 14
      job-farm.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 4
            COLUMN-FONT 0
      job-farm.i-no FORMAT "x(15)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      itemfg.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      job-farm.std-cost FORMAT ">>>,>>9.99<<":U
      job-farm.sc-uom COLUMN-LABEL "Cost!UOM" FORMAT "x(4)":U WIDTH 7
      job-farm.cost-m COLUMN-LABEL "Set Cost/M" FORMAT "->,>>9.9999":U
      job-farm.qty FORMAT "->>>,>>>,>>9.9<<<<<":U
      job-farm.qty-uom COLUMN-LABEL "Qty!UOM" FORMAT "x(4)":U WIDTH 7
      job-farm.est-cost COLUMN-LABEL "Estimate Cost" FORMAT "->>>,>>9.99<<":U
            WIDTH 19.4
      job-farm.est-uom FORMAT "x(3)":U WIDTH 10.2
      job-farm.est-setup FORMAT ">>,>>9.99":U WIDTH 15.2
      job-farm.qty-mr FORMAT "->>>>9.99<<<<":U
      job-farm.qty-iss FORMAT "->>,>>9.99<<<<":U
      job-farm.vend-po-qty FORMAT "->>,>>>,>>9":U
      job-farm.po-no COLUMN-LABEL "Vendor PO" FORMAT "x(15)":U
      job-farm.pur-uom COLUMN-LABEL "PO UOM" FORMAT "x(3)":U
      job-farm.po-setup FORMAT ">>,>>9.99":U WIDTH 14.6
      job-farm.std-tot-cost COLUMN-LABEL "Total Std Cost" FORMAT "->>>,>>9.99<<":U
            WIDTH 20.2
      job-farm.act-tot-cost COLUMN-LABEL "Total Act Cost" FORMAT "->>>,>>9.99<<":U
            WIDTH 21.8
      job-farm.all-flg COLUMN-LABEL "Committed?" FORMAT "Y/N":U
            WIDTH 15.2
      job-farm.act-cost COLUMN-LABEL "Actual Cost/M" FORMAT "->>>,>>9.99<<":U
            WIDTH 19.2
  ENABLE
      job-farm.frm
      job-farm.blank-no
      job-farm.i-no
      job-farm.std-cost
      job-farm.sc-uom
      job-farm.cost-m
      job-farm.qty
      job-farm.qty-uom
      job-farm.est-cost
      job-farm.est-uom
      job-farm.est-setup
      job-farm.qty-mr
      job-farm.qty-iss
      job-farm.vend-po-qty
      job-farm.po-no
      job-farm.pur-uom
      job-farm.po-setup
      job-farm.std-tot-cost
      job-farm.act-tot-cost
      job-farm.all-flg
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 11.67
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.job
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
         HEIGHT             = 11.67
         WIDTH              = 146.
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
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.job-farm WHERE ASI.job   ...,ASI.itemfg OF ASI.job-farm"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _JoinCode[1]      = "ASI.job-farm.company = ASI.job.company
  AND ASI.job-farm.job = ASI.job.job
  AND ASI.job-farm.job-no = ASI.job.job-no
  AND ASI.job-farm.job-no2 = ASI.job.job-no2
use-index seq-idx"
     _Where[2]         = "itemfg.company EQ job-farm.company AND
itemfg.i-no EQ job-farm.i-no"
     _FldNameList[1]   > ASI.job-farm.frm
"job-farm.frm" "S" ">>>" "integer" ? ? 0 14 ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-farm.blank-no
"job-farm.blank-no" "B" ">>>" "integer" ? ? 0 ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-farm.i-no
"job-farm.i-no" ? ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.itemfg.i-name
"itemfg.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.job-farm.std-cost
"job-farm.std-cost" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-farm.sc-uom
"job-farm.sc-uom" "Cost!UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job-farm.cost-m
"job-farm.cost-m" "Set Cost/M" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-farm.qty
"job-farm.qty" ? "->>>,>>>,>>9.9<<<<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.job-farm.qty-uom
"job-farm.qty-uom" "Qty!UOM" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.job-farm.est-cost
"job-farm.est-cost" "Estimate Cost" ? "decimal" ? ? ? ? ? ? yes ? no no "19.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.job-farm.est-uom
"job-farm.est-uom" ? ? "character" ? ? ? ? ? ? yes ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.job-farm.est-setup
"job-farm.est-setup" ? ? "decimal" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.job-farm.qty-mr
"job-farm.qty-mr" ? "->>>>9.99<<<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.job-farm.qty-iss
"job-farm.qty-iss" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.job-farm.vend-po-qty
"job-farm.vend-po-qty" ? "->>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.job-farm.po-no
"job-farm.po-no" "Vendor PO" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.job-farm.pur-uom
"job-farm.pur-uom" "PO UOM" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.job-farm.po-setup
"job-farm.po-setup" ? ? "decimal" ? ? ? ? ? ? yes ? no no "14.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.job-farm.std-tot-cost
"job-farm.std-tot-cost" "Total Std Cost" ? "decimal" ? ? ? ? ? ? yes ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.job-farm.act-tot-cost
"job-farm.act-tot-cost" "Total Act Cost" ? "decimal" ? ? ? ? ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.job-farm.all-flg
"job-farm.all-flg" "Committed?" "Y/N" "logical" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.job-farm.act-cost
"job-farm.act-cost" "Actual Cost/M" ? "decimal" ? ? ? ? ? ? no ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON HELP OF br_table IN FRAME F-Main
DO:
  def VAR char-val AS CHAR no-undo. 
  DEF VAR lv-indus AS CHAR INIT "" NO-UNDO.
  DEF VAR lv-matyp LIKE itemfg.mat-type INIT "" NO-UNDO.


  IF AVAIL job-farm THEN DO:
    FIND FIRST itemfg
        WHERE itemfg.company EQ g_company
          AND itemfg.i-no    EQ job-farm.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      lv-matyp = itemfg.mat-type.

      IF itemfg.mat-type EQ "B" THEN lv-indus = "IN".
    END.
  END.
  
  case focus:NAME:
      when "i-no" then do:
        run windows/l-item3.w (g_company,lv-indus,lv-matyp,job-farm.i-no:screen-value IN BROWSE {&browse-name}, output char-val).
        if char-val <> "" then do:
          FIND itemfg WHERE RECID(itemfg) EQ int(char-val) NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
            job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-no.
            APPLY "value-changed" TO job-farm.i-no IN BROWSE {&browse-name}.
          END.
        end.                
      end. 
  end case.
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  DEF VAR ld AS DEC NO-UNDO.


  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

  RUN calc-job-qty (OUTPUT ld).
  prev-cost = ld * DEC(job-farm.std-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.
  
  ASSIGN
    lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
    lv-column-nam = lh-column:NAME
    lv-column-lab = lh-column:LABEL.

  IF NOT CAN-DO('frm,i-no,i-name,wid,len',lh-column:NAME) THEN RETURN.
  
  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
  ELSE
  ASSIGN
    lv-sort-by     = lv-column-nam
    lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
  
  IF ll-sort-asc THEN
  OPEN QUERY {&BROWSE-NAME} {&QUERY-STRING-{&BROWSE-NAME}} {&sortby-phrase-asc}.
  ELSE
  OPEN QUERY {&BROWSE-NAME} {&QUERY-STRING-{&BROWSE-NAME}} {&sortby-phrase-desc}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.frm br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.frm IN BROWSE br_table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.blank-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.blank-no IN BROWSE br_table /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.i-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-farm.i-no IN BROWSE br_table /* Item# */
DO:
/*   IF DEC(job-farm.qty-all:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND      */
/*      job-farm.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Y"    THEN DO: */
/*     IF KEYFUNCTION(LASTKEY) EQ "back-tab" THEN                                 */
/*       APPLY "back-tab" TO {&self-name}.                                        */
/*     ELSE                                                                       */
/*       APPLY "tab" TO {&self-name}.                                             */
/*     RETURN NO-APPLY.                                                           */
/*   END.                                                                         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.i-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.i-no IN BROWSE br_table /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.i-no br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-farm.i-no IN BROWSE br_table /* Item# */
DO:
  RUN new-i-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.std-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.std-cost br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.std-cost IN BROWSE br_table /* Costs */
DO:
  DEF VAR v-uom LIKE job-farm.sc-uom NO-UNDO. 
  DEF VAR v-qty LIKE job-farm.qty NO-UNDO.
  DEF VAR v-cost LIKE job-farm.std-cost NO-UNDO.
  DEF VAR v-cost-m LIKE job-farm.cost-m NO-UNDO.
  /* Copied from new-i-mat */
  FOR EACH job-hdr
      WHERE job-hdr.company  EQ job-farm.company
        AND job-hdr.job      EQ job-farm.job
        AND job-hdr.job-no   EQ job-farm.job-no
        AND job-hdr.job-no2  EQ job-farm.job-no2
        AND job-hdr.frm      EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})
      NO-LOCK
      BY job-hdr.qty / IF job-hdr.n-on EQ 0 THEN 1 ELSE job-hdr.n-on DESC:
    LEAVE.
  END.

  ASSIGN
         v-cost   = decimal(job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name})
         v-uom    = job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   
         v-cost-m = decimal(job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   )
         v-qty    = decimal(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name})
         v-uom    = job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  .

  IF AVAIL(job-hdr) AND job-farm.len GT 0 AND job-farm.wid GT 0 AND v-qty GT 0
     AND v-cost GT 0 AND job-hdr.qty GT 0 AND job-farm.sc-uom = "MSF" THEN
     v-cost-m = ((job-farm.LEN * job-farm.wid / 144) * (v-qty / 1000) * v-cost)
                      / job-hdr.qty * 1000.
   IF v-cost-m EQ ? THEN v-cost-m = 0.

   job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost-m).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.sc-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.sc-uom br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.sc-uom IN BROWSE br_table /* Cost!UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom ({&self-name}:NAME IN BROWSE {&browse-name},
                   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*IF job-farm.qty-uom:SCREEN-VALUE <> job-farm.sc-uom:SCREEN-VALUE THEN DO:
       RUN calc-qty.
    END.
    ELSE job-farm.qty-all:SCREEN-VALUE = job-farm.qty:SCREEN-VALUE.*/

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.qty-uom br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm.qty-uom IN BROWSE br_table /* Qty!UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom ({&self-name}:NAME IN BROWSE {&browse-name},
                   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*IF job-farm.qty-uom:SCREEN-VALUE <> job-farm.sc-uom:SCREEN-VALUE THEN DO:
       RUN calc-qty.
    END.
    ELSE job-farm.qty-all:SCREEN-VALUE = job-farm.qty:SCREEN-VALUE.*/

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm.all-flg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm.all-flg br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-farm.all-flg IN BROWSE br_table /* Committed? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN check-for-commit (OUTPUT ll-commit).

    IF NOT ll-commit THEN DO:
      {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = "no".
      APPLY "row-leave" TO BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SUBSCRIBE TO 'refresh-job-farm' ANYWHERE.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-job-qty B-table-Win 
PROCEDURE calc-job-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-qty AS DEC DECIMALS 10 NO-UNDO.

  DEF BUFFER bf-itemfg FOR itemfg.

  def var v-bwt       like itemfg.weight-100                      no-undo.
  DEF var v-len       like itemfg.t-len                        no-undo.
  def var v-wid       like itemfg.t-wid                        no-undo.


/*   DO WITH FRAME {&FRAME-NAME}:                                                         */
/*     FIND FIRST bf-itemfg                                                               */
/*         WHERE bf-itemfg.company EQ job-farm.company                                    */
/*           AND bf-itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} */
/*         NO-LOCK NO-ERROR.                                                              */
/*     IF NOT AVAIL BF-itemfg THEN RETURN.                                                */
/*                                                                                        */
/*     ASSIGN                                                                             */
/*      v-bwt = DEC(job-farm.basis-w:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})               */
/*      v-len = DEC(job-farm.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                   */
/*      v-wid = DEC(job-farm.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).                  */
/*                                                                                        */
/*     IF v-len EQ 0 THEN v-len = bf-itemfg.t-len.                                        */
/*                                                                                        */
/*     IF v-wid EQ 0 THEN                                                                 */
/*       v-wid = IF bf-itemfg.t-wid NE 0 THEN bf-itemfg.t-wid ELSE bf-itemfg.t-wid.       */
/*                                                                                        */
/*     IF v-bwt EQ 0 THEN v-bwt = bf-itemfg.weight-100.                                   */
/*                                                                                        */
/*     op-qty = DEC(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}).                  */
/*                                                                                        */
/*     IF job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE                       */
/*        job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}  THEN                     */
/*       RUN sys/ref/convquom.p(job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name},   */
/*                              job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name},    */
/*                              v-bwt, v-len, v-wid, bf-itemfg.t-dep,                     */
/*                              op-qty, OUTPUT op-qty).                                   */
/*   END.                                                                                 */

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
  DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER bf-itemfg FOR itemfg.


  DO WITH FRAME {&FRAME-NAME}:
/*     FIND FIRST bf-itemfg                                                               */
/*         WHERE bf-itemfg.company EQ job-farm.company                                    */
/*           AND bf-itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} */
/*           AND bf-itemfg.i-code  EQ "R"                                                 */
/*         NO-LOCK NO-ERROR.                                                              */
/*     IF NOT AVAIL BF-itemfg THEN RETURN.                                                */
/*     RUN calc-job-qty (OUTPUT ld).                                                      */
/*     job-farm.qty-all:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld).               */
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-commit B-table-Win 
PROCEDURE check-for-commit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-commit AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND itemfg.i-code  EQ "R"
        NO-LOCK NO-ERROR.

/*     op-commit = INDEX("LRAW",job.stat) GT 0                               AND                    */
/*                 job-farm.post:SCREEN-VALUE IN BROWSE {&browse-name} EQ "N" AND                   */
/*                 AVAIL itemfg AND                                                                 */
/*                 (lv-allocated OR job-farm.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} EQ "y"). */
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE issue-mat B-table-Win 
PROCEDURE issue-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL job-farm THEN RUN jc/issuemat.p (ROWID(job-farm)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF ll-commit THEN RUN jc/jc-all2.p (ROWID(job-farm), -1).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  job-farm.i-no = job-farm.i-no.

  IF ll-commit THEN DO:
    job-farm.all-flg = YES.  
    RUN jc/jc-all2.p (ROWID(job-farm), 1).
  END.

  ELSE job-farm.all-flg = NO. 

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
  ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = NO
           job-farm.BLANK-no:READ-ONLY = NO 
           job-farm.i-no:READ-ONLY = NO 
           job-farm.std-cost:READ-ONLY = NO 
           job-farm.sc-uom:READ-ONLY = NO 
           job-farm.cost-m:READ-ONLY = NO 
           job-farm.qty:READ-ONLY = NO 
           job-farm.qty-uom:READ-ONLY = NO .
/*            job-farm.wid:READ-ONLY = NO  */
/*            job-farm.len:READ-ONLY = NO  */
/*            job-farm.n-up:READ-ONLY = NO */
/*            job-farm.basis-w:READ-ONLY = NO */
/*            job-farm.post:READ-ONLY = NO.   */

  ASSIGN lv-allocated = NO
         ll-commit  = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN windows/l-jobmt1.w (STRING(ROWID(job-farm)), "", 0, "", OUTPUT char-val, OUTPUT look-recid).
  IF CAN-FIND(FIRST w-po-ordl) THEN RUN update-job-farms (ROWID(job-farm)).

  /* Dispatch standard ADM method.                             */
  ELSE RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   job-farm.company = cocode
   job-farm.job     = job.job
   job-farm.job-no  = job.job-no
   job-farm.job-no2 = job.job-no2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
/*     RUN jc/maydeletejob-farm.p (BUFFER job-farm, OUTPUT ll).      */
/*                                                                   */
/*     IF ll THEN DO:                                                */
/*       {custom/askdel.i}                                           */
/*     END.                                                          */
/*                                                                   */
/*     ELSE                                                          */
/*     IF ll EQ ? THEN DO:                                           */
/*       ll = NO.                                                    */
/*       MESSAGE "Material has been Allocated, delete anyway?"       */
/*           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                */
/*           UPDATE ll.                                              */
/*     END.                                                          */
/*                                                                   */
/*     ELSE                                                          */
/*       MESSAGE "Sorry, this RM has been processed for this job " + */
/*               "and may not be deleted..."                         */
/*           VIEW-AS ALERT-BOX ERROR.                                */
/*                                                                   */
/*     IF NOT ll THEN RETURN ERROR.                                  */
  END.

  FIND CURRENT job.
  RUN jc/jc-all.p (ROWID(job-farm), -1, INPUT-OUTPUT job.stat).
  FIND CURRENT job NO-LOCK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hHistory) THEN
      RUN adm-destroy IN hHistory.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-check-qty = NO.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO job-farm.frm IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF VALID-HANDLE(hHistory) THEN
      DELETE OBJECT hHistory.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.
  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
   ASSIGN
    li-est-type = IF AVAIL est THEN est.est-type ELSE 0
    li-est-type = li-est-type - (IF li-est-type GT 4 THEN 4 ELSE 0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT AVAIL job-farm THEN RUN dispatch ('open-query').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-frm NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-uom ("sc-uom", job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-uom ("qty-uom", job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-qty-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN check-for-commit (OUTPUT ll-commit).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*
  IF ll-commit THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    FIND FIRST job-hdr OF job-farm NO-LOCK NO-ERROR.
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(job-hdr)).
  END.
*/
  ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = NO
           job-farm.BLANK-no:READ-ONLY = NO 
           job-farm.i-no:READ-ONLY = NO 
           job-farm.std-cost:READ-ONLY = NO 
           job-farm.sc-uom:READ-ONLY = NO 
           job-farm.cost-m:READ-ONLY = NO 
           job-farm.qty:READ-ONLY = NO 
           job-farm.qty-uom:READ-ONLY = NO 
/*            job-farm.wid:READ-ONLY = NO     */
/*            job-farm.len:READ-ONLY = NO     */
/*            job-farm.n-up:READ-ONLY = NO    */
/*            job-farm.basis-w:READ-ONLY = NO */
/*            job-farm.post:READ-ONLY = NO    */
           lv-allocated = NO
           ll-commit = NO.
/* wfk - this was erroring out */
/*   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). */
/*   RUN view-user-id IN WIDGET-HANDLE(char-hdl).                                           */
/*   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"job-target",OUTPUT char-hdl).    */
/*   RUN view-user-id IN WIDGET-HANDLE(char-hdl).                                           */

  RUN dispatch ("display-fields").

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no B-table-Win 
PROCEDURE new-i-no PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-uom LIKE job-farm.sc-uom NO-UNDO. 
  DEF VAR v-qty LIKE job-farm.qty NO-UNDO.
  DEF VAR v-cost LIKE job-farm.std-cost NO-UNDO.
  DEF VAR v-cost-m LIKE job-farm.cost-m NO-UNDO.
  DEF VAR j AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgW.i}
          AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
        
    IF AVAIL itemfg THEN DO:
      
      IF adm-new-record OR itemfg.i-code EQ "R" THEN DO:
        /*job-farm.basis-w:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.weight-100).
        
        IF itemfg.t-wid NE 0 THEN
          job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-wid).
        ELSE
          ASSIGN
            job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-wid)
            job-farm.len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-len).
          */
        job-farm.sc-uom:SCREEN-VALUE = itemfg.cons-uom.
        /*IF job-farm.qty-uom:SCREEN-VALUE <> job-farm.sc-uom:SCREEN-VALUE THEN
           RUN calc-qty.
        ELSE job-farm.qty-all:SCREEN-VALUE = job-farm.qty:SCREEN-VALUE.*/
      END.
      
      itemfg.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.

      FIND FIRST e-itemfg OF itemfg NO-LOCK NO-ERROR.

      IF job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF itemfg.i-code EQ "E" AND AVAIL e-itemfg THEN e-itemfg.std-uom ELSE itemfg.cons-uom.
                            
      IF job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF itemfg.i-code EQ "E" AND AVAIL e-itemfg THEN e-itemfg.std-uom ELSE itemfg.cons-uom.

      IF adm-adding-record THEN DO:
        FOR EACH job-hdr
            WHERE job-hdr.company  EQ job-farm.company
              AND job-hdr.job      EQ job-farm.job
              AND job-hdr.job-no   EQ job-farm.job-no
              AND job-hdr.job-no2  EQ job-farm.job-no2
              AND job-hdr.frm      EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            NO-LOCK
            BY job-hdr.qty / IF job-hdr.n-on EQ 0 THEN 1 ELSE job-hdr.n-on DESC:
          LEAVE.
        END.

        ASSIGN
         v-uom  = job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}
         v-qty  = INT(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name})
         v-cost = 0.

        IF itemfg.i-code EQ "R" THEN
          v-cost = IF ce-ctrl.r-cost THEN itemfg.avg-cost ELSE itemfg.last-cost.

        ELSE
        IF AVAIL e-itemfg THEN DO:
          FIND FIRST e-itemfg-vend OF e-itemfg
              WHERE e-itemfg-vend.item-type EQ YES
              NO-LOCK NO-ERROR.

          EMPTY TEMP-TABLE tt-eiv.
          EMPTY TEMP-TABLE tt-ei.

          IF AVAIL e-itemfg-vend THEN
          DO:
             CREATE tt-eiv.
             DO j = 1 TO 10:
                ASSIGN
                   tt-eiv.run-qty[j] = e-itemfg-vend.run-qty[j]
                   tt-eiv.run-cost[j] = e-itemfg-vend.run-cost[j].
             END.
            

            
             IF AVAIL b-qty THEN
             DO:

            
                DO j = 1 TO 10:
                   ASSIGN
                      tt-eiv.run-qty[j + 10] = e-item-vend.runQtyXtra[j]
                      tt-eiv.run-cost[j + 10] = e-item-vend.runCostXtra[j].
                END.
             END.

             DO j = 1 TO 20:
                IF tt-eiv.run-qty[j] GE v-qty THEN DO:
                   v-cost = tt-eiv.run-cost[j].
                   LEAVE.
                END.
             END.
          END.
          ELSE
          DO:
             CREATE tt-ei.
             DO j = 1 TO 10:
                ASSIGN
                   tt-ei.run-qty[j] = e-itemfg.run-qty[j]
                   tt-ei.run-cost[j] = e-itemfg.run-cost[j].
             END.
            
             FIND FIRST b-qty WHERE
                  b-qty.reftable = "blank-vend-qty" AND
                  b-qty.company = e-itemfg.company AND
                      b-qty.CODE    = e-itemfg.i-no
                  NO-LOCK NO-ERROR.
            
             IF AVAIL b-qty THEN
             DO:
                FIND FIRST b-cost WHERE
                     b-cost.reftable = "blank-vend-cost" AND
                     b-cost.company = e-itemfg.company AND
                         b-cost.CODE    = e-itemfg.i-no
                     NO-LOCK NO-ERROR.
            
                DO j = 1 TO 10:
                   ASSIGN
                      tt-ei.run-qty[j + 10] = b-qty.val[j]
                      tt-ei.run-cost[j + 10] = b-cost.val[j].
                END.
             END.

             DO j = 1 TO 20:
                IF tt-ei.run-qty[j] GE v-qty THEN DO:
                   v-cost = tt-ei.run-cost[j].
                   LEAVE.
                END.
             END.
          END.

/*           IF AVAIL e-itemfg-vend THEN                                                      */
/*             RUN est/dim-charge.p (e-itemfg-vend.rec_key,                                   */
/*                                   DEC(job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name}), */
/*                                   DEC(job-farm.len:SCREEN-VALUE IN BROWSE {&browse-name}), */
/*                                   INPUT-OUTPUT v-cost).                                    */
        END.

        /* 02201203 */
        ASSIGN
           v-cost   = decimal(job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name})
           v-uom    = job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   
           v-cost-m = decimal(job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   )
           v-qty    = decimal(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name})
           v-uom    = job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  .
        IF AVAIL(job-hdr) THEN 
           IF  job-farm.len GT 0 AND job-farm.wid GT 0 AND v-qty GT 0
           AND v-cost GT 0 AND job-hdr.qty GT 0 AND job-farm.sc-uom = "MSF" THEN
           v-cost-m = ((job-farm.LEN * job-farm.wid / 144) * (v-qty / 1000) * v-cost)
                       / job-hdr.qty * 1000.        
         ELSE
           v-cost-m = (v-qty * v-cost) / (job-hdr.qty / 1000). 

        IF v-cost-m EQ ? THEN v-cost-m = 0.

        ASSIGN
         job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost)
         job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   = v-uom
         job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(v-cost-m)
         job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(v-qty)
         job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = v-uom.
      END.

      IF NOT adm-adding-record AND prev-cost NE 0 THEN DO:
        RUN calc-job-qty (OUTPUT v-qty).
        job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(prev-cost / v-qty).
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-browser B-table-Win 
PROCEDURE refresh-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF BUFFER b-job-hdr FOR job-hdr.

   IF AVAIL job-farm THEN lv-rowid = ROWID(job-farm).
   RUN dispatch ("open-query").
   RUN repo-query (lv-rowid).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-job-farm B-table-Win 
PROCEDURE refresh-job-farm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN refresh-browser.
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
                                                                

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-alloc B-table-Win 
PROCEDURE run-alloc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-ask AS LOG NO-UNDO.

  DEF VAR lv-alloc-char AS cha NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.

  DEF BUFFER b-job-farm FOR job-farm.


  IF INDEX("LRAW",job.stat) = 0 THEN DO:
     MESSAGE "The job status must be 'R'eleased in order to perform this selection"
            VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  /*IF job-farm.post:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Y" THEN DO:
     MESSAGE "It's posted. Unpost first..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.*/

  FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND itemfg.i-code  EQ "R"
        NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
      MESSAGE "Estimated Material cannot be Allocated, Press Update Key to Add Real Material ..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  lv-allocated = YES.

   /*

  def var phandle as widget-handle no-undo.
  def var char-hdl as cha no-undo.   
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).

    RUN new-state in phandle ('update-begin':U).
  ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = YES
           job-farm.BLANK-no:READ-ONLY = YES
           job-farm.i-no:READ-ONLY = YES
           job-farm.std-cost:READ-ONLY = YES
           job-farm.sc-uom:READ-ONLY = YES
           job-farm.cost-m:READ-ONLY = YES
           job-farm.qty:READ-ONLY = YES
           job-farm.qty-uom:READ-ONLY = YES
           job-farm.wid:READ-ONLY = YES
           job-farm.len:READ-ONLY = YES
           job-farm.n-up:READ-ONLY = YES
           job-farm.basis-w:READ-ONLY = YES
           job-farm.post:READ-ONLY = YES.  

  

    APPLY "entry" TO job-farm.qty-all IN BROWSE {&browse-name}.
 */
   IF ip-ask THEN DO:
     IF AVAIL job-farm AND job-farm.all-flg THEN lv-alloc-char = "Deallocate?".
     ELSE lv-alloc-char = "Allocate?".

     MESSAGE "Are you sure you want to " lv-alloc-char VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll.
   END.

   ELSE ll = YES.

   IF ll THEN DO:
      FIND b-job-farm WHERE ROWID(b-job-farm) EQ ROWID(job-farm) EXCLUSIVE-LOCK.
      b-job-farm.all-flg = YES.  
      IF lv-alloc-char BEGINS "alloc" THEN RUN jc/jc-all2.p (ROWID(b-job-farm), 1).
      ELSE RUN jc/jc-all2.p (ROWID(b-job-farm), -1).
      FIND CURRENT b-job-farm EXCLUSIVE-LOCK.
      IF b-job-farm.qty-all EQ 0 AND
         NOT b-job-farm.all-flg  THEN b-job-farm.qty-all = b-job-farm.qty.
      FIND CURRENT b-job-farm NO-LOCK.
      RUN dispatch ("display-fields").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "job-farm" "company"}
  {src/adm/template/sndkycas.i "j-no" "job-farm" "j-no"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "job-farm"}
  {src/adm/template/snd-list.i "itemfg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sheet-calc B-table-Win 
PROCEDURE sheet-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  IF AVAIL job-farm THEN DO:
    lv-rowid = ROWID(job-farm).

    RUN jc/shtcalc.p (INPUT-OUTPUT lv-rowid).

    IF lv-rowid NE ? THEN DO:
      RUN dispatch ("open-query").

      RUN repo-query (lv-rowid).

      RUN dispatch ("display-fields").

      IF rmissue-log THEN do:
         ll = NO.
         MESSAGE "Would you like to issue RM?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
         IF ll THEN RUN issue-mat.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-history B-table-Win 
PROCEDURE show-history :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


RUN jc/w-jf-rct.w  PERSISTENT SET hHistory  (INPUT job-farm.job-no,
                      INPUT job-farm.job-no2,
                      INPUT job-farm.i-no) .
RUN local-initialize IN hHistory.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-job-farms B-table-Win 
PROCEDURE update-job-farms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR ll AS LOG EXTENT 2 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     adm-new-record    = NO
     adm-adding-record = NO.

    RUN check-for-commit (OUTPUT ll[1]).

    FOR EACH w-po-ordl:
      REPOSITION {&browse-name} TO ROWID w-po-ordl.job-mat-rowid NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:
        RUN check-for-commit (OUTPUT ll[2]).

        IF ll[2] THEN RUN run-alloc (NO).

        APPLY "row-entry" TO BROWSE {&BROWSE-NAME}.

        ASSIGN
         job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = w-po-ordl.i-no
         job-farm.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} = "N".

        RUN new-i-no.

        ASSIGN
/*          job-farm.qty-all:SCREEN-VALUE IN BROWSE {&browse-name} = */
/*             job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}    */
         lv-allocated = NO.

        RUN dispatch ("update-record").

        IF ll[1] THEN RUN run-alloc (YES).
      END.

      DELETE w-po-ordl.
    END.

    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no B-table-Win 
PROCEDURE valid-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE job-hdr.
  RELEASE eb.
        
  IF li-est-type NE 1 THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) ne 0 then do:  
       
      IF li-est-type EQ 2 OR li-est-type EQ 6 THEN
      FIND FIRST eb
          WHERE eb.company  EQ est.company
            AND eb.est-no   EQ est.est-no
            AND eb.blank-no EQ INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST job-hdr
          WHERE job-hdr.company  EQ job-farm.company
            AND job-hdr.job      EQ job-farm.job 
            AND job-hdr.job-no   EQ job-farm.job-no
            AND job-hdr.job-no2  EQ job-farm.job-no2
            AND job-hdr.frm      EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-hdr.blank-no EQ INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
            
      IF NOT AVAIL job-hdr AND NOT AVAIL eb THEN DO:
        MESSAGE "Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-farm.blank-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frm B-table-Win 
PROCEDURE valid-frm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE job-hdr.
  RELEASE ef.

  DO WITH FRAME {&frame-name}:
    IF li-est-type EQ 2 OR li-est-type EQ 6 THEN
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    ELSE
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job     EQ job-farm.job
          AND job-hdr.job-no  EQ job-farm.job-no
          AND job-hdr.job-no2 EQ job-farm.job-no2
          AND job-hdr.frm     EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:
      MESSAGE "Must enter a valid form..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-farm.frm IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST itemfg
                    {sys/look/itemfgW.i}
                      AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Must enter a valid RM..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-farm.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty-all B-table-Win 
PROCEDURE valid-qty-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
/*     RUN check-for-commit (OUTPUT ll-commit).                                  */
/*                                                                               */
/*     IF DEC(job-farm.qty-all:SCREEN-VALUE IN BROWSE {&browse-name}) GT         */
/*            DEC(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}) AND        */
/*        ll-commit                                                  AND         */
/*        NOT ll-check-qty                                           THEN DO:    */
/*                                                                               */
/*       MESSAGE "Allocation QTY is greater than required QTY, continue anyway?" */
/*               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                        */
/*               UPDATE ll-check-qty.                                            */
/*       IF NOT ll-check-qty THEN DO:                                            */
/*         APPLY "entry" TO job-farm.qty-all IN BROWSE {&browse-name}.           */
/*         RETURN ERROR.                                                         */
/*       END.                                                                    */
/*     END.                                                                      */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-field AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-value AS CHAR NO-UNDO.

  DEF VAR lv-uom-list AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgW.i}
          AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN RUN sys/ref/uom-fg.p (YES, OUTPUT lv-uom-list).

    IF LOOKUP(ip-value,lv-uom-list) LE 0 THEN DO:
      MESSAGE "Must enter a valid UOM for this RM..." VIEW-AS ALERT-BOX ERROR.
      IF ip-field EQ "sc-uom" THEN
        APPLY "entry" TO job-farm.sc-uom IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO job-farm.qty-uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

