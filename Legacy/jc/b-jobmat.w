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
DEF VAR cItemName AS CHAR NO-UNDO.

{cec/bestfitc.i NEW SHARED}
{windows/l-jobmt1.i NEW}

&SCOPED-DEFINE sortby-log ~
    IF lv-sort-by EQ "rm-i-no" THEN job-mat.rm-i-no ELSE ~
    IF lv-sort-by EQ "i-name" THEN item.i-name ELSE ~
    IF lv-sort-by EQ "wid" THEN STRING(job-mat.wid,'>>9.99<<') ELSE ~
    IF lv-sort-by EQ "len" THEN STRING(job-mat.len,'>>9.99<<') ELSE ~
    STRING(job-mat.frm,'>>9')
&SCOPED-DEFINE sortby BY job-mat.frm BY job-mat.blank-no
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
&Scoped-define INTERNAL-TABLES job-mat

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-mat.frm job-mat.blank-no ~
job-mat.rm-i-no getItemName() @ cItemName job-mat.std-cost job-mat.sc-uom ~
job-mat.cost-m job-mat.qty job-mat.qty-uom job-mat.wid job-mat.len ~
job-mat.n-up job-mat.basis-w job-mat.post job-mat.qty-all job-mat.all-flg 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table job-mat.frm ~
job-mat.blank-no job-mat.rm-i-no job-mat.std-cost job-mat.sc-uom ~
job-mat.cost-m job-mat.qty job-mat.qty-uom job-mat.wid job-mat.len ~
job-mat.n-up job-mat.basis-w job-mat.post job-mat.qty-all 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-mat
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-mat
&Scoped-define QUERY-STRING-br_table FOR EACH job-mat WHERE job-mat.company = job.company ~
  AND job-mat.job = job.job ~
  AND job-mat.job-no = job.job-no ~
  AND job-mat.job-no2 = job.job-no2 ~
use-index seq-idx NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job-mat WHERE job-mat.company = job.company ~
  AND job-mat.job = job.job ~
  AND job-mat.job-no = job.job-no ~
  AND job-mat.job-no2 = job.job-no2 ~
use-index seq-idx NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-mat
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-mat


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
company||y|ASI.job-mat.company
j-no||y|ASI.job-mat.j-no
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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getItemName B-table-Win 
FUNCTION getItemName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job-mat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      job-mat.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 4 COLUMN-FONT 0
            LABEL-BGCOLOR 14
      job-mat.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 4
            COLUMN-FONT 0
      job-mat.rm-i-no COLUMN-LABEL "RM Item#" FORMAT "x(10)":U
            COLUMN-FONT 0 LABEL-BGCOLOR 14
      getItemName() @ cItemName COLUMN-LABEL "Item Name" FORMAT "X(30)":U
      job-mat.std-cost FORMAT ">>>,>>9.99<<":U
      job-mat.sc-uom COLUMN-LABEL "Cost!UOM" FORMAT "x(4)":U WIDTH 7
      job-mat.cost-m COLUMN-LABEL "FG Cost/M" FORMAT "->>>,>>9.9999":U
      job-mat.qty FORMAT "->>>,>>>,>>9.9<<<<<":U
      job-mat.qty-uom COLUMN-LABEL "Qty!UOM" FORMAT "x(4)":U WIDTH 7
      job-mat.wid FORMAT ">>9.99<<":U LABEL-BGCOLOR 14
      job-mat.len FORMAT ">>9.99<<":U LABEL-BGCOLOR 14
      job-mat.n-up COLUMN-LABEL "#Up" FORMAT ">>9":U
      job-mat.basis-w COLUMN-LABEL "MSF!Weight" FORMAT ">>9.99":U
      job-mat.post COLUMN-LABEL "Auto!Post?" FORMAT "Y/N":U
      job-mat.qty-all COLUMN-LABEL "Qty to!Commit" FORMAT ">>>,>>>,>>9.99<<<<":U
      job-mat.all-flg COLUMN-LABEL "Committed?" FORMAT "Y/N":U
  ENABLE
      job-mat.frm
      job-mat.blank-no
      job-mat.rm-i-no
      job-mat.std-cost
      job-mat.sc-uom
      job-mat.cost-m HELP "Cost per Thousand Finished Goods"
      job-mat.qty
      job-mat.qty-uom
      job-mat.wid
      job-mat.len
      job-mat.n-up
      job-mat.basis-w
      job-mat.post
      job-mat.qty-all
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
         HEIGHT             = 11.71
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
     _TblList          = "ASI.job-mat WHERE ASI.job   ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _JoinCode[1]      = "ASI.job-mat.company = ASI.job.company
  AND ASI.job-mat.job = ASI.job.job
  AND ASI.job-mat.job-no = ASI.job.job-no
  AND ASI.job-mat.job-no2 = ASI.job.job-no2
use-index seq-idx"
     _FldNameList[1]   > ASI.job-mat.frm
"job-mat.frm" "S" ">>>" "integer" ? ? 0 14 ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-mat.blank-no
"job-mat.blank-no" "B" ">>>" "integer" ? ? 0 ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-mat.rm-i-no
"job-mat.rm-i-no" "RM Item#" ? "character" ? ? 0 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"getItemName() @ cItemName" "Item Name" "X(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.job-mat.std-cost
"job-mat.std-cost" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-mat.sc-uom
"job-mat.sc-uom" "Cost!UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job-mat.cost-m
"job-mat.cost-m" "FG Cost/M" "->>>,>>9.9999" "decimal" ? ? ? ? ? ? yes "Cost per Thousand Finished Goods" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-mat.qty
"job-mat.qty" ? "->>>,>>>,>>9.9<<<<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.job-mat.qty-uom
"job-mat.qty-uom" "Qty!UOM" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.job-mat.wid
"job-mat.wid" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.job-mat.len
"job-mat.len" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.job-mat.n-up
"job-mat.n-up" "#Up" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.job-mat.basis-w
"job-mat.basis-w" "MSF!Weight" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.job-mat.post
"job-mat.post" "Auto!Post?" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.job-mat.qty-all
"job-mat.qty-all" "Qty to!Commit" ">>>,>>>,>>9.99<<<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.job-mat.all-flg
"job-mat.all-flg" "Committed?" "Y/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR lv-indus LIKE ITEM.industry INIT "" NO-UNDO.
  DEF VAR lv-matyp LIKE ITEM.mat-type INIT "" NO-UNDO.


  IF AVAIL job-mat THEN DO:
    FIND FIRST ITEM
        WHERE ITEM.company EQ g_company
          AND ITEM.i-no    EQ job-mat.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN DO:
      lv-matyp = ITEM.mat-type.

      IF ITEM.mat-type EQ "B" THEN lv-indus = ITEM.industry.
    END.
  END.
  
  case focus:NAME:
      when "rm-i-no" then do:
        run windows/l-item3.w (g_company,lv-indus,lv-matyp,job-mat.rm-i-no:screen-value IN BROWSE {&browse-name}, output char-val).
        if char-val <> "" then do:
          FIND ITEM WHERE RECID(ITEM) EQ int(char-val) NO-LOCK NO-ERROR.
          IF AVAIL ITEM THEN DO:
            job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.i-no.
            APPLY "value-changed" TO job-mat.rm-i-no IN BROWSE {&browse-name}.
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
  prev-cost = ld * DEC(job-mat.std-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).  
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

  IF NOT CAN-DO('frm,rm-i-no,i-name,wid,len',lh-column:NAME) THEN RETURN.
  
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


&Scoped-define SELF-NAME job-mat.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.frm br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.frm IN BROWSE br_table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.blank-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.blank-no IN BROWSE br_table /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.rm-i-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mat.rm-i-no IN BROWSE br_table /* RM Item# */
DO:
  IF DEC(job-mat.qty-all:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
     job-mat.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Y"    THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "back-tab" THEN
      APPLY "back-tab" TO {&self-name}.
    ELSE
      APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.rm-i-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.rm-i-no IN BROWSE br_table /* RM Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-rm-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    /*RUN SheetCalcSimple.*/ /* task 05141503 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.rm-i-no br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mat.rm-i-no IN BROWSE br_table /* RM Item# */
DO:
    RUN new-rm-i-no.
    FIND FIRST ITEM   /* task 05141503 */
        {sys/look/itemW.i}
          AND item.i-no EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
        
    IF AVAIL ITEM THEN DO:
        RUN SheetCalcSimple.
    END.   /* task 05141503 */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.std-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.std-cost br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.std-cost IN BROWSE br_table /* Costs */
DO:
  DEF VAR v-uom LIKE job-mat.sc-uom NO-UNDO. 
  DEF VAR v-qty LIKE job-mat.qty NO-UNDO.
  DEF VAR v-cost LIKE job-mat.std-cost NO-UNDO.
  DEF VAR v-cost-m LIKE job-mat.cost-m NO-UNDO.
  /* Copied from new-i-mat */
  FOR EACH job-hdr
      WHERE job-hdr.company  EQ job-mat.company
        AND job-hdr.job      EQ job-mat.job
        AND job-hdr.job-no   EQ job-mat.job-no
        AND job-hdr.job-no2  EQ job-mat.job-no2
        AND job-hdr.frm      EQ INT(job-mat.frm:SCREEN-VALUE IN BROWSE {&browse-name})
      NO-LOCK
      BY job-hdr.qty / IF job-hdr.n-on EQ 0 THEN 1 ELSE job-hdr.n-on DESC:
    LEAVE.
  END.

  ASSIGN
         v-cost   = decimal(job-mat.std-cost:SCREEN-VALUE IN BROWSE {&browse-name})
         v-uom    = job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   
         v-cost-m = decimal(job-mat.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   )
         v-qty    = decimal(job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name})
         v-uom    = job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  .

  IF AVAIL(job-hdr) AND job-mat.len GT 0 AND job-mat.wid GT 0 AND v-qty GT 0
     AND v-cost GT 0 AND job-hdr.qty GT 0 AND job-mat.sc-uom = "MSF" THEN
     v-cost-m = ((job-mat.LEN * job-mat.wid / 144) * (v-qty / 1000) * v-cost)
                      / job-hdr.qty * 1000.
   IF v-cost-m EQ ? THEN v-cost-m = 0.

   job-mat.cost-m:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost-m).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.sc-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.sc-uom br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.sc-uom IN BROWSE br_table /* Cost!UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom ({&self-name}:NAME IN BROWSE {&browse-name},
                   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*IF job-mat.qty-uom:SCREEN-VALUE <> job-mat.sc-uom:SCREEN-VALUE THEN DO:
       RUN calc-qty.
    END.
    ELSE job-mat.qty-all:SCREEN-VALUE = job-mat.qty:SCREEN-VALUE.*/

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.qty-uom br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.qty-uom IN BROWSE br_table /* Qty!UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom ({&self-name}:NAME IN BROWSE {&browse-name},
                   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*IF job-mat.qty-uom:SCREEN-VALUE <> job-mat.sc-uom:SCREEN-VALUE THEN DO:
       RUN calc-qty.
    END.
    ELSE job-mat.qty-all:SCREEN-VALUE = job-mat.qty:SCREEN-VALUE.*/

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.qty-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.qty-all br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mat.qty-all IN BROWSE br_table /* Qty to!Commit */
DO:
  /*
  IF NOT lv-allocated THEN RETURN.

  DO WITH FRAME {&FRAME-NAME}:
    RUN check-for-commit (OUTPUT ll-commit).

    IF NOT ll-commit THEN DO:
      APPLY "row-leave" TO BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.qty-all br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mat.qty-all IN BROWSE br_table /* Qty to!Commit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-qty-all NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.qty-all br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mat.qty-all IN BROWSE br_table /* Qty to!Commit */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    job-mat.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} =
        IF DEC({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
           CAN-FIND(FIRST item
                    WHERE item.company EQ cocode
                      AND item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND item.i-code  EQ "R")                          THEN "Y" ELSE "N".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mat.all-flg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mat.all-flg br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mat.all-flg IN BROWSE br_table /* Committed? */
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

  DEF BUFFER bf-item FOR item.

  def var v-bwt       like item.basis-w                      no-undo.
  DEF var v-len       like item.s-len                        no-undo.
  def var v-wid       like item.s-wid                        no-undo.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-item
        WHERE bf-item.company EQ job-mat.company
          AND bf-item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL BF-ITEM THEN RETURN.

    ASSIGN
     v-bwt = DEC(job-mat.basis-w:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
     v-len = DEC(job-mat.len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
     v-wid = DEC(job-mat.wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF v-len EQ 0 THEN v-len = bf-item.s-len.

    IF v-wid EQ 0 THEN
      v-wid = IF bf-item.r-wid NE 0 THEN bf-item.r-wid ELSE bf-item.s-wid.

    IF v-bwt EQ 0 THEN v-bwt = bf-item.basis-w.

    op-qty = DEC(job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE 
       job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}  THEN
      RUN sys/ref/convquom.p(job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                             job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                             v-bwt, v-len, v-wid, bf-item.s-dep,
                             op-qty, OUTPUT op-qty).
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
  DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER bf-item FOR item.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-item
        WHERE bf-item.company EQ job-mat.company
          AND bf-item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND bf-item.i-code  EQ "R"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL BF-ITEM THEN RETURN.
    RUN calc-job-qty (OUTPUT ld).
    job-mat.qty-all:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld).
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
    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-code  EQ "R"
        NO-LOCK NO-ERROR.

    op-commit = INDEX("LRAW",job.stat) GT 0                               AND
                AVAIL ITEM AND
                (lv-allocated OR job-mat.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} EQ "y").
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

  IF AVAIL job-mat THEN RUN jc/issuemat.p (ROWID(job-mat)).

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
  IF ll-commit THEN RUN jc/jc-all2.p (ROWID(job-mat), -1).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  job-mat.i-no = job-mat.rm-i-no.

  IF ll-commit THEN DO:
    job-mat.all-flg = YES.  
    RUN jc/jc-all2.p (ROWID(job-mat), 1).
  END.

  ELSE job-mat.all-flg = NO. 

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
  ASSIGN job-mat.frm:READ-ONLY IN BROWSE {&browse-name} = NO
           job-mat.BLANK-no:READ-ONLY = NO 
           job-mat.rm-i-no:READ-ONLY = NO 
           job-mat.std-cost:READ-ONLY = NO 
           job-mat.sc-uom:READ-ONLY = NO 
           job-mat.cost-m:READ-ONLY = NO 
           job-mat.qty:READ-ONLY = NO 
           job-mat.qty-uom:READ-ONLY = NO 
           job-mat.wid:READ-ONLY = NO 
           job-mat.len:READ-ONLY = NO 
           job-mat.n-up:READ-ONLY = NO 
           job-mat.basis-w:READ-ONLY = NO 
           job-mat.post:READ-ONLY = NO.

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
  RUN windows/l-jobmt1.w (STRING(ROWID(job-mat)), "", 0, "", OUTPUT char-val, OUTPUT look-recid).
  IF CAN-FIND(FIRST w-po-ordl) THEN RUN update-job-mats (ROWID(job-mat)).

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
   job-mat.company = cocode
   job-mat.job     = job.job
   job-mat.job-no  = job.job-no
   job-mat.job-no2 = job.job-no2.

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
    RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).

    IF ll THEN DO:
      {custom/askdel.i}
    END.

    ELSE
    IF ll EQ ? THEN DO:
      ll = NO.
      MESSAGE "Material has been Allocated, delete anyway?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
    END.

    ELSE
      MESSAGE "Sorry, this RM has been processed for this job " +
              "and may not be deleted..."
          VIEW-AS ALERT-BOX ERROR.

    IF NOT ll THEN RETURN ERROR.
  END.

  FIND CURRENT job.
  RUN jc/jc-all.p (ROWID(job-mat), -1, INPUT-OUTPUT job.stat).
  FIND CURRENT job NO-LOCK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
  IF NOT winReSize THEN
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
    APPLY "entry" TO job-mat.frm IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.

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
  IF NOT AVAIL job-mat THEN RUN dispatch ('open-query').
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
  DEF VAR char-hdl AS CHAR NO-UNDO.  
  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-frm NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-rm-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  /*RUN SheetCalcSimple.*/ /* task 05141503 */
    
  RUN valid-uom ("sc-uom", job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-uom ("qty-uom", job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
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
    FIND FIRST job-hdr OF job-mat NO-LOCK NO-ERROR.
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(job-hdr)).
  END.
*/
  ASSIGN job-mat.frm:READ-ONLY IN BROWSE {&browse-name} = NO
           job-mat.BLANK-no:READ-ONLY = NO 
           job-mat.rm-i-no:READ-ONLY = NO 
           job-mat.std-cost:READ-ONLY = NO 
           job-mat.sc-uom:READ-ONLY = NO 
           job-mat.cost-m:READ-ONLY = NO 
           job-mat.qty:READ-ONLY = NO 
           job-mat.qty-uom:READ-ONLY = NO 
           job-mat.wid:READ-ONLY = NO 
           job-mat.len:READ-ONLY = NO 
           job-mat.n-up:READ-ONLY = NO 
           job-mat.basis-w:READ-ONLY = NO 
           job-mat.post:READ-ONLY = NO
           lv-allocated = NO
           ll-commit = NO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN view-user-id IN WIDGET-HANDLE(char-hdl).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"job-target",OUTPUT char-hdl).
  RUN view-user-id IN WIDGET-HANDLE(char-hdl).

  RUN dispatch ("display-fields").

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&BROWSE-NAME}.
    END.
  END.
  
  /* Without this code, viewer on material page was displaying info from */
  /* the first record in the browse on page 1.                           */
  /* Switching to page 1 and back again fixes the problem                */
  run get-link-handle in adm-broker-hdl (this-procedure, "container-source", output char-hdl).  
  run select-page in widget-handle(char-hdl) (1).
  run select-page in widget-handle(char-hdl) (3).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rm-i-no B-table-Win 
PROCEDURE new-rm-i-no PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-uom LIKE job-mat.sc-uom NO-UNDO. 
  DEF VAR v-qty LIKE job-mat.qty NO-UNDO.
  DEF VAR v-cost LIKE job-mat.std-cost NO-UNDO.
  DEF VAR v-cost-m LIKE job-mat.cost-m NO-UNDO.
  DEF VAR j AS INT NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST ITEM
        {sys/look/itemW.i}
          AND item.i-no EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
        
    IF AVAIL ITEM THEN DO:
      
      IF adm-new-record OR item.i-code EQ "R" THEN DO:
        job-mat.basis-w:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(item.basis-w).
        
        IF item.r-wid NE 0 THEN
          job-mat.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(item.r-wid).
        ELSE
          ASSIGN
            job-mat.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(item.s-wid)
            job-mat.len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(item.s-len).

        job-mat.sc-uom:SCREEN-VALUE = ITEM.cons-uom.
        /*IF job-mat.qty-uom:SCREEN-VALUE <> job-mat.sc-uom:SCREEN-VALUE THEN
           RUN calc-qty.
        ELSE job-mat.qty-all:SCREEN-VALUE = job-mat.qty:SCREEN-VALUE.*/
      END.
      
     cItemName:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.i-name.

      FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

      IF job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF item.i-code EQ "E" AND AVAIL e-item THEN e-item.std-uom ELSE item.cons-uom.
                            
      IF job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF item.i-code EQ "E" AND AVAIL e-item THEN e-item.std-uom ELSE item.cons-uom.

      IF adm-adding-record THEN DO:
        FOR EACH job-hdr
            WHERE job-hdr.company  EQ job-mat.company
              AND job-hdr.job      EQ job-mat.job
              AND job-hdr.job-no   EQ job-mat.job-no
              AND job-hdr.job-no2  EQ job-mat.job-no2
              AND job-hdr.frm      EQ INT(job-mat.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            NO-LOCK
            BY job-hdr.qty / IF job-hdr.n-on EQ 0 THEN 1 ELSE job-hdr.n-on DESC:
          LEAVE.
        END.

        ASSIGN
         v-uom  = job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}
         v-qty  = INT(job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name})
         v-cost = 0.

        IF item.i-code EQ "R" THEN
          v-cost = IF ce-ctrl.r-cost THEN item.avg-cost ELSE item.last-cost.

        ELSE
        IF AVAIL e-item THEN DO:
          FIND FIRST e-item-vend OF e-item
              WHERE e-item-vend.item-type EQ YES
              NO-LOCK NO-ERROR.

          EMPTY TEMP-TABLE tt-eiv.
          EMPTY TEMP-TABLE tt-ei.

          IF AVAIL e-item-vend THEN
          DO:
             CREATE tt-eiv.
             DO j = 1 TO 10:
                ASSIGN
                   tt-eiv.run-qty[j] = e-item-vend.run-qty[j]
                   tt-eiv.run-cost[j] = e-item-vend.run-cost[j].
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
                   tt-ei.run-qty[j] = e-item.run-qty[j]
                   tt-ei.run-cost[j] = e-item.run-cost[j].
             END.
            
             FIND FIRST b-qty WHERE
                  b-qty.reftable = "blank-vend-qty" AND
                  b-qty.company = e-item.company AND
                      b-qty.CODE    = e-item.i-no
                  NO-LOCK NO-ERROR.
            
             IF AVAIL b-qty THEN
             DO:
                FIND FIRST b-cost WHERE
                     b-cost.reftable = "blank-vend-cost" AND
                     b-cost.company = e-item.company AND
                         b-cost.CODE    = e-item.i-no
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

          IF AVAIL e-item-vend THEN
            RUN est/dim-charge.p (e-item-vend.rec_key,
                                  DEC(job-mat.wid:SCREEN-VALUE IN BROWSE {&browse-name}),
                                  DEC(job-mat.len:SCREEN-VALUE IN BROWSE {&browse-name}),
                                  INPUT-OUTPUT v-cost).
        END.

        /* 02201203 */
        ASSIGN
           v-cost   = decimal(job-mat.std-cost:SCREEN-VALUE IN BROWSE {&browse-name})
           v-uom    = job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   
           v-cost-m = decimal(job-mat.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   )
           v-qty    = decimal(job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name})
           v-uom    = job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  .
        IF AVAIL(job-hdr) THEN 
           IF  job-mat.len GT 0 AND job-mat.wid GT 0 AND v-qty GT 0
           AND v-cost GT 0 AND job-hdr.qty GT 0 AND job-mat.sc-uom = "MSF" THEN
           v-cost-m = ((job-mat.LEN * job-mat.wid / 144) * (v-qty / 1000) * v-cost)
                       / job-hdr.qty * 1000.        
         ELSE
           v-cost-m = (v-qty * v-cost) / (job-hdr.qty / 1000). 

        IF v-cost-m EQ ? THEN v-cost-m = 0.

        ASSIGN
         job-mat.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost)
         job-mat.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   = v-uom
         job-mat.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(v-cost-m)
         job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(v-qty)
         job-mat.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = v-uom.
      END.

      IF NOT adm-adding-record AND prev-cost NE 0 THEN DO:
        RUN calc-job-qty (OUTPUT v-qty).
        job-mat.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(prev-cost / v-qty).
      END.
    END.
  END.

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
  DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
  DEF BUFFER b-job-mat FOR job-mat.


  FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-code  EQ "R"
        NO-LOCK NO-ERROR.
  
      
 IF AVAIL job-mat AND job-mat.all-flg THEN lv-alloc-char = "Deallocate".
     ELSE lv-alloc-char = "Allocate".
 
  IF job.stat = "H" THEN DO:

      IF NOT AVAIL ITEM THEN DO:
          MESSAGE "Estimated Material cannot be Allocated, Press Update Key to Add Real Material ..." VIEW-AS ALERT-BOX ERROR.
          RETURN ERROR.
      END.

      lCheck = YES .
     MESSAGE "The job status is Hold, are you sure you want to" SKIP lv-alloc-char "this material ?"
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll.
     IF NOT ll THEN do:
         RETURN ERROR.
     END.
  END.
  ELSE IF INDEX("LRAW",job.stat) = 0 THEN DO:
     MESSAGE "The job status must be 'R'eleased in order to perform this selection"
            VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.


  IF NOT AVAIL ITEM AND NOT lCheck THEN DO:
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
  ASSIGN job-mat.frm:READ-ONLY IN BROWSE {&browse-name} = YES
           job-mat.BLANK-no:READ-ONLY = YES
           job-mat.rm-i-no:READ-ONLY = YES
           job-mat.std-cost:READ-ONLY = YES
           job-mat.sc-uom:READ-ONLY = YES
           job-mat.cost-m:READ-ONLY = YES
           job-mat.qty:READ-ONLY = YES
           job-mat.qty-uom:READ-ONLY = YES
           job-mat.wid:READ-ONLY = YES
           job-mat.len:READ-ONLY = YES
           job-mat.n-up:READ-ONLY = YES
           job-mat.basis-w:READ-ONLY = YES
           job-mat.post:READ-ONLY = YES.  

  

    APPLY "entry" TO job-mat.qty-all IN BROWSE {&browse-name}.
 */
   IF ip-ask THEN DO:
     IF AVAIL job-mat AND job-mat.all-flg THEN lv-alloc-char = "Deallocate?".
     ELSE lv-alloc-char = "Allocate?".
     IF NOT lCheck THEN
     MESSAGE "Are you sure you want to " lv-alloc-char VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll.
   END.

   ELSE ll = YES.

   IF ll THEN DO:
      FIND b-job-mat WHERE ROWID(b-job-mat) EQ ROWID(job-mat) EXCLUSIVE-LOCK.
      b-job-mat.all-flg = YES.  
      IF lv-alloc-char BEGINS "alloc" THEN RUN jc/jc-all2.p (ROWID(b-job-mat), 1).
      ELSE RUN jc/jc-all2.p (ROWID(b-job-mat), -1).
      FIND CURRENT b-job-mat EXCLUSIVE-LOCK.
      IF b-job-mat.qty-all EQ 0 AND
         NOT b-job-mat.all-flg  THEN b-job-mat.qty-all = b-job-mat.qty.
      FIND CURRENT b-job-mat NO-LOCK.
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
  {src/adm/template/sndkycas.i "company" "job-mat" "company"}
  {src/adm/template/sndkycas.i "j-no" "job-mat" "j-no"}

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
  {src/adm/template/snd-list.i "job-mat"}

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


  IF AVAIL job-mat THEN DO:
    lv-rowid = ROWID(job-mat).

    RUN jc/shtcalc.p (INPUT-OUTPUT lv-rowid,YES,"").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SheetCalcSimple B-table-Win 
PROCEDURE SheetCalcSimple :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR cRMINo AS CHAR NO-UNDO.


  IF AVAIL job-mat THEN DO:
    cRMINo = job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}.
    lv-rowid = ROWID(job-mat).

    RUN jc/shtcalc.p (INPUT-OUTPUT lv-rowid,NO,cRMINo).

    IF lv-rowid NE ? THEN DO:
/*       RUN dispatch ("open-query"). */
/*                                        */
/*       RUN repo-query (lv-rowid).       */
/*                                        */
      RUN dispatch ("display-fields").
      FIND FIRST ITEM
        {sys/look/itemW.i}
          AND item.i-no EQ cRMINO
        NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
        cItemName:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.i-name.
/*                                        */
/*       IF rmissue-log THEN do:                                     */
/*          ll = NO.                                                 */
/*          MESSAGE "Would you like to issue RM?"                    */
/*               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. */
/*          IF ll THEN RUN issue-mat.                                */
/* /*       END.                                                        */ */
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-job-mats B-table-Win 
PROCEDURE update-job-mats :
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
         job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name} = w-po-ordl.i-no
         job-mat.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} = "N".

        RUN new-rm-i-no.

        ASSIGN
         job-mat.qty-all:SCREEN-VALUE IN BROWSE {&browse-name} =
            job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name}
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
    IF INT(job-mat.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) ne 0 then do:  
       
      IF li-est-type EQ 2 OR li-est-type EQ 6 THEN
      FIND FIRST eb
          WHERE eb.company  EQ est.company
            AND eb.est-no   EQ est.est-no
            AND eb.blank-no EQ INT(job-mat.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST job-hdr
          WHERE job-hdr.company  EQ job-mat.company
            AND job-hdr.job      EQ job-mat.job 
            AND job-hdr.job-no   EQ job-mat.job-no
            AND job-hdr.job-no2  EQ job-mat.job-no2
            AND job-hdr.frm      EQ INT(job-mat.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-hdr.blank-no EQ INT(job-mat.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
            
      IF NOT AVAIL job-hdr AND NOT AVAIL eb THEN DO:
        MESSAGE "Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-mat.blank-no IN BROWSE {&browse-name}.
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
          AND ef.form-no EQ INT(job-mat.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    ELSE
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job     EQ job-mat.job
          AND job-hdr.job-no  EQ job-mat.job-no
          AND job-hdr.job-no2 EQ job-mat.job-no2
          AND job-hdr.frm     EQ INT(job-mat.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:
      MESSAGE "Must enter a valid form..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-mat.frm IN BROWSE {&browse-name}.
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
    RUN check-for-commit (OUTPUT ll-commit).

    IF DEC(job-mat.qty-all:SCREEN-VALUE IN BROWSE {&browse-name}) GT
           DEC(job-mat.qty:SCREEN-VALUE IN BROWSE {&browse-name}) AND
       ll-commit                                                  AND
       NOT ll-check-qty                                           THEN DO:

      MESSAGE "Allocation QTY is greater than required QTY, continue anyway?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-check-qty.
      IF NOT ll-check-qty THEN DO:
        APPLY "entry" TO job-mat.qty-all IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-i-no B-table-Win 
PROCEDURE valid-rm-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST ITEM
                    {sys/look/itemW.i}
                      AND item.i-no EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Must enter a valid RM..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-mat.rm-i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
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
    FIND FIRST item
        {sys/look/itemW.i}
          AND item.i-no EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT lv-uom-list).

    IF LOOKUP(ip-value,lv-uom-list) LE 0 THEN DO:
      MESSAGE "Must enter a valid UOM for this RM..." VIEW-AS ALERT-BOX ERROR.
      IF ip-field EQ "sc-uom" THEN
        APPLY "entry" TO job-mat.sc-uom IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO job-mat.qty-uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getItemName B-table-Win 
FUNCTION getItemName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST ITEM
        {sys/look/itemW.i}
          AND item.i-no EQ job-mat.rm-i-no
        NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
        RETURN ITEM.i-name.  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

