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
DEF var  ipFarmJob AS CHAR.
DEF var ipFarmJobNo2 AS INT.
DEF var  ipFarmIno AS CHAR.
DEF VAR li-est-type LIKE est.est-type NO-UNDO.
DEF VAR ll-check-qty AS LOG NO-UNDO.
DEF VAR ll-commit AS LOG NO-UNDO.
DEF VAR lv-allocated AS LOG NO-UNDO.
DEF VAR prev-cost AS DEC NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR lv-sort-by AS CHAR NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR NO-UNDO.    
DEF VAR lcDesc AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR ldTotalQty AS DEC NO-UNDO.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-farm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-farm-rctd.rct-date ~
job-farm-rctd.tag job-farm-rctd.po-no itemDesc() @ lcDesc ~
job-farm-rctd.job-no job-farm-rctd.job-no2 job-farm-rctd.loc ~
job-farm-rctd.loc-bin job-farm-rctd.cases job-farm-rctd.qty-case ~
job-farm-rctd.cases-unit job-farm-rctd.partial job-farm-rctd.t-qty ~
job-farm-rctd.std-cost job-farm-rctd.cost-uom job-farm-rctd.frt-cost ~
job-farm-rctd.ext-cost job-farm-rctd.stack-code job-farm-rctd.tot-wt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table job-farm-rctd.rct-date ~
job-farm-rctd.tag job-farm-rctd.po-no job-farm-rctd.job-no ~
job-farm-rctd.job-no2 job-farm-rctd.loc job-farm-rctd.loc-bin ~
job-farm-rctd.cases job-farm-rctd.qty-case job-farm-rctd.cases-unit ~
job-farm-rctd.partial job-farm-rctd.t-qty job-farm-rctd.std-cost ~
job-farm-rctd.cost-uom job-farm-rctd.frt-cost job-farm-rctd.ext-cost ~
job-farm-rctd.stack-code job-farm-rctd.tot-wt 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-farm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-farm-rctd
&Scoped-define QUERY-STRING-br_table FOR EACH job-farm-rctd WHERE ~{&KEY-PHRASE} ~
      AND job-farm-rctd.company = cocode ~
 AND job-farm-rctd.job-no = ipFarmJob ~
 and job-farm-rctd.job-no2 = ipFarmJobNo2 ~
 and job-farm-rctd.i-no = ipFarmINo NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job-farm-rctd WHERE ~{&KEY-PHRASE} ~
      AND job-farm-rctd.company = cocode ~
 AND job-farm-rctd.job-no = ipFarmJob ~
 and job-farm-rctd.job-no2 = ipFarmJobNo2 ~
 and job-farm-rctd.i-no = ipFarmINo NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-farm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-farm-rctd


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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ItemDesc B-table-Win 
FUNCTION ItemDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD totalQty B-table-Win 
FUNCTION totalQty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job-farm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      job-farm-rctd.rct-date FORMAT "99/99/9999":U
      job-farm-rctd.tag COLUMN-LABEL "Tag" FORMAT "x(21)":U WIDTH 28.6
      job-farm-rctd.po-no FORMAT "x(9)":U
      itemDesc() @ lcDesc COLUMN-LABEL "Item Name" FORMAT "x(30)":U
            WIDTH 47
      job-farm-rctd.job-no FORMAT "x(6)":U
      job-farm-rctd.job-no2 FORMAT "99":U
      job-farm-rctd.loc FORMAT "x(5)":U
      job-farm-rctd.loc-bin FORMAT "x(8)":U
      job-farm-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U
      job-farm-rctd.qty-case COLUMN-LABEL "Qty/Unit" FORMAT "->>>,>>9":U
      job-farm-rctd.cases-unit COLUMN-LABEL "Units Per!Pallet" FORMAT "->>9":U
      job-farm-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      job-farm-rctd.t-qty COLUMN-LABEL "Total Qty" FORMAT "->>>,>>>,>>9.99":U
      job-farm-rctd.std-cost COLUMN-LABEL "Std Costs" FORMAT ">>>,>>9.99<<":U
      job-farm-rctd.cost-uom FORMAT "x(3)":U
      job-farm-rctd.frt-cost COLUMN-LABEL "Frt Costs" FORMAT ">>>,>>9.99<<":U
      job-farm-rctd.ext-cost COLUMN-LABEL "Ext Cost" FORMAT "->>>,>>9.99<<":U
      job-farm-rctd.stack-code FORMAT "x":U
      job-farm-rctd.tot-wt COLUMN-LABEL "Total Weight" FORMAT ">>,>>9.99":U
  ENABLE
      job-farm-rctd.rct-date
      job-farm-rctd.tag
      job-farm-rctd.po-no
      job-farm-rctd.job-no
      job-farm-rctd.job-no2
      job-farm-rctd.loc
      job-farm-rctd.loc-bin
      job-farm-rctd.cases
      job-farm-rctd.qty-case
      job-farm-rctd.cases-unit
      job-farm-rctd.partial
      job-farm-rctd.t-qty
      job-farm-rctd.std-cost
      job-farm-rctd.cost-uom
      job-farm-rctd.frt-cost
      job-farm-rctd.ext-cost
      job-farm-rctd.stack-code
      job-farm-rctd.tot-wt
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

ASSIGN 
       job-farm-rctd.stack-code:VISIBLE IN BROWSE br_table = FALSE
       job-farm-rctd.tot-wt:VISIBLE IN BROWSE br_table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.job-farm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST,"
     _Where[1]         = "ASI.job-farm-rctd.company = cocode
 AND ASI.job-farm-rctd.job-no = ipFarmJob
 and ASI.job-farm-rctd.job-no2 = ipFarmJobNo2
 and ASI.job-farm-rctd.i-no = ipFarmINo"
     _FldNameList[1]   > asi.job-farm-rctd.rct-date
"job-farm-rctd.rct-date" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.job-farm-rctd.tag
"job-farm-rctd.tag" "Tag" "x(21)" "character" ? ? ? ? ? ? yes ? no no "28.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.job-farm-rctd.po-no
"job-farm-rctd.po-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"itemDesc() @ lcDesc" "Item Name" "x(30)" ? ? ? ? ? ? ? no ? no no "47" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.job-farm-rctd.job-no
"job-farm-rctd.job-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.job-farm-rctd.job-no2
"job-farm-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.job-farm-rctd.loc
"job-farm-rctd.loc" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.job-farm-rctd.loc-bin
"job-farm-rctd.loc-bin" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.job-farm-rctd.cases
"job-farm-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.job-farm-rctd.qty-case
"job-farm-rctd.qty-case" "Qty/Unit" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.job-farm-rctd.cases-unit
"job-farm-rctd.cases-unit" "Units Per!Pallet" "->>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.job-farm-rctd.partial
"job-farm-rctd.partial" "Partial" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.job-farm-rctd.t-qty
"job-farm-rctd.t-qty" "Total Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.job-farm-rctd.std-cost
"job-farm-rctd.std-cost" "Std Costs" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.job-farm-rctd.cost-uom
"job-farm-rctd.cost-uom" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.job-farm-rctd.frt-cost
"job-farm-rctd.frt-cost" "Frt Costs" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > asi.job-farm-rctd.ext-cost
"job-farm-rctd.ext-cost" "Ext Cost" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > asi.job-farm-rctd.stack-code
"job-farm-rctd.stack-code" ? ? "character" ? ? ? ? ? ? yes ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > asi.job-farm-rctd.tot-wt
"job-farm-rctd.tot-wt" "Total Weight" ? "decimal" ? ? ? ? ? ? yes ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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


/*   IF AVAIL job-farm THEN DO:                                                                                                      */
/*     FIND FIRST itemfg                                                                                                             */
/*         WHERE itemfg.company EQ g_company                                                                                         */
/*           AND itemfg.i-no    EQ job-farm.i-no                                                                                     */
/*         NO-LOCK NO-ERROR.                                                                                                         */
/*     IF AVAIL itemfg THEN DO:                                                                                                      */
/*       lv-matyp = itemfg.mat-type.                                                                                                 */
/*                                                                                                                                   */
/*       IF itemfg.mat-type EQ "B" THEN lv-indus = "IN".                                                                             */
/*     END.                                                                                                                          */
/*   END.                                                                                                                            */
/*                                                                                                                                   */
/*   case focus:NAME:                                                                                                                */
/*       when "i-no" then do:                                                                                                        */
/*         run windows/l-item3.w (g_company,lv-indus,lv-matyp,job-farm.i-no:screen-value IN BROWSE {&browse-name}, output char-val). */
/*         if char-val <> "" then do:                                                                                                */
/*           FIND itemfg WHERE RECID(itemfg) EQ int(char-val) NO-LOCK NO-ERROR.                                                      */
/*           IF AVAIL itemfg THEN DO:                                                                                                */
/*             job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-no.                                                    */
/*             APPLY "value-changed" TO job-farm.i-no IN BROWSE {&browse-name}.                                                      */
/*           END.                                                                                                                    */
/*         end.                                                                                                                      */
/*       end.                                                                                                                        */
/*   end case.                                                                                                                       */
/*   return no-apply.                                                                                                                */

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
  prev-cost = ld * DEC(job-farm-rctd.std-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).  
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


&Scoped-define SELF-NAME job-farm-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm-rctd.cases br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm-rctd.cases IN BROWSE br_table /* Units */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm-rctd.qty-case br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm-rctd.qty-case IN BROWSE br_table /* Qty/Unit */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-farm-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-farm-rctd.partial br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-farm-rctd.partial IN BROWSE br_table /* Partial */
DO:
  RUN new-qty.
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


/*   DO WITH FRAME {&FRAME-NAME}:                                                                         */
/*     FIND FIRST itemfg                                                                                  */
/*         WHERE itemfg.company EQ cocode                                                                 */
/*           AND itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}                    */
/*           AND itemfg.i-code  EQ "R"                                                                    */
/*         NO-LOCK NO-ERROR.                                                                              */
/*                                                                                                        */
/* /*     op-commit = INDEX("LRAW",job.stat) GT 0                               AND                    */ */
/* /*                 job-farm.post:SCREEN-VALUE IN BROWSE {&browse-name} EQ "N" AND                   */ */
/* /*                 AVAIL itemfg AND                                                                 */ */
/* /*                 (lv-allocated OR job-farm.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} EQ "y"). */ */
/*   END.                                                                                                 */

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

  /* IF AVAIL job-farm THEN RUN jc/issuemat.p (ROWID(job-farm)). */

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


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

FIND FIRST job WHERE job.job-no = job-farm-rctd.job-no
   AND job.job-no2 EQ job-farm-rctd.job-no2
  NO-LOCK NO-ERROR.
IF AVAIL job THEN
  RUN recalcJobFarm (INPUT ROWID(job), INPUT job-farm-rctd.i-no).

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
/*   ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = NO */
/*            job-farm.BLANK-no:READ-ONLY = NO                   */
/*            job-farm.i-no:READ-ONLY = NO                       */
/*            job-farm.std-cost:READ-ONLY = NO                   */
/*            job-farm.sc-uom:READ-ONLY = NO                     */
/*            job-farm.cost-m:READ-ONLY = NO                     */
/*            job-farm.qty:READ-ONLY = NO                        */
/*            job-farm.qty-uom:READ-ONLY = NO .                  */
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
/*   ASSIGN                           */
/*    job-farm.company = cocode       */
/*    job-farm.job     = job.job      */
/*    job-farm.job-no  = job.job-no   */
/*    job-farm.job-no2 = job.job-no2. */

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
  DEF VAR cIno AS CHAR NO-UNDO.

FIND FIRST job WHERE job.job-no = job-farm-rctd.job-no
   AND job.job-no2 EQ job-farm-rctd.job-no2
  NO-LOCK NO-ERROR.
cIno = job-farm-rctd.i-no.
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

/*   FIND CURRENT job.                                             */
/*   RUN jc/jc-all.p (ROWID(job-farm), -1, INPUT-OUTPUT job.stat). */
/*   FIND CURRENT job NO-LOCK.                                     */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

IF AVAIL job THEN
  RUN recalcJobFarm (INPUT ROWID(job), INPUT cIno).
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

/*   DO WITH FRAME {&FRAME-NAME}:                              */
/*     APPLY "entry" TO job-farm.frm IN BROWSE {&browse-name}. */
/*     RETURN NO-APPLY.                                        */
/*   END.                                                      */

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
  IF NOT AVAIL job-farm-rctd THEN RUN dispatch ('open-query').
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

  
/*   /* Code placed here will execute PRIOR to standard behavior. */                             */
/*   RUN valid-frm NO-ERROR.                                                                     */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN valid-blank-no NO-ERROR.                                                                */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN valid-i-no NO-ERROR.                                                                    */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN valid-uom ("sc-uom", job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.   */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN valid-uom ("qty-uom", job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR. */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN valid-qty-all NO-ERROR.                                                                 */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                 */
/*                                                                                               */
/*   RUN check-for-commit (OUTPUT ll-commit).                                                    */

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
/*   ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = NO */
/*            job-farm.BLANK-no:READ-ONLY = NO                   */
/*            job-farm.i-no:READ-ONLY = NO                       */
/*            job-farm.std-cost:READ-ONLY = NO                   */
/*            job-farm.sc-uom:READ-ONLY = NO                     */
/*            job-farm.cost-m:READ-ONLY = NO                     */
/*            job-farm.qty:READ-ONLY = NO                        */
/*            job-farm.qty-uom:READ-ONLY = NO                    */
/* /*            job-farm.wid:READ-ONLY = NO     */              */
/* /*            job-farm.len:READ-ONLY = NO     */              */
/* /*            job-farm.n-up:READ-ONLY = NO    */              */
/* /*            job-farm.basis-w:READ-ONLY = NO */              */
/* /*            job-farm.post:READ-ONLY = NO    */              */
/*            lv-allocated = NO                                  */
/*            ll-commit = NO.                                    */
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

/*   DO WITH FRAME {&FRAME-NAME}:                                                                   */
/*     FIND FIRST itemfg                                                                            */
/*         {sys/look/itemfgW.i}                                                                      */
/*           AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}                 */
/*         NO-LOCK NO-ERROR.                                                                        */
/*                                                                                                  */
/*     IF AVAIL itemfg THEN DO:                                                                     */
/*                                                                                                  */
/*       IF adm-new-record OR itemfg.i-code EQ "R" THEN DO:                                         */
/*         /*job-farm.basis-w:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.weight-100).    */
/*                                                                                                  */
/*         IF itemfg.t-wid NE 0 THEN                                                                */
/*           job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-wid).             */
/*         ELSE                                                                                     */
/*           ASSIGN                                                                                 */
/*             job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-wid)            */
/*             job-farm.len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.t-len).           */
/*           */                                                                                     */
/* /*         job-farm.sc-uom:SCREEN-VALUE = itemfg.cons-uom. */                                    */
/*         /*IF job-farm.qty-uom:SCREEN-VALUE <> job-farm.sc-uom:SCREEN-VALUE THEN                  */
/*            RUN calc-qty.                                                                         */
/*         ELSE job-farm.qty-all:SCREEN-VALUE = job-farm.qty:SCREEN-VALUE.*/                        */
/*       END.                                                                                       */
/*                                                                                                  */
/*       itemfg.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.                       */
/*                                                                                                  */
/*       FIND FIRST e-itemfg OF itemfg NO-LOCK NO-ERROR.                                            */
/*                                                                                                  */
/*       IF job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN                       */
/*         job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name} =                                 */
/*           IF itemfg.i-code EQ "E" AND AVAIL e-itemfg THEN e-itemfg.std-uom ELSE itemfg.cons-uom. */
/*                                                                                                  */
/*       IF job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN                        */
/*         job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name} =                                  */
/*           IF itemfg.i-code EQ "E" AND AVAIL e-itemfg THEN e-itemfg.std-uom ELSE itemfg.cons-uom. */
/*                                                                                                  */
/*       IF adm-adding-record THEN DO:                                                              */
/*         FOR EACH job-hdr                                                                         */
/*             WHERE job-hdr.company  EQ job-farm.company                                           */
/*               AND job-hdr.job      EQ job-farm.job                                               */
/*               AND job-hdr.job-no   EQ job-farm.job-no                                            */
/*               AND job-hdr.job-no2  EQ job-farm.job-no2                                           */
/*               AND job-hdr.frm      EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})    */
/*             NO-LOCK                                                                              */
/*             BY job-hdr.qty / IF job-hdr.n-on EQ 0 THEN 1 ELSE job-hdr.n-on DESC:                 */
/*           LEAVE.                                                                                 */
/*         END.                                                                                     */
/*                                                                                                  */
/*         ASSIGN                                                                                   */
/*          v-uom  = job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}                         */
/*          v-qty  = INT(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name})                        */
/*          v-cost = 0.                                                                             */
/*                                                                                                  */
/*         IF itemfg.i-code EQ "R" THEN                                                             */
/*           v-cost = IF ce-ctrl.r-cost THEN itemfg.avg-cost ELSE itemfg.last-cost.                 */
/*                                                                                                  */
/*         ELSE                                                                                     */
/*         IF AVAIL e-itemfg THEN DO:                                                               */
/*           FIND FIRST e-itemfg-vend OF e-itemfg                                                   */
/*               WHERE e-itemfg-vend.item-type EQ YES                                               */
/*               NO-LOCK NO-ERROR.                                                                  */
/*                                                                                                  */
/*           EMPTY TEMP-TABLE tt-eiv.                                                               */
/*           EMPTY TEMP-TABLE tt-ei.                                                                */
/*                                                                                                  */
/*           IF AVAIL e-itemfg-vend THEN                                                            */
/*           DO:                                                                                    */
/*              CREATE tt-eiv.                                                                      */
/*              DO j = 1 TO 10:                                                                     */
/*                 ASSIGN                                                                           */
/*                    tt-eiv.run-qty[j] = e-itemfg-vend.run-qty[j]                                  */
/*                    tt-eiv.run-cost[j] = e-itemfg-vend.run-cost[j].                               */
/*              END.                                                                                */
/*                                                                                                  */
/*              FIND FIRST b-qty WHERE                                                              */
/*                   b-qty.reftable = "vend-qty" AND                                                */
/*                   b-qty.company = e-itemfg-vend.company AND                                      */
/*                           b-qty.CODE    = e-itemfg-vend.i-no AND                                 */
/*                   b-qty.code2   = e-itemfg-vend.vend-no                                          */
/*                   NO-LOCK NO-ERROR.                                                              */
/*                                                                                                  */
/*              IF AVAIL b-qty THEN                                                                 */
/*              DO:                                                                                 */
/*                 FIND FIRST b-cost WHERE                                                          */
/*                      b-cost.reftable = "vend-cost" AND                                           */
/*                      b-cost.company = e-itemfg-vend.company AND                                  */
/*                              b-cost.CODE    = e-itemfg-vend.i-no AND                             */
/*                      b-cost.code2   = e-itemfg-vend.vend-no                                      */
/*                      NO-LOCK NO-ERROR.                                                           */
/*                                                                                                  */
/*                 DO j = 1 TO 10:                                                                  */
/*                    ASSIGN                                                                        */
/*                       tt-eiv.run-qty[j + 10] = b-qty.val[j]                                      */
/*                       tt-eiv.run-cost[j + 10] = b-cost.val[j].                                   */
/*                 END.                                                                             */
/*              END.                                                                                */
/*                                                                                                  */
/*              DO j = 1 TO 20:                                                                     */
/*                 IF tt-eiv.run-qty[j] GE v-qty THEN DO:                                           */
/*                    v-cost = tt-eiv.run-cost[j].                                                  */
/*                    LEAVE.                                                                        */
/*                 END.                                                                             */
/*              END.                                                                                */
/*           END.                                                                                   */
/*           ELSE                                                                                   */
/*           DO:                                                                                    */
/*              CREATE tt-ei.                                                                       */
/*              DO j = 1 TO 10:                                                                     */
/*                 ASSIGN                                                                           */
/*                    tt-ei.run-qty[j] = e-itemfg.run-qty[j]                                        */
/*                    tt-ei.run-cost[j] = e-itemfg.run-cost[j].                                     */
/*              END.                                                                                */
/*                                                                                                  */
/*              FIND FIRST b-qty WHERE                                                              */
/*                   b-qty.reftable = "blank-vend-qty" AND                                          */
/*                   b-qty.company = e-itemfg.company AND                                           */
/*                       b-qty.CODE    = e-itemfg.i-no                                              */
/*                   NO-LOCK NO-ERROR.                                                              */
/*                                                                                                  */
/*              IF AVAIL b-qty THEN                                                                 */
/*              DO:                                                                                 */
/*                 FIND FIRST b-cost WHERE                                                          */
/*                      b-cost.reftable = "blank-vend-cost" AND                                     */
/*                      b-cost.company = e-itemfg.company AND                                       */
/*                          b-cost.CODE    = e-itemfg.i-no                                          */
/*                      NO-LOCK NO-ERROR.                                                           */
/*                                                                                                  */
/*                 DO j = 1 TO 10:                                                                  */
/*                    ASSIGN                                                                        */
/*                       tt-ei.run-qty[j + 10] = b-qty.val[j]                                       */
/*                       tt-ei.run-cost[j + 10] = b-cost.val[j].                                    */
/*                 END.                                                                             */
/*              END.                                                                                */
/*                                                                                                  */
/*              DO j = 1 TO 20:                                                                     */
/*                 IF tt-ei.run-qty[j] GE v-qty THEN DO:                                            */
/*                    v-cost = tt-ei.run-cost[j].                                                   */
/*                    LEAVE.                                                                        */
/*                 END.                                                                             */
/*              END.                                                                                */
/*           END.                                                                                   */
/*                                                                                                  */
/* /*           IF AVAIL e-itemfg-vend THEN                                                      */ */
/* /*             RUN est/dim-charge.p (e-itemfg-vend.rec_key,                                   */ */
/* /*                                   DEC(job-farm.wid:SCREEN-VALUE IN BROWSE {&browse-name}), */ */
/* /*                                   DEC(job-farm.len:SCREEN-VALUE IN BROWSE {&browse-name}), */ */
/* /*                                   INPUT-OUTPUT v-cost).                                    */ */
/*         END.                                                                                     */
/*                                                                                                  */
/*         /* 02201203 */                                                                           */
/*         ASSIGN                                                                                   */
/*            v-cost   = decimal(job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name})           */
/*            v-uom    = job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}                      */
/*            v-cost-m = decimal(job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   )          */
/*            v-qty    = decimal(job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name})                */
/*            v-uom    = job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  .                  */
/*         IF AVAIL(job-hdr) THEN                                                                   */
/*            IF  job-farm.len GT 0 AND job-farm.wid GT 0 AND v-qty GT 0                            */
/*            AND v-cost GT 0 AND job-hdr.qty GT 0 AND job-farm.sc-uom = "MSF" THEN                 */
/*            v-cost-m = ((job-farm.LEN * job-farm.wid / 144) * (v-qty / 1000) * v-cost)            */
/*                        / job-hdr.qty * 1000.                                                     */
/*          ELSE                                                                                    */
/*            v-cost-m = (v-qty * v-cost) / (job-hdr.qty / 1000).                                   */
/*                                                                                                  */
/*         IF v-cost-m EQ ? THEN v-cost-m = 0.                                                      */
/*                                                                                                  */
/*         ASSIGN                                                                                   */
/*          job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost)                */
/*          job-farm.sc-uom:SCREEN-VALUE IN BROWSE {&browse-name}   = v-uom                         */
/*          job-farm.cost-m:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(v-cost-m)              */
/*          job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(v-qty)                 */
/*          job-farm.qty-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = v-uom.                        */
/*       END.                                                                                       */
/*                                                                                                  */
/*       IF NOT adm-adding-record AND prev-cost NE 0 THEN DO:                                       */
/*         RUN calc-job-qty (OUTPUT v-qty).                                                         */
/*         job-farm.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(prev-cost / v-qty).     */
/*       END.                                                                                       */
/*     END.                                                                                         */
/*   END.                                                                                           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty B-table-Win 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
job-farm-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
    STRING(totalQty()).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcJobFarm B-table-Win 
PROCEDURE recalcJobFarm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprJobRow AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcINo AS CHAR NO-UNDO.
DEF VAR dTotCost AS DEC NO-UNDO.
DEF VAR dTotIssue AS DEC NO-UNDO.
DEF VAR dTotExt AS DEC NO-UNDO.
DEF VAR cCostM AS DEC NO-UNDO.
DEF VAR dCostM AS DEC NO-UNDO.
DEF BUFFER bf-job-farm-rctd FOR job-farm-rctd.
FIND FIRST job WHERE ROWID(job) EQ iprJobRow NO-LOCK NO-ERROR.

FOR EACH job-farm WHERE job-farm.company EQ job.company   
      AND  job-farm.job EQ job.job
      AND (job-farm.i-no EQ ipcINo OR ipcINo = "")
    NO-LOCK.
  RUN jc/updJobFarmActual.p (INPUT iprJobRow, INPUT job-farm.i-no).

END.

PUBLISH 'refresh-job-farm'.
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

/*                                                                                                                            */
/*   IF INDEX("LRAW",job.stat) = 0 THEN DO:                                                                                   */
/*      MESSAGE "The job status must be 'R'eleased in order to perform this selection"                                        */
/*             VIEW-AS ALERT-BOX ERROR.                                                                                       */
/*      RETURN ERROR.                                                                                                         */
/*   END.                                                                                                                     */
/*                                                                                                                            */
/*   /*IF job-farm.post:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Y" THEN DO:                                                 */
/*      MESSAGE "It's posted. Unpost first..." VIEW-AS ALERT-BOX ERROR.                                                       */
/*      RETURN ERROR.                                                                                                         */
/*   END.*/                                                                                                                   */
/*                                                                                                                            */
/*   FIND FIRST itemfg                                                                                                        */
/*         WHERE itemfg.company EQ cocode                                                                                     */
/*           AND itemfg.i-no    EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}                                        */
/*           AND itemfg.i-code  EQ "R"                                                                                        */
/*         NO-LOCK NO-ERROR.                                                                                                  */
/*   IF NOT AVAIL itemfg THEN DO:                                                                                             */
/*       MESSAGE "Estimated Material cannot be Allocated, Press Update Key to Add Real Material ..." VIEW-AS ALERT-BOX ERROR. */
/*       RETURN ERROR.                                                                                                        */
/*   END.                                                                                                                     */
/*                                                                                                                            */
/*   lv-allocated = YES.                                                                                                      */
/*                                                                                                                            */
/*    /*                                                                                                                      */
/*                                                                                                                            */
/*   def var phandle as widget-handle no-undo.                                                                                */
/*   def var char-hdl as cha no-undo.                                                                                         */
/*     RUN get-link-handle IN adm-broker-hdl                                                                                  */
/*        (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).                                                                */
/*     phandle = WIDGET-HANDLE(char-hdl).                                                                                     */
/*                                                                                                                            */
/*     RUN new-state in phandle ('update-begin':U).                                                                           */
/*   ASSIGN job-farm.frm:READ-ONLY IN BROWSE {&browse-name} = YES                                                             */
/*            job-farm.BLANK-no:READ-ONLY = YES                                                                               */
/*            job-farm.i-no:READ-ONLY = YES                                                                                   */
/*            job-farm.std-cost:READ-ONLY = YES                                                                               */
/*            job-farm.sc-uom:READ-ONLY = YES                                                                                 */
/*            job-farm.cost-m:READ-ONLY = YES                                                                                 */
/*            job-farm.qty:READ-ONLY = YES                                                                                    */
/*            job-farm.qty-uom:READ-ONLY = YES                                                                                */
/*            job-farm.wid:READ-ONLY = YES                                                                                    */
/*            job-farm.len:READ-ONLY = YES                                                                                    */
/*            job-farm.n-up:READ-ONLY = YES                                                                                   */
/*            job-farm.basis-w:READ-ONLY = YES                                                                                */
/*            job-farm.post:READ-ONLY = YES.                                                                                  */
/*                                                                                                                            */
/*                                                                                                                            */
/*                                                                                                                            */
/*     APPLY "entry" TO job-farm.qty-all IN BROWSE {&browse-name}.                                                            */
/*  */                                                                                                                        */
/*    IF ip-ask THEN DO:                                                                                                      */
/*      IF AVAIL job-farm AND job-farm.all-flg THEN lv-alloc-char = "Deallocate?".                                            */
/*      ELSE lv-alloc-char = "Allocate?".                                                                                     */
/*                                                                                                                            */
/*      MESSAGE "Are you sure you want to " lv-alloc-char VIEW-AS ALERT-BOX WARNING                                                    */
/*          BUTTON YES-NO UPDATE ll.                                                                                          */
/*    END.                                                                                                                    */
/*                                                                                                                            */
/*    ELSE ll = YES.                                                                                                          */
/*                                                                                                                            */
/*    IF ll THEN DO:                                                                                                          */
/*       FIND b-job-farm WHERE ROWID(b-job-farm) EQ ROWID(job-farm) EXCLUSIVE-LOCK.                                           */
/*       b-job-farm.all-flg = YES.                                                                                            */
/*       IF lv-alloc-char BEGINS "alloc" THEN RUN jc/jc-all2.p (ROWID(b-job-farm), 1).                                        */
/*       ELSE RUN jc/jc-all2.p (ROWID(b-job-farm), -1).                                                                       */
/*       FIND CURRENT b-job-farm EXCLUSIVE-LOCK.                                                                              */
/*       IF b-job-farm.qty-all EQ 0 AND                                                                                       */
/*          NOT b-job-farm.all-flg  THEN b-job-farm.qty-all = b-job-farm.qty.                                                 */
/*       FIND CURRENT b-job-farm NO-LOCK.                                                                                     */
/*       RUN dispatch ("display-fields").                                                                                     */
/*    END.                                                                                                                    */
/*                                                                                                                            */
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
  {src/adm/template/snd-list.i "job-farm-rctd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-values B-table-Win 
PROCEDURE set-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcFarmJob AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiFarmJobNo2    AS INT NO-UNDO.
DEF INPUT PARAMETER ipcFarmIno       AS CHAR NO-UNDO.


ASSIGN
    ipFarmJob = ipcFarmJob
    ipFarmJobNo2 = ipiFarmJobNo2
    ipFarmIno    = ipcFarmIno.

RUN dispatch IN THIS-PROCEDURE ('open-query':U). 
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


/*   IF AVAIL job-farm THEN DO:                                      */
/*     lv-rowid = ROWID(job-farm).                                   */
/*                                                                   */
/*     RUN jc/shtcalc.p (INPUT-OUTPUT lv-rowid).                     */
/*                                                                   */
/*     IF lv-rowid NE ? THEN DO:                                     */
/*       RUN dispatch ("open-query").                                */
/*                                                                   */
/*       RUN repo-query (lv-rowid).                                  */
/*                                                                   */
/*       RUN dispatch ("display-fields").                            */
/*                                                                   */
/*       IF rmissue-log THEN do:                                     */
/*          ll = NO.                                                 */
/*          MESSAGE "Would you like to issue RM?"                    */
/*               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. */
/*          IF ll THEN RUN issue-mat.                                */
/*       END.                                                        */
/*     END.                                                          */
/*   END.                                                            */

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

/*                                                                               */
/*   DO WITH FRAME {&FRAME-NAME}:                                                */
/*     ASSIGN                                                                    */
/*      adm-new-record    = NO                                                   */
/*      adm-adding-record = NO.                                                  */
/*                                                                               */
/*     RUN check-for-commit (OUTPUT ll[1]).                                      */
/*                                                                               */
/*     FOR EACH w-po-ordl:                                                       */
/*       REPOSITION {&browse-name} TO ROWID w-po-ordl.job-mat-rowid NO-ERROR.    */
/*                                                                               */
/*       IF NOT ERROR-STATUS:ERROR THEN DO:                                      */
/*         RUN check-for-commit (OUTPUT ll[2]).                                  */
/*                                                                               */
/*         IF ll[2] THEN RUN run-alloc (NO).                                     */
/*                                                                               */
/*         APPLY "row-entry" TO BROWSE {&BROWSE-NAME}.                           */
/*                                                                               */
/*         ASSIGN                                                                */
/*          job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = w-po-ordl.i-no */
/*          job-farm.all-flg:SCREEN-VALUE IN BROWSE {&browse-name} = "N".        */
/*                                                                               */
/*         RUN new-i-no.                                                         */
/*                                                                               */
/*         ASSIGN                                                                */
/* /*          job-farm.qty-all:SCREEN-VALUE IN BROWSE {&browse-name} = */       */
/* /*             job-farm.qty:SCREEN-VALUE IN BROWSE {&browse-name}    */       */
/*          lv-allocated = NO.                                                   */
/*                                                                               */
/*         RUN dispatch ("update-record").                                       */
/*                                                                               */
/*         IF ll[1] THEN RUN run-alloc (YES).                                    */
/*       END.                                                                    */
/*                                                                               */
/*       DELETE w-po-ordl.                                                       */
/*     END.                                                                      */
/*                                                                               */
/*     RUN dispatch ("open-query").                                              */
/*     REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.                     */
/*     RUN dispatch ("row-changed").                                             */
/*   END.                                                                        */

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
        
/*   IF li-est-type NE 1 THEN                                                                       */
/*   DO WITH FRAME {&FRAME-NAME}:                                                                   */
/*     IF INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) ne 0 then do:                */
/*                                                                                                  */
/*       IF li-est-type EQ 2 OR li-est-type EQ 6 THEN                                               */
/*       FIND FIRST eb                                                                              */
/*           WHERE eb.company  EQ est.company                                                       */
/*             AND eb.est-no   EQ est.est-no                                                        */
/*             AND eb.blank-no EQ INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})      */
/*           NO-LOCK NO-ERROR.                                                                      */
/*       ELSE                                                                                       */
/*       FIND FIRST job-hdr                                                                         */
/*           WHERE job-hdr.company  EQ job-farm.company                                             */
/*             AND job-hdr.job      EQ job-farm.job                                                 */
/*             AND job-hdr.job-no   EQ job-farm.job-no                                              */
/*             AND job-hdr.job-no2  EQ job-farm.job-no2                                             */
/*             AND job-hdr.frm      EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})      */
/*             AND job-hdr.blank-no EQ INT(job-farm.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) */
/*           NO-LOCK NO-ERROR.                                                                      */
/*                                                                                                  */
/*       IF NOT AVAIL job-hdr AND NOT AVAIL eb THEN DO:                                             */
/*         MESSAGE "Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.                           */
/*         APPLY "entry" TO job-farm.blank-no IN BROWSE {&browse-name}.                             */
/*         RETURN ERROR.                                                                            */
/*       END.                                                                                       */
/*     END.                                                                                         */
/*   END.                                                                                           */

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
/*                                                                                          */
/*   RELEASE job-hdr.                                                                       */
/*   RELEASE ef.                                                                            */
/*                                                                                          */
/*   DO WITH FRAME {&frame-name}:                                                           */
/*     IF li-est-type EQ 2 OR li-est-type EQ 6 THEN                                         */
/*     FIND FIRST ef                                                                        */
/*         WHERE ef.company EQ est.company                                                  */
/*           AND ef.est-no  EQ est.est-no                                                   */
/*           AND ef.form-no EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name})      */
/*         NO-LOCK NO-ERROR.                                                                */
/*     ELSE                                                                                 */
/*     FIND FIRST job-hdr                                                                   */
/*         WHERE job-hdr.company EQ cocode                                                  */
/*           AND job-hdr.job     EQ job-farm.job                                            */
/*           AND job-hdr.job-no  EQ job-farm.job-no                                         */
/*           AND job-hdr.job-no2 EQ job-farm.job-no2                                        */
/*           AND job-hdr.frm     EQ INT(job-farm.frm:SCREEN-VALUE IN BROWSE {&browse-name}) */
/*         NO-LOCK NO-ERROR.                                                                */
/*     IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:                                       */
/*       MESSAGE "Must enter a valid form..." VIEW-AS ALERT-BOX ERROR.                      */
/*       APPLY "entry" TO job-farm.frm IN BROWSE {&browse-name}.                            */
/*       RETURN ERROR.                                                                      */
/*     END.                                                                                 */
/*   END.                                                                                   */
/*                                                                                          */
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
/*                                                                                               */
/*   DO WITH FRAME {&FRAME-NAME}:                                                                */
/*     job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =                                     */
/*         CAPS(job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}).                            */
/*                                                                                               */
/*     IF NOT CAN-FIND(FIRST itemfg                                                              */
/*                     {sys/look/itemfgW.i}                                                       */
/*                       AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}) */
/*     THEN DO:                                                                                  */
/*       MESSAGE "Must enter a valid RM..." VIEW-AS ALERT-BOX ERROR.                             */
/*       APPLY "entry" TO job-farm.i-no IN BROWSE {&browse-name}.                                */
/*       RETURN ERROR.                                                                           */
/*     END.                                                                                      */
/*   END.                                                                                        */

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

/*                                                                                      */
/*   DO WITH FRAME {&FRAME-NAME}:                                                       */
/*     FIND FIRST itemfg                                                                */
/*         {sys/look/itemfgW.i}                                                          */
/*           AND itemfg.i-no EQ job-farm.i-no:SCREEN-VALUE IN BROWSE {&browse-name}     */
/*         NO-LOCK NO-ERROR.                                                            */
/*     IF AVAIL itemfg THEN RUN sys/ref/uom-rm.p (itemfg.mat-type, OUTPUT lv-uom-list). */
/*                                                                                      */
/*     IF LOOKUP(ip-value,lv-uom-list) LE 0 THEN DO:                                    */
/*       MESSAGE "Must enter a valid UOM for this RM..." VIEW-AS ALERT-BOX ERROR.       */
/*       IF ip-field EQ "sc-uom" THEN                                                   */
/*         APPLY "entry" TO job-farm.sc-uom IN BROWSE {&browse-name}.                   */
/*       ELSE                                                                           */
/*         APPLY "entry" TO job-farm.qty-uom IN BROWSE {&browse-name}.                  */
/*       RETURN ERROR.                                                                  */
/*     END.                                                                             */
/*   END.                                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ItemDesc B-table-Win 
FUNCTION ItemDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDesc AS CHAR NO-UNDO.
  
  IF AVAIL job-farm-rctd THEN
  FIND FIRST ASI.itemfg OF ASI.job-farm-rctd NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN
      cDesc = itemfg.i-name.
  RETURN cDesc.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION totalQty B-table-Win 
FUNCTION totalQty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR dTotalQty AS DEC NO-UNDO.
dtotalQty = (DECIMAL(job-farm-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
            DECIMAL(job-farm-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
            DECIMAL(job-farm-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}).
RETURN dTotalQty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

