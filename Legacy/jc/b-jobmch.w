&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

&SCOPED-DEFINE yellowColumnsName b-jobmch
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

DEF TEMP-TABLE w-jm NO-UNDO
   FIELD d-seq    LIKE mach.d-seq
   FIELD rec-id   AS   RECID.

DEF VAR lv-start-time AS cha COLUMN-LABEL "Start!Time" NO-UNDO FORMAT 'X(5)'.
DEF VAR lv-end-time AS cha COLUMN-LABEL "End!Time" NO-UNDO FORMAT 'X(5)'.
DEF VAR lv-start-time-su AS cha COLUMN-LABEL "Setup!Start" NO-UNDO FORMAT 'X(5)'.
DEF VAR lv-end-time-su AS cha COLUMN-LABEL "Setup!End" NO-UNDO FORMAT 'X(5)'.
DEF VAR char-hdl AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES job-mch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-mch.frm job-mch.blank-no ~
job-mch.pass job-mch.m-code display-i-name () @ job-mch.i-name ~
job-mch.i-name job-mch.lag-time job-mch.dept ~
display-i-name () @ job-mch.i-name job-mch.mr-hr job-mch.mr-waste ~
job-mch.run-hr job-mch.speed job-mch.wst-prct job-mch.mr-rate ~
job-mch.mr-fixoh job-mch.mr-varoh job-mch.run-rate job-mch.run-fixoh ~
job-mch.run-varoh job-mch.start-date-su job-mch.end-date-su ~
job-mch.mr-complete ~
cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-start-time-su ~
job-mch.start-date job-mch.end-date ~
cvt-time-to-string('',job-mch.end-time-su,0.00) @ lv-end-time-su ~
job-mch.run-complete job-mch.anchored job-mch.run-qty ~
cvt-time-to-string('',job-mch.start-time,0.00) @ lv-start-time ~
cvt-time-to-string('',job-mch.end-time,0.00) @ lv-end-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table job-mch.frm ~
job-mch.blank-no job-mch.pass job-mch.m-code job-mch.i-name ~
job-mch.lag-time job-mch.dept job-mch.mr-hr job-mch.mr-waste job-mch.run-hr ~
job-mch.speed job-mch.wst-prct job-mch.run-qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-mch
&Scoped-define QUERY-STRING-br_table FOR EACH job-mch WHERE job-mch.company = job.company ~
  AND job-mch.job = job.job ~
  AND job-mch.job-no = job.job-no ~
  AND job-mch.job-no2 = job.job-no2 ~
use-index line-idx NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job-mch WHERE job-mch.company = job.company ~
  AND job-mch.job = job.job ~
  AND job-mch.job-no = job.job-no ~
  AND job-mch.job-no2 = job.job-no2 ~
use-index line-idx NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-mch
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-mch


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
company||y|ASI.job-mch.company
j-no||y|ASI.job-mch.j-no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
      job-mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      job-mch.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 4 COLUMN-FONT 0
            LABEL-BGCOLOR 14
      job-mch.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 4
            COLUMN-FONT 0
      job-mch.pass COLUMN-LABEL "P" FORMAT ">>9":U WIDTH 5
      job-mch.m-code COLUMN-LABEL "Machine" FORMAT "x(6)":U WIDTH 9
            LABEL-BGCOLOR 14
      display-i-name () @ job-mch.i-name
      job-mch.i-name COLUMN-LABEL "Item Name" FORMAT "x(20)":U
      job-mch.lag-time COLUMN-LABEL "Lag!Time" FORMAT ">>>>9":U
      job-mch.dept COLUMN-LABEL "Dept" FORMAT "x(2)":U WIDTH 4
      display-i-name () @ job-mch.i-name
      job-mch.mr-hr COLUMN-LABEL "MRHrs" FORMAT ">>9.99":U WIDTH 9
      job-mch.mr-waste COLUMN-LABEL "MRWst" FORMAT ">>>9":U WIDTH 6
      job-mch.run-hr COLUMN-LABEL "RunHrs" FORMAT ">>>,>>9.99":U
            WIDTH 9
      job-mch.speed FORMAT ">,>>>,>>9":U WIDTH 8
      job-mch.wst-prct COLUMN-LABEL "Wst %" FORMAT ">>9.99":U WIDTH 9
      job-mch.mr-rate COLUMN-LABEL "MR DL" FORMAT ">>9.99":U WIDTH 10
      job-mch.mr-fixoh COLUMN-LABEL "MR FOH" FORMAT ">>9.99":U
            WIDTH 10
      job-mch.mr-varoh COLUMN-LABEL "MR VOH" FORMAT ">>9.99":U
            WIDTH 10
      job-mch.run-rate COLUMN-LABEL "Run DL" FORMAT ">>9.99":U
            WIDTH 10
      job-mch.run-fixoh COLUMN-LABEL "Run FOH" FORMAT ">>9.99":U
            WIDTH 10
      job-mch.run-varoh COLUMN-LABEL "Run VOH" FORMAT ">>9.99":U
            WIDTH 10
      job-mch.start-date-su COLUMN-LABEL "Setup!Start Date" FORMAT "99/99/9999":U
      job-mch.end-date-su COLUMN-LABEL "Setup!End Date" FORMAT "99/99/9999":U
      job-mch.mr-complete COLUMN-LABEL "MR!Completed" FORMAT "yes/no":U
      cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-start-time-su
      job-mch.start-date COLUMN-LABEL "Run!Start Date" FORMAT "99/99/9999":U
      job-mch.end-date COLUMN-LABEL "Run!End Date" FORMAT "99/99/9999":U
      cvt-time-to-string('',job-mch.end-time-su,0.00) @ lv-end-time-su
      job-mch.run-complete COLUMN-LABEL "Run!Completed" FORMAT "yes/no":U
      job-mch.anchored COLUMN-LABEL "Locked?" FORMAT "Y/N":U
      job-mch.run-qty FORMAT ">>>,>>>,>>9":U WIDTH 18
      cvt-time-to-string('',job-mch.start-time,0.00) @ lv-start-time
      cvt-time-to-string('',job-mch.end-time,0.00) @ lv-end-time
  ENABLE
      job-mch.frm
      job-mch.blank-no
      job-mch.pass
      job-mch.m-code
      job-mch.i-name
      job-mch.lag-time
      job-mch.dept
      job-mch.mr-hr
      job-mch.mr-waste
      job-mch.run-hr
      job-mch.speed
      job-mch.wst-prct
      job-mch.run-qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 10.48
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     fi_sortby AT ROW 11.71 COL 15 COLON-ALIGNED
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
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 12.71
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/yellowColumns.i}
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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.job-mch WHERE ASI.job ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.job-mch.company = ASI.job.company
  AND ASI.job-mch.job = ASI.job.job
  AND ASI.job-mch.job-no = ASI.job.job-no
  AND ASI.job-mch.job-no2 = ASI.job.job-no2
use-index line-idx"
     _FldNameList[1]   > ASI.job-mch.frm
"job-mch.frm" "S" ">>>" "integer" ? ? 0 14 ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-mch.blank-no
"job-mch.blank-no" "B" ">>>" "integer" ? ? 0 ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-mch.pass
"job-mch.pass" "P" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.job-mch.m-code
"job-mch.m-code" "Machine" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-i-name () @ job-mch.i-name" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-mch.i-name
"job-mch.i-name" "Item Name" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job-mch.lag-time
"job-mch.lag-time" "Lag!Time" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-mch.dept
"job-mch.dept" "Dept" ? "character" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"display-i-name () @ job-mch.i-name" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.job-mch.mr-hr
"job-mch.mr-hr" "MRHrs" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.job-mch.mr-waste
"job-mch.mr-waste" "MRWst" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.job-mch.run-hr
"job-mch.run-hr" "RunHrs" ">>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.job-mch.speed
"job-mch.speed" ? ">,>>>,>>9" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.job-mch.wst-prct
"job-mch.wst-prct" "Wst %" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.job-mch.mr-rate
"job-mch.mr-rate" "MR DL" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.job-mch.mr-fixoh
"job-mch.mr-fixoh" "MR FOH" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.job-mch.mr-varoh
"job-mch.mr-varoh" "MR VOH" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.job-mch.run-rate
"job-mch.run-rate" "Run DL" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.job-mch.run-fixoh
"job-mch.run-fixoh" "Run FOH" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.job-mch.run-varoh
"job-mch.run-varoh" "Run VOH" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.job-mch.start-date-su
"job-mch.start-date-su" "Setup!Start Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.job-mch.end-date-su
"job-mch.end-date-su" "Setup!End Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.job-mch.mr-complete
"job-mch.mr-complete" "MR!Completed" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-start-time-su" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.job-mch.start-date
"job-mch.start-date" "Run!Start Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.job-mch.end-date
"job-mch.end-date" "Run!End Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"cvt-time-to-string('',job-mch.end-time-su,0.00) @ lv-end-time-su" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.job-mch.run-complete
"job-mch.run-complete" "Run!Completed" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.job-mch.anchored
"job-mch.anchored" "Locked?" "Y/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > ASI.job-mch.run-qty
"job-mch.run-qty" ? ">>>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"cvt-time-to-string('',job-mch.start-time,0.00) @ lv-start-time" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"cvt-time-to-string('',job-mch.end-time,0.00) @ lv-end-time" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  
  
  case focus:NAME:
      when "m-code" then do:
        run windows/l-mach.w (g_company,g_loc,focus:screen-value, output char-val).
        if char-val <> "" then do:
          FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          APPLY "value-changed" TO FOCUS.
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
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  RUN startSearch.
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


&Scoped-define SELF-NAME job-mch.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.frm br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mch.frm IN BROWSE br_table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.blank-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mch.blank-no IN BROWSE br_table /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.pass br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.pass IN BROWSE br_table /* P */
DO:
  IF job-mch.dept:SCREEN-VALUE IN BROWSE {&browse-name} EQ "GL" AND
     NOT CAN-FIND(FIRST sys-ctrl
                  WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name EQ 'JCGLPass'
                    AND sys-ctrl.log-fld EQ YES) THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.m-code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mch.m-code IN BROWSE br_table /* Machine */
DO:
  job-mch.m-code:SCREEN-VALUE IN BROWSE {&browse-name} =
      CAPS(job-mch.m-code:SCREEN-VALUE IN BROWSE {&browse-name}).

  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.m-code br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mch.m-code IN BROWSE br_table /* Machine */
DO:
  FIND FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ job-mch.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
      
  IF AVAIL mach THEN DO:
    IF mach.p-type NE "B" THEN
      ASSIGN
       job-mch.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = ""
       job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = "0".         

    ASSIGN
     job-mch.dept:SCREEN-VALUE IN BROWSE {&browse-name}      = mach.dept[1]
     job-mch.mr-rate:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(mach.mr-rate)
     job-mch.mr-fixoh:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(mach.mr-fixoh)
     job-mch.mr-varoh:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(mach.mr-varoh)
     job-mch.run-rate:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(mach.run-rate)
     job-mch.run-fixoh:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(mach.run-fixoh)
     job-mch.run-varoh:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(mach.run-varoh).
                
    IF adm-new-record THEN
      ASSIGN
       job-mch.wst-prct:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(mach.run-spoil)
       job-mch.lag-time:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(mach.daily-prod-hours).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.i-name br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.i-name IN BROWSE br_table /* Item Name */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.dept br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.dept IN BROWSE br_table /* Dept */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.dept br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mch.dept IN BROWSE br_table /* Dept */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dept (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-hr br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mch.run-hr IN BROWSE br_table /* RunHrs */
DO:
  RUN calc-new-speed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.speed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.speed br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mch.speed IN BROWSE br_table /* Speed */
DO:
  RUN calc-new-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.mr-rate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.mr-rate br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.mr-rate IN BROWSE br_table /* MR DL */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.mr-fixoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.mr-fixoh br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.mr-fixoh IN BROWSE br_table /* MR FOH */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.mr-varoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.mr-varoh br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.mr-varoh IN BROWSE br_table /* MR VOH */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-rate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-rate br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.run-rate IN BROWSE br_table /* Run DL */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-fixoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-fixoh br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.run-fixoh IN BROWSE br_table /* Run FOH */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-varoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-varoh br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.run-varoh IN BROWSE br_table /* Run VOH */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.mr-complete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.mr-complete br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.mr-complete IN BROWSE br_table /* MR!Completed */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-complete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-complete br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-mch.run-complete IN BROWSE br_table /* Run!Completed */
DO:
  APPLY "leave" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-qty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-mch.run-qty IN BROWSE br_table /* Run!Quantity */
DO:
  IF LASTKEY NE -1 AND KEYFUNCTION(LASTKEY) NE "back-tab" THEN DO:
    RUN dispatch ("update-record").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-qty br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-mch.run-qty IN BROWSE br_table /* Run!Quantity */
DO:
  RUN calc-new-hrs.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-new-hrs B-table-Win 
PROCEDURE calc-new-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-run-hrs AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ld-run-hrs = DEC(job-mch.run-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                 DEC(job-mch.speed:SCREEN-VALUE IN BROWSE {&browse-name}).
    job-mch.run-hr:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-run-hrs).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-new-speed B-table-Win 
PROCEDURE calc-new-speed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-run-speed AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ld-run-speed = DEC(job-mch.run-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                   DEC(job-mch.run-hr:SCREEN-VALUE IN BROWSE {&browse-name}).
    {sys/inc/roundup.i ld-run-speed}
    job-mch.speed:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-run-speed).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF BUFFER b-job-mch FOR job-mch.
  DEF VAR v-mr-completed AS LOG NO-UNDO.
  DEF VAR v-run-completed AS LOG NO-UNDO.

  ASSIGN v-mr-completed = job-mch.mr-complete
         v-run-completed = job-mch.run-complete.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  EMPTY TEMP-TABLE w-jm.

  FOR EACH b-job-mch
      WHERE b-job-mch.company EQ cocode
        AND b-job-mch.job     EQ job.job
        AND b-job-mch.job-no  EQ job.job-no
        AND b-job-mch.job-no2 EQ job.job-no2
      USE-INDEX line-idx:

    FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ b-job-mch.m-code
        NO-LOCK NO-ERROR.
    CREATE w-jm.
    ASSIGN
     w-jm.d-seq  = IF AVAIL mach THEN mach.d-seq ELSE 0
     w-jm.rec-id = RECID(b-job-mch).
  END.

  i = 0.
  FOR EACH w-jm, 
      FIRST b-job-mch WHERE RECID(b-job-mch) EQ w-jm.rec-id

      BY b-job-mch.frm
      BY w-jm.d-seq
      BY b-job-mch.blank-no
      BY b-job-mch.pass:
  
    i = i + 1.
    b-job-mch.line = i.
  END.

  IF ((v-mr-completed AND v-mr-completed <> job-mch.mr-complete) OR
     (v-run-completed AND v-run-completed <> job-mch.run-complete)) THEN
  FOR EACH b-job-mch WHERE b-job-mch.company EQ cocode
             AND b-job-mch.job     EQ job.job
             AND b-job-mch.job-no  EQ job.job-no
             AND b-job-mch.job-no2 EQ job.job-no2
             AND b-job-mch.frm = job-mch.frm             
             AND b-job-mch.LINE > job-mch.LINE
             USE-INDEX line-idx:
      ASSIGN b-job-mch.mr-complete = job-mch.mr-complete
             b-job-mch.run-complete = job-mch.run-complete.
  END.


  IF adm-new-record THEN DO:
     DEF VAR v-schedule-char AS cha NO-UNDO.
     find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
     v-schedule-char = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
     IF v-schedule-char = "Nodate" THEN
        ASSIGN job-mch.start-date = ?
               job-mch.start-date-su = ?
               job-mch.end-date = ?
               job-mch.end-date-su = ?.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-job-hdr FOR job-hdr.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   job-mch.company = job.company
   job-mch.job     = job.job
   job-mch.job-no  = job.job-no
   job-mch.job-no2 = job.job-no2.
  
  FIND FIRST b-job-hdr WHERE b-job-hdr.company  EQ job-mch.company
                         AND b-job-hdr.job      EQ job-mch.job 
                         AND b-job-hdr.job-no   EQ job-mch.job-no
                         AND b-job-hdr.job-no2  EQ job-mch.job-no2
                         AND b-job-hdr.frm      EQ INT(job-mch.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                         AND b-job-hdr.blank-no EQ INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NO-LOCK NO-ERROR.
  IF AVAIL b-job-hdr THEN
   job-mch.i-no = b-job-hdr.i-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN move-cursor-left.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO job-mch.frm IN BROWSE {&browse-name}.
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
  IF NOT AVAIL job-mch THEN RUN dispatch ('open-query').
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
  RUN valid-frm NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-m-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-dept (job-mch.dept:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN repo-query (ROWID(job-mch)).

  RUN move-cursor-left.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN view-user-id IN WIDGET-HANDLE(char-hdl).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"job-target",OUTPUT char-hdl).
  RUN view-user-id IN WIDGET-HANDLE(char-hdl).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-cursor-left B-table-Win 
PROCEDURE move-cursor-left :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&browse-name}:NUM-COLUMNS:
      APPLY "cursor-left" TO {&browse-name}.
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
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("value-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-schedule B-table-Win 
PROCEDURE run-schedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN schedule/capacityPage.p ("Job", ROWID(job), job.company).

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
  {src/adm/template/sndkycas.i "company" "job-mch" "company"}
  {src/adm/template/sndkycas.i "j-no" "job-mch" "j-no"}

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
  {src/adm/template/snd-list.i "job-mch"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no B-table-Win 
PROCEDURE valid-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE job-hdr.
  RELEASE eb.
        
  /*IF li-est-type NE 1 then*/  /* task 03141306   */
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) ne 0 then do:  
       
      IF li-est-type EQ 2 THEN
      FIND FIRST eb
          WHERE eb.company  EQ est.company
            AND eb.est-no   EQ est.est-no
            AND eb.blank-no EQ INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST job-hdr
          WHERE job-hdr.company  EQ job-mch.company
            AND job-hdr.job      EQ job-mch.job 
            AND job-hdr.job-no   EQ job-mch.job-no
            AND job-hdr.job-no2  EQ job-mch.job-no2
            AND job-hdr.frm      EQ INT(job-mch.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-hdr.blank-no EQ INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
            
      IF NOT AVAIL job-hdr AND NOT AVAIL eb THEN DO:
        MESSAGE "Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-mch.blank-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dept B-table-Win 
PROCEDURE valid-dept :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE EQ "GL" AND
       NOT CAN-FIND(FIRST sys-ctrl
                  WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name EQ 'JCGLPass'
                    AND sys-ctrl.log-fld EQ YES) THEN
      job-mch.pass:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
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
    IF li-est-type EQ 2 THEN
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ INT(job-mch.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    ELSE
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ job-mch.company
          AND job-hdr.job     EQ job-mch.job
          AND job-hdr.job-no  EQ job-mch.job-no
          AND job-hdr.job-no2 EQ job-mch.job-no2
          AND job-hdr.frm     EQ INT(job-mch.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:
      MESSAGE "Must enter a valid form..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-mch.frm IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code B-table-Win 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST mach
          {sys/look/machW.i}
            AND mach.m-code EQ job-mch.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

    IF AVAIL mach                             AND
       CAN-FIND(FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "mach.obsolete"
                  AND reftable.company  EQ mach.company
                  AND reftable.loc      EQ mach.loc
                  AND reftable.code     EQ mach.m-code
                  AND reftable.val[1]   EQ 1) THEN DO:
      MESSAGE "Machine is obsolete, please enter new machine..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-mch.m-code IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF NOT AVAIL mach       OR
       (INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
        mach.p-type EQ "B") THEN DO:
      IF AVAIL mach THEN DO:
        MESSAGE "ERROR: Must enter a valid blank..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-mch.blank-no IN BROWSE {&browse-name}.
      END.
      ELSE DO:
        MESSAGE "ERROR: Must enter a valid machine..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-mch.m-code IN BROWSE {&browse-name}.
      END.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF ip-type = "END" THEN DO:
     DEF VAR li-end-time AS INT NO-UNDO.
     li-end-time = ip-stime + ip-hour * 3600.
     RETURN STRING(li-end-time,"HH:MM").
  END.
  ELSE
  RETURN STRING(ip-stime,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-nam LIKE job-mch.i-name NO-UNDO.
  DEF VAR lv-frm LIKE job-mch.frm NO-UNDO.
  DEF VAR lv-blk LIKE job-mch.blank-no NO-UNDO.
  


  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL job-mch THEN
      ASSIGN
       lv-nam = job-mch.i-name
       lv-frm = job-mch.frm
       lv-blk = job-mch.blank-no.
    ELSE
      ASSIGN
       lv-nam = job-mch.i-name:SCREEN-VALUE IN BROWSE {&browse-name}
       lv-frm = INT(job-mch.frm:SCREEN-VALUE IN BROWSE {&browse-name})
       lv-blk = INT(job-mch.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF lv-nam EQ "" THEN DO:
      FIND job-hdr
          WHERE job-hdr.company   EQ job.company
            AND job-hdr.job       EQ job.job
            AND job-hdr.job-no    EQ job.job-no
            AND job-hdr.job-no2   EQ job.job-no2
            AND job-hdr.frm       EQ lv-frm
            AND (job-hdr.blank-no EQ lv-blk OR lv-blk EQ 0)
          NO-LOCK NO-ERROR.

      RELEASE itemfg.
      IF AVAIL job-hdr THEN
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN do:
          lv-nam = itemfg.i-name.
          IF lv-nam EQ "" THEN DO:
             FOR FIRST fg-set WHERE fg-set.company EQ itemfg.company
                                 AND fg-set.set-no = itemfg.i-no
                               NO-LOCK,
                FIRST itemfg WHERE itemfg.i-no = fg-set.part-no
                               AND itemfg.i-name GT ""
                                   NO-LOCK .
                  lv-nam = itemfg.i-name.
             END.
          END.
      END.
    END.
    IF lv-nam EQ "" AND avail(job-mch) THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-mch.company
            AND itemfg.i-no    EQ job-mch.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
          lv-nam = itemfg.i-name.
          IF lv-nam EQ "" THEN DO:

             FOR EACH fg-set WHERE fg-set.company EQ itemfg.company
                                 AND fg-set.set-no = itemfg.i-no
                               NO-LOCK.
                              
                 FIND FIRST itemfg WHERE itemfg.i-no = fg-set.part-no
                                   NO-LOCK NO-ERROR.
                 IF AVAIL itemfg AND itemfg.i-name GT "" THEN DO:
                     lv-nam = itemfg.i-name.
                     LEAVE.
                 END.
             END.
          END.
      END.

    END.

  END.
  RETURN lv-nam.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

