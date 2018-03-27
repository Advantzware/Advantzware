&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  pc\b-prdd.w

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

&SCOPED-DEFINE yellowColumnsName pc-prdd
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ls-start AS cha FORM "x(5)" NO-UNDO.
DEF VAR ls-stop AS cha FORM "x(5)" NO-UNDO.
DEF VAR li-help-job LIKE job.job NO-UNDO.
DEF VAR v-est-type AS INT INIT 1 NO-UNDO.
DEF VAR ll-no-frm AS LOG NO-UNDO.
DEF VAR ll-no-blk AS LOG NO-UNDO.
DEF VAR ll-skip AS LOG NO-UNDO.
DEF VAR lv-crt-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE oeDateAuto-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT g_company, "DCClosedJobs", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-log = LOGICAL(cRtnChar) NO-ERROR.

DEF TEMP-TABLE tt-job-mch NO-UNDO LIKE job-mch 
    FIELD row-id AS ROWID
    FIELD xtra-copy AS LOG.

DEF TEMP-TABLE w-jm NO-UNDO
   FIELD frm      LIKE job-mch.frm
   FIELD d-seq    LIKE mach.d-seq
   FIELD blank-no LIKE job-mch.blank-no
   FIELD row-id   AS   ROWID.

DEF BUFFER bf-prdd FOR pc-prdd.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
   {sys/inc/fgrecpt.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES pc-prdh
&Scoped-define FIRST-EXTERNAL-TABLE pc-prdh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR pc-prdh.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pc-prdd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table pc-prdd.job-no pc-prdd.job-no2 ~
pc-prdd.frm pc-prdd.blank-no pc-prdd.pass pc-prdd.i-no pc-prdd.i-name ~
pc-prdd.code pc-prdd.startx pc-prdd.stopx pc-prdd.hours pc-prdd.crew ~
pc-prdd.qty pc-prdd.waste pc-prdd.complete 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table pc-prdd.job-no ~
pc-prdd.job-no2 pc-prdd.frm pc-prdd.blank-no pc-prdd.pass pc-prdd.i-no ~
pc-prdd.code pc-prdd.startx pc-prdd.stopx pc-prdd.hours pc-prdd.crew ~
pc-prdd.qty pc-prdd.waste pc-prdd.complete 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table pc-prdd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table pc-prdd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH pc-prdd WHERE pc-prdd.company = pc-prdh.company ~
  AND pc-prdd.m-code = pc-prdh.m-code ~
  AND pc-prdd.op-date = pc-prdh.trans-date ~
  AND pc-prdd.shift = pc-prdh.shift NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH pc-prdd WHERE pc-prdd.company = pc-prdh.company ~
  AND pc-prdd.m-code = pc-prdh.m-code ~
  AND pc-prdd.op-date = pc-prdh.trans-date ~
  AND pc-prdd.shift = pc-prdh.shift NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table pc-prdd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table pc-prdd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD actual-entered B-table-Win 
FUNCTION actual-entered RETURNS LOGICAL
  ( INPUT ip-m-code AS CHAR, INPUT ip-job AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-time B-table-Win 
FUNCTION display-time RETURNS CHARACTER
  ( INPUT ip-time AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      pc-prdd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      pc-prdd.job-no COLUMN-LABEL "  Job#" FORMAT "x(6)":U WIDTH 8
            LABEL-BGCOLOR 14
      pc-prdd.job-no2 COLUMN-LABEL "" FORMAT "99":U
      pc-prdd.frm COLUMN-LABEL "Sheet" FORMAT ">>>":U LABEL-BGCOLOR 14
      pc-prdd.blank-no COLUMN-LABEL "Blank" FORMAT ">>>":U LABEL-BGCOLOR 14
      pc-prdd.pass FORMAT ">>>":U LABEL-BGCOLOR 14
      pc-prdd.i-no COLUMN-LABEL "Item#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      pc-prdd.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      pc-prdd.code FORMAT "x(5)":U LABEL-BGCOLOR 14
      pc-prdd.startx FORMAT "99:99":U LABEL-BGCOLOR 14
      pc-prdd.stopx FORMAT "99:99":U LABEL-BGCOLOR 14
      pc-prdd.hours FORMAT ">>9.99-":U LABEL-BGCOLOR 14
      pc-prdd.crew FORMAT ">9.9":U LABEL-BGCOLOR 14
      pc-prdd.qty FORMAT ">>>>>>>9-":U LABEL-BGCOLOR 14
      pc-prdd.waste FORMAT ">>>>9-":U LABEL-BGCOLOR 14
      pc-prdd.complete FORMAT "Y/N":U LABEL-BGCOLOR 14
  ENABLE
      pc-prdd.job-no
      pc-prdd.job-no2
      pc-prdd.frm
      pc-prdd.blank-no
      pc-prdd.pass
      pc-prdd.i-no
      pc-prdd.code
      pc-prdd.startx HELP "Enter Time in Military Time. (e.g. 2:00 PM = 14:00)"
      pc-prdd.stopx HELP "Enter Time in Military Time. (e.g. 2:00 PM = 14:00)"
      pc-prdd.hours
      pc-prdd.crew
      pc-prdd.qty
      pc-prdd.waste
      pc-prdd.complete
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.29 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 10.29 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 10.29 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.29 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.29 COL 2
     RECT-4 AT ROW 10.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.pc-prdh
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
         HEIGHT             = 10.48
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.pc-prdd WHERE ASI.pc-prdh <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _OrdList          = "ASI.pc-prdd.start|yes,ASI.pc-prdd.stopp|yes"
     _JoinCode[1]      = "ASI.pc-prdd.company = ASI.pc-prdh.company
  AND ASI.pc-prdd.m-code = ASI.pc-prdh.m-code
  AND ASI.pc-prdd.op-date = ASI.pc-prdh.trans-date
  AND ASI.pc-prdd.shift = ASI.pc-prdh.shift"
     _FldNameList[1]   > ASI.pc-prdd.job-no
"pc-prdd.job-no" "  Job#" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.pc-prdd.job-no2
"pc-prdd.job-no2" "" "99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.pc-prdd.frm
"pc-prdd.frm" "Sheet" ">>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.pc-prdd.blank-no
"pc-prdd.blank-no" "Blank" ">>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.pc-prdd.pass
"pc-prdd.pass" ? ">>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.pc-prdd.i-no
"pc-prdd.i-no" "Item#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.pc-prdd.i-name
"pc-prdd.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.pc-prdd.code
"pc-prdd.code" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.pc-prdd.startx
"pc-prdd.startx" ? "99:99" "character" ? ? ? 14 ? ? yes "Enter Time in Military Time. (e.g. 2:00 PM = 14:00)" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.pc-prdd.stopx
"pc-prdd.stopx" ? "99:99" "character" ? ? ? 14 ? ? yes "Enter Time in Military Time. (e.g. 2:00 PM = 14:00)" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.pc-prdd.hours
"pc-prdd.hours" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.pc-prdd.crew
"pc-prdd.crew" ? ">9.9" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.pc-prdd.qty
"pc-prdd.qty" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.pc-prdd.waste
"pc-prdd.waste" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.pc-prdd.complete
"pc-prdd.complete" ? ? "logical" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).

    RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR lw-focus AS HANDLE NO-UNDO.


    lw-focus = FOCUS.

    CASE lw-focus:NAME :
        WHEN "job-no" THEN DO:
             RUN windows/l-jobno.w (g_company,lw-focus:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
             IF rec-val NE ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr AND
                   (TRIM(job-hdr.job-no) NE TRIM(pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                    job-hdr.job-no2 NE DEC(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})      OR
                    job-hdr.frm NE DEC(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})              OR
                    job-hdr.blank-no NE DEC(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}))   THEN DO:
                  ASSIGN
                   pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = job-hdr.job-no
                   pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(job-hdr.job-no2)
                   pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(job-hdr.frm)
                   pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-hdr.blank-no).

                  RUN new-job-no (NO).
                  RUN new-job-hdr.
                END.
             END.
        END.
        WHEN "frm"      THEN RUN item-help (lw-focus).
        WHEN "blank-no" THEN RUN item-help (lw-focus).
        WHEN "i-no"     THEN RUN item-help (lw-focus).
        WHEN "code" THEN DO:
            RUN windows/l-jobcod.w (lw-focus:SCREEN-VALUE , OUTPUT char-val).
            IF char-val NE "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.        
    END CASE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
    
  {src/adm/template/brsentry.i}

  IF AVAIL pc-prdd THEN RUN get-est-type.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */

     {brsleave.i}
   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
   RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF pc-prdd.job-no IN BROWSE Browser-Table /*   Job# */
DO:
  IF pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE ""                    AND
     adm-new-record AND NOT adm-adding-record AND KEYLABEL(LASTKEY) NE "shift-tab" THEN DO:
    APPLY "entry" TO pc-prdd.CODE IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.job-no IN BROWSE Browser-Table /*   Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED THEN RUN new-job-no (YES).

    RUN valid-job-no (YES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED THEN RUN new-job-no (NO).

    RUN valid-job-no (NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.frm Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF pc-prdd.frm IN BROWSE Browser-Table /* Sheet */
DO:
  IF ll-no-frm THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.frm Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.frm IN BROWSE Browser-Table /* Sheet */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.blank-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF pc-prdd.blank-no IN BROWSE Browser-Table /* Blank */
DO:
  IF ll-no-blk THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.blank-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.blank-no IN BROWSE Browser-Table /* Blank */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.pass Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF pc-prdd.pass IN BROWSE Browser-Table /* Pass */
DO:
  IF pc-prdh.dept EQ "GL" OR (li-help-job EQ 0 AND pc-prdd.job EQ 0) THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.pass Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.pass IN BROWSE Browser-Table /* Pass */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-pass NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF pc-prdd.i-no IN BROWSE Browser-Table /* Item# */
DO:
  IF ll-no-blk THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.i-no IN BROWSE Browser-Table /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.code IN BROWSE Browser-Table /* Code */
DO:
   IF LASTKEY = -1 THEN RETURN.

   FIND FIRST job-code WHERE job-code.CODE = pc-prdd.CODE:SCREEN-VALUE IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL job-code THEN DO:
      MESSAGE "Invalid Job Code. Try Help. " VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   
   pc-prdd.CODE:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(pc-prdd.CODE:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.code Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF pc-prdd.code IN BROWSE Browser-Table /* Code */
DO:
  FIND FIRST job-code
      WHERE job-code.code EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL job-code THEN RUN show-crew (job-code.cat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.startx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.startx Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.startx IN BROWSE Browser-Table /* Start Time */
DO:
   IF LASTKEY = -1 THEN RETURN.

   IF pc-prdd.startx:MODIFIED IN BROWSE {&browse-name} THEN RUN new-mm.

   IF int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) >= 24 THEN DO:
      MESSAGE "Invalid Hours." substring(pc-prdd.startx:SCREEN-VALUE,1,2)
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) < 0 OR 
      int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) >= 60 THEN DO:
      MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   pc-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
      ROUND(( 
              (int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
       ( int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
      ) / 3600 ,2) ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.startx Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF pc-prdd.startx IN BROWSE Browser-Table /* Start Time */
DO:
  RUN new-hh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.stopx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.stopx Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.stopx IN BROWSE Browser-Table /* Stop Time */
DO:
  IF LASTKEY = -1 THEN RETURN.

  IF pc-prdd.stopx:MODIFIED IN BROWSE {&browse-name} THEN RUN new-mm.

   IF int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,1,2)) > 24 THEN DO:
      MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) < 0 OR 
      int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) >= 60 OR
        (int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) EQ 24 AND
         int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) NE 0) THEN DO:
      MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   pc-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
      ROUND(( 
              (int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
       ( int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
      ) / 3600 ,2) ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.stopx Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF pc-prdd.stopx IN BROWSE Browser-Table /* Stop Time */
DO:
  RUN new-hh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.crew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.crew Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-prdd.crew IN BROWSE Browser-Table /* Crew Size */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-crew NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/yellowColumns.i}
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
  {src/adm/template/row-list.i "pc-prdh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "pc-prdh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).

    RUN auto-add in phandle .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add-next B-table-Win 
PROCEDURE auto-add-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-prev FOR pc-prdd.

      /* stacey */
      MESSAGE "auto-add-next"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  FIND bf-prev WHERE RECID(bf-prev) = RECID(pc-prdd) NO-LOCK NO-ERROR.

  /* stacey */
  MESSAGE "bf-prev.i-no: " bf-prev.i-no
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  RUN auto-add.
  
  ASSIGN pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = bf-prev.job-no
         pc-prdd.job-no2:SCREEN-VALUE = string(bf-prev.job-no2).

  RUN new-job-no (NO).

  ASSIGN pc-prdd.blank-no:SCREEN-VALUE = STRING(bf-prev.blank-no)
         pc-prdd.frm:SCREEN-VALUE = STRING(bf-prev.frm)
         pc-prdd.i-no:SCREEN-VALUE = bf-prev.i-no
         pc-prdd.i-name:SCREEN-VALUE = bf-prev.i-name
         pc-prdd.job = bf-prev.job
         pc-prdd.dept = bf-prev.dept.

  RUN new-job-hdr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-copy B-table-Win 
PROCEDURE auto-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
    RUN auto-copy IN WIDGET-HANDLE(char-hdl).

    ASSIGN
     pc-prdd.startx:SCREEN-VALUE IN BROWSE {&browse-name} = "0000"
     pc-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name}  = "0000"
     pc-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name}  = ""
     pc-prdd.qty:SCREEN-VALUE IN BROWSE {&browse-name}    = ""
     pc-prdd.waste:SCREEN-VALUE IN BROWSE {&browse-name}  = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST job-hdr
        WHERE job-hdr.company    EQ pc-prdd.company
          AND job-hdr.job-no     EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2    EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (job-hdr.frm       EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               v-est-type EQ 2 OR v-est-type EQ 6)
          AND ((job-hdr.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
                INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0) OR
               (job-hdr.i-no     EQ pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
                pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "") OR
               v-est-type EQ 2 OR v-est-type EQ 6 OR
               INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
        NO-LOCK NO-ERROR.

    IF AVAIL job-hdr THEN DO:

      RELEASE reftable.

      FIND FIRST mach
          WHERE mach.company EQ pc-prdd.company
            AND mach.loc     EQ locode
            AND mach.m-code  EQ pc-prdd.m-code
          NO-LOCK NO-ERROR.

      IF (v-est-type EQ 2 OR v-est-type EQ 6)             AND
         (NOT AVAIL mach OR INDEX("AP",mach.p-type) LE 0) THEN DO:
          FIND FIRST reftable
              WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ pc-prdd.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(li-help-job,"999999999")
                AND reftable.val[12]  EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                AND (reftable.val[13] EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                     INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
              NO-LOCK NO-ERROR.
      END.

      pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF AVAIL reftable THEN reftable.code2 ELSE job-hdr.i-no.

      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

      pc-prdd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF AVAIL itemfg THEN itemfg.i-name ELSE "".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-job-hdr B-table-Win 
PROCEDURE display-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid AS RECID NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  FIND job-hdr WHERE RECID(job-hdr) EQ ip-recid NO-LOCK NO-ERROR.

  IF AVAIL job-hdr AND
     (job-hdr.frm NE INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})             OR
      (job-hdr.blank-no NE 0 AND
       job-hdr.blank-no NE INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})) OR
      job-hdr.i-no NE pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
  THEN DO:
    RUN display-item.

    IF v-est-type NE 2 AND v-est-type NE 6 THEN DO:
      IF NOT ll-no-frm THEN
        pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-hdr.frm).
      IF NOT ll-no-blk AND job-hdr.blank-no NE 0 THEN
        pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} =
                                                        STRING(job-hdr.blank-no).
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-est-type B-table-Win 
PROCEDURE get-est-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF pc-prdd.job <> 0 THEN DO:
     FIND FIRST job WHERE job.job = pc-prdd.job NO-LOCK NO-ERROR.
     IF AVAIL job THEN DO:
        FIND est WHERE est.company = job.company
                   AND est.est-no = job.est-no NO-LOCK NO-ERROR.
        IF AVAIL est THEN v-est-type = est.est-type - IF est.est-type > 4 THEN 4 ELSE 0.
     END.
  END.
  ELSE IF li-help-job <> 0 THEN DO:
      FIND FIRST job WHERE job.job = li-help-job NO-LOCK NO-ERROR.
      IF AVAIL job THEN DO:
        FIND est WHERE est.company = job.company
                   AND est.est-no = job.est-no NO-LOCK NO-ERROR.
        IF AVAIL est THEN v-est-type = est.est-type - IF est.est-type > 4 THEN 4 ELSE 0.
     END.
  END.

  FIND mach WHERE mach.company = g_company AND
                  mach.m-code = pc-prdh.m-code 
                  NO-LOCK NO-ERROR.
  ASSIGN
   ll-no-frm = v-est-type EQ 1
   ll-no-blk = v-est-type EQ 1 OR (mach.p-type NE "B" AND (v-est-type NE 3 OR pc-prdh.dept NE "PR")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-on B-table-Win 
PROCEDURE get-num-on :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    tt-job-mch.n-on = tt-job-mch.n-out.

    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ tt-job-mch.company
          AND mach.m-code  EQ tt-job-mch.m-code
        NO-ERROR.
    IF AVAIL mach AND NOT CAN-DO("A,P,B",mach.p-type) THEN
    FOR EACH job NO-LOCK
        WHERE job.company      EQ tt-job-mch.company
          AND job.job          EQ tt-job-mch.job
          AND job.job-no       EQ tt-job-mch.job-no
          AND job.job-no2      EQ tt-job-mch.job-no2
          AND TRIM(job.est-no) NE "",
        FIRST ef NO-LOCK
        WHERE ef.company EQ job.company
          AND ef.est-no  EQ job.est-no
          AND ef.form-no EQ tt-job-mch.frm:
      RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT tt-job-mch.n-on).
      tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE item-help B-table-Win 
PROCEDURE item-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.
  DEF VAR lv-jobitm AS CHAR NO-UNDO.
  DEF BUFFER bf-mach FOR mach.


  DO WITH FRAME {&FRAME-NAME}:
    lv-jobitm = TRIM(pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}) + "," +
                TRIM(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) + ","  +
                TRIM(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-jobit2.w (g_company, pc-prdd.job-no:SCREEN-VALUE, pc-prdd.job-no2:SCREEN-VALUE, lv-jobitm, OUTPUT char-val, OUTPUT rec-val).
    IF char-val NE "" THEN DO:
      ASSIGN
        pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}      = ENTRY(2,char-val).
      
     /*Don't apply blank number if machine is sheet or roll fed - task 02281303*/
      FIND FIRST bf-mach WHERE bf-mach.company = cocode 
          AND bf-mach.m-code = pc-prdd.m-code NO-LOCK NO-ERROR.
      IF AVAIL bf-mach AND LOOKUP(bf-mach.p-type,"R,S") = 0  THEN
           pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(3,char-val).
      
      RUN display-job-hdr (rec-val).
    END.
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
  DEF VAR ll-adding-record AS LOG NO-UNDO.
  
  DEF BUFFER bf-prdd FOR pc-prdd.


  /* Code placed here will execute PRIOR to standard behavior. */
  ll-adding-record = adm-adding-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-crt-rowid NE ? AND ll-adding-record THEN DO:
    FIND bf-prdd WHERE ROWID(bf-prdd) EQ lv-crt-rowid NO-LOCK NO-ERROR.
    IF AVAIL bf-prdd THEN DO:
      REPOSITION {&browse-name} TO ROWID lv-crt-rowid NO-ERROR.
      BROWSE {&browse-name}:SELECT-FOCUSED-ROW().
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-name AS cha NO-UNDO.
  DEF VAR ll-job-mch AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ls-name = pc-prdd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST job WHERE job.company = pc-prdd.company
                       AND job.job-no = pc-prdd.job-no
                       AND job.job-no2 = pc-prdd.job-no2
                       NO-LOCK NO-ERROR.
  IF li-help-job <> 0 THEN ASSIGN pc-prdd.job = li-help-job.
  ELSE IF pc-prdd.job = 0 THEN DO:
      IF AVAIL job THEN pc-prdd.job = job.job.
  END.
  IF AVAIL job AND job.start-date = ? THEN DO: /* task 07181407 */
  FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR.
  ASSIGN job.start-date = pc-prdd.op-date .
  END.

  FIND mach WHERE mach.company = g_company AND
                  mach.m-code = pc-prdh.m-code 
                  NO-LOCK NO-ERROR.  
  ASSIGN
   pc-prdd.i-name = ls-name
   pc-prdd.CODE   = CAPS(pc-prdd.CODE)
   pc-prdd.START  = (INT(SUBSTR(pc-prdd.startx,1,2)) * 3600) +
                    (INT(SUBSTR(pc-prdd.startx,3,2)) * 60)
   pc-prdd.stopp  = (INT(SUBSTR(pc-prdd.stopx,1,2)) * 3600) +
                    (INT(SUBSTR(pc-prdd.stopx,3,2)) * 60).

  ll-job-mch = NO.
  FOR EACH tt-job-mch:
    FIND FIRST job-mch WHERE ROWID(job-mch) EQ tt-job-mch.row-id NO-ERROR.
    IF NOT AVAIL job-mch THEN CREATE job-mch.
    BUFFER-COPY tt-job-mch TO job-mch NO-ERROR.
    /* this let's SB know data collection made changes */
    job-mch.est-op_rec_key = "DC " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS").

    IF tt-job-mch.row-id EQ ? THEN DO:
      IF ERROR-STATUS:ERROR THEN DELETE job-mch.
      IF AVAIL job-mch THEN DO:
        ASSIGN
         ll-job-mch       = YES
         job-mch.j-no     = 1
         job-mch.i-name   = pc-prdd.i-name
         job-mch.dept     = pc-prdd.dept
         job-mch.run-hr   = pc-prdd.hours
         job-mch.mr-rate  = mach.mr-rate
         job-mch.mr-varoh = mach.mr-varoh
         job-mch.mr-fixoh = mach.mr-fixoh
         job-mch.wst-prct = mach.run-spoil.
        IF pc-prdd.speed GT 0 THEN
          job-mch.speed    = pc-prdd.speed.
      END.

    END.

    FIND CURRENT job-mch NO-LOCK NO-ERROR.

    RELEASE job-mch.

    DELETE tt-job-mch.
  END.

  IF ll-job-mch THEN DO:
    EMPTY TEMP-TABLE w-jm.

    FOR EACH job-mch
        WHERE job-mch.company EQ pc-prdd.company
          AND job-mch.job     EQ pc-prdd.job
          AND job-mch.job-no  EQ pc-prdd.job-no
          AND job-mch.job-no2 EQ pc-prdd.job-no2
        NO-LOCK:
      FIND FIRST mach
          WHERE mach.company EQ g_company
            AND mach.loc     EQ g_loc
            AND mach.m-code  EQ job-mch.m-code
          NO-LOCK NO-ERROR.
       CREATE w-jm.
       ASSIGN
        w-jm.frm      = job-mch.frm
        w-jm.d-seq    = IF AVAIL mach THEN mach.d-seq ELSE 9999
        w-jm.blank-no = job-mch.blank-no
        w-jm.row-id   = ROWID(job-mch).
    END.

    li = 0.
    FOR EACH w-jm BY w-jm.frm BY w-jm.d-seq BY w-jm.blank-no:
      FIND job-mch WHERE ROWID(job-mch) EQ w-jm.row-id.
      li = li + 1.
      job-mch.LINE = li.
      FIND CURRENT job-mch NO-LOCK NO-ERROR.
      RELEASE job-mch.
    END.
  END.
  
  /*
  /* task 11170511 */  
  IF pc-prdd.code EQ "RUN" AND fgrecpt-char eq "AUTOPOST" THEN
  FOR EACH job-mch
      WHERE job-mch.company EQ cocode
        AND job-mch.job     EQ pc-prdd.job
        AND job-mch.job-no  EQ pc-prdd.job-no
        AND job-mch.job-no2 EQ pc-prdd.job-no2
        AND job-mch.frm     EQ pc-prdd.frm
      USE-INDEX line-idx NO-LOCK,
      FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ job-mch.m-code
        AND INDEX("AP",mach.p-type) LE 0
      NO-LOCK
      BY job-mch.line DESC:

    IF job-mch.m-code EQ pc-prdd.m-code THEN RUN proc-form-cmplt.

    LEAVE.
  END.  /* run */
  /* end of mods task 11170511*/
  */

  RUN pc/pcprdd3u.p (ROWID(pc-prdd)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN "adm-error".

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
  ASSIGN
   li-help-job       = 0
   adm-new-record    = NO
   adm-adding-record = NO.

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
  FIND mach WHERE mach.company = g_company AND
                  mach.m-code = pc-prdh.m-code 
                  NO-LOCK NO-ERROR.

  ASSIGN pc-prdd.company = pc-prdh.company
         pc-prdd.m-code = pc-prdh.m-code
         pc-prdd.op-date = pc-prdh.trans-date
         pc-prdd.op-time = TIME
         pc-prdd.shift = pc-prdh.shift
         pc-prdd.dept =  pc-prdh.dept
         pc-prdd.opn = YES
         pc-prdd.crew = 0
         pc-prdd.START = TIME
         pc-prdd.startx = "0000"
         pc-prdd.stopx = "0000"
         pc-prdd.USER-ID = USERID(LDBNAME(1)) 
         lv-crt-rowid = ROWID(pc-prdd).

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

  DISABLE TRIGGERS FOR LOAD OF pc-prdd.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH tt-job-mch:
    DELETE tt-job-mch.
  END.

  IF AVAIL pc-prdd THEN DO WITH FRAME {&FRAME-NAME}:
    li-help-job = pc-prdd.job.

    RUN get-est-type.

    BROWSE {&browse-name}:SELECT-FOCUSED-ROW() NO-ERROR.

    IF NOT adm-new-record OR adm-adding-record THEN
      APPLY "entry" TO pc-prdd.job-no IN BROWSE {&browse-name}.
  END.

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
  FOR EACH tt-job-mch:
    DELETE tt-job-mch.
  END.
      
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO browse-order.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
   /* ======== validation ==========*/
   RUN valid-job-no (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-frm (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-blank-no (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-pass NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-i-no (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   FIND FIRST job WHERE job.company = pc-prdd.company
                     AND job.job-no = pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     AND job.job-no2 = INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                   NO-LOCK NO-ERROR.
    if avail job and job.stat = "H" then do:
       message "JOB ON HOLD. DO NOT PROCEED!" 
                VIEW-AS alert-box.
       APPLY "entry" TO pc-prdd.job-no.
       return .
    end.

   FIND FIRST job-code WHERE job-code.CODE = pc-prdd.CODE:SCREEN-VALUE IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL job-code THEN DO:
      MESSAGE "Invalid Job Code. Try Help. " VIEW-AS ALERT-BOX.
      APPLY "entry" TO pc-prdd.code.
      RETURN NO-APPLY.
   END.

   IF int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) >= 24 THEN DO:
      MESSAGE "Invalid Hours." 
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) < 0 OR 
      int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) >= 60 THEN DO:
      MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.startx.
      RETURN NO-APPLY.
   END.
   IF int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,1,2)) > 24 THEN DO:
      MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.stopx.
      RETURN NO-APPLY.
   END.
   IF int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) < 0 OR 
      int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) >= 60 OR
        (int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) EQ 24 AND
         int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) NE 0) THEN DO:
      MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.stopx.
      RETURN NO-APPLY.
   END.

   pc-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
      ROUND(( 
              (int(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
       ( int(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
         INT(SUBSTRING(pc-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
      ) / 3600 ,2) ).
   IF INT(pc-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name}) < 0 THEN DO:
      message "Time entered will create negative hours, OK to proceed? "
              VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
      IF NOT ll-ans OR ll-ans = ? THEN DO:
         APPLY "entry" TO pc-prdd.stopx.
         RETURN .
      END.
   END.

  RUN valid-crew NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  ll-new-record = adm-new-record.
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   li-help-job       = 0
   adm-new-record    = NO
   adm-adding-record = NO.

  RUN repo-query (ROWID(pc-prdd)).

  IF ll-new-record THEN DO:
    RUN auto-copy /*auto-add-next.*/.
    RETURN NO-APPLY.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-frm B-table-Win 
PROCEDURE new-frm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-hh B-table-Win 
PROCEDURE new-hh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-hh AS CHAR NO-UNDO.
  DEF VAR lv-mm AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hh = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},1,2)
     lv-mm = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},4,2).

    IF INT(lv-hh) GT 2 AND INT(lv-hh) LT 10 THEN DO:
      FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING("0" + STRING(INT(lv-hh),"9") + lv-mm,FOCUS:FORMAT IN BROWSE {&browse-name}).
      APPLY "cursor-right" TO FOCUS IN BROWSE {&browse-name}.
      APPLY "cursor-right" TO FOCUS IN BROWSE {&browse-name}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-hdr B-table-Win 
PROCEDURE new-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST job-hdr
        WHERE job-hdr.company    EQ pc-prdd.company
          AND job-hdr.job-no     EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2    EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (job-hdr.frm       EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               v-est-type EQ 2 OR v-est-type EQ 6)
          AND ((job-hdr.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
                INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0) OR
               (job-hdr.i-no     EQ pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
                pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "") OR
               v-est-type EQ 2 OR v-est-type EQ 6 OR
               INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
        NO-LOCK NO-ERROR.

    IF AVAIL job-hdr THEN RUN display-job-hdr (RECID(job-hdr)).
  END.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no B-table-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.

  DEF VAR lv-frm LIKE pc-prdd.frm INIT 1 NO-UNDO.
  DEF VAR lv-blk LIKE pc-prdd.blank-no INIT 0 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + 
        TRIM(pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST job
        WHERE job.company   EQ pc-prdd.company
          AND job.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (job.job-no2  EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-log)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job THEN RETURN.
    li-help-job = job.job.

    RUN get-est-type.

    FIND FIRST mach
        WHERE mach.company EQ g_company
          AND mach.loc     EQ g_loc
          AND mach.m-code  EQ pc-prdd.m-code
        NO-LOCK NO-ERROR.

    IF AVAIL mach THEN lv-blk = IF mach.p-type EQ "B" THEN 1 ELSE 0.

    RELEASE job-mch.

    FIND FIRST job-mch
        WHERE job-mch.company  EQ pc-prdd.company
          AND job-mch.m-code   EQ pc-prdd.m-code
          AND job-mch.job      EQ li-help-job
          AND job-mch.job-no   EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-log)
        NO-LOCK NO-ERROR.

    IF AVAIL job-mch THEN
      ASSIGN
       lv-frm = job-mch.frm
       lv-blk = job-mch.blank-no.
    ELSE
      ASSIGN
       lv-frm = INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
       lv-blk = IF mach.p-type EQ "B" THEN INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) ELSE 0.
    /*
    FIND FIRST job-hdr
        WHERE job-hdr.company   EQ pc-prdd.company
          AND job-hdr.job-no    EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (job-hdr.job-no2  EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-log)
          AND job-hdr.frm       EQ lv-frm
          AND (job-hdr.blank-no EQ lv-blk OR lv-blk EQ 0)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN DO:
      pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.i-no.
      
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN pc-prdd.i-name:SCREEN-VALUE = itemfg.i-name.
    END.
    */

    ASSIGN
     pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(lv-frm)
     pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-blk).

    RUN new-job-hdr.

    RUN show-crew ("RUN").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-mm B-table-Win 
PROCEDURE new-mm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-hh AS CHAR NO-UNDO.
  DEF VAR lv-mm AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hh = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},1,2)
     lv-mm = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},4,2).

    lv-hh = STRING(INT(lv-hh),"99").

    FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(lv-hh + TRIM(lv-mm) + SUBSTR("00",1,2 - LENGTH(TRIM(lv-mm))),FOCUS:FORMAT IN BROWSE {&browse-name}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-form-cmplt B-table-Win 
PROCEDURE proc-form-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* from pc/pcprdd3u.p pcprdd4u.p */
   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   def var v-up-hs     like eb.num-up NO-UNDO.
   def var v-on        like eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   DEF VAR li-units AS INT NO-UNDO.

   DEF BUFFER b-reftable FOR reftable.

   FIND FIRST job WHERE job.company EQ pc-prdd.company
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

 /* IF v-assembled THEN do for both assembled or unassembled */
  FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
      EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ reftable.code2 NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = reftable.code2
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = reftable.val[2]
       fg-bin.std-lab-cost = reftable.val[1]
       fg-bin.std-fix-cost = reftable.val[4]
       fg-bin.std-var-cost = reftable.val[3]
       fg-bin.std-tot-cost = reftable.val[5]
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.case-count   = itemfg.case-count
       fg-bin.cases-unit   = 1
       fg-bin.unit-count   = fg-bin.case-count * fg-bin.cases-unit.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ cocode
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = cocode
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key
     li-units            = b-reftable.val[1].
    
    v-runqty = 0. 
    FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                       AND bf-prdd.m-code = pc-prdd.m-code
                       AND bf-prdd.job-no = pc-prdd.job-no
                       AND bf-prdd.job-no2 = pc-prdd.job-no2
                       AND bf-prdd.FRM = pc-prdd.frm
                       AND bf-prdd.blank-no = pc-prdd.blank-no
                       AND bf-prdd.pass = pc-prdd.pass
                       NO-LOCK:
        v-runqty = v-runqty + bf-prdd.qty.
    END.                                      /*employee_code*/
    /*RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/*/
    RUN pc/d-updbin.w  (ROWID(fg-bin), v-runqty, INPUT-OUTPUT li-units). /* pc-prdd.qty*/

    FIND CURRENT fg-bin NO-LOCK.

    ASSIGN
     b-reftable.val[1] = li-units
     b-reftable.val[2] = fg-bin.case-count
     b-reftable.val[3] = fg-bin.cases-unit.
  END.  /* v-assembled */


    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ pc-prdd.job-no
                        AND job.job-no2 EQ pc-prdd.job-no2
                        USE-INDEX job-no NO-ERROR.
    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

    IF AVAIL mach THEN
       FOR EACH mach-part WHERE
           mach-part.company EQ mach.company AND
           mach-part.m-code EQ mach.m-code
           EXCLUSIVE-LOCK:
           mach-part.total-impressions-run = mach-part.total-impressions-run
                                           + pc-prdd.qty + pc-prdd.waste.

           FIND FIRST reftable WHERE
                reftable.reftable EQ "MACHPARTHOURS" AND
                reftable.company  EQ mach-part.company AND
                reftable.loc      EQ mach-part.m-code AND
                reftable.code     EQ mach-part.rm-part-code
                EXCLUSIVE-LOCK NO-ERROR.
           
           IF NOT AVAIL reftable THEN DO:
              CREATE reftable.
              ASSIGN
                reftable.reftable = "MACHPARTHOURS"
                reftable.company  = mach-part.company
                reftable.loc      = mach-part.m-code
                reftable.code     = mach-part.rm-part-code. 
           END.
           
           reftable.val[1] = reftable.val[1]
                           + pc-prdd.hours.
           
           RELEASE reftable.
        END.

    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die ("D", v-est-type).

    if avail est then do:
       run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq job-mch.frm
        no-lock no-error.

    IF AVAIL ef THEN DO:
      RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      v-on = v-up * v-on.
    END.
                      
    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq job-mch.frm
          and (est-op.b-num  eq job-mch.blank-no OR job-mch.blank-no eq 0)
          and est-op.m-code  eq job-mch.m-code
          and est-op.op-pass eq job-mch.pass
          and est-op.dept    eq job-mch.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
      else v-out = 1.
      v-up = v-up * v-out.
    end.
    else v-up = 1.

    v-on = v-on / v-up.
  end.
           
  v-up-hs = 1.

  if job-mch.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */

/*======= 
 /* IF v-assembled THEN */
    IF pc-prdd.qty > 0 OR v-runqty > 0 THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0),
        EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type <> 4)
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 OR v-est-type <> 4) ,
        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq reftable.code2
          and itemfg.case-count gt 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = reftable.code2
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.
      
      if avail est and mach.p-type ne "B" then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq job-hdr.frm
              and (est-op.b-num  eq job-hdr.blank-no or
                   job-hdr.blank-no eq 0)
              and est-op.m-code  eq job-mch.m-code
              and est-op.op-pass eq job-mch.pass
              and est-op.dept    eq job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = reftable.val[13]
       fg-rctd.s-num      = reftable.val[12]
       fg-rctd.t-qty      = (IF pc-prdd.qty = 0 THEN v-runqty ELSE pc-prdd.qty)
                                 / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = reftable.val[5]
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ cocode
            AND b-reftable.code     EQ STRING(RECID(job-hdr))
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE RECID(fg-bin) EQ INT(b-reftable.code2) NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq cocode
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */

*/
   /* end of fg receipt creation */
 
  RELEASE fg-rctd.
  RELEASE job.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-set-cmplt B-table-Win 
PROCEDURE proc-set-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* from pc/pcprdd3u.p pcprdd4u.p */
   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   def var v-up-hs     like eb.num-up NO-UNDO.
   def var v-on        like eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   DEF VAR li-units AS INT NO-UNDO.
   
   DEF BUFFER b-reftable FOR reftable.

   FIND FIRST job WHERE job.company EQ cocode
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

 /* IF v-assembled THEN do for both assembled or unassembled */
/* FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
    */         

  FOR EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = job-hdr.i-no 
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = job-hdr.std-mat-cost
       fg-bin.std-lab-cost = job-hdr.std-lab-cost
       fg-bin.std-fix-cost = job-hdr.std-fix-cost
       fg-bin.std-var-cost = job-hdr.std-var-cost
       fg-bin.std-tot-cost = job-hdr.std-tot-cost
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ cocode
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = cocode
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key
     li-units            = b-reftable.val[1].
    
    v-runqty = 0. 
    FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                       AND bf-prdd.m-code = pc-prdd.m-code
                       AND bf-prdd.job-no = pc-prdd.job-no
                       AND bf-prdd.job-no2 = pc-prdd.job-no2
                       AND bf-prdd.FRM = pc-prdd.frm
                       AND bf-prdd.blank-no = pc-prdd.blank-no
                       AND bf-prdd.pass = pc-prdd.pass
                       NO-LOCK:
        v-runqty = v-runqty + bf-prdd.qty.
    END.                                      /*employee_code*/
    /*RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/*/
    RUN pc/d-updbin.w  (ROWID(fg-bin), v-runqty, INPUT-OUTPUT li-units). /* pc-prdd.qty*/

    FIND CURRENT fg-bin NO-LOCK.

    ASSIGN
     b-reftable.val[1] = li-units
     b-reftable.val[2] = fg-bin.case-count
     b-reftable.val[3] = fg-bin.cases-unit.
  END.  /* v-assembled */

/* === NEED more code later
  ELSE DO:     /* for unassembled sets
       THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
       ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
               */     
        {addon/touch/jobbin.i}
  END.
===*/

  /*=========== create fg receipt : from pc/r-wippst.w */
  /*FOR EACH bf-machtran WHERE bf-machtran.company = cocode AND
                                      bf-machtran.machine = machine_code AND
                                      bf-machtran.job_number = job_number AND
                                      bf-machtran.job_sub = INTEGER(job_sub) AND
                                      bf-machtran.form_number = INTEGER(form_number) AND
                                      bf-machtran.blank_number = INTEGER(blank_number) AND
                                      bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                      */
    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ pc-prdd.job-no
                        AND job.job-no2 EQ pc-prdd.job-no2
                        USE-INDEX job-no NO-ERROR.
    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

    IF AVAIL mach THEN
       FOR EACH mach-part WHERE
           mach-part.company EQ mach.company AND
           mach-part.m-code EQ mach.m-code
           EXCLUSIVE-LOCK:
           mach-part.total-impressions-run = mach-part.total-impressions-run
                                           + pc-prdd.qty + pc-prdd.waste.

           FIND FIRST reftable WHERE
                reftable.reftable EQ "MACHPARTHOURS" AND
                reftable.company  EQ mach-part.company AND
                reftable.loc      EQ mach-part.m-code AND
                reftable.code     EQ mach-part.rm-part-code
                EXCLUSIVE-LOCK NO-ERROR.
           
           IF NOT AVAIL reftable THEN DO:
              CREATE reftable.
              ASSIGN
                reftable.reftable = "MACHPARTHOURS"
                reftable.company  = mach-part.company
                reftable.loc      = mach-part.m-code
                reftable.code     = mach-part.rm-part-code. 
           END.
           
           reftable.val[1] = reftable.val[1]
                           + pc-prdd.hours.
           
           RELEASE reftable.
        END.

    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die ("D", v-est-type).

    if avail est then do:
       run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq job-mch.frm
        no-lock no-error.

    if avail ef then
      v-on = v-up *
             (if ef.n-out   eq 0 then 1 else ef.n-out) *
             (if ef.n-out-l eq 0 then 1 else ef.n-out-l) *
             (if ef.n-out-d eq 0 then 1 else ef.n-out-d).
                      
    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq job-mch.frm
          and (est-op.b-num  eq job-mch.blank-no OR job-mch.blank-no eq 0)
          and est-op.m-code  eq job-mch.m-code
          and est-op.op-pass eq job-mch.pass
          and est-op.dept    eq job-mch.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
      else v-out = 1.
      v-up = v-up * v-out.
    end.
    else v-up = 1.

    v-on = v-on / v-up.
  end.
           
  v-up-hs = 1.

  if job-mch.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */

 /* IF v-assembled THEN */
    IF pc-prdd.qty > 0 OR v-runqty > 0 THEN
    /*FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0), */
     FOR EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) ,
        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq job-hdr.i-no
          and itemfg.case-count gt 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
       fg-rctd.trans-time = TIME
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = job-hdr.i-no
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.
      
      if avail est and mach.p-type ne "B" then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq job-hdr.frm
              and (est-op.b-num  eq job-hdr.blank-no or
                   job-hdr.blank-no eq 0)
              and est-op.m-code  eq job-mch.m-code
              and est-op.op-pass eq job-mch.pass
              and est-op.dept    eq job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = job-mch.blank-no
       fg-rctd.s-num      = job-mch.frm
       fg-rctd.t-qty      = (IF pc-prdd.qty = 0 THEN v-runqty ELSE pc-prdd.qty) 
                               / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ cocode
            AND b-reftable.code     EQ job-hdr.rec_key
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE fg-bin.rec_key EQ b-reftable.code2 NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq cocode
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */

   /* end of fg receipt creation */
 
  RELEASE fg-rctd.
  RELEASE job.

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


  RUN dispatch ('open-query').

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ('row-changed').

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
  {src/adm/template/snd-list.i "pc-prdh"}
  {src/adm/template/snd-list.i "pc-prdd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-crew B-table-Win 
PROCEDURE show-crew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type LIKE job-code.cat NO-UNDO.

  DEF VAR lv-crew LIKE pc-prdd.crew NO-UNDO.
  DEF VAR lv-frm LIKE pc-prdd.frm INIT 1 NO-UNDO.
  DEF VAR lv-blk LIKE pc-prdd.blank-no INIT 0 NO-UNDO.


  IF ip-type EQ "MR" THEN ip-type = "M R".

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST job
        WHERE job.company EQ pc-prdd.company
          AND job.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

    RELEASE mach.
    IF AVAIL job THEN
    FIND FIRST mach
        WHERE mach.company EQ pc-prdd.company
          AND mach.m-code  EQ pc-prdd.m-code
        NO-LOCK NO-ERROR.

    IF AVAIL mach THEN DO:
      ASSIGN
       lv-crew = IF ip-type EQ "RUN" THEN mach.run-crusiz ELSE mach.mr-crusiz
       lv-frm  = INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
       lv-blk  = INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}).

      RELEASE eb.
      IF TRIM(job.est-no) NE "" THEN
      FIND FIRST eb
          WHERE eb.company   EQ job.company
            AND eb.est-no    EQ job.est-no
            AND eb.form-no   EQ lv-frm
            AND (eb.blank-no EQ lv-blk OR lv-blk EQ 0)
          NO-LOCK NO-ERROR.

      IF AVAIL eb THEN
        RUN est/getcrusz.p (ROWID(mach), ROWID(eb), pc-prdd.dept, ip-type,
                            INPUT-OUTPUT lv-crew).
    END.

    pc-prdd.crew:SCREEN-VALUE IN BROWSE {&browse-name} =
                                 STRING(IF lv-crew EQ 0 THEN 1 ELSE lv-crew).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die B-table-Win 
PROCEDURE update-plate-die :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-upd-type AS   CHAR NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.
  
  IF AVAIL itemfg THEN DO:
      IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.plate-no
          NO-ERROR.

      ELSE
      IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.die-no
          NO-ERROR.

      IF AVAIL prep THEN prep.no-of-impressions = prep.no-of-impressions +
                                                  pc-prdd.qty +
                                                  pc-prdd.waste.
      RELEASE prep.
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
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.
      
  
  IF NOT ll-no-blk THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-mch
                    WHERE job-mch.company  EQ pc-prdd.company
                      AND job-mch.job      EQ li-help-job
                      AND job-mch.job-no   EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND job-mch.job-no2  EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND job-mch.frm      EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND job-mch.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      IF v-est-type EQ 2 OR v-est-type EQ 6 THEN
      FIND FIRST job
          WHERE job.company EQ pc-prdd.company
            AND job.job     EQ li-help-job
            AND job.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ pc-prdd.company
                        AND eb.est-no   EQ job.est-no
                        AND eb.form-no  EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND eb.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}))
      THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO pc-prdd.blank-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    IF ip-changed THEN DO:
      IF INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN pc-prdd.i-no:SCREEN-VALUE = "".
      RUN new-job-hdr.
    END.
  END.

  IF ll-no-blk OR NOT ip-changed THEN RUN display-item.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-crew B-table-Win 
PROCEDURE valid-crew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(pc-prdd.crew:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0 THEN DO:
      MESSAGE TRIM(pc-prdd.crew:LABEL IN BROWSE {&browse-name}) +
              " must be greater than 0..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.crew IN BROWSE {&browse-name}.
      RETURN ERROR.
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
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.


  IF NOT ll-no-frm THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-mch
                    WHERE job-mch.company EQ pc-prdd.company
                      AND job-mch.job     EQ li-help-job
                      AND job-mch.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.frm IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF ip-changed THEN DO:
      IF INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN pc-prdd.i-no:SCREEN-VALUE = "".
      RUN new-job-hdr.
    END.
  END.

  IF ll-no-frm OR NOT ip-changed THEN RUN display-item.
  
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
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.
  

  IF NOT ll-no-blk THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company  EQ pc-prdd.company
                      AND job-hdr.job      EQ li-help-job
                      AND job-hdr.job-no   EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND job-hdr.job-no2  EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND (job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                           v-est-type EQ 2 OR v-est-type EQ 6) 
                      AND job-hdr.i-no     EQ pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      IF v-est-type EQ 2 OR v-est-type EQ 6 THEN
      FIND FIRST job
          WHERE job.company EQ pc-prdd.company
            AND job.job     EQ li-help-job
            AND job.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ pc-prdd.company
                        AND eb.est-no   EQ job.est-no
                        AND eb.form-no  EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND eb.stock-no EQ pc-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
      THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO pc-prdd.i-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    IF ip-changed THEN RUN new-job-hdr.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no B-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.
 

  DO WITH FRAME {&FRAME-NAME}:
    ll-skip = NO.

    pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + 
        TRIM(pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

     FIND FIRST job NO-LOCK 
          WHERE job.company  EQ g_company
            AND job.job-no   EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND (job.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                           ip-log) NO-ERROR .
    IF NOT AVAIL job THEN DO:
      MESSAGE "Invalid Job#, Try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-prdd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    ELSE IF AVAIL job AND job.opened EQ NO AND oeDateAuto-log THEN DO:
        MESSAGE "Job " STRING(job.job-no) " is currently closed. You must re-open the job to add data collection data." VIEW-AS ALERT-BOX INFO.
        APPLY "entry" TO pc-prdd.job-no IN BROWSE {&browse-name}.
       RETURN ERROR.
    END.

    ll-skip = YES.

    RUN display-item.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pass B-table-Win 
PROCEDURE valid-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-msg AS CHAR EXTENT 2 NO-UNDO.

  DEF BUFFER b-tt-job-mch FOR tt-job-mch.

           
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST tt-job-mch
        WHERE tt-job-mch.company EQ pc-prdd.company
          AND tt-job-mch.job     EQ li-help-job
          AND tt-job-mch.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND tt-job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND tt-job-mch.m-code  EQ pc-prdd.m-code
          AND tt-job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (tt-job-mch.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
               ll-no-blk)
          AND tt-job-mch.dept    EQ pc-prdd.dept
          AND tt-job-mch.pass    EQ INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
        USE-INDEX seq-idx NO-LOCK NO-ERROR.

    IF AVAIL tt-job-mch THEN
    FOR EACH b-tt-job-mch WHERE ROWID(b-tt-job-mch) NE ROWID(tt-job-mch)
        AND b-tt-job-mch.xtra-copy = NO:
      DELETE b-tt-job-mch.
    END.

    ELSE DO:
      FIND FIRST job-mch
          WHERE job-mch.company EQ pc-prdd.company
            AND job-mch.job     EQ li-help-job
            AND job-mch.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-mch.m-code  EQ pc-prdd.m-code
            AND job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND (job-mch.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                 ll-no-blk)
            AND job-mch.dept    EQ pc-prdd.dept
            AND job-mch.pass    EQ INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
          USE-INDEX seq-idx NO-LOCK NO-ERROR.

      IF NOT AVAIL job-mch THEN
      /* search without using dept */
      FIND FIRST job-mch
          WHERE job-mch.company EQ pc-prdd.company
            AND job-mch.job     EQ li-help-job
            AND job-mch.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-mch.m-code  EQ pc-prdd.m-code
            AND job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND (job-mch.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                 ll-no-blk)
            AND job-mch.pass    EQ INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
          USE-INDEX seq-idx NO-LOCK NO-ERROR.

      IF NOT AVAIL job-mch THEN DO:
        choice = NO.

        FIND FIRST job-mch
            WHERE job-mch.company EQ pc-prdd.company
              AND job-mch.job     EQ li-help-job
              AND job-mch.job-no  EQ pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              AND job-mch.job-no2 EQ INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
              AND job-mch.frm     EQ INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
              AND (job-mch.blank-no EQ INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                   ll-no-blk)
              AND job-mch.dept    EQ pc-prdd.dept
              AND job-mch.pass    EQ INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
            USE-INDEX seq-idx NO-LOCK NO-ERROR.

        IF AVAIL job-mch THEN DO:
          IF actual-entered(job-mch.m-code, job-mch.job) = NO AND job-mch.run-hr = 0 then
              ASSIGN
               v-msg[1] = "Machine " +
                          TRIM(pc-prdd.m-code) +
                          " is not defined in job standards for this job/form/blank/pass."
               v-msg[2] = "Would you like to replace machine " +
                          TRIM(job-mch.m-code) +
                          " with machine " + 
                          TRIM(pc-prdd.m-code) + "?".
          ELSE
               ASSIGN
               v-msg[1] = "Machine " +
                           TRIM(pc-prdd.m-code) +
                           " is not defined in job standards for this job/form/blank/pass."
               v-msg[2] = "Would you like to copy machine " +
                           TRIM(job-mch.m-code) +
                           " to machine " +
                           TRIM(pc-prdd.m-code) + "?".
        END.
        ELSE
          ASSIGN
           v-msg[1] = "ERROR: This dept is not valid for this job/form/blank/pass."
           v-msg[2] = "Would you like to add the department to job standards?".
        
        MESSAGE v-msg[1] SKIP v-msg[2]
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.

        DEF VAR v-save-n-out LIKE job-mch.n-out.
        DEF VAR v-save-original-mach LIKE job-mch.m-code NO-UNDO.
        IF AVAIL(job-mch) THEN
          v-save-original-mach = job-mch.m-code.

        IF choice THEN DO:
          CREATE tt-job-mch.

          IF AVAIL job-mch THEN
            BUFFER-COPY job-mch TO tt-job-mch
            ASSIGN
             tt-job-mch.row-id = ROWID(job-mch)
             tt-job-mch.m-code = pc-prdd.m-code.

          ELSE
            ASSIGN
             tt-job-mch.row-id   = ?
             tt-job-mch.company  = pc-prdd.company
             tt-job-mch.job      = li-help-job
             tt-job-mch.job-no   = pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
             tt-job-mch.job-no2  = INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.frm      = INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.blank-no = INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.pass     = INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.m-code   = pc-prdd.m-code
             tt-job-mch.dept     = pc-prdd.dept
             tt-job-mch.n-out    = 0
             tt-job-mch.n-on     = 0.

          /* 08281203 If has mr hr, do not replace, but copy and zero out mr-hr */
          IF AVAIL(job-mch) AND actual-entered(job-mch.m-code, job-mch.job) = YES THEN
              tt-job-mch.mr-hr = 0.

          IF tt-job-mch.n-out EQ 0 THEN tt-job-mch.n-out = 1.

          IF tt-job-mch.n-on  EQ 0 THEN RUN get-num-on.

          IF CAN-DO("CR,RC,GU",pc-prdd.dept) THEN DO:
            tt-job-mch.n-on = tt-job-mch.n-on / tt-job-mch.n-out.

            MESSAGE "Please enter #out for this pass?"
                UPDATE tt-job-mch.n-out.
DEF VAR lv-save-n-out LIKE job-mch.n-out.
            lv-save-n-out = tt-job-mch.n-out.
            v-save-n-out = tt-job-mch.n-out.
            tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
          END.

          /* task 08281203 - create 2nd record to copy instead of replace */
          CREATE tt-job-mch.

          IF AVAIL job-mch THEN
            BUFFER-COPY job-mch TO tt-job-mch
            ASSIGN
             /* tt-job-mch.row-id = ROWID(job-mch) */
             tt-job-mch.m-code = job-mch.m-code.
        
          ELSE
            ASSIGN
             tt-job-mch.row-id   = ?
             tt-job-mch.company  = pc-prdd.company
             tt-job-mch.job      = li-help-job
             tt-job-mch.job-no   = pc-prdd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
             tt-job-mch.job-no2  = INT(pc-prdd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.frm      = INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.blank-no = INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.pass     = INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.m-code   = pc-prdd.m-code
             tt-job-mch.dept     = pc-prdd.dept
             tt-job-mch.n-out    = 0
             tt-job-mch.n-on     = 0.
        
          IF tt-job-mch.n-out EQ 0 THEN tt-job-mch.n-out = 1.
        
          IF tt-job-mch.n-on  EQ 0 THEN RUN get-num-on.
        
          IF CAN-DO("CR,RC,GU",pc-prdd.dept) THEN DO:
            tt-job-mch.n-on = tt-job-mch.n-on / tt-job-mch.n-out.
        
/*            MESSAGE "Please enter #out for this pass?"
                UPDATE tt-job-mch.n-out. */

            tt-job-mch.n-out = lv-save-n-out.
        
            tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.

          END.


         IF avail(job-mch) AND actual-entered(job-mch.m-code, job-mch.job) = YES THEN DO:
            /* task 08281203 - create 2nd record to copy instead of replace */
            CREATE tt-job-mch.
            
            IF AVAIL job-mch THEN
              BUFFER-COPY job-mch TO tt-job-mch
                 ASSIGN
                   /* tt-job-mch.row-id = ROWID(job-mch) */
                   tt-job-mch.m-code = v-save-original-mach /* job-mch.m-code */
                   tt-job-mch.row-id = ?
                   tt-job-mch.xtra-copy = TRUE.          
            ELSE
              ASSIGN
               tt-job-mch.row-id   = ?
               tt-job-mch.company  = pc-prdd.company
               tt-job-mch.job      = li-help-job
               tt-job-mch.job-no   = pc-prdd.job-no
               tt-job-mch.job-no2  = pc-prdd.job-no2
               tt-job-mch.frm      = INT(pc-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.blank-no = INT(pc-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.pass     =  INT(pc-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.m-code   = pc-prdd.m-code
               tt-job-mch.dept     = pc-prdd.dept
               tt-job-mch.n-out    = 0
               tt-job-mch.n-on     = 0.
          
            IF tt-job-mch.n-out EQ 0 THEN tt-job-mch.n-out = 1.
          
            IF tt-job-mch.n-on  EQ 0 THEN RUN get-num-on.
          
            IF CAN-DO("CR,RC,GU",pc-prdd.dept) THEN DO:
              tt-job-mch.n-on = tt-job-mch.n-on / tt-job-mch.n-out.         

              tt-job-mch.n-out = v-save-n-out.
          
              tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
            
            END. 
         END.                          

        END.

        ELSE DO:
          APPLY "entry" TO pc-prdd.pass.
          RETURN ERROR.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION actual-entered B-table-Win 
FUNCTION actual-entered RETURNS LOGICAL
  ( INPUT ip-m-code AS CHAR, INPUT ip-job AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-qty AS DEC NO-UNDO.

v-qty = 0.
for each mch-act where mch-act.company = cocode and
                       mch-act.job = ip-job AND
                       mch-act.m-code = ip-m-code
                       no-lock:
  v-qty = v-qty + mch-act.hours.
END.

IF v-qty GT 0 THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-time B-table-Win 
FUNCTION display-time RETURNS CHARACTER
  ( INPUT ip-time AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN STRING(ip-time,"HH:MM") .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

