&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE yellowColumnsName b-trans

{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
 
DO TRANSACTION:
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}
END.

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF VAR lv-new-tag-number-chosen AS LOG NO-UNDO.
DEFINE VARIABLE unitsOH LIKE fg-rctd.t-qty NO-UNDO.
DEFINE VARIABLE iLineCnt AS INTEGER     NO-UNDO.
DEFINE VARIABLE lPostAuto-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-post-date AS DATE INITIAL TODAY.
DEF VAR fg-uom-list  AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-line-cnt NO-UNDO
  FIELD line-rowid AS ROWID 
  FIELD line-number AS INT .

cocode = g_company.
locode = g_loc.

DEF STREAM logFile.
DEF STREAM st-email.
DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.
DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DO TRANSACTION:
   {sys/inc/sstransf.i}
  END.

RUN sys/ref/nk1look.p (INPUT cocode, "SSPostFGTransfer", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lPostAuto-log = LOGICAL(cRtnChar) NO-ERROR.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table iLineCnt fg-rctd.tag ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.partial ~
fg-rctd.tag2 fg-rctd.loc2 fg-rctd.loc-bin2 fg-rctd.cust-no fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name fg-rctd.created-by ~
fg-rctd.updated-by fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.tag ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.partial ~
fg-rctd.loc2 fg-rctd.loc-bin2 fg-rctd.i-name 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = gcompany and ~
fg-rctd.rita-code = "T" and ~
fg-rctd.created-by = USERID("NOSWEAT") NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = gcompany and ~
fg-rctd.rita-code = "T" and ~
fg-rctd.created-by = USERID("NOSWEAT") NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

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
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 85 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      iLineCnt COLUMN-LABEL "#" FORMAT "99":U LABEL-BGCOLOR 14
      fg-rctd.tag COLUMN-LABEL "From!Tag" FORMAT "x(23)":U LABEL-BGCOLOR 14
      fg-rctd.loc COLUMN-LABEL "From!Whse" FORMAT "x(13)":U WIDTH 8
            LABEL-BGCOLOR 14
      fg-rctd.loc-bin COLUMN-LABEL "From!Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT ">>>,>>9":U WIDTH 9
            LABEL-BGCOLOR 14
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
            WIDTH 9 LABEL-BGCOLOR 14
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
            WIDTH 10
      fg-rctd.tag2 COLUMN-LABEL "To!Tag" FORMAT "x(23)":U LABEL-BGCOLOR 14
      fg-rctd.loc2 COLUMN-LABEL "To!Whse" FORMAT "x(13)":U WIDTH 7
            LABEL-BGCOLOR 14
      fg-rctd.loc-bin2 COLUMN-LABEL "To!Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      fg-rctd.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12
      fg-rctd.job-no FORMAT "x(6)":U WIDTH 10.4
      fg-rctd.job-no2 FORMAT "99":U WIDTH 3.2
      fg-rctd.i-no FORMAT "x(15)":U WIDTH 22
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.created-by COLUMN-LABEL "User!Created" FORMAT "x(8)":U
      fg-rctd.updated-by COLUMN-LABEL "User!Updated" FORMAT "x(8)":U
      fg-rctd.rct-date COLUMN-LABEL "Transfer!Date" FORMAT "99/99/9999":U
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Transfer!Time"
  ENABLE
      fg-rctd.tag
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.partial
      fg-rctd.loc2
      fg-rctd.loc-bin2
      fg-rctd.i-name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 15.05 COL 73 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 101 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 133 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
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
         HEIGHT             = 18.43
         WIDTH              = 154.4.
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:BOX-SELECTABLE   = TRUE
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:SELECTABLE       = TRUE
       FRAME F-Main:RESIZABLE        = TRUE.

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
     _TblList          = "asi.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "fg-rctd.company = gcompany and
fg-rctd.rita-code = ""T"" and
fg-rctd.created-by = USERID(""NOSWEAT"")"
     _FldNameList[1]   > "_<CALC>"
"iLineCnt" "#" "99" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.fg-rctd.tag
"fg-rctd.tag" "From!Tag" "x(23)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.fg-rctd.loc
"fg-rctd.loc" "From!Whse" "x(13)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.fg-rctd.loc-bin
"fg-rctd.loc-bin" "From!Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.fg-rctd.cases
"fg-rctd.cases" "Units" ? "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.fg-rctd.partial
"fg-rctd.partial" "Partial" ? "integer" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.fg-rctd.tag2
"fg-rctd.tag2" "To!Tag" "x(23)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.fg-rctd.loc2
"fg-rctd.loc2" "To!Whse" "x(13)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.fg-rctd.loc-bin2
"fg-rctd.loc-bin2" "To!Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.fg-rctd.cust-no
"fg-rctd.cust-no" "Customer#" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.fg-rctd.job-no
"fg-rctd.job-no" ? ? "character" ? ? ? ? ? ? no ? no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.fg-rctd.job-no2
"fg-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? no ? no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.fg-rctd.i-no
"fg-rctd.i-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.fg-rctd.created-by
"fg-rctd.created-by" "User!Created" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > asi.fg-rctd.updated-by
"fg-rctd.updated-by" "User!Updated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > asi.fg-rctd.rct-date
"fg-rctd.rct-date" "Transfer!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Transfer!Time" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 DEF VAR lv-rowid AS ROWID NO-UNDO.
 DEF VAR rec-val AS RECID NO-UNDO.
 DEF BUFFER bf-tmp FOR fg-rctd.

 IF NOT avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid no-lock no-error. 
 
 def var ll-tag# as log no-undo.
 ll-help-run = yes.
 case focus:name :
     when "i-no" then do:
           run windows/l-itemfg.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, output char-val).
           if char-val <> "" then do :
              assign fg-rctd.i-no:screen-value in browse {&browse-name} = entry(1,char-val)
                     fg-rctd.i-name:screen-value in browse {&browse-name} = entry(2,char-val)
                     .
           end.
           return no-apply.   
     end.

     WHEN "job-no" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "job-no2" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "loc" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "loc-bin" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "cust-no" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "tag" THEN DO:
       /*RUN fgbin-help.*/
          run windows/l-ldtag3.w (gcompany,no,focus:screen-value,output char-val,OUTPUT rec-val).
          if char-val <> "" then do :
             fg-rctd.tag:SCREEN-VALUE = ENTRY(1,char-val).
             /*  ===*/
             FIND FIRST bf-tmp WHERE bf-tmp.company = gcompany AND
                                     bf-tmp.rita-code = "T" AND
                                     bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                                 AND RECID(bf-tmp) <> RECID(fg-rctd)
                       NO-LOCK NO-ERROR.
             IF AVAIL bf-tmp THEN DO:
                MESSAGE "This Tag Number Has Already Been Used." skip
                        "Please Enter A Unique Tag Number." 
                        VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.
             
             FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF NOT AVAIL loadtag THEN DO:
                MESSAGE "Invalid Loadtag#. " 
                     VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.

             ASSIGN fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
                    fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
                    /*   fg-rctd.ord-no:SCREEN-VALUE = STRING(loadtag.ord-no) */
                    fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
                    fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name              
                    /*fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.pallet-count) /*qty*/*/
                    fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
                    fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.tot-cases)
                    /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle)*/
                    /*fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.case-bundle)*/
                    fg-rctd.loc:SCREEN-VALUE = loadtag.loc
                    fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
                    fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE  .
              /*      fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
                                    INT(fg-rctd.cases:SCREEN-VALUE) *
                                    INT(fg-rctd.qty-case:SCREEN-VALUE) +
                                    INT(fg-rctd.partial:SCREEN-VALUE)
                                    ,"->>>,>>>,>>9.99").

   FIND FIRST job-hdr WHERE job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   IF AVAIL job-hdr THEN 
      ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost) .
                */
             RUN leave-tag.
          END.
     END.
     WHEN "tag2" THEN DO:
       RUN fgbin2-help.
     END.
     when "loc2" then do:
           run windows/l-loc.w (fg-rctd.company,fg-rctd.loc2:screen-value, output char-val).
           if char-val <> "" then do :
              assign fg-rctd.loc2:screen-value in  browse {&browse-name}  = entry(1,char-val).             
              APPLY "tab" TO fg-rctd.loc2.
           end.
           
           return no-apply.   
     end.
     when "loc-bin2" then do:
           run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc2:screen-value,fg-rctd.loc-bin2:screen-value, output char-val).
           if char-val <> "" then do :
              assign fg-rctd.loc-bin2:screen-value  = entry(1,char-val).
              APPLY "tab" TO fg-rctd.loc-bin2.
           end.
           return no-apply.   
     end.

   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  /* display calculated field */
  /* def var ii as int.
   ii = if avail fg-rctd then integer(fg-rctd.po-no) else 0.
   
   if avail fg-rctd then    run get-matrix (true).
*/
  DEF VAR iHoldCnt AS INT.

  IF AVAIL(fg-rctd) AND ROWID(fg-rctd) NE ? THEN DO:
    FIND FIRST tt-line-cnt WHERE tt-line-cnt.line-rowid = ROWID(fg-rctd)
        NO-LOCK NO-ERROR.
    IF AVAIL tt-line-cnt THEN DO:
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:       
         iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-line-cnt.line-number).
      END.      
    END.
    ELSE DO:      
        iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:    
          iLineCnt = iLineCnt + 1.        
          iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt, "99").
          CREATE tt-line-cnt.
          ASSIGN tt-line-cnt.line-rowid = ROWID(fg-rctd)
                 tt-line-cnt.line-number = iLineCnt.
        END. /* if no error */
    END. /* if avail tt-line-cnt */
  END. /* if has a rowid */
  
END. /* Do */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i}*/
   {est/brsleave.i}  /* same as src but update will be same as add record*/
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


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:

   IF INTEGER(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) EQ iLineCnt 
      AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""
      AND adm-new-record 
    THEN
     iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt + 1).
   IF NOT adm-new-record THEN DO:
      APPLY "leave" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:
    
  IF LASTKEY NE -1 THEN DO:
    RUN leave-tag.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* From!Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
     IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
     END.

     RUN valid-job-loc-bin-tag NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* From!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
   /* Default */
   lv-new-tag-number-chosen = ?.
   IF sstransf-char = "Pallet" THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  DEF BUFFER b-loadtag FOR loadtag.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  FIND FIRST b-loadtag WHERE
       b-loadtag.company = gcompany AND
       b-loadtag.item-type = NO AND
       b-loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
       NO-LOCK NO-ERROR.
  /* 10081201 - units can be less if tag is different ***
  IF AVAIL b-loadtag AND NOT lv-new-tag-number-chosen THEN
  DO:
     IF INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) < b-loadtag.tot-cases THEN
     DO:
        MESSAGE "Units is Less Than Loadtag O/H Cases."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  IF sstransf-char = "Pallet" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc2 IN BROWSE Browser-Table /* To!Whse */
DO:
  DEF VAR lv-scanned-loc AS cha NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    lv-scanned-loc = focus:SCREEN-VALUE.
    IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = FOCUS:SCREEN-VALUE.
          ASSIGN fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin2:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
       END.

    RUN valid-loc2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF length(lv-scanned-loc) > 5 THEN do:
       RUN valid-loc-bin2 NO-ERROR.    
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       APPLY "tab" TO fg-rctd.loc-bin2 IN BROWSE {&browse-name} .       
       RETURN NO-APPLY.
    END.
    ELSE RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin2 IN BROWSE Browser-Table /* To!Bin */
DO:
  
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin2 NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  ELSE RETURN.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cust-no IN BROWSE Browser-Table /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  IF LASTKEY NE -1 THEN DO:
    lv-new-tag-number-chosen = ?.
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  FIND itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    BEGINS {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN
    ASSIGN
     {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}   = itemfg.i-no
     fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.i-name IN BROWSE Browser-Table /* Item Name */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
{custom/yellowColumns.i}
DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
    {sys/inc/fgpost.i}
END.

ASSIGN
 cocode = gcompany
 locode = gloc.

/* Done again because g_company is not set above */
find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "SSTRANSF"
    no-lock no-error.
IF AVAIL sys-ctrl THEN
ASSIGN sstransf-log = sys-ctrl.log-fld
       sstransf-char = sys-ctrl.char-fld
       sstransf-int = sys-ctrl.int-fld
       .
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-post B-table-Win 
PROCEDURE auto-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* IF no rows are selected and this is run, it will return an error */
  IF lPostAuto-log AND BROWSE Browser-Table:NUM-SELECTED-ROWS GT 0 THEN DO:
     RUN post-finish-goods.
  END.
  ELSE ERROR-STATUS:ERROR = NO .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE can-exit B-table-Win 
PROCEDURE can-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-can-exit AS LOG NO-UNDO.

   IF AVAIL fg-rctd AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
      RUN dispatch ('delete-record').
   END.

   op-can-exit = /*IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN YES ELSE NO.*/
                 YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-fgibn2.w (gcompany, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}      NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.tag     OR
                         fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.cust-no)
    THEN DO:
      FIND FIRST itemfg WHERE itemfg.company = gcompany AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
      ASSIGN
       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}    = fg-bin.i-no
       fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no.

      RUN new-bin.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin2-help B-table-Win 
PROCEDURE fgbin2-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR lv-rowid AS ROWID NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-fgibn1.w (gcompany, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag)
    THEN DO:
      ASSIGN
       fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag.

      
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-tag B-table-Win 
PROCEDURE leave-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-fg-rctd FOR fg-rctd.
  DEFINE BUFFER bf-reftable FOR reftable.

  DO WITH FRAME {&FRAME-NAME}:

    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF sstransf-char = "Pallet" THEN
       APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
    ELSE IF sstransf-char = "Case" THEN
         APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
    ELSE DO:

       FIND itemfg WHERE itemfg.company = g_company
                  AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF AVAIL itemfg AND itemfg.ship-meth THEN APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
       ELSE APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
    END.

   
    IF sstransf-int = 1 THEN
    DO:
      FOR LAST  b-fg-rctd FIELDS (b-fg-rctd.loc2 b-fg-rctd.loc-bin2) 
         WHERE b-fg-rctd.company = g_company
           AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} 
           AND b-fg-rctd.rita-code = "T" 
           AND ROWID(b-fg-rctd) NE ROWID (fg-rctd)
           AND b-fg-rctd.created-by = USERID("nosweat")
         NO-LOCK:
        
        ASSIGN fg-rctd.loc2:SCREEN-VALUE = b-fg-rctd.loc2
               fg-rctd.loc-bin2:SCREEN-VALUE = b-fg-rctd.loc-bin2.

             IF FOCUS:NAME = "loc2" AND adm-new-record THEN DO:
                 APPLY 'entry' TO fg-rctd.tag IN BROWSE {&browse-name}.
                 APPLY 'return' TO fg-rctd.tag IN BROWSE {&browse-name}.
                 
             END.
                
/*           DISPLAY b-fg-rctd.loc2 @ fg-rctd.loc2          */
/*                   b-fg-rctd.loc-bin2  @ fg-rctd.loc-bin2 */
/*                   WITH FRAME {&FRAME-NAME}.              */
        /* wfk - taken out 6/5/13 since it was causing problems */
        /* RUN local-update-record. */
/*         APPLY 'return' TO fg-rctd.loc2. */
      END. /* For last b-fg-rctd ... */
    END. /* If sstransf-int = 1 ... */

  END. /* Do with frame ... */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vNumSelected AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
DEF VAR wasmod AS LOG.

/* /* Trying to avoid error message when adding */                               */
/*  RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.                   */
/*                                                                               */
/*     vNumSelected = {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME}. */
/*    IF vNumSelected = 0 THEN DO:                                               */
/*        {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   */
/*    END.                                                                       */
/*                                                                               */
/*     IF {&BROWSE-NAME}:NUM-SELECTED-ROWS = 0 THEN                              */
/*     DO:                                                                       */
/*        /* workaround - this was erroring out if no rows exist on */           */
/*        /* entry into the browse                                  */           */
/*        CREATE fg-rctd.                                                        */
/*        ASSIGN fg-rctd.company = cocode                                        */
/*               fg-rctd.rita-code = "T"                                         */
/*               fg-rctd.created-by = USERID("NOSWEAT").                         */
/*        lv-recid = RECID(fg-rctd).                                             */
/*        RUN dispatch ('open-query').                                           */
/*                                                                               */
/*        APPLY "END" TO {&BROWSE-NAME}.                                         */
/*        {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.                          */
/*     END.                                                                      */

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
  DEF VAR lv-tag2 LIKE fg-rctd.tag2 NO-UNDO.
  DEF VAR lcJobNo LIKE fg-rctd.job-no NO-UNDO.
  DEF VAR liJobNo2 LIKE fg-rctd.job-no2 NO-UNDO.
  DEF VAR v-tag-no AS INT NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER bf-tag FOR loadtag.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    lv-tag2 = fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}.
    lcJobNo = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
    liJobNo2 = int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
   fg-rctd.tag2  = lv-tag2
   fg-rctd.job-no = lcJobNo
   fg-rctd.job-no2 = liJobNo2.

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     fg-rctd.pur-uom  = itemfg.prod-uom
     fg-rctd.cost-uom = itemfg.prod-uom
     fg-rctd.std-cost = itemfg.std-tot-cost.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.cust-no EQ fg-rctd.cust-no
      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
    ASSIGN
     fg-rctd.pur-uom  = fg-bin.pur-uom
     fg-rctd.cost-uom = fg-bin.pur-uom
     fg-rctd.std-cost = fg-bin.std-tot-cost
     fg-rctd.units-pallet = fg-bin.units-pallet
     fg-rctd.cases-unit   = fg-bin.cases-unit.

  ld = fg-rctd.std-cost.

  IF fg-rctd.pur-uom NE "EA" THEN
    RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                           ld, OUTPUT ld).

  fg-rctd.ext-cost = fg-rctd.t-qty * ld.

  /* Old 04/27/07 JLF
  FIND FIRST loadtag WHERE loadtag.company = g_company
                     AND loadtag.ITEM-type = NO
                     AND loadtag.tag-no = fg-rctd.tag NO-LOCK NO-ERROR.
  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
       /* AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin */
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.qty > 0
      NO-LOCK NO-ERROR.
  IF adm-new-record and
     fg-rctd.cases <> /*loadtag.tot-cases*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
  THEN DO:  /* Partial transfer */
     v-tag-no = 1.
     REPEAT:
     FIND first bf-tag WHERE bf-tag.company   EQ g_company
                           AND bf-tag.item-type EQ NO
                           AND bf-tag.tag-no    EQ STRING(CAPS(fg-rctd.i-no),"x(15)") +
                                                STRING(v-tag-no,"99999") 
                           NO-LOCK NO-ERROR.

         IF NOT AVAIL bf-tag THEN LEAVE.
         v-tag-no = v-tag-no + 1.
     END. /* repeat */
     IF v-tag-no = 0 THEN v-tag-no = 1.
  
     CREATE bf-tag.
     BUFFER-COPY loadtag EXCEPT loadtag.tag-no TO bf-tag.
     ASSIGN bf-tag.tag-no       = /*string(ip-tag-no,"99999") + STRING(w-ord.i-no,"x(15)") */
                          STRING(CAPS(fg-rctd.i-no),"x(15)") + STRING(v-tag-no,"99999") 
            /*loadtag.po-no      = w-ord.po-no*/
           bf-tag.job-no       = fg-rctd.job-no
           bf-tag.job-no2      = fg-rctd.job-no2
           /*loadtag.ord-no       = fg-rctd.ord-no*/
           bf-tag.i-no         = CAPS(fg-rctd.i-no)
           bf-tag.i-name       = fg-rctd.i-name
           bf-tag.qty          = fg-rctd.t-qty
           bf-tag.qty-case     = fg-rctd.qty-case
           bf-tag.tot-case     = fg-rctd.cases
           /*bf-tag.case-bundle  = fg-rctd.cases*/
           bf-tag.pallet-count = fg-rctd.cases * fg-rctd.qty-case
           bf-tag.partial      = fg-rctd.partial /*fg-rctd.total-unit MOD fg-rctd.pcs*/
           bf-tag.sts = "Transfered"  /* task 10190414 */
           bf-tag.tag-date = TODAY
           bf-tag.tag-time = TIME
           bf-tag.loc = fg-rctd.loc2
           bf-tag.loc-bin = fg-rctd.loc-bin2.
           .
           fg-rctd.tag2 = bf-tag.tag-no.

  END.  /* partial */
  Old 04/27/07 JLF */

  /* New 04/27/07 JLF */
    IF AVAIL fg-bin                AND
     fg-bin.qty GT fg-rctd.t-qty AND
     fg-bin.tag NE ""            AND
     fg-bin.tag EQ fg-rctd.tag2  AND
     lv-new-tag-number-chosen  THEN DO:
            RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT fg-rctd.tag2).
            FIND FIRST loadtag 
                WHERE loadtag.company EQ cocode
                  AND loadtag.tag-no EQ fg-rctd.tag2
              NO-LOCK NO-ERROR.
            IF AVAIL loadtag THEN
                RUN addon/loadtags/d-tagprt.w (RECID(loadtag)).
    END.
  /* New 04/27/07 JLF */
   lv-new-tag-number-chosen = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN BROWSE {&browse-name}
         fg-rctd.i-no
         fg-rctd.tag2
         fg-rctd.rct-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER bf-fg-rctd FOR fg-rctd.
DEF BUFFER bf-reftable FOR reftable.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-new-tag-number-chosen = ?.

  /* Remove any blank records created by local-add */
  
  FOR EACH  b-fg-rctd 
      WHERE b-fg-rctd.company   EQ cocode
        AND b-fg-rctd.rita-code EQ "T"
        AND b-fg-rctd.i-no      EQ ""
        AND b-fg-rctd.tag       EQ ""
        AND b-fg-rctd.loc       EQ ""
        AND b-fg-rctd.loc-bin   EQ ""
        AND b-fg-rctd.created-by EQ USERID("NOSWEAT")
      NO-LOCK:
      FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ ROWID(b-fg-rctd)
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL bf-fg-rctd THEN
        DELETE bf-fg-rctd.
  END.
    
  BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
  RUN adm-open-query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER bf-rctd FOR fg-rctd.

  /* Code placed here will execute PRIOR to standard behavior. */

  FOR EACH bf-rctd WHERE bf-rctd.company EQ gcompany NO-LOCK
      USE-INDEX fg-rctd
      BY bf-rctd.r-no DESC:
    lv-rno = bf-rctd.r-no.
    LEAVE.
  END.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  assign fg-rctd.company = gcompany
         fg-rctd.r-no    = lv-rno + 1
         fg-rctd.rita-code = "T"
         fg-rctd.s-num  = 0
         fg-rctd.rct-date = today
         fg-rctd.trans-time = TIME
         fg-rctd.units-pallet = 1
         fg-rctd.cases-unit   = 1.

  disp fg-rctd.rct-date with browse {&browse-name}. 
  lv-recid = recid(fg-rctd).  
/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and fg-rctd:po-no:screen*/
  run tag-sequence.
*/  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-trans-tag-no AS cha NO-UNDO.
  DEF VAR vNumSelected AS INT NO-UNDO.
  DEF BUFFER bf-fg-rctd FOR fg-rctd.
  DEF BUFFER bf-tt-line-cnt FOR tt-line-cnt.
  DEF VAR iCnt AS INT.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  v-trans-tag-no = "".
  IF fg-rctd.tag <> fg-rctd.tag2 THEN DO:
     v-trans-tag-no = fg-rctd.tag2.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */

/* Trying to avoid error message when adding */
    vNumSelected = {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME}.
   IF vNumSelected = 0 THEN DO:
       {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid NO-LOCK no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-trans-tag-no <> "" THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = v-trans-tag-no NO-ERROR.      

     IF AVAIL loadtag AND loadtag.sts = "Transfered" THEN DELETE loadtag.
  END.

  /* Renumber temp-table records after a delete */
  iCnt = 0.
  FOR EACH tt-line-cnt 
    BY tt-line-cnt.line-number:

    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ tt-line-cnt.line-rowid
       NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-fg-rctd THEN DO:
       DELETE tt-line-cnt.
    END.
    ELSE DO:
      FIND bf-tt-line-cnt WHERE ROWID(bf-tt-line-cnt) EQ ROWID(tt-line-cnt)
        NO-ERROR.
      iCnt = iCnt + 1.      
      bf-tt-line-cnt.line-number = iCnt.
    END.

  END. /* For each */
  iLineCnt = iCnt.
  run dispatch ('open-query').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Prevents error relating to setting attribute on browse that has focus */
  APPLY 'entry' TO Btn_Clear_Find /* auto_find */ IN FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iHoldCnt AS INTEGER     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL(fg-rctd) AND ROWID(fg-rctd) NE ? THEN DO:

    FIND FIRST tt-line-cnt WHERE tt-line-cnt.line-rowid = ROWID(fg-rctd)
        NO-LOCK NO-ERROR.
    IF AVAIL tt-line-cnt THEN DO:
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:       
         iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-line-cnt.line-number).
      END.
      
    END.
    ELSE DO:
      
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:    
        iLineCnt = iLineCnt + 1.        
        iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt, "99").
        CREATE tt-line-cnt.
        ASSIGN tt-line-cnt.line-rowid = ROWID(fg-rctd)
               tt-line-cnt.line-number = iLineCnt.
      END.
    END.  
  END.

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
   
 /* def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
     hd-post-child = hd-post:current-window.    
    /*  
     do while valid-handle(hd-post-child):
        ii = ii + 1.
        hd-post-child = hd-post-child:first-child.  /* frame */
       /* if hd-post-child:type = "field-group" 
           then hd-next = hd-post-child:next-sibling.
       */
       message ii valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.   
     end. 
    */ 
     hd-post-child = hd-post-child:first-child.  /* frame */
     hd-post-child = hd-post-child:first-child. /* field-group */
     hd-post-child = hd-post-child:first-child.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:sensitive = no.
  end.
====*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

FRAME {&FRAME-NAME}:VISIBLE = TRUE.
BROWSE {&browse-name}:MOVE-TO-TOP().
BROWSE {&browse-name}:SELECT-FOCUSED-ROW() NO-ERROR.

  IF NOT adm-new-record THEN DO:
     FIND itemfg WHERE itemfg.company = g_company
                  AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
     IF AVAIL itemfg AND itemfg.ship-meth THEN APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
     ELSE APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.

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
  DEF VAR v-dumb AS LOG NO-UNDO.
  DEF VAR v-progstack AS CHAR NO-UNDO.

v-progstack = (IF PROGRAM-NAME(1) NE ? THEN "," + PROGRAM-NAME(1) ELSE "") 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
        + (IF PROGRAM-NAME(8) NE ? THEN "," + PROGRAM-NAME(8) ELSE "")
        + (IF PROGRAM-NAME(9) NE ? THEN "," + PROGRAM-NAME(9) ELSE "")
        + (IF PROGRAM-NAME(10) NE ? THEN "," + PROGRAM-NAME(10) ELSE "")
        + (IF PROGRAM-NAME(11) NE ? THEN "," + PROGRAM-NAME(11) ELSE "").
  /* Code placed here will execute PRIOR to standard behavior. */

  /* when new record created from last row, get error "No fg-rctd" record ava */

  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN RETURN.
  IF NOT AVAIL fg-rctd THEN
     FIND fg-rctd WHERE RECID(fg-rctd) EQ lv-recid NO-LOCK NO-ERROR.

  IF int(fg-rctd.cases:SCREEN-VALUE) <= 0 THEN DO:
     MESSAGE "Unit Count must be greater than 0." VIEW-AS ALERT-BOX.
     APPLY "entry" TO fg-rctd.tag.
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
     RETURN NO-APPLY.
  END.

  RUN valid-job-loc-bin-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-loc2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-loc-bin2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
 
  IF AVAIL fg-rctd THEN DO:  
    RUN local-initialize.
    DEF VAR rCurrentRow AS ROWID.
    DEF VAR iNumRows AS INT .
    iNumRows = BROWSE {&BROWSe-name}:FOCUSED-ROW.
    rCurrentRow = ROWID(fg-rctd).
    BROWSE {&BROWSE-NAME}:SELECT-ROW(iNumRows) NO-ERROR.
    IF ERROR-STAT:ERROR THEN
     BROWSE {&BROWSE-NAME}:SELECT-ROW(iNumRows - 1) NO-ERROR.    
  END.

  /*
  FIND loadtag WHERE loadtag.company = g_company
                 AND loadtag.ITEM-type = NO
                 AND loadtag.tag-no = fg-rctd.tag2 NO-LOCK NO-ERROR.
  */
  lv-new-tag-number-chosen = ?.
  /* Workaround: if tab through to row-leave, scan-next doesn't get run */
  /* so if we're not here because of p-updbar, then run it              */
  IF INDEX(v-progstack, "p-updba") = 0 THEN DO: 
      RUN scan-next.          
  END.
  ELSE
      IF INDEX(v-progstack, "trnss") GT 0 THEN 
          RUN scan-next. /* Didn't work on the SS icon without this */

  v-dumb = NO NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-small B-table-Win 
PROCEDURE make-small :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    BROWSE {&browse-name}:HEIGHT-CHARS = 6.19.
     browse-order:VISIBLE = NO.
          
     auto_find:VISIBLE = NO.
     Btn_Clear_Find:VISIBLE = NO.
     RECT-4:VISIBLE = NO.
     RECT-5:VISIBLE = NO.
END.

FRAME {&FRAME-NAME}:HEIGHT-CHARS = 7.19.
FRAME {&FRAME-NAME}:VISIBLE = TRUE.
RUN set-resize-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin B-table-Win 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-record B-table-Win 
PROCEDURE post-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCnt AS INTEGER     NO-UNDO.
  DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
  DEFINE BUFFER bf-tt-line-cnt FOR tt-line-cnt.
  RUN fg/fgpstall.w (?, "T"). 
  iCnt = 0.
  FOR EACH tt-line-cnt 
    BY tt-line-cnt.line-number:

    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ tt-line-cnt.line-rowid
       NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-fg-rctd OR (AVAIL bf-fg-rctd AND bf-fg-rctd.rita-code EQ "P") THEN DO:
       DELETE tt-line-cnt.
    END.
    ELSE DO:
      FIND bf-tt-line-cnt WHERE ROWID(bf-tt-line-cnt) EQ ROWID(tt-line-cnt)
        NO-ERROR.
      iCnt = iCnt + 1.      
      bf-tt-line-cnt.line-number = iCnt.
    END.

  END. /* For each */
  iLineCnt = iCnt.  
  RUN dispatch ('open-query').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-finish-goods B-table-Win 
PROCEDURE post-finish-goods :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF BUFFER b-fg-rcpts FOR fg-rcpts.
  DEF BUFFER b-fg-rdtl FOR fg-rdtl.
  DEF BUFFER b-fg-bin FOR fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  DEF BUFFER b-itemfg1 FOR itemfg.
  DEF BUFFER ps-rctd FOR fg-rctd .
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR v-one-item AS LOG.
  DEF VAR v-dec AS DEC DECIMALS 10.
  DEF VAR v-po-no LIKE rm-rcpt.po-no NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR v-r-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-i-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-t-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-overrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  DEF VAR v-recid AS RECID NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  DEF VAR ld-cvt-qty AS DEC NO-UNDO.
  DEF VAR ld-cvt-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR v-autobin  AS cha NO-UNDO.
  DEF VAR v-newhdr AS LOG NO-UNDO. 
  DEF VAR v-fin-qty AS DEC NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.
  DEF VAR uperiod AS INT NO-UNDO.
  DEF VAR sysdate AS DATE INIT TODAY NO-UNDO.    
  DEF VAR v-date LIKE sysdate NO-UNDO.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:  
    RETURN ERROR.
  END.


  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN
  OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

  SESSION:SET-WAIT-STATE ("general").
  IF fgPostLog THEN RUN fgPostLog ('Started').
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                         AND sys-ctrl.name    EQ "AUTOPOST"
       NO-LOCK NO-ERROR.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  /* Create a single workfile record for the finished good being posted */
  CREATE w-fg-rctd.
  BUFFER-COPY fg-rctd TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = ROWID(fg-rctd)
         w-fg-rctd.has-rec = YES.

  FOR EACH w-fg-rctd,

        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no

        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:

      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.
      IF NOT AVAIL fg-bin THEN
          FIND FIRST fg-bin WHERE fg-bin.company = fg-rctd.company
                              AND fg-bin.tag  = fg-rctd.tag
                            NO-LOCK NO-ERROR.
      
      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

      IF AVAIL fg-rctd THEN DO:
        ASSIGN
         fg-rctd.rita-code = "P"  /* posted */
         fg-rctd.post-date = v-post-date
         fg-rctd.tag2      = w-fg-rctd.tag2.

        FOR EACH fg-rcpts
            WHERE fg-rcpts.company EQ fg-rctd.company
              AND fg-rcpts.r-no    EQ fg-rctd.r-no:
          fg-rcpts.rita-code = fg-rctd.rita-code.
        END.
      END.

      IF fgPostLog THEN RUN fgPostLog ('End loop'). 
    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no
              BY w-fg-rctd.job-no
              BY w-fg-rctd.job-no2
              BY w-fg-rctd.loc
              BY w-fg-rctd.loc-bin
              BY w-fg-rctd.tag:

      IF LAST-OF(w-fg-rctd.tag) THEN DO:
        IF TRIM(w-fg-rctd.tag) NE "" THEN 
        /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
        
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ g_company
              AND fg-bin.i-no    EQ loadtag.i-no
              AND fg-bin.tag     EQ loadtag.tag-no
            USE-INDEX tag:
          RUN fg/calcbinq.p (ROWID(fg-bin)).
        END.

        /* IF w-fg-rctd.tag <> "" then*/
        FIND FIRST loadtag
            WHERE loadtag.company   EQ g_company
              AND loadtag.item-type EQ NO
              AND loadtag.tag-no    EQ w-fg-rctd.tag
              AND loadtag.i-no      EQ w-fg-rctd.i-no
              AND loadtag.job-no    EQ w-fg-rctd.job-no
            USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
        IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

        IF AVAIL loadtag THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ g_company
                AND fg-bin.i-no    EQ loadtag.i-no
                AND fg-bin.tag     EQ loadtag.tag-no
              /*AND fg-bin.job-no = loadtag.job-no
                AND fg-bin.job-no2 = loadtag.job-no2*/
                AND fg-bin.qty     GT 0
              USE-INDEX tag NO-LOCK NO-ERROR.
          IF NOT AVAIL fg-bin THEN
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ g_company
                AND fg-bin.i-no    EQ loadtag.i-no
                AND fg-bin.tag     EQ loadtag.tag-no
              /*AND fg-bin.job-no = loadtag.job-no
                AND fg-bin.job-no2 = loadtag.job-no2*/
                /* AND fg-bin.qty     GT 0 */
              USE-INDEX tag NO-LOCK NO-ERROR.
          
          IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
             TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
            ASSIGN
             loadtag.loc          = w-fg-rctd.loc2   
             loadtag.loc-bin      = w-fg-rctd.loc-bin2
             loadtag.qty          = fg-bin.qty
             loadtag.pallet-count = fg-bin.qty
             loadtag.partial      = fg-bin.partial-count
             loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
          ELSE /*partial transfer */
            ASSIGN
             loadtag.loc     = w-fg-rctd.loc
             loadtag.loc-bin = w-fg-rctd.loc-bin.

          FIND CURRENT loadtag NO-LOCK NO-ERROR.
        END.
      END.
    END.

    FOR EACH w-inv:
      DELETE w-inv.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

      CREATE w-inv.
      w-inv.row-id = w-fg-rctd.row-id.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

    IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
    RUN fg/invrecpt.p (?, 2).
    IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

      IF LAST-OF(w-fg-rctd.i-no) THEN DO:
        IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
        RUN fg/updfgcs1.p (RECID(itemfg), NO).
        IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.opened  EQ YES
              AND oe-ordl.i-no    EQ w-fg-rctd.i-no
              AND oe-ordl.job-no  EQ ""
              AND oe-ordl.cost    EQ 0
            USE-INDEX opened NO-LOCK
            BREAK BY oe-ordl.ord-no
            TRANSACTION :

          DO i = 1 TO 1000:
            FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
            IF AVAIL b-oe-ordl THEN DO:
              IF itemfg.prod-uom EQ "M" THEN
                b-oe-ordl.cost = itemfg.total-std-cost.
              ELSE
                RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                        THEN "EA" ELSE itemfg.prod-uom),
                                       "M", 0, 0, 0, 0,
                                       itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
              LEAVE.
            END.
          END.
        END.
      END.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

    IF v-fgpostgl NE "None" THEN DO TRANSACTION:
      /* gdm - 11050906 */
      REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:
         ASSIGN v-trnum       = gl-ctrl.trnum + 1
                gl-ctrl.trnum = v-trnum.

         FIND CURRENT gl-ctrl NO-LOCK.
         
         IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').         
         RUN gl-from-work (1, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
         RUN gl-from-work (2, v-trnum).
         IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
         
         LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */
    END.
    FIND FIRST w-job NO-ERROR.
    IF AVAIL w-job THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
      RUN jc/d-jclose.w.
      IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
    END.

    IF v-adjustgl THEN DO TRANSACTION:
      /** GET next G/L TRANS. POSTING # **/
      FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.
      ASSIGN
       v-trnum       = gl-ctrl.trnum + 1
       gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
      FOR EACH work-job BREAK BY work-job.actnum:
         CREATE gltrans.
        ASSIGN
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "ADJUST"
         gltrans.tr-date = v-post-date
         gltrans.period  = period.pnum
         gltrans.trnum   = v-trnum.

        IF work-job.fg THEN
          ASSIGN
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT FG".
        ELSE
          ASSIGN
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
      END. /* each work-job */
      IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
    END.
    IF v-got-fgemail THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
      RUN send-fgemail (v-fgemail-file).
      IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
    END.
    IF fgPostLog THEN RUN fgPostLog ('End').
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    SESSION:SET-WAIT-STATE ("").
    /* testing RUN adm-delete-record . */
   /*  testing RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).  */
/* wfk - out temporarily         */                            
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) . 
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
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN auto-add IN WIDGET-HANDLE(char-hdl).
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
  {src/adm/template/snd-list.i "fg-rctd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-resize-no B-table-Win 
PROCEDURE set-resize-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN 
       FRAME F-Main:BOX-SELECTABLE   = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:SELECTABLE       = FALSE
       FRAME F-Main:RESIZABLE        = FALSE.

ASSIGN 
       browser-table:SELECTABLE IN FRAME F-Main             = FALSE
       browser-table:RESIZABLE IN FRAME F-Main              = FALSE.

ASSIGN FRAME F-Main:HIDDEN           = FALSE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      RETURN ERROR.
    END.
    IF AVAILABLE itemfg AND itemfg.pur-uom EQ "" THEN 
    DO:
      MESSAGE "The finished goods item must have a valid Puchase Quantity UOM..." VIEW-AS ALERT-BOX.
      RETURN ERROR.
        
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-loc-bin-tag B-table-Win 
PROCEDURE valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-fields AS CHAR INIT "job-no,job-no2,loc,loc-bin,tag,cust-no" NO-UNDO.
  DEF VAR li-field# AS INT NO-UNDO.
  DEF VAR li-fieldc AS CHAR NO-UNDO.

  DEF BUFFER bf-tmp FOR fg-rctd.

  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
     FIND FIRST bf-tmp WHERE bf-tmp.company = gcompany AND
                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                         AND RECID(bf-tmp) <> RECID(fg-rctd) NO-LOCK NO-ERROR.
     IF AVAIL bf-tmp THEN DO:
        MESSAGE "This Tag Number Has Already Been Used." skip
                "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fg-rctd.tag .
        RETURN ERROR.
     END.

     FIND FIRST loadtag WHERE loadtag.company = g_company
                    AND loadtag.ITEM-type = NO
                    AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag THEN DO:
        MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fg-rctd.tag .
        RETURN error.
     END.
     FIND FIRST fg-bin WHERE fg-bin.company = g_company
                            AND fg-bin.i-no = loadtag.i-no
                            AND fg-bin.tag = loadtag.tag-no
                            /*AND fg-bin.job-no = loadtag.job-no
                            AND fg-bin.job-no2 = loadtag.job-no2*/
                            AND fg-bin.qty > 0
                            NO-LOCK NO-ERROR.
     IF NOT AVAIL fg-bin THEN DO:
        MESSAGE "Invalid Inventory for the tag." VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fg-rctd.tag .
        RETURN ERROR.
     END.

     ASSIGN
      li-fieldc = TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
      li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
      fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = li-fieldc

      li-field# = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-fields).

     IF li-field# EQ 0 THEN li-field# = 9999.

     IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 5 THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.

         ASSIGN fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.job-no /*loadtag.job-no */
                fg-rctd.job-no2:SCREEN-VALUE = string(fg-bin.job-no2) /*loadtag.job-no2*/
                fg-rctd.i-no:SCREEN-VALUE = fg-bin.i-no /*loadtag.i-no */
                fg-rctd.i-name:SCREEN-VALUE =  itemfg.i-name         
                fg-rctd.qty-case:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.case-count)
                                                ELSE string(loadtag.qty-case)
                fg-rctd.cases:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
                                                     ELSE STRING(loadtag.tot-cases)
                        /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                        fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                          ELSE string(loadtag.case-bundle)*/
                fg-rctd.loc:SCREEN-VALUE = fg-bin.loc /*loadtag.loc*/
                fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin /*loadtag.loc-bin*/
                fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE  
                fg-rctd.partial:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.partial-count)
                                               ELSE string(loadtag.partial)       
                .
     END.
     RELEASE fg-bin.

    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */
    
    IF AVAIL loadtag AND INT(fg-rctd.cases:SCREEN-VALUE) = 0 AND li-field# = 5 THEN
       FIND FIRST fg-bin WHERE fg-bin.company = gcompany
                           AND fg-bin.tag = loadtag.tag-no
                           AND fg-bin.loc = loadtag.loc
                           AND fg-bin.loc-bin = loadtag.loc-bin
                           AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN  ASSIGN fg-rctd.cases:SCREEN-VALUE = 
         string(ROUND((fg-bin.qty - fg-bin.partial-count) / loadtag.qty-case,0)).   

    FIND FIRST fg-bin
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no   EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               li-field#      LT 2)
          AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 3)
          AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 4)
          AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 5)
          AND (fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 6)
        NO-LOCK NO-ERROR.

    IF li-field# EQ 5 AND AVAIL fg-bin THEN
      fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.partial-count).

    IF AVAIL fg-bin AND
       fg-bin.qty GE (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                      DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                      DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
    
      ASSIGN
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no)
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = 
                    IF adm-new-record THEN CAPS(fg-bin.tag) ELSE fg-rctd.tag2:SCREEN-VALUE.
       IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND lv-new-tag-number-chosen = ? 
           AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}  THEN DO:
       
           IF fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} NE  STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)) 
               AND int(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
                 MESSAGE "Units not matching units in Tag will need to create a new tag #"
                         SKIP(1)
                         "Click OK to continue and create a new tag number on save or cancel"
                         VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                         TITLE "" UPDATE choice AS LOGICAL.
                 IF choice THEN
                    lv-new-tag-number-chosen = TRUE.
                 ELSE
                    lv-new-tag-number-chosen = FALSE.
        
                 IF NOT lv-new-tag-number-chosen THEN
                    fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)).
           END. /* ask if creating a new tag number */

       END.
    END.
    ELSE DO:
        
      IF AVAIL fg-bin THEN DO:
        MESSAGE "Insufficient Qty (" fg-bin.qty ") in Bin..." SKIP(1)
          'Cases:' fg-rctd.cases '*' SKIP
          'Qty/Case:' fg-rctd.qty-case '+' SKIP
          'Partial:' fg-rctd.partial '='
          (DEC(fg-rctd.cases:SCREEN-VALUE) *
           DEC(fg-rctd.qty-case:SCREEN-VALUE)) +
           DEC(fg-rctd.partial:SCREEN-VALUE)
        VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
      END.
      ELSE DO:
        MESSAGE "Invalid entry, try help..."  /*li-field# SKIP
              fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} SKIP
              fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} "," length(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) SKIP
              int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) SKIP
              fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} SKIP
              fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} SKIP
              fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} SKIP
            AVAIL fg-bin */
             VIEW-AS ALERT-BOX.                
        IF li-field# LE 6 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      END.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 B-table-Win 
PROCEDURE valid-loc-bin2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.

  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      EQ
         fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     AND
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  EQ
         fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}
        USE-INDEX co-ino NO-LOCK NO-ERROR.

      IF NOT AVAIL fg-bin THEN lv-msg = "Invalid entry, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..."  VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc-bin2 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 B-table-Win 
PROCEDURE valid-loc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.

  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty B-table-Win 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
  /* (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag2 B-table-Win 
PROCEDURE valid-tag2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
      IF {&BROWSE-NAME}:NUM-SELECTED-ROWS   IN FRAME {&FRAME-NAME} = 0 THEN 
      RETURN.
    FIND FIRST fg-bin
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
       /*   AND fg-bin.job-no   EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               li-field#      LT 2)
          AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 3)
          AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 4)
               */
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}       /*   OR
               li-field#      LT 5)
          AND fg-bin.qty      NE 0 */
       NO-LOCK NO-ERROR.

     IF AVAIL fg-bin THEN
     ASSIGN
       fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = CAPS(fg-bin.tag).

    ELSE DO:
        MESSAGE "Invalid Tag#, try help..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.tag2 IN BROWSE {&browse-name}.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work B-table-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  DEF VAR credits AS DEC INIT 0 NO-UNDO.
  DEF VAR debits AS DEC INIT 0 NO-UNDO. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  FOR EACH work-gl 
      WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
         OR (ip-run EQ 2 AND work-gl.job-no EQ "")
      BREAK BY work-gl.actnum:
      
    ASSIGN
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    IF LAST-OF(work-gl.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = IF work-gl.job-no NE "" THEN "FG Receipt from Job"
                                                 ELSE "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  run get-matrix (true).
  return ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

