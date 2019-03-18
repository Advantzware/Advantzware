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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
def var char-val as cha no-undo.
def var ld-cost as decimal no-undo.
def var lv-uom as char no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
DEF VAR glvNewRow AS ROWID NO-UNDO.
def var ll-help-run as log no-undo. /* set on browse help, reset row-entry */
def var fg-uom-list  as char NO-UNDO.
DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */

DEF VAR lv-fgrecpt-val AS INT NO-UNDO.
DEF VAR trans-time AS CHAR NO-UNDO.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO
    FIELD old-tag AS CHAR
    FIELD ret-loc AS CHAR
    FIELD ret-loc-bin AS CHAR.

ASSIGN
 cocode = g_company
 locode = g_loc.

DO TRANSACTION:
  {sys/inc/fgrecpt.i}
  lv-fgrecpt-val = sys-ctrl.int-fld.
  {sys/inc/fgsecur.i}
END.
  DO TRANSACTION:
    {sys/inc/jobreopn.i}
END.
{oe/d-selbin.i NEW}
DEF VAR lvReturnChar AS CHAR NO-UNDO.
DEF VAR lvFound AS LOG NO-UNDO.
DEF VAR fgRecptPassWord-log AS LOGICAL NO-UNDO.
DEF VAR fgRecptPassWord-char AS CHARACTER NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR ll-set-parts AS LOG NO-UNDO.
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

RUN sys/ref/nk1look.p (cocode, "FGRecptPassWord", "L", no, no, "", "", 
    Output lvReturnChar, output lvFound).
IF lvFound THEN
    fgRecptPassWord-log = LOGICAL(lvReturnChar).
RUN sys/ref/nk1look.p (cocode, "FGRecptPassWord", "C", no, no, "", "", 
    Output fgRecptPassWord-char, output lvFound).

&SCOPED-DEFINE item-key-phrase TRUE

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.r-no fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time fg-rctd.i-no fg-rctd.i-name ~
fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag ~
fg-rctd.cases fg-rctd.qty-case fg-rctd.partial fg-rctd.t-qty ~
fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cases fg-rctd.qty-case fg-rctd.partial 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "F" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "F" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 118 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 107 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 18.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>>":U WIDTH 12
      fg-rctd.rct-date COLUMN-LABEL "Issue!Date" FORMAT "99/99/9999":U
            WIDTH 14.4
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Issue!Time"
            WIDTH 14
      fg-rctd.i-no FORMAT "X(15)":U
      fg-rctd.i-name FORMAT "x(30)":U
      fg-rctd.job-no FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U
      fg-rctd.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>9":U
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
      fg-rctd.t-qty COLUMN-LABEL "Total Qty" FORMAT "->,>>>,>>>,>>9":U
            WIDTH 18
      fg-rctd.ext-cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<":U
            WIDTH 15
      fg-rctd.created-by COLUMN-LABEL "Created By" FORMAT "x(8)":U
            WIDTH 15
      fg-rctd.updated-by COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U
            WIDTH 15
  ENABLE
      fg-rctd.rct-date
      fg-rctd.i-no
      fg-rctd.i-name
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.tag
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 8 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.67 COL 12 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.67 COL 133 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.48 COL 3
     RECT-4 AT ROW 16.24 COL 2
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
         HEIGHT             = 18.38
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", OUTER"
     _Where[1]         = "fg-rctd.company = g_company and
fg-rctd.rita-code = ""F"""
     _FldNameList[1]   > ASI.fg-rctd.r-no
"r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.fg-rctd.rct-date
"rct-date" "Issue!Date" ? "date" ? ? ? ? ? ? yes ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Issue!Time" ? ? ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.fg-rctd.i-no
"i-no" ? "X(15)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.fg-rctd.i-name
"i-name" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.fg-rctd.job-no
"job-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.fg-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.fg-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.fg-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.fg-rctd.tag
"tag" "Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.fg-rctd.cases
"cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.fg-rctd.qty-case
"qty-case" "Qty/Unit" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.fg-rctd.partial
"partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.fg-rctd.t-qty
"t-qty" "Total Qty" "->,>>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.fg-rctd.ext-cost
"ext-cost" "Cost" ? "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.created-by
"created-by" "Created By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.updated-by
"updated-by" "Last Updated By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   def var ll-tag# as log no-undo.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     ll-help-run = yes.
     case focus:name:
       /* fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name},  */
       when "i-no" then do:
            IF fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
                  RUN windows/l-jobit1.w (fg-rctd.company,fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
                  IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
/*                   IF rec-val <> ? THEN DO:                                                       */
/*                      FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.         */
/*                                                                                                  */
/*                      IF AVAIL tt-job-hdr THEN                                                    */
/*                          ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(tt-job-hdr.std-mat-cost + */
/*                                                               tt-job-hdr.std-lab-cost +          */
/*                                                               tt-job-hdr.std-fix-cost +          */
/*                                                               tt-job-hdr.std-var-cost)           */
/*                                .                                                                 */
/*                                                                                                  */
/*                   END.                                                                           */

             END.
             ELSE DO:
                  RUN windows/l-itemf2.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT rec-val).
                  IF rec-val <> ? THEN DO:
                     FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                     ASSIGN fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}  = itemfg.i-no
                       fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
                       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.def-loc
                       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.def-loc-bin.
/*                        fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = string(itemfg.avg-cost) */
/*                        fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom  .      */
                  END.
             END.
             return no-apply.   
       end.
       when "job-no" /*or when "job-no2" */ then do:
             /* run windows/l-jobno.w (fg-rctd.company, focus:SCREEN-VALUE,output char-val, OUTPUT rec-val). */
             RUN windows/l-jobnop.w (fg-rctd.company, focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
             FIND FIRST job-hdr WHERE RECID(job-hdr) EQ rec-val NO-LOCK NO-ERROR.
        
             IF AVAIL job-hdr THEN
             
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       */ 
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.job-no
                       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-hdr.job-no2)
                       /* fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.i-no*/
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.

/*                 IF AVAIL job-hdr THEN                                                                     */
/*                    fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = string(job-hdr.std-mat-cost + */
/*                                                           job-hdr.std-lab-cost +                          */
/*                                                           job-hdr.std-fix-cost +                          */
/*                                                           job-hdr.std-var-cost)                           */
/*                           .                                                                               */
             end.
             FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
                        /* fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom  */.

             return no-apply.   
       end.  
       when "job-no2" then do:
             RUN windows/l-jobnop.w (fg-rctd.company, fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT rec-val).
             FIND FIRST job-hdr WHERE RECID(job-hdr) EQ rec-val NO-LOCK NO-ERROR.
        
             IF AVAIL job-hdr THEN
             
                assign fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.job-no
                       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-hdr.job-no2)
                       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.i-no
                       .
                       
             FIND itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
                        /* fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom  */.
             return no-apply.   
       end.  
       when "loc" then do:
             run windows/l-loc.w (fg-rctd.company,focus:screen-value, output char-val).
             if char-val <> "" then do :
                assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val)
                       .

             end.
             return no-apply.   
       end.
       when "loc-bin" then do:
             run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},output char-val).
             if char-val <> "" then do :
                assign focus:screen-value  = entry(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             end.
             return no-apply.   
       end.  
       WHEN "tag" THEN DO:  /* task 11111306 */
             IF TRUE THEN DO:
                  DEF VAR lv-all-or-one AS cha NO-UNDO.
                  DEF VAR lchk AS LOG NO-UNDO.
                  DEF VAR fg-item-name AS CHAR NO-UNDO.

                  /*IF relmerge-int NE 0 THEN
                     MESSAGE "Select Bins for All Jobs?" VIEW-AS ALERT-BOX
                      QUESTION BUTTON YES-NO UPDATE lchk .

                 lv-all-or-one = IF lchk THEN "ALL" ELSE "ONE".*/
                 lv-all-or-one = "ALL" .

/*                  FIND FIRST b-tag-rctd WHERE  b-tag-rctd.company eq cocode and                         */
/*                        b-tag-rctd.r-no EQ int(SUBSTRING(reftable.dscr,9,35)) and                       */
/*                        (b-tag-rctd.rita-code eq "R" or b-tag-rctd.rita-code eq "E")  NO-LOCK NO-ERROR. */
/*                                                                                                        */
/*                  IF AVAIL b-tag-rctd THEN ASSIGN fg-item-name = b-tag-rctd.i-no .                      */
/*                  ELSE fg-item-name = ""  .                                                             */

                 RUN oe/l-tagnew.w (6, ROWID(fg-rctd), lv-all-or-one,fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} ,fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name},
                      "", output char-val).
                 IF char-val <> "" then do :
                       ASSIGN fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(1,char-val)
                           fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(2,char-val)
                           fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(3,char-val) .
                 END.
             END. /* If True */
         END.   
     end case.
   END.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  ASSIGN 
  lv-closed-checked  = NO.
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /*{src/adm/template/brsleave.i}*/
  {est/brsleave.i}  /* same as src but update will be same as add record*/

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


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  IF LASTKEY = -1 THEN RETURN.
   
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /*IF SELF:MODIFIED THEN*/ RUN get-def-values.
  IF fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

  IF fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  DEF VAR li AS INT NO-UNDO.


  FIND itemfg
      {sys/look/itemfgrlW.i}
        AND itemfg.i-no EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN DO:
/*     RUN get-def-values. */
/*     DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) + 1: */
/*       APPLY "cursor-right" TO {&self-name} IN BROWSE {&browse-name}.             */
/*     END.                                                                         */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
    lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
  DEF VAR liDummy AS INT NO-UNDO.
  liDummy =  BROWSE {&browse-name}:NUM-SELECTED-ROWS.

  /* should never be here if no row is selected */
  IF liDummy EQ 0 THEN
    RETURN.

  IF LASTKEY NE -1 THEN DO:
    IF NOT fgRecptPassWord-log THEN
      RUN valid-job-no (INPUT YES) NO-ERROR.
    ELSE
      /* run with 'no' so no message until save */
      RUN valid-job-no (INPUT NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
    lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
 lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF lv-job-no2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
       RUN new-job-no.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF SELF:MODIFIED THEN DO:
       FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
    IF LASTKEY = -1 THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                           AND fg-bin.i-no = ""
                           AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                           USE-INDEX co-ino NO-LOCK NO-ERROR.
       IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.
   RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag */
DO:
  IF LASTKEY NE -1 THEN DO:    
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.t-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.t-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.t-qty IN BROWSE Browser-Table /* Total Qty */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.ext-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.ext-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.ext-cost IN BROWSE Browser-Table /* Cost */
DO:
  /*APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.*/
    APPLY "row-leave" TO BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.
   
   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      ASSIGN
         fg-rctd.ext-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).
DO WITH FRAME {&FRAME-NAME}:
  {custom/usrprint.i}
  auto_find:SCREEN-VALUE = "".
END.        
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING((DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
               DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
               fg-rctd.t-qty:FORMAT IN BROWSE {&browse-name})
     fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
               (IF lv-uom EQ "M" THEN 1000 ELSE 1) * ld-cost,
               fg-rctd.ext-cost:FORMAT IN BROWSE {&browse-name}).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE farmOutComp B-table-Win 
PROCEDURE farmOutComp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cJob AS CHAR NO-UNDO.
DEF VAR iJobNo2 AS INT NO-UNDO.
DEF BUFFER bf-fg-rctd FOR fg-rctd.

  FIND itemfg WHERE itemfg.company EQ cocode 
      AND itemfg.i-no EQ w-fg-rctd.i-no
      NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN
      RETURN.
  cJob = "".
  iJobNo2 = 0.   

  IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN DO:
      
      /* Find a job for this po if this is a farmout */
      IF w-fg-rctd.job-no GT "" THEN
          ASSIGN cJob = w-fg-rctd.job-no
                 iJobNo2 = w-fg-rctd.job-no2.
      ELSE IF w-fg-rctd.po-no GT "" THEN  DO:
           FIND FIRST po-ordl WHERE po-ordl.company EQ w-fg-rctd.company
               AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
               AND po-ordl.i-no  EQ w-fg-rctd.i-no
               NO-LOCK NO-ERROR.
           
           IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN DO:
           
              FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                  AND oe-ordl.ord-no EQ po-ordl.ord-no
                  AND oe-ordl.i-no   EQ po-ordl.i-no
                  NO-LOCK NO-ERROR.
              /* assumption is that for farm jobs, order and job are always the same */
              /* This is to obtain the job-no2 since job-no is assumed to be the order # */
              IF NOT AVAIL oe-ordl THEN
                  FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                      AND oe-ordl.ord-no EQ po-ordl.ord-no
                      AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                      NO-LOCK NO-ERROR.
               
              IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
                  ASSIGN cJob = oe-ordl.job-no
                         iJobNo2 = oe-ordl.job-no2.
           END.
          
      END.


      FIND FIRST job WHERE job.company EQ w-fg-rctd.company
          AND job.job-no EQ cJob
          AND job.job-no2 EQ iJobNo2
          NO-LOCK NO-ERROR.
      
      IF AVAIL job AND cJob GT "" 
                   AND w-fg-rctd.rita-code EQ "F" 
                   THEN DO:             
          /* Copy fg-rctd for the jobs farmout tab */
          CREATE job-farm-rctd.
          BUFFER-COPY w-fg-rctd EXCEPT rec_key TO job-farm-rctd.
          ASSIGN job-farm-rctd.job-no = cJob
                 job-farm-rctd.job-no2 = iJobNo2.
          /* ASSIGN job-farm-rctd.job = job.job. */
        
          RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
      END.
      FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ w-fg-rctd.row-id
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bf-fg-rctd THEN
          DELETE bf-fg-rctd.
  END.




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

    RUN windows/l-fgibn4.w (fg-rctd.company, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag     /* OR
                         fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}     NE fg-bin.cust-no */)
    THEN DO:
      ASSIGN
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
      /* fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no */.

      RUN new-bin.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-cost LIKE fg-rctd.std-cost NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN
        RETURN.
    fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
      
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND (fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} OR fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") 
          AND (fg-bin.job-no2 EQ INTEGER(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")
          AND (fg-bin.tag  EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} OR fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") 
        NO-LOCK NO-ERROR.
    /* If not found, try without job# since that is the thing we are issuing to the bin */
    IF NOT AVAIL fg-bin THEN
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
              AND fg-bin.i-no    EQ itemfg.i-no
              AND (fg-bin.tag  EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} OR fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") 
            NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       /* fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no */.
/*     IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN                      */
/*        ASSIGN                                                                               */
/*            fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no            */
/*            fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2).  */

    RUN new-bin.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-item-defaults B-table-Win 
PROCEDURE get-item-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN
        RETURN.
    fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE issue-all B-table-Win 
PROCEDURE issue-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH fg-rctd WHERE fg-rctd.company EQ cocode
    AND fg-rctd.rita-code EQ "F"
    NO-LOCK:
  
   CREATE w-fg-rctd.
   BUFFER-COPY fg-rctd TO w-fg-rctd.
   ASSIGN w-fg-rctd.row-id = ROWID(fg-rctd)
          w-fg-rctd.has-rec = TRUE.
END.

 RUN farmOutComp.
 RUN adm-open-query.

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
  RUN validate-record.
/*   IF ERROR-STATUS:ERROR THEN */
/*       RETURN ERROR.          */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  ASSIGN fg-rctd.trans-time   = TIME 
         fg-rctd.enteredBy = USERID("asi")
         fg-rctd.enteredDT = DATETIME(TODAY, MTIME) 
         .
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN BROWSE {&browse-name} fg-rctd.t-qty fg-rctd.ext-cost.
  fg-rctd.cost-uom = lv-uom.

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
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rno = 0.
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  assign fg-rctd.company = g_company
         fg-rctd.r-no    = lv-rno
         fg-rctd.rita-code = "F".

  IF adm-adding-record THEN DO:
    ASSIGN
     fg-rctd.rct-date     = TODAY
     fg-rctd.trans-time   = TIME
     fg-rctd.s-num        = 0
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1.
    DISPLAY fg-rctd.rct-date WITH BROWSE {&browse-name}.
  END.  
  glvNewRow = ROWID(fg-rctd).

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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
   
  IF NOT AVAIL(fg-rctd) THEN DO:
      FIND fg-rctd WHERE ROWID(fg-rctd) EQ glvNewRow EXCLUSIVE-LOCK NO-ERROR.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
  DEF VAR li AS INT NO-UNDO.

   
  /* Code placed here will execute PRIOR to standard behavior. */

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  /*run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
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
  end.*/

  /*IF adm-new-record THEN
    MESSAGE "Update Cost? (Leave 'NO' to update Quantity)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-do-cost.
  ELSE
    ll-do-cost = fg-rctd.t-qty EQ 0.*/

  
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   ld-cost = 0
   lv-uom  = "EA".
  /*IF NOT adm-new-record THEN RUN new-bin.*/
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL fg-rctd THEN DO:
      ASSIGN
       lv-job-no  = fg-rctd.job-no
       lv-job-no2 = STRING(fg-rctd.job-no2).
    END.

    ELSE
      ASSIGN
       lv-job-no  = ""
       lv-job-no2 = "00".


  END.
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fg-rctd.rct-date IN BROWSE {&browse-name}.
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
  def var li as int no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN calc-qty.

  RUN validate-record.
  IF ERROR-STATUS:ERROR THEN
      RETURN ERROR.
  IF NOT AVAIL(fg-rctd) THEN     
      FIND fg-rctd WHERE ROWID(fg-rctd) EQ glvNewRow EXCLUSIVE-LOCK NO-ERROR.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*
  RUN repo-query (ROWID(fg-rctd)).

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
    */

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
    /* ##PN## job# shuold not be used since that's the thing we're assigning with the issue */
    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
/*           AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       */
/*           AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) */
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          
        NO-LOCK NO-ERROR.
    
    IF AVAIL fg-bin THEN DO:    

      ASSIGN             
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)       
       ld-cost                                                = fg-bin.std-tot-cost
       lv-uom                                                 = fg-bin.pur-uom.
  
      IF adm-new-record AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" AND DECIMAL(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
        ASSIGN fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)) /* wfk - added this */
               fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count) .
    END.

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
  DO WITH FRAME {&FRAME-NAME}:
    lv-closed-checked = NO.
    lv-new-job-ran = YES.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
    FOR EACH job-hdr WHERE job-hdr.company EQ fg-rctd.company
          AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK,
        EACH job-farm WHERE job-farm.company EQ fg-rctd.company          
          AND job-farm.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-farm.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        BREAK BY job-farm.frm      DESC
              BY job-farm.blank-no DESC:

      IF LAST(job-farm.frm)                                                  OR
         job-farm.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
        ASSIGN
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE.
         IF TRIM(fg-rctd.i-no:SCREEN-VALUE) EQ "" THEN 
           fg-rctd.i-no:SCREEN-VALUE     = job-farm.i-no.


        RUN get-def-values.

        LEAVE.
      END.
    END.
  END.
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
  DEF VAR liDummy AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    liDummy =  BROWSE {&browse-name}:NUM-SELECTED-ROWS.

    /* should never be here if no row is selected */
    IF liDummy EQ 0 THEN
        RETURN ERROR.

    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
               INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) +
               INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})).
/*                                                       */
/*     IF NOT adm-new-record OR ll-qty-case-ent THEN DO: */
/*       RUN show-freight.                               */
/*                                                       */
/*       RUN get-matrix (NO).                            */
/*     END.                                              */
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
    RUN clear_auto_find.
    RUN change-order (browse-order:SCREEN-VALUE).
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

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
  {src/adm/template/snd-list.i "fg-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lActive AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:


    IF NOT CAN-FIND(FIRST itemfg
                    {sys/look/itemfgrlW.i}
                      AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND itemfg.pur-man EQ TRUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    RUN fg/GetItemfgActInact.p (INPUT cocode,
                                INPUT fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                OUTPUT lActive).
    IF NOT lActive THEN DO:
       MESSAGE fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} + " has InActive Status. FG Issue cannot be created for the Inactive Item."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.i-no.
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
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fg-bin 
                    WHERE fg-bin.company  EQ cocode
                      AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND (fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       OR ip-int LT 1)
                      AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-int LT 2)
                      AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 3)
                      AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR ip-int LT 4)
                      AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 5)
                      /* AND (fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      OR ip-int LT 6)*/ )
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN
        APPLY "entry" TO fg-rctd.job-no2 IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 3 THEN
        APPLY "entry" TO fg-rctd.loc IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 4 THEN
        APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 5 THEN
        APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
      /*ELSE
        APPLY "entry" TO fg-rctd.cust-no IN BROWSE {&browse-name}.*/
      RETURN ERROR.
    END.
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
  DEF INPUT PARAMETER iplAskPasswd AS LOG NO-UNDO.
  DEF VAR lvPasswordEntered AS LOG NO-UNDO.
  DEF VAR lcRitaCode AS CHAR NO-UNDO.
  DEF VAR lJobFound AS LOG NO-UNDO.

  IF AVAIL(fg-rctd) THEN
      lcRitaCode = fg-rctd.rita-code.
  ELSE
      lcRitaCode = "F".
  DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).
    /* In loop since no index on i-no */
   
    lJobFound = NO.    
    FOR EACH job-hdr WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK:
      
        IF CAN-FIND( FIRST job-farm WHERE job-farm.company = job-hdr.company
                       AND job-farm.job-no EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND job-farm.job-no2 EQ job-hdr.job-no2
                       AND (job-farm.i-no EQ TRIM(fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
                           OR fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")) THEN
            lJobFound = TRUE.
    END.

    IF lJobFound EQ NO THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.


  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 B-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ans AS LOG NO-UNDO.
  DEF VAR lv-err AS LOG INIT NO NO-UNDO.

  DO WITH FRAME {&frame-name}:
      IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
          FIND FIRST fg-rctd
              WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
              NO-LOCK NO-ERROR.
      END.  /*Mode 001*/

      fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}). /*Mode 001*/

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FOR EACH job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
          NO-LOCK,
          FIRST job
          WHERE job.company EQ job-hdr.company
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
          NO-LOCK:
        LEAVE.
      END.
          
      IF NOT AVAIL job-hdr THEN
      FOR EACH job
          WHERE job.company EQ fg-rctd.company
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
          NO-LOCK,
          FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
          NO-LOCK:
        LEAVE.
      END.

      IF NOT AVAIL job-hdr THEN DO:
        MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
        lv-err = YES.
      END.      

      IF NOT lv-err AND NOT lv-closed-checked AND
         job.opened EQ NO                     THEN DO:
        ASSIGN
         lv-ans            = NO
         lv-closed-checked = YES.

        /* gdm - 11160901 */
        IF jobreopn-log EQ YES 
          THEN
           MESSAGE 
              "Job is CLOSED, would you like to reopen?"
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
          ELSE 
           ASSIGN lv-ans = jobreopn-log.
        /* gdm - 11160901 end */
        
        CASE lv-ans:
           WHEN YES THEN RUN jc/jc-reopn.p (ROWID(job)).
           WHEN NO  THEN.
           OTHERWISE lv-err = YES.
         END CASE.
      END.
    END.

    IF lv-err THEN DO:
      lv-closed-checked = NO.
      APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF lv-fgrecpt-val = 1 THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = g_company
                          AND loadtag.item-type = NO
                          AND loadtag.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                          AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag /*or fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""*/ THEN DO:
        MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
        RETURN ERROR.
     END.
  END.
/*   FIND FIRST job-farm-rctd                                                                           */
/*          WHERE job-farm-rctd.company = g_company                                                     */
/*            AND job-farm-rctd.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}                 */
/*          NO-LOCK NO-ERROR.                                                                           */
/*   IF AVAIL job-farm-rctd /*or fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""*/ THEN DO:      */
/*      MESSAGE "Tag# has already been issued. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR. */
/*      APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.                                          */
/*      RETURN ERROR.                                                                                   */
/*   END.                                                                                               */

  RUN new-bin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no .
            RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND first itemfg {sys/look/itemfgrlW.i}
                       and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       no-lock no-error.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.

  RUN valid-job-no (NO).
  IF ERROR-STATUS:ERROR THEN DO:        
        APPLY "entry" TO fg-rctd.job-no .
        RETURN ERROR.
  END.

  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
          AND fg-bin.tag EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no GT ""
          NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND fg-bin.job-no GT "" AND fg-bin.job-no NE fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
        MESSAGE "Tag is already associated with a different job. " SKIP
            "Tag: " fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
            "Bin: " fg-bin.loc fg-bin.loc-bin SKIP
            "Bin Job: " fg-bin.job-no  
            "Iss. Job:" fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} SKIP VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.tag .
        RETURN ERROR.
      END.
  END.

  IF dec(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
     MESSAGE "Issue quantity cannot be 0."
         VIEW-AS ALERT-BOX.
     APPLY "entry" TO fg-rctd.cases.
     RETURN ERROR.
  END.
  
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
          AND fg-bin.tag EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND fg-bin.qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
        MESSAGE "The quantity entered is greater than the bin quantity for this tag. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.tag .
        RETURN ERROR.
      END.
  END.

  FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.

/*   FIND FIRST fg-bin WHERE fg-bin.company = g_company                                             */
/*                       AND fg-bin.i-no = ""                                                       */
/*                       AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         */
/*                       AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} */
/*                       USE-INDEX co-ino NO-LOCK NO-ERROR.                                         */
/*   IF NOT AVAIL fg-bin THEN DO:                                                                   */
/*           MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.                            */
/*           APPLY "entry" TO fg-rctd.loc-bin.                                                      */
/*           RETURN ERROR.                                                                          */
/*   END.                                                                                           */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

