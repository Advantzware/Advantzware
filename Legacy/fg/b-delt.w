&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon\fg\b-delt.w
  
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
{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF SHARED VAR g-sharpshooter AS LOG NO-UNDO.

def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
def var ls-prev-po as cha no-undo.
DEF VAR lv-overrun-checked AS LOG NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.
DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR v-case-tag AS LOG NO-UNDO.
DEF VAR v-ssfgscan AS LOG NO-UNDO.
DEF VAR lv-do-what AS cha NO-UNDO.  /* will be receipt or delete(negative receipt)*/

DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER reftable-job FOR reftable.

{pc/pcprdd4u.i NEW}

DO TRANSACTION:
  {sys/inc/fgpofrt.i}
  {sys/inc/fgrecpt.i}

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ g_company
                          AND sys-ctrl.name    EQ "SSFGSCAN" NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN sys-ctrl.company = g_company
            sys-ctrl.NAME = "SSFGSCAN"
            sys-ctrl.descrip = "Prompt for the Warehouse/Bin?"
            sys-ctrl.log-fld = YES.
  END.
  v-ssfgscan = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE YES.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ g_company
                          AND sys-ctrl.name    EQ "CASETAG" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN v-case-tag = sys-ctrl.log-fld.
END.

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.r-no fg-rctd.tag ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.job-no fg-rctd.job-no2 fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time fg-rctd.po-no fg-rctd.i-no ~
fg-rctd.i-name fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit ~
fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom fg-rctd.t-qty ~
fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.tag ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.job-no fg-rctd.job-no2 fg-rctd.rct-date ~
fg-rctd.po-no fg-rctd.i-no fg-rctd.i-name fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost fg-rctd.t-qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq g_company and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq g_company and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
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
     SIZE 117.8 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 115 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.86.

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
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      fg-rctd.loc COLUMN-LABEL "Whs" FORMAT "x(13)":U
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.rct-date COLUMN-LABEL "Receipt!Date" FORMAT "99/99/9999":U
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Receipt!Time"
            WIDTH 9.2
      fg-rctd.po-no FORMAT "x(9)":U WIDTH 14
      fg-rctd.i-no COLUMN-LABEL "Item#" FORMAT "X(15)":U
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
      fg-rctd.cases-unit COLUMN-LABEL "Unit!per Pallet" FORMAT ">>9":U
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
      fg-rctd.std-cost COLUMN-LABEL "Cost/UOM" FORMAT ">>>,>>9.99<<":U
            WIDTH 18
      fg-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      fg-rctd.t-qty COLUMN-LABEL "Total!Qty" FORMAT "->>>,>>>,>>9.99":U
      fg-rctd.frt-cost FORMAT ">>>,>>9.99<<":U
      fg-rctd.ext-cost COLUMN-LABEL "Extended Cost" FORMAT "->>>,>>9.99<<":U
      fg-rctd.created-by COLUMN-LABEL "Created By" FORMAT "x(8)":U
            WIDTH 15
      fg-rctd.updated-by COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U
            WIDTH 15
  ENABLE
      fg-rctd.tag
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.rct-date
      fg-rctd.po-no
      fg-rctd.i-no HELP "FG Item Number"
      fg-rctd.i-name
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.cases-unit
      fg-rctd.partial
      fg-rctd.std-cost
      fg-rctd.t-qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 15.76 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.95 COL 11 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.95 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 15.76 COL 2
     RECT-4 AT ROW 15.52 COL 1
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
         HEIGHT             = 19.52
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", OUTER"
     _Where[1]         = "fg-rctd.company eq g_company and
(fg-rctd.rita-code eq ""R"" or fg-rctd.rita-code eq ""E"")"
     _FldNameList[1]   > ASI.fg-rctd.r-no
"fg-rctd.r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whs" "x(13)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.job-no
"fg-rctd.job-no" "Job#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.job-no2
"fg-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Receipt!Date" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Receipt!Time" ? ? ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.po-no
"fg-rctd.po-no" ? ? "character" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" "Item#" "X(15)" "character" ? ? ? ? ? ? yes "FG Item Number" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Unit!per Pallet" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost/UOM" ? "decimal" ? ? ? ? ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.cost-uom
"fg-rctd.cost-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   = ASI.fg-rctd.frt-cost
     _FldNameList[20]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Extended Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "Created By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.fg-rctd.updated-by
"fg-rctd.updated-by" "Last Updated By" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON CURSOR-DOWN OF Browser-Table IN FRAME F-Main
DO:
  RUN get-matrix (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

   DO WITH FRAME {&FRAME-NAME}:
     ll-help-run = yes.
     case focus:name:
        when "po-no" then do:
             run windows/l-pofg.w (fg-rctd.company,focus:screen-value, output char-val).
             if char-val <> "" then do:
                assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.i-no:screen-value in browse {&browse-name} = entry(2,char-val)
                       fg-rctd.i-name:screen-value in browse {&browse-name} = entry(3,char-val)
                       fg-rctd.job-no:screen-value in browse {&browse-name} = entry(4,char-val)
                       fg-rctd.job-no2:screen-value in browse {&browse-name} = entry(5,char-val)
                       .
               find po-ordl where po-ordl.company = fg-rctd.company and
                                  po-ordl.po-no = integer(entry(1,char-val)) and
                                  po-ordl.line = integer(entry(6,char-val))
                                  no-lock no-error.
               if avail po-ordl then do:
                  assign /*-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                         fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom
                         fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cost)
                         .
               end.

               find first itemfg where itemfg.company = fg-rctd.company and
                                        itemfg.i-no = entry(2,char-val)
                                        no-lock no-error.
               if avail itemfg then do:                         
                  assign fg-rctd.loc:screen-value in browse {&browse-name} =  itemfg.def-loc
                         fg-rctd.loc-bin:screen-value in browse {&browse-name} =  itemfg.def-loc-bin
                         fg-rctd.qty-case:screen-value in browse {&browse-name} = string(itemfg.case-count)
                       /*  fg-rctd.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                                            else itemfg.prod-uom  */                        
                         .
               end.
  /*
               run tag-method (output ll-tag#).
               if ll-tag# and fg-rctd.po-no:screen-value in browse {&browse-name} <> ""
               then do:
                   run tag-sequence.
               end.
    */
               fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
             end.  /* char-val <> "" */
             return no-apply.   
       end.
       when "i-no" then do:
             IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
                RUN windows/l-poitem.w (fg-rctd.company,fg-rctd.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
                if char-val <> "" then do :
                   assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.i-name:screen-value = entry(2,char-val)
                       fg-rctd.job-no:screen-value = entry(3,char-val)
                       fg-rctd.job-no2:screen-value = entry(4,char-val)
                       .
                end.
             END.
             ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
                  RUN windows/l-jobit1.w (fg-rctd.company,fg-rctd.job-no:SCREEN-VALUE,fg-rctd.job-no2:screen-value, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
                  IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                  IF rec-val <> ? THEN DO:
                     FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.
                     IF AVAIL tt-job-hdr THEN 
                         ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(tt-job-hdr.std-mat-cost +
                                                              tt-job-hdr.std-lab-cost +
                                                              tt-job-hdr.std-fix-cost +
                                                              tt-job-hdr.std-var-cost)
                               .

                  END.

             END.
             ELSE DO:
                  RUN windows/l-itemf2.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                  IF rec-val <> ? THEN DO:
                     FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                     ASSIGN fg-rctd.i-no:SCREEN-VALUE  = itemfg.i-no
                       fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                       fg-rctd.std-cost:SCREEN-VALUE = string(itemfg.avg-cost)
                       fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

                  END.
             END.
             return no-apply.   
       end.
       when "job-no" /*or when "job-no2" */ then do:
             run windows/l-jobno.w (fg-rctd.company, focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       */ 
                       fg-rctd.job-no:screen-value = entry(1,char-val)
                       fg-rctd.job-no2:screen-value = entry(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN fg-rctd.loc:SCREEN-VALUE = job-hdr.loc
                          fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost)
                          .
             end.
             FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

             return no-apply.   
       end.  
       when "job-no2" then do:
             run windows/l-jobno2.w (fg-rctd.company, fg-rctd.job-no:screen-value,focus:screen-value,output char-val, OUTPUT rec-val).
             if char-val <> "" THEN
                assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       fg-rctd.job-no:screen-value = entry(1,char-val) */
                       fg-rctd.job-no2:screen-value = entry(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN fg-rctd.loc:SCREEN-VALUE = job-hdr.loc
                          fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost)
                   .
             end.
             FIND itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
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
             run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc:screen-value, fg-rctd.loc-bin:screen-value,output char-val).
             if char-val <> "" then do :
                assign focus:screen-value  = entry(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             end.
             return no-apply.   
       end.
         WHEN "tag" THEN DO:
           run windows/l-ldtag.w (g_company,no,focus:screen-value,output char-val,OUTPUT rec-val).
           if char-val <> "" then do :
              FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
              /*  ===*/
              FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                            bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
              IF AVAIL bf-tmp THEN DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
              END.
              ELSE DO:
                 find first xfg-rdtlh where xfg-rdtlh.company   eq g_company
                      and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                      and xfg-rdtlh.qty       gt 0
                      and xfg-rdtlh.rita-code ne "S"
                      use-index tag no-lock no-error.
                 if avail xfg-rdtlh THEN  DO:
                    MESSAGE "This Tag Number Has Already Been Used." skip
                            "Please Enter A Unique Tag Number." 
                            VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                 END.
              END.
              {addon/loadtags/disptagf.i "FGItem" FOCUS:SCREEN-VALUE}
              /*IF fg-rctd.loc:SCREEN-VALUE = "" THEN*/ RUN get-def-values.

              lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
  
              RETURN NO-APPLY.
           END.
       END.
     end case.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
    {est/brsleave.i}

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
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
    

    IF LASTKEY = -1 /*OR fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" */
    THEN RETURN.

    FIND FIRST bf-tmp WHERE bf-tmp.company = g_company 
                        AND bf-tmp.tag = SELF:SCREEN-VALUE
                        AND SELF:SCREEN-VALUE <> ""
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        find first xfg-rdtlh
               where xfg-rdtlh.company   eq g_company
                 and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                 AND xfg-rdtlh.tag <> ""
                 and xfg-rdtlh.qty       gt 0
                 and xfg-rdtlh.rita-code ne "S"
               use-index tag no-lock no-error.
           if avail xfg-rdtlh THEN  DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
           END.
           RUN valid-tag NO-ERROR.
           IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
           {addon/loadtags/disptgf2.i "FGItem" FOCUS:SCREEN-VALUE}
           /*IF fg-rctd.loc:SCREEN-VALUE = "" THEN*/ RUN get-def-values.
           lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
           lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
           lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.

           IF v-case-tag THEN DO:
              IF v-ssfgscan THEN APPLY "entry" TO fg-rctd.loc.
              ELSE APPLY "entry" TO fg-rctd.cases.
              RETURN NO-APPLY.
           END.
           IF NOT v-ssfgscan THEN do:
              APPLY "row-leave" TO BROWSE {&browse-name}.
              RETURN NO-APPLY.
           END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* Whs */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
       END.

       FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                           AND fg-bin.i-no = ""
                           AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                           USE-INDEX co-ino NO-LOCK NO-ERROR.
       IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc .
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.

  IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
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
  lv-prev-job2 =  SELF:SCREEN-VALUE.

  IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
       RUN new-job-no.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  /*RUN new-job-no.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
     fg-rctd.rita-code EQ "E"                                   THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = "".

  IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ fg-rctd.company
          AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
          AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF NOT AVAIL po-ordl THEN
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ fg-rctd.company
          AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN DO:
      ASSIGN
       fg-rctd.i-no:screen-value in browse {&browse-name} = po-ordl.i-no
       fg-rctd.i-name:screen-value in browse {&browse-name} = po-ordl.i-name
       fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom
       fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cost).

      find first itemfg where itemfg.company = fg-rctd.company and
                        itemfg.i-no = po-ordl.i-no
                        no-lock no-error.
      if avail itemfg then 
         assign fg-rctd.loc:screen-value in browse {&browse-name} =  itemfg.def-loc
                fg-rctd.loc-bin:screen-value in browse {&browse-name} =  itemfg.def-loc-bin
                fg-rctd.qty-case:screen-value in browse {&browse-name} = string(itemfg.case-count)
            /*  fg-rctd.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                            else itemfg.prod-uom  */                        
         .
    END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item# */
DO:
  IF LASTKEY = -1 THEN RETURN.

  IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
     fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE ""      THEN DO:
    RUN valid-po-no (0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "FG does not exist on PO..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.

  find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                 no-lock no-error.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
        RETURN NO-apply.
     END.
     ELSE DO:
       MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF NOT ll-ans THEN DO:
           APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
           RETURN NO-APPLY.           
       END.
       ELSE DO:
           RUN fg/d-crtitm.w (SELF:SCREEN-VALUE) .
           find first itemfg {sys/look/itemfgrlW.i}
                     and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     no-lock no-error.
           IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                                        .
       END.
     END.
  END.

  /*IF SELF:MODIFIED THEN*/ RUN get-def-values.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  
    /*
    RUN get-matrix (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.std-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.std-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.std-cost IN BROWSE Browser-Table /* Cost/UOM */
DO:
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
cocode = g_company.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-new-record THEN
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = ""
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = ""
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = ""
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = "".

    RUN get-values.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-fg-bin-cost B-table-Win 
PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.std-tot-cost)
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.pur-uom).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-freight-cost B-table-Win 
PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ld-qty AS DEC NO-UNDO.
  DEF VAR ld-wgt AS DEC EXTENT 2 NO-UNDO.
  DEF VAR ld-cst AS DEC EXTENT 2 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE po-ord.

    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ fg-rctd.company
          AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no    EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no2   EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN DO:
      FIND FIRST po-ord WHERE
           po-ord.company EQ po-ordl.company AND
           po-ord.po-no EQ po-ordl.po-no
           NO-LOCK NO-ERROR.
      lv-rowid = ROWID(po-ordl).
    END.

    RELEASE po-ordl.

    IF AVAIL po-ord AND po-ord.t-freight GT 0 THEN DO:
      FOR EACH po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no   EQ po-ord.po-no AND
          po-ordl.item-type EQ NO NO-LOCK,
          FIRST itemfg
          WHERE itemfg.company EQ po-ordl.company
            AND itemfg.i-no    EQ po-ordl.i-no
          NO-LOCK:

        ld-qty = po-ordl.ord-qty.

        IF po-ordl.pr-qty-uom NE "EA" THEN
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                                 0, po-ordl.s-len, po-ordl.s-wid, 0,
                                 ld-qty, OUTPUT ld-qty).
      
        ld-wgt[2] = ld-wgt[2] + (ld-qty / 100 * itemfg.weight-100).

        IF ROWID(po-ordl) EQ lv-rowid THEN
          ld-wgt[1] = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                      100 * itemfg.weight-100.
      END.

      op-cost = po-ord.t-freight * (ld-wgt[1] / ld-wgt[2]).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix B-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.

  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.


  if not avail fg-rctd then return.  /* no records */

DO WITH FRAME {&FRAME-NAME}:
find itemfg where itemfg.company eq cocode
              and itemfg.i-no  eq fg-rctd.i-no:screen-value in browse {&browse-name}
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" then do: /* for row-display */  
  find first po-ordl where po-ordl.company = cocode
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  ASSIGN
   lv-out-qty  = fg-rctd.t-qty
   lv-out-cost = fg-rctd.std-cost.

  /* convert cost pr-uom*/
  IF fg-rctd.cost-uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).
end. /* avail fg-rctd */
/* ======================================================================= */
else
if avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE <> "" then do: /* in update mode - use screen-value */
  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) 
                       and po-ordl.i-no  = fg-rctd.i-no:screen-value
                       and po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       and po-ordl.item-type = no
                       no-lock no-error.
  
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = po-ordl.t-rec-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

    IF po-ordl.pr-qty-uom <> "EA" THEN
       run sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, output v-rec-qty).
    if v-rec-qty gt (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
       AND NOT lv-overrun-checked
    then do:
       message "The PO Qty + overrun has been exceeded. "
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
       /*APPLY "entry" TO fg-rctd.cases.
       RETURN ERROR.  */
    end.
  END.
  ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no:screen-value
                       and job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       no-lock no-error.
       IF AVAIL job-hdr THEN DO: 
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + ((IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                                IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
      
          END.
          IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX Warning.
             /*RETURN ERROR.*/
             lv-overrun-checked = YES.
          END.
          
       END.
  END.

  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
   lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}). 

  IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-out-cost).
END.
  
IF lv-cost-uom NE "EA" THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

ASSIGN
 fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom
 fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-cost)
 fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING((lv-out-qty * lv-out-cost) +
           dec(fg-rctd.frt-cost:screen-value in browse {&browse-name})).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-all B-table-Win 
PROCEDURE get-matrix-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.


  cocode = g_company.

  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company eq g_company and
           (b-fg-rctd.rita-code eq "R" or b-fg-rctd.rita-code eq "E")
           AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
           AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
           AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE 
           AND (recid(b-fg-rctd) <> recid(fg-rctd) 
                OR (adm-new-record AND NOT adm-adding-record))
           NO-LOCK :

      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
  END.
  
  lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

  IF fg-rctd.i-no:SCREEN-VALUE <> "" then do: /* in update mode - use screen-value */
       find itemfg  where itemfg.company eq cocode
                and itemfg.i-no  eq fg-rctd.i-no:screen-value in browse {&browse-name}
                      use-index i-no no-lock no-error.
       find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) 
                       and po-ordl.i-no  = fg-rctd.i-no:screen-value
                       and po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       and po-ordl.item-type = no
                       no-lock no-error.
  
       IF AVAIL po-ordl THEN DO:
          v-rec-qty = po-ordl.t-rec-qty + lv-out-qty.
          RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).
          IF NOT ll-ea THEN
            run sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                                   v-rec-qty, output v-rec-qty).
         if v-rec-qty gt (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
            AND NOT lv-overrun-checked
          then do:
             message "The PO Qty + overrun has been exceeded. "
                     VIEW-AS ALERT-BOX WARNING .
             lv-overrun-checked = YES.
          end.
       END.
       ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
         find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no:screen-value
                       and job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       no-lock no-error.
         IF AVAIL job-hdr THEN DO: 
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
           IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
           ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + (IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                               IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0 / 100))).
      
           END.
           IF v-rec-qty <  lv-out-qty AND NOT lv-overrun-checked THEN DO:
              MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX Warning.
             /*RETURN ERROR.*/
              lv-overrun-checked = YES.
           END.
           
         END.
       END.
     
  END. /* i-no <> ""*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-values B-table-Win 
PROCEDURE get-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-loc AS CHAR NO-UNDO.
  DEF VAR lv-loc-bin AS CHAR NO-UNDO.
  DEF VAR lv-qty-case AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-std-cost AS CHAR NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-save AS CHAR EXTENT 20 NO-UNDO.


  {sys/inc/fgrecpt.i}
  fgrecpt = fgrecpt.

  DO WITH FRAME {&FRAME-NAME}:
    find first itemfg
        {sys/look/itemfgrlW.i}
          and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        no-lock no-error.
    fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.

    find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.

    assign
     lv-qty-case = string(itemfg.case-count)
     lv-cost-uom = if itemfg.pur-man then itemfg.pur-uom else itemfg.prod-uom.

    RUN fg/autopost.p (ROWID(itemfg),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       OUTPUT lv-loc, OUTPUT lv-loc-bin).

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.loc     eq lv-loc
          and fg-bin.loc-bin eq lv-loc-bin
          and fg-bin.i-no    eq ""
        no-lock no-error.
    if avail fg-bin then 
      assign
       lv-std-cost = IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" and
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                               THEN string(itemfg.last-cost) 
                                               ELSE lv-std-cost
       lv-qty-case = /*IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" and
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                                THEN   STRING(itemfg.case-count)
                                                ELSE lv-qty-case
                                                */
                                                STRING(itemfg.case-count)
       lv-cost-uom = itemfg.prod-uom.

    ASSIGN
     lv-save[1] = fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-save[2] = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    RUN get-fg-bin-cost.

    ASSIGN
     lv-std-cost = fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-cost-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}

     fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = lv-save[1]
     fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-save[2].

    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          and job-hdr.job-no  eq fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          and job-hdr.job-no2 eq int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK no-error.

    IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable-job
          WHERE reftable-job.reftable EQ "jc/jc-calc.p"
            AND reftable-job.company  EQ job.company
            AND reftable-job.loc      EQ ""
            AND reftable-job.code     EQ STRING(job.job,"999999999")
            AND reftable-job.code2    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    END.
       
    if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = string(job-hdr.std-tot-cost).
    ELSE
    IF AVAIL reftable-job AND reftable-job.val[5] GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = string(reftable-job.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = STRING(po-ordl.cost).

        RUN show-freight.
      END.
     
      else
      if avail itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        assign
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = STRING(itemfg.total-std-cost).

        IF /*itemfg.total-std-cost EQ 0 AND*/ itemfg.isaset THEN DO:
          RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

          IF lv-cost-uom NE "M" THEN
            RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                   0, 0, 0, 0, v-cost, OUTPUT v-cost).

          lv-std-cost = STRING(v-cost).
        END.
      END.
    END.

    IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ "" OR
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = lv-loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = lv-loc-bin.

    IF INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = lv-qty-case.

    IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom.

    IF DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = lv-std-cost.
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
  
  DEF VAR ls-tmp-qty AS cha NO-UNDO.
  DEF VAR ls-tmp-uom AS cha NO-UNDO.
  DEF VAR ls-tmp-cst AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ls-tmp-qty = fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-cst = fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty    = DEC(ls-tmp-qty)
   fg-rctd.pur-uom  = ls-tmp-uom
   fg-rctd.cost-uom = ls-tmp-uom
   fg-rctd.ext-cost = DEC(ls-tmp-cst).

  RUN fg/comprcpt.p (ROWID(fg-rctd)).

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
  lv-overrun-checked = NO.

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
         fg-rctd.rita-code = "R"
         fg-rctd.rct-date = TODAY
         fg-rctd.trans-time = TIME
         fg-rctd.units-pallet = 1
         fg-rctd.cases-unit = 1
         fg-rctd.ext-cost = 0
         .

  disp fg-rctd.rct-date fg-rctd.trans-time with browse {&browse-name}. 

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
  DEF BUFFER b-fg-rctd FOR fg-rctd.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  FOR EACH fg-rcpts
      WHERE fg-rcpts.company EQ cocode
        AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"):

    FOR EACH b-fg-rctd
        WHERE b-fg-rctd.company EQ cocode
          AND b-fg-rctd.r-no    EQ fg-rcpts.r-no:
      DELETE b-fg-rctd.
    END.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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

    lv-closed-checked = NO.

    APPLY "entry" TO fg-rctd.rct-date IN BROWSE {&browse-name}.
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
  DO WITH FRAME {&FRAME-NAME}:
    IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ ""      OR
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""      OR
       INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 OR
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""     OR
       DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      RUN get-values.
  END.

  RUN get-matrix (NO).

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-po-no (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} AND
     NOT lv-new-job-ran THEN RUN new-job-no.

  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-overrun-checked = NO.

  RUN repo-query (ROWID(fg-rctd)).

  ASSIGN lv-new-job-ran = NO
         lv-prev-job2 = "".

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  RUN scan-next.

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

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN DO:
        ASSIGN
         fg-rctd.job-no:SCREEN-VALUE   = job-hdr.job-no
         fg-rctd.job-no2:SCREEN-VALUE  = STRING(job-hdr.job-no2)
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE
         fg-rctd.i-no:SCREEN-VALUE     = job-hdr.i-no
         fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost).

        RUN get-def-values.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
               INT(fg-rctd.qty-case:SCREEN-VALUE) +
               INT(fg-rctd.partial:SCREEN-VALUE)).

    RUN show-freight.

    RUN get-matrix (NO).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-freight B-table-Win 
PROCEDURE show-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  IF fgpofrt-log THEN 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld = DEC(fg-rctd.frt-cost:SCREEN-VALUE IN BROWSE {&browse-name})
     fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE in browse {&browse-name}) - ld).

    RUN get-freight-cost (OUTPUT ld).

    ASSIGN
     fg-rctd.frt-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld)
     fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}) + ld).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  def output parameter op-tag# as log no-undo.
  def var cocode like rm-rcth.company no-undo.
  
  
  {rm/tag#.i}
  op-tag# = v-tag#.
  
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var v-tag-seq as int no-undo.
  def var v-locode as cha no-undo.
  def buffer xfg-rctd for fg-rctd.
  
  assign v-tag-seq = 0
         v-locode  = "".

  do while TRUE WITH FRAME {&FRAME-NAME}:
    find first xfg-rctd
        where xfg-rctd.company eq fg-rctd.company
          and xfg-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xfg-rctd then do:
      v-locode = xfg-rctd.loc.

      for each xfg-rctd where xfg-rctd.company eq fg-rctd.company
            and xfg-rctd.loc     eq v-locode
            and xfg-rctd.tag     begins string(int(fg-rctd.po-no:screen-value in browse {&browse-name}),"999999")
            use-index tag no-lock
            by xfg-rctd.tag desc:

           if int(substr(xfg-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xfg-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first fg-rctdh"where fg-rctdh.company eq rm-rcth.company
          and fg-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail fg-rctdh then do:
      v-locode = fg-rctdh.loc.

      for each fg-rctdh
          where fg-rctdh.company eq cocode
            and fg-rctdh.loc     eq v-locode
            and fg-rctdh.tag     begins string(int(fg-rctd.po-no),"999999")
          use-index tag no-lock
          by fg-rctdh.tag desc:

        if int(substr(fg-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(fg-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign
   v-tag-seq   = v-tag-seq + 1.
/*   fg-rctd.tag:screen-value in browse {&browse-name}
          = string(int(fg-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
*/          

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
  
  DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      IF fgrecpt                                                AND
         fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
         fg-rctd.rita-code NE "E"                                  THEN DO:
        MESSAGE "You must enter a Job or a PO..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    ELSE DO:
      IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        MESSAGE "You may only enter a Job or a PO, Job No will be erased..."
            VIEW-AS ALERT-BOX ERROR.
      END.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
        MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
    END.
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
         INDEX("CZ",job.stat) GT 0            THEN DO:
        ASSIGN
         lv-ans            = NO
         lv-closed-checked = YES.
        MESSAGE "Job is CLOSED, would you like to reopen?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no B-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type AS INT NO-UNDO.

  DO WITH FRAME {&frame-name}:
    IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        MESSAGE "You may only enter a Job or a PO, PO will be erased..." VIEW-AS ALERT-BOX ERROR.
        fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        RETURN.
      END.

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ fg-rctd.company
            AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ NO
            AND (po-ordl.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                 fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        IF ip-type NE 0 THEN DO:
          MESSAGE "Invalid PO#, try help..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.po-no IN BROWSE {&browse-name}.
        END.
        RETURN ERROR.
      END.
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

  IF fgrecpt-int = 1 THEN DO:
     FIND FIRST loadtag WHERE loadtag.company = g_company
                          AND loadtag.item-type = NO
                          AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
     IF NOT AVAIL loadtag /*or fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""*/ THEN DO:
        MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
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

  DEF VAR lv-uom-list AS cha NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    RUN sys/ref/uom-fg.p (NO, OUTPUT lv-uom-list).

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.cost-uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

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
  DEF VAR li-max-qty AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

 
  FIND itemfg WHERE itemfg.company = fg-rctd.company
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

  IF itemfg.isaset                                                        AND
     (itemfg.alloc EQ NO                OR
      (itemfg.alloc EQ YES       AND
       fgrecpt-char EQ "AUTOPOST" AND
       TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "")) THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING((INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                 INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),"->>>,>>>,>>9.99")
     li-max-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
            
    RUN fg/checkset.w (ROWID(itemfg),
                       ROWID(fg-rctd),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       INPUT fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                       INPUT-OUTPUT li-max-qty).

    IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      ll = NO.
      MESSAGE "Create receipt with maximum quantity available?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

      IF ll THEN  
        ASSIGN
         fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(li-max-qty)
         fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}  = 
              STRING(TRUNC((li-max-qty - DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})) /
                           DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0))
         fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = 
              STRING(li-max-qty - (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                                   DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).

      IF NOT ll OR li-max-qty EQ 0 THEN DO:
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
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
  
  FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc-bin.
          RETURN ERROR.
  END.
  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> ""
  THEN DO:
  
    FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                            bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                        AND RECID(bf-tmp) <> RECID(fg-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.tag.
       RETURN ERROR.
    END.
    ELSE DO:
        find first xfg-rdtlh
               where xfg-rdtlh.company   eq g_company
                 and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                 and xfg-rdtlh.qty       gt 0
                 and xfg-rdtlh.rita-code ne "S"
               use-index tag no-lock no-error.
           if avail xfg-rdtlh THEN  DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO fg-rctd.tag.
               RETURN error.
           END.
    END.
  END.

   RUN get-matrix (NO) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN error.

   run get-matrix-all (false) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

