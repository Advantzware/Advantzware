&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon\fg\b-rcptd.w

  Modify addon\fg\b-rcptds.w (Sharpshooter Version if necessary)

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


DEF VAR ll-help-run AS LOG NO-UNDO.  /* set on browse help, reset row-entry */
DEF VAR ls-prev-po AS cha NO-UNDO.
DEF VAR lv-overrun-checked AS LOG NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.


DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR fg-uom-list  AS CHAR NO-UNDO.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-post-date AS DATE INITIAL TODAY.

DEF SHARED VAR g-sharpshooter AS LOG NO-UNDO.
DEF VAR v-case-tag AS LOG NO-UNDO.
DEF VAR v-ssfgscan AS LOG NO-UNDO.
DEF VAR lvlAutoAdd AS LOG NO-UNDO.
DEF VAR lv-do-what AS cha NO-UNDO.  /* will be receipt or delete(negative receipt)*/
DEFINE VARIABLE currentRowID AS ROWID NO-UNDO.
DEFINE VARIABLE cFgEmails AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFgEmails AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFgEmails AS LOGICAL   NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.  /* for tag validation */
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER reftable-job FOR reftable.
DEF VAR lv-frst-rno AS INT NO-UNDO.
DEF VAR lv-linker LIKE fg-rcpts.linker NO-UNDO.
DEF VAR ll-set-parts AS LOG NO-UNDO.

{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}




/* For fgpostBatch.p */
{fg/fgPostBatch.i}
    
DEF STREAM logFile.
DEF STREAM st-email.

DO TRANSACTION:
   {sys/inc/fgpofrt.i}
   {sys/inc/fgrecpt.i}
   {sys/inc/sspostfg.i}

   FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "SSFGSCAN" NO-LOCK NO-ERROR.
   IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.NAME = "SSFGSCAN"
             sys-ctrl.descrip = "Prompt for the Warehouse/Bin?"
             sys-ctrl.log-fld = YES.
   END.
   v-ssfgscan = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE YES.
   lvlAutoAdd = IF AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 1 THEN NO ELSE YES.

   FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "CASETAG" NO-LOCK NO-ERROR.
   IF AVAIL sys-ctrl THEN v-case-tag = sys-ctrl.log-fld.
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

&SCOPED-DEFINE item-key-phrase TRUE
&SCOPED-DEFINE init-proc init-proc
/* gdm - */
{sys/inc/jobreopn.i}

DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FGSetAssembly",
    INPUT "C",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cFGSetAssembly,
    OUTPUT lFound).

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FgEmails",
                       INPUT "I",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFgEmails,
                       OUTPUT lFound).
IF lFound THEN
    iFgEmails = INTEGER(cFgEmails) NO-ERROR.
lFgEmails = (IF iFgEmails EQ 1 THEN YES ELSE NO).
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
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit ~
fg-rctd.partial fg-rctd.t-qty fg-rctd.stack-code fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.po-no fg-rctd.i-no fg-rctd.i-name fg-rctd.std-cost ~
fg-rctd.cost-uom fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.r-no ~
fg-rctd.rct-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.tag ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.stack-code fg-rctd.rct-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
LOOKUP(fg-rctd.rita-code,"R,E") > 0 ~
and ((lv-do-what = "delete" and fg-rctd.t-qty < 0) or  ~
     (lv-do-what <> "delete" and fg-rctd.t-qty >= 0)) ~
AND fg-rctd.SetHeaderRno EQ 0 ~
use-index fg-rctd NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
LOOKUP(fg-rctd.rita-code,"R,E") > 0 ~
and ((lv-do-what = "delete" and fg-rctd.t-qty < 0) or  ~
     (lv-do-what <> "delete" and fg-rctd.t-qty >= 0)) ~
AND fg-rctd.SetHeaderRno EQ 0 ~
use-index fg-rctd NO-LOCK ~
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
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 29
      fg-rctd.loc COLUMN-LABEL "Whs" FORMAT "x(13)":U WIDTH 8
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U WIDTH 9
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT "->>>,>>9":U
            WIDTH 9
      fg-rctd.cases-unit COLUMN-LABEL "Units/!Skid" FORMAT "->>>9":U
            WIDTH 9
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
            WIDTH 10
      fg-rctd.t-qty COLUMN-LABEL "Total!Qty" FORMAT "->>>,>>>,>>9.99":U
      fg-rctd.stack-code COLUMN-LABEL "FG Lot#" FORMAT "x(20)":U
            WIDTH 21.8
      fg-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 10
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.po-no FORMAT "x(9)":U WIDTH 14
      fg-rctd.i-no COLUMN-LABEL "Item#" FORMAT "X(15)":U WIDTH 22
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.std-cost COLUMN-LABEL "Cost/UOM" FORMAT ">,>>>,>>9.99<<":U
            WIDTH 18
      fg-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      fg-rctd.frt-cost COLUMN-LABEL "Freight Cost" FORMAT ">>>,>>9.99<<":U
      fg-rctd.ext-cost COLUMN-LABEL "Extended Cost" FORMAT "->>>,>>9.99<<":U
      fg-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>>":U WIDTH 12
      fg-rctd.rct-date COLUMN-LABEL "Receipt!Date" FORMAT "99/99/9999":U
  ENABLE
      fg-rctd.tag
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.cases-unit
      fg-rctd.partial
      fg-rctd.stack-code
      fg-rctd.rct-date
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
     _TblOptList       = ", FIRST"
     _Where[1]         = "fg-rctd.company eq cocode and
fg-rctd.r-no ge lv-frst-rno and
LOOKUP(fg-rctd.rita-code,""R,E"") > 0
and ((lv-do-what = ""delete"" and fg-rctd.t-qty < 0) or 
     (lv-do-what <> ""delete"" and fg-rctd.t-qty >= 0))
AND fg-rctd.SetHeaderRno EQ 0
use-index fg-rctd"
     _FldNameList[1]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whs" "x(13)" "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units/!Skid" "->>>9" "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.stack-code
"fg-rctd.stack-code" "FG Lot#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rctd.job-no
"fg-rctd.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   = ASI.fg-rctd.job-no2
     _FldNameList[12]   > ASI.fg-rctd.po-no
"fg-rctd.po-no" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" "Item#" "X(15)" "character" ? ? ? ? ? ? no "FG Item Number" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost/UOM" ">,>>>,>>9.99<<" "decimal" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.cost-uom
"fg-rctd.cost-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.frt-cost
"fg-rctd.frt-cost" "Freight Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Extended Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.r-no
"fg-rctd.r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Receipt!Date" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   RUN new-state IN phandle ('update-begin':U).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO: 
   DEF VAR ll-tag# AS LOG NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     ll-help-run = YES.
     CASE FOCUS:NAME:
        /*
        when "po-no" then do:
             run windows/l-pofg.w (cocode,focus:screen-value, output char-val).
             if char-val <> "" then do:
                assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.i-no:screen-value in browse {&browse-name} = entry(2,char-val)
                       fg-rctd.i-name:screen-value in browse {&browse-name} = entry(3,char-val)
                       fg-rctd.job-no:screen-value in browse {&browse-name} = entry(4,char-val)
                       fg-rctd.job-no2:screen-value in browse {&browse-name} = entry(5,char-val)
                       .
               find po-ordl where po-ordl.company = cocode and
                                  po-ordl.po-no = integer(entry(1,char-val)) and
                                  po-ordl.line = integer(entry(6,char-val))
                                  no-lock no-error.
               if avail po-ordl then do:
                  assign /*-rctd.pur-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                         fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom
                         fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cost)
                         .
               end.

               find first itemfg where itemfg.company = cocode and
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
                RUN windows/l-poitem.w (cocode,fg-rctd.po-no:screen-value in browse {&browse-name}, focus:screen-value in browse {&browse-name}, output char-val).
                if char-val <> "" then do :
                   assign focus:screen-value in browse {&browse-name} = entry(1,char-val)
                       fg-rctd.i-name:screen-value = entry(2,char-val)
                       fg-rctd.job-no:screen-value = entry(3,char-val)
                       fg-rctd.job-no2:screen-value = entry(4,char-val)
                       .
                end.
             END.
             ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
                  RUN windows/l-jobit1.w (cocode,fg-rctd.job-no:SCREEN-VALUE,fg-rctd.job-no2:screen-value, focus:screen-value, OUTPUT char-val,OUTPUT rec-val).
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
                  RUN windows/l-itemf2.w (cocode, "", fg-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
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
             run windows/l-jobno.w (cocode, focus:screen-value,output char-val, OUTPUT rec-val).
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
             FIND FIRST itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

             return no-apply.   
       end.  
       when "job-no2" then do:
             run windows/l-jobno2.w (cocode, fg-rctd.job-no:screen-value,focus:screen-value,output char-val, OUTPUT rec-val).
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
             FIND itemfg WHERE itemfg.company = cocode
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
             return no-apply.   
       end.  
       */
       WHEN "loc" THEN DO:
             RUN windows/l-loc.w (cocode,fg-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN fg-rctd.loc:screen-value IN  BROWSE {&browse-name}  = ENTRY(1,char-val)
                       .

             END.
             RETURN NO-APPLY.   
       END.
       WHEN "loc-bin" THEN DO:
             RUN windows/l-fgbin.w (cocode,fg-rctd.loc:screen-value, fg-rctd.loc-bin:screen-value,OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN fg-rctd.loc-bin:screen-value  = ENTRY(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             END.
             RETURN NO-APPLY.   
       END.
         WHEN "tag" THEN DO:
           IF lv-do-what = "delete" THEN
               RUN windows/l-ldtag5.w (cocode,NO,fg-rctd.tag:screen-value,OUTPUT char-val,OUTPUT rec-val).
           ELSE RUN windows/l-ldtag.w (cocode,NO,fg-rctd.tag:screen-value,OUTPUT char-val,OUTPUT rec-val).

           IF char-val <> "" THEN DO :
              fg-rctd.tag:SCREEN-VALUE = ENTRY(1,char-val).
              /*  ===*/
              IF CAN-FIND(FIRST b-fg-rctd 
                          WHERE b-fg-rctd.company   EQ cocode 
                            AND b-fg-rctd.tag       EQ fg-rctd.tag:SCREEN-VALUE
                            AND b-fg-rctd.rita-code NE "P"
                            AND ((lv-do-what        EQ "delete" 
                            AND b-fg-rctd.rita-code NE "P") OR lv-do-what <> "delete" )
                            AND RECID(b-fg-rctd) <> RECID(fg-rctd)) THEN DO:
                 MESSAGE "This Tag Number Has Already Been Used." SKIP
                         "Please Enter A Unique Tag Number."    
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
              END.
              ELSE DO:
                 IF lv-do-what <> "Delete" AND
                 CAN-FIND(FIRST b-fg-rdtlh WHERE b-fg-rdtlh.company   EQ cocode
                      AND b-fg-rdtlh.loc       EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND b-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE
                      AND b-fg-rdtlh.qty       GT 0
                      AND b-fg-rdtlh.rita-code NE "S"
                      USE-INDEX tag) THEN DO:
                    MESSAGE "This Tag Number Has Already Been Used." SKIP
                            "Please Enter A Unique Tag Number."
                            VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                 END.
              END.
              
              {addon/loadtags/disptgf2.i "FGItem" fg-rctd.tag:SCREEN-VALUE}
              
              RUN get-def-values.
              ASSIGN
              lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
              lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
  
              RETURN NO-APPLY.
           END.
       END.
     END CASE.
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
  
      RUN set-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
    /* If not on a row, don't check anything */
    IF BROWSE browser-table:NUM-SELECTED-ROWS = 0 THEN
        RETURN.    
    /* gdm - 08260803 */
    IF LASTKEY = -1 OR LASTKEY = -2 THEN
       RETURN.

    DEF VAR op-error AS LOG NO-UNDO.
    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
    
          MESSAGE "This Tag Number Cannot Be Blank." SKIP
                  "Please Enter A Tag Number." 
             VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.

    END.
    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" AND
       CAN-FIND(FIRST b-fg-rctd 
                WHERE b-fg-rctd.company = cocode 
                AND b-fg-rctd.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} 
                AND b-fg-rctd.rita-code <> "P" 
                AND RECID(b-fg-rctd) <> RECID(fg-rctd)) THEN
       DO:
          MESSAGE "This Tag Number Has Already Been Used." SKIP
                  "Please Enter A Unique Tag Number." 
             VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    ELSE DO:
        IF lv-do-what <> "Delete" AND fg-rctd.tag:SCREEN-VALUE <> "" AND
        CAN-FIND(FIRST b-fg-rdtlh WHERE
        b-fg-rdtlh.company   EQ cocode AND
        b-fg-rdtlh.loc       EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} AND
        b-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
        b-fg-rdtlh.qty       GT 0 AND
        b-fg-rdtlh.rita-code NE "S"
        USE-INDEX tag) THEN
        DO:
           MESSAGE "This Tag Number Has Already Been Used." SKIP
                   "Please Enter A Unique Tag Number." 
                   VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        RUN valid-tag (fg-rctd.tag:HANDLE IN BROWSE {&browse-name}, OUTPUT op-error).
        IF op-error THEN RETURN NO-APPLY.
        IF lv-do-what = "delete" THEN DO:
           RUN valid-delete-tag(OUTPUT op-error).
           IF op-error THEN RETURN NO-APPLY.
        END.
        IF AVAIL fg-rctd AND fg-rctd.tag <> SELF:SCREEN-VALUE THEN DO:
          {addon/loadtags/disptgf2.i "FGItem" fg-rctd.tag:SCREEN-VALUE}
        END.
        /*IF fg-rctd.loc:SCREEN-VALUE = "" THEN*/
            
        RUN get-def-values.
        ASSIGN
        lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
        lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
        lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.

        FIND FIRST loadtag NO-LOCK
        WHERE loadtag.company   EQ cocode
          AND loadtag.item-type EQ NO
          AND loadtag.tag-no    EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-ERROR.

        IF AVAIL loadtag THEN 
           fg-rctd.stack-code:SCREEN-VALUE IN BROWSE {&browse-name} = loadtag.misc-char[2] .  /* task 12051303 */

        IF v-case-tag THEN DO:
           IF v-ssfgscan THEN APPLY "entry" TO fg-rctd.loc.
           ELSE APPLY "entry" TO fg-rctd.cases.
           RETURN NO-APPLY.
        END.
        
        IF NOT v-ssfgscan AND adm-new-record THEN DO:
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

       FIND FIRST loc WHERE loc.company = cocode
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       FIND FIRST fg-bin WHERE fg-bin.company = cocode 
                           AND fg-bin.i-no = ""
                           AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                           USE-INDEX co-ino NO-LOCK NO-ERROR.
       IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc .
          RETURN NO-APPLY.
       END.

      /* User only needs to tab past loc to continue to scan next tag */
      IF SSPostFG-log THEN DO:
        
        APPLY 'entry' TO fg-rctd.tag IN BROWSE {&browse-name}.
        DEF VAR h AS HANDLE.
        h = fg-rctd.tag:HANDLE IN BROWSE {&browse-name}.
        BROWSE {&browse-name}:CURRENT-COLUMN = h.
        APPLY "RETURN" TO fg-rctd.tag.
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
       FIND FIRST fg-bin WHERE fg-bin.company = cocode 
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


&Scoped-define SELF-NAME fg-rctd.stack-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.stack-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.stack-code IN BROWSE Browser-Table /* FG Lot# */
DO:
  IF LASTKEY NE -1 THEN DO:
    DEF VAR op-error AS LOG NO-UNDO.

    RUN valid-lot#(fg-rctd.stack-code:HANDLE IN BROWSE {&browse-name},
                   OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.
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
    DEF VAR op-error AS LOG NO-UNDO.
    RUN valid-job-no(OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  ASSIGN
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
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

    DEF VAR op-error AS LOG NO-UNDO.

    RUN valid-job-no2(OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.

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
     DEF VAR op-error AS LOG NO-UNDO.

     RUN valid-po-no (1,OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  /*
  IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = "".

  IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
          AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL po-ordl THEN
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.
    
    IF AVAIL po-ordl THEN DO:
      ASSIGN
       fg-rctd.i-no:screen-value in browse {&browse-name} = po-ordl.i-no
       fg-rctd.i-name:screen-value in browse {&browse-name} = po-ordl.i-name
       fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.pr-uom
       fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cost).

      find first itemfg where itemfg.company = cocode and
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
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item# */
DO:
  IF LASTKEY = -1 THEN RETURN.

  DEF VAR op-error AS LOG NO-UNDO.

  IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
     fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE ""      THEN DO:
    RUN valid-po-no (0,OUTPUT op-error).
    IF op-error THEN DO:
      MESSAGE "FG does not exist on PO..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.

  FIND FIRST itemfg {sys/look/itemfgrlW.i}
             AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                 NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
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
           FIND FIRST itemfg {sys/look/itemfgrlW.i}
                     AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     NO-LOCK NO-ERROR.
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

    DEF VAR op-error AS LOG NO-UNDO.
    RUN valid-uom(OUTPUT op-error).

    IF op-error THEN RETURN NO-APPLY.

    RUN get-matrix (NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
    {sys/inc/fgpost.i}
END.

v-fgpostgl = fgpostgl.

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

  IF AVAIL fg-rctd AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
    RUN dispatch ('delete-record').

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}: 
  {addon/loadtags/disptgf2.i "FGItem" 
    "fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgPostlog B-table-Win 
PROCEDURE fgPostlog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.
        
 PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
     STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

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
     /*  fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = ""       
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = ""*/
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = ""
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-r-no B-table-Win 
PROCEDURE get-first-r-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bq-fg-rctd FOR fg-rctd.

  lv-frst-rno = 999999999.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "R"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  /*RELEASE bq-fg-rctd.*/

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "E"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  /*RELEASE bq-fg-rctd.*/
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
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no    EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no2   EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN
      RUN po/getfrtcs.p (ROWID(po-ordl),
                         DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         OUTPUT op-cost).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-linker B-table-Win 
PROCEDURE get-linker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-linker LIKE lv-linker NO-UNDO.


  op-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".

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
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

  DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR lvCalcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
  DEF VAR lvCalcStdCost LIKE fg-rctd.std-cost NO-UNDO.
  DEF VAR lvCalcExtCost LIKE fg-rctd.ext-cost NO-UNDO.
  DEF VAR lvCalcFrtCost LIKE fg-rctd.frt-cost NO-UNDO.
  DEF VAR lvSetupPerCostUom AS DEC NO-UNDO.

  IF NOT AVAIL fg-rctd THEN RETURN.  /* no records */

DO WITH FRAME {&FRAME-NAME}:
FIND  FIRST itemfg WHERE itemfg.company EQ cocode
              AND itemfg.i-no  EQ fg-rctd.i-no:screen-value IN BROWSE {&browse-name}
            USE-INDEX i-no NO-LOCK NO-ERROR.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0.

IF ip-first-disp  AND AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO: /* for row-display */  
  FIND FIRST po-ordl WHERE po-ordl.company = cocode
                       AND po-ordl.po-no = int(fg-rctd.po-no)
                       AND po-ordl.i-no  = fg-rctd.i-no
                       AND po-ordl.job-no = fg-rctd.job-no
                       AND po-ordl.job-no2 = fg-rctd.job-no2
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  lv-out-qty = fg-rctd.t-qty.

  /* convert cost pr-uom*/
  IF fg-rctd.cost-uom EQ lv-cost-uom               OR
     (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list)      GT 0)   THEN
    lv-out-cost = fg-rctd.std-cost.
  ELSE
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).
END. /* avail fg-rctd */
/* ======================================================================= */
ELSE
IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE <> "" THEN DO: /* in update mode - use screen-value */
  FIND FIRST po-ordl WHERE po-ordl.company = cocode
                       AND po-ordl.po-no = integer(fg-rctd.po-no:screen-value IN BROWSE {&browse-name}) 
                       AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                       AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.
  
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = po-ordl.t-rec-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

    IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
       RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).
    IF v-rec-qty GT (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
       AND NOT lv-overrun-checked
    THEN DO:
       MESSAGE "The PO Qty + overrun has been exceeded. "
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
    END.

    DEF VAR lv-use-full-qty AS LOG.
    DEF VAR lv-full-qty AS DEC NO-UNDO.

    /* Created task 09261318 to be used by receiving screens in addition */            
    RUN fg/calcRcptCostFromPO.p 
      (INPUT cocode ,
      INPUT ROWID(po-ordl),
      INPUT ROWID(fg-rctd),
      INPUT fg-rctd.qty-case:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cases:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.partial:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no2:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.t-qty:screen-value IN BROWSE {&browse-name},
      OUTPUT lv-use-full-qty,
      OUTPUT lv-full-qty,
      OUTPUT lvCalcCostUom,
      OUTPUT lvCalcStdCost,
      OUTPUT lvCalcExtCost,
      OUTPUT lvCalcFrtCost,
      OUTPUT lvSetupPerCostUom).
    
    ASSIGN
      fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name} = lvCalcCostUom
      fg-rctd.std-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcStdCost)
      fg-rctd.ext-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcExtCost).
    ASSIGN
     lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-cost-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).

  END.
  ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       FIND FIRST job-hdr WHERE job-hdr.company = cocode                       
                       AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                       AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       NO-LOCK NO-ERROR.
       IF AVAIL job-hdr THEN DO: 
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
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
          IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX WARNING.
             lv-overrun-checked = YES.
          END.
          
       END.
  END.

  lv-out-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}). 
  
  IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ lv-cost-uom               OR
     (LOOKUP(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list)                                            GT 0)   THEN
    lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  ELSE
    RUN rm/convcuom.p(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-out-cost).
END.
  
IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

ASSIGN
 fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom
 fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-cost)
 fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING((lv-out-qty * lv-out-cost) +
           dec(fg-rctd.frt-cost:screen-value IN BROWSE {&browse-name})).
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
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

  DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.
  
  FOR EACH b-fg-rctd WHERE
      b-fg-rctd.company EQ cocode AND
      LOOKUP(b-fg-rctd.rita-code,"R,E") > 0
      AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
      AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
      AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE
      NO-LOCK :

      IF NOT((RECID(b-fg-rctd) <> recid(fg-rctd) OR
             (adm-new-record AND NOT adm-adding-record))) THEN
         NEXT.
      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
  END.
  
  lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

  IF fg-rctd.i-no:SCREEN-VALUE <> "" THEN DO: /* in update mode - use screen-value */
       FIND FIRST itemfg  WHERE itemfg.company EQ cocode
                AND itemfg.i-no  EQ fg-rctd.i-no:screen-value IN BROWSE {&browse-name}
                      USE-INDEX i-no NO-LOCK NO-ERROR.
       FIND FIRST po-ordl WHERE po-ordl.company = cocode
                       AND po-ordl.po-no = integer(fg-rctd.po-no:screen-value IN BROWSE {&browse-name}) 
                       AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                       AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.
  
       IF AVAIL po-ordl THEN DO:
          v-rec-qty = po-ordl.t-rec-qty + lv-out-qty.
          RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).
          IF NOT ll-ea THEN
            RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                                   v-rec-qty, OUTPUT v-rec-qty).
         IF v-rec-qty GT (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
            AND NOT lv-overrun-checked
          THEN DO:
             MESSAGE "The PO Qty + overrun has been exceeded. "
                     VIEW-AS ALERT-BOX WARNING .
             lv-overrun-checked = YES.
          END.
       END.
       ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
         FIND FIRST job-hdr WHERE job-hdr.company = cocode                       
                       AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                       AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       NO-LOCK NO-ERROR.
         IF AVAIL job-hdr THEN DO: 
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
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
              MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX WARNING.
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
  DEF VAR ll-ea AS LOG NO-UNDO.

  {sys/inc/fgrecpt.i}
  fgrecpt = fgrecpt.

  DO WITH FRAME {&FRAME-NAME}: 
    IF adm-new-record AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""
       AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN DO:
      RUN display-item.
    END.
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    /*find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.*/

    ASSIGN
    fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
    lv-qty-case = STRING(itemfg.case-count)
    lv-cost-uom = IF itemfg.pur-man THEN itemfg.pur-uom ELSE itemfg.prod-uom.

    RUN fg/autopost.p (ROWID(itemfg),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       OUTPUT lv-loc, OUTPUT lv-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.loc     EQ lv-loc
          AND fg-bin.loc-bin EQ lv-loc-bin
          AND fg-bin.i-no    EQ ""
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN 
      ASSIGN
       lv-std-cost = IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" AND
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                               THEN STRING(itemfg.last-cost) 
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
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

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
       
    IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = STRING(job-hdr.std-tot-cost).
    ELSE
    IF AVAIL reftable-job AND reftable-job.val[5] GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = STRING(reftable-job.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
    ELSE DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ cocode           
            AND po-ordl.po-no     EQ int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ NO
          NO-LOCK NO-ERROR.
          
      IF AVAIL po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = STRING(po-ordl.cost).

        RUN show-freight.
      END.
     
      ELSE
      IF AVAIL itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        ASSIGN
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = STRING(itemfg.total-std-cost).

        IF /*itemfg.total-std-cost EQ 0 AND*/ itemfg.isaset THEN DO:
          RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

          IF lv-cost-uom NE "M" THEN DO:
            RUN sys/ref/ea-um-fg.p (lv-cost-uom, OUTPUT ll-ea).
            IF ll-ea THEN lv-cost-uom = "EA".
            RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                   0, 0, 0, 0, v-cost, OUTPUT v-cost).
            IF ll-ea THEN lv-cost-uom = itemfg.prod-uom.
          END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc B-table-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'linker-source':U,OUTPUT char-hdl).
  ll-set-parts = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-in-update B-table-Win 
PROCEDURE is-in-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-in-update AS LOG NO-UNDO.

   op-in-update = /*IF fg-rctd.tag:SENSITIVE IN BROWSE {&browse-name} THEN YES ELSE NO*/
                  IF adm-brs-in-update OR adm-new-record THEN YES ELSE NO.

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
  DEF VAR lcJobNo LIKE fg-rctd.job-no NO-UNDO.
  DEF VAR liJobNo2 LIKE fg-rctd.job-no2 NO-UNDO.
  DEF VAR lcPoNo LIKE fg-rctd.po-no NO-UNDO.
  DEF VAR lcINo LIKE fg-rctd.i-no NO-UNDO.
  DEF VAR lcIName LIKE fg-rctd.i-name NO-UNDO.
  DEF VAR ldStdCost LIKE fg-rctd.std-cost NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ls-tmp-qty = fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-cst = fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}
     lcJobNo = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
     liJobNo2 = INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
     lcPoNo = fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}
     lciNo = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     lcIName = fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}
     ldStdCost = DECIMAL(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty    = DEC(ls-tmp-qty)
   fg-rctd.pur-uom  = ls-tmp-uom
   fg-rctd.cost-uom = ls-tmp-uom
   fg-rctd.ext-cost = DEC(ls-tmp-cst)
   fg-rctd.job-no = lcJobNo
   fg-rctd.job-no2 = liJobNo2
   fg-rctd.po-no = lcPoNo
   fg-rctd.i-no = lcINo
   fg-rctd.i-name = lcIName
   fg-rctd.std-cost = ldStdCost
   fg-rctd.enteredBy = USERID("asi")
   fg-rctd.enteredDT = DATETIME(TODAY, MTIME) 
   .

  IF fg-rctd.po-no GT "" THEN DO:
      /* 12131305 - Populate job-no so during posting will update */
      /*            farm out tab on job screen                    */
      FIND FIRST po-ord WHERE po-ord.company EQ fg-rctd.company
          AND po-ord.po-no EQ INTEGER(fg-rctd.po-no)
          NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
          FIND FIRST po-ordl WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no EQ po-ord.po-no
              AND po-ordl.i-no  EQ fg-rctd.i-no 
          NO-LOCK NO-ERROR.
      DEF BUFFER bfItemfg FOR itemfg.
      IF AVAIL po-ordl THEN
          FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
            AND itemfg.i-no EQ po-ordl.i-no 
          NO-LOCK NO-ERROR.
     
      IF AVAIL itemfg AND itemfg.pur-man = TRUE AND po-ordl.job-no GT "" THEN
          ASSIGN fg-rctd.job-no = po-ordl.job-no
                 fg-rctd.job-no2 = po-ordl.job-no2.

      
  END.
  RUN fg/comprcpt.p (ROWID(fg-rctd)).


    IF fg-rctd.created-by = "" THEN DO:
         ASSIGN fg-rctd.created-by = USERID("nosweat").
    END.
    ASSIGN
        fg-rctd.updated-by = USERID("nosweat")
        fg-rctd.upd-date = TODAY
        fg-rctd.upd-time = TIME.



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

  ASSIGN fg-rctd.company = cocode
         fg-rctd.r-no    = lv-rno
         fg-rctd.rita-code = "R"
         fg-rctd.rct-date = TODAY
         fg-rctd.trans-time = TIME
         fg-rctd.units-pallet = 1
         fg-rctd.cases-unit = 1
         fg-rctd.ext-cost = 0
         .

  DISP fg-rctd.rct-date WITH BROWSE {&browse-name}. 

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
        AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
      NO-LOCK:

    FOR EACH b-fg-rctd
        WHERE b-fg-rctd.company EQ cocode
          AND b-fg-rctd.r-no    EQ fg-rcpts.r-no
      EXCLUSIVE-LOCK USE-INDEX fg-rctd:
      DELETE b-fg-rctd.
    END.
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
  /* Prevents error relating to setting attribute on browse that has focus */
  APPLY 'entry' TO Btn_Clear_Find IN FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

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
  DEF VAR out-hd-lst AS cha NO-UNDO.
  DEF VAR ii AS INT NO-UNDO.
  DEF VAR hd-next AS WIDGET-HANDLE NO-UNDO.
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

    APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
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

  IF ll-set-parts THEN DO:
    lv-linker = "".
    RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'linker-source':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN get-linker IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-linker).
    IF lv-linker EQ "" THEN RETURN "adm-error".
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN get-do-what IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-do-what).
  
  RUN get-first-r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-query.

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
  DEF VAR op-error AS LOG NO-UNDO.
    DEF VAR lvrSaveRowid AS ROWID NO-UNDO.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  lvrSaveRowid = ROWID(fg-rctd).  
  
  RUN valid-tag (fg-rctd.tag:HANDLE IN BROWSE {&browse-name}, OUTPUT op-error).
  IF op-error THEN DO: 
    IF NOT AVAIL fg-rctd THEN
       FIND fg-rctd WHERE ROWID(fg-rctd) EQ lvrSaveRowid NO-LOCK NO-ERROR.     
    RETURN NO-APPLY.
  END.
  IF lv-do-what = "delete" THEN DO:
     RUN valid-delete-tag(OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ ""      OR
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""      OR
       INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 OR
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""     OR
       DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      RUN get-values.
  END.

  RUN get-matrix (NO).
  
  RUN valid-lot# (fg-rctd.stack-code:HANDLE,OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.

  RUN valid-po-no (1,OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.
  
  RUN valid-job-no(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.

  RUN valid-job-no2(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.

  IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} AND
     NOT lv-new-job-ran THEN RUN new-job-no.

  RUN validate-record(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-overrun-checked = NO.

  RUN repo-query (ROWID(fg-rctd)).

  ASSIGN lv-new-job-ran = NO
         lv-prev-job2 = "".

  RUN fg/invrecpt.p (ROWID(fg-rctd), 1).

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  IF SSPostFG-log THEN
     RUN post-finish-goods.
  lvrSaveRowid = ROWID(fg-rctd).
  IF lvlAutoAdd THEN
    RUN scan-next.
  ELSE DO:
      /* Make sure to redisplay since cost may have been updated */
      /* on all lines.                                           */
      RUN local-open-query.
      REPOSITION {&browse-name} TO ROWID lvrSaveRowid NO-ERROR.
      RUN dispatch ('row-changed').
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
    ASSIGN
    lv-closed-checked = NO
    lv-new-job-ran = YES.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        BREAK BY job-hdr.frm      DESC
              BY job-hdr.blank-no DESC:

      IF LAST(job-hdr.frm)                                                  OR
         job-hdr.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
        ASSIGN
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE
         fg-rctd.i-no:SCREEN-VALUE     = job-hdr.i-no
         fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost).

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

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.

  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.

  SESSION:SET-WAIT-STATE ("general").
  /* IF fgPostLog THEN RUN fgPostLog ('Started'). */
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
  /* task - add additional values to w-fg-rctd */
  CREATE w-fg-rctd.
  BUFFER-COPY fg-rctd TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = ROWID(fg-rctd)
         w-fg-rctd.has-rec = YES.
         
    /* task - add values passed in */  
    ASSIGN
        v-post-date = TODAY
        .       
  RUN fg/fgpostBatch.p ( 
        INPUT v-post-date, /* Post date      */
        INPUT NO,          /* tg-recalc-cost */
        INPUT "R",         /* Receipts       */
        INPUT lFgEmails,   /* Send fg emails */
        INPUT TABLE w-fg-rctd BY-reference,
        INPUT TABLE tt-fgemail BY-reference,
        INPUT TABLE tt-email BY-reference,
        INPUT TABLE tt-inv BY-reference).
        
/*  FOR EACH w-fg-rctd,                                                                                                       */
/*                                                                                                                            */
/*        FIRST itemfg                                                                                                        */
/*        WHERE itemfg.company EQ cocode                                                                                      */
/*          AND itemfg.i-no    EQ w-fg-rctd.i-no                                                                              */
/*                                                                                                                            */
/*        BY w-fg-rctd.tag                                                                                                    */
/*        BY w-fg-rctd.rct-date                                                                                               */
/*        BY w-fg-rctd.r-no:                                                                                                  */
/*                                                                                                                            */
/*      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).                                          */
/*      {fg/fg-post.i w-fg-rctd w-fg-rctd}                                                                                    */
/*                                                                                                                            */
/*      FIND CURRENT po-ordl NO-LOCK NO-ERROR.                                                                                */
/*      FIND CURRENT fg-bin NO-LOCK NO-ERROR.                                                                                 */
/*      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.                                                                             */
/*      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').                                           */
/*      IF w-fg-rctd.rita-code = "R" THEN DO:                                                                                 */
/*         {fg/fgemails.i}                                                                                                    */
/*      END.                                                                                                                  */
/*                                                                                                                            */
/*      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').                                                       */
/*                                                                                                                            */
/*      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.                                                 */
/*                                                                                                                            */
/*      IF AVAIL fg-rctd THEN DO:                                                                                             */
/*        ASSIGN                                                                                                              */
/*         fg-rctd.rita-code = "P"  /* posted */                                                                              */
/*         fg-rctd.post-date = v-post-date                                                                                    */
/*         fg-rctd.tag2      = w-fg-rctd.tag2.                                                                                */
/*                                                                                                                            */
/*        FOR EACH fg-rcpts                                                                                                   */
/*            WHERE fg-rcpts.company EQ fg-rctd.company                                                                       */
/*              AND fg-rcpts.r-no    EQ fg-rctd.r-no:                                                                         */
/*          fg-rcpts.rita-code = fg-rctd.rita-code.                                                                           */
/*        END.                                                                                                                */
/*      END.                                                                                                                  */
/*                                                                                                                            */
/*      IF fgPostLog THEN RUN fgPostLog ('End loop').                                                                         */
/*    END.  /* for each fg-rctd */                                                                                            */
/*                                                                                                                            */
/*    FIND CURRENT itemfg NO-LOCK NO-ERROR.                                                                                   */
/*                                                                                                                            */
/*    IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').                                                  */
/*    FOR EACH w-fg-rctd                                                                                                      */
/*        BREAK BY w-fg-rctd.i-no                                                                                             */
/*              BY w-fg-rctd.job-no                                                                                           */
/*              BY w-fg-rctd.job-no2                                                                                          */
/*              BY w-fg-rctd.loc                                                                                              */
/*              BY w-fg-rctd.loc-bin                                                                                          */
/*              BY w-fg-rctd.tag:                                                                                             */
/*                                                                                                                            */
/*      IF LAST-OF(w-fg-rctd.tag) THEN DO:                                                                                    */
/*        IF TRIM(w-fg-rctd.tag) NE "" THEN                                                                                   */
/*        /* Ensure Bin/Tags Qty is correct.  Task 01270602 */                                                                */
/*                                                                                                                            */
/*        FOR EACH fg-bin NO-LOCK                                                                                             */
/*            WHERE fg-bin.company EQ g_company                                                                               */
/*              AND fg-bin.i-no    EQ loadtag.i-no                                                                            */
/*              AND fg-bin.tag     EQ loadtag.tag-no                                                                          */
/*            USE-INDEX tag:                                                                                                  */
/*          RUN fg/calcbinq.p (ROWID(fg-bin)).                                                                                */
/*        END.                                                                                                                */
/*                                                                                                                            */
/*        /* IF w-fg-rctd.tag <> "" then*/                                                                                    */
/*        FIND FIRST loadtag                                                                                                  */
/*            WHERE loadtag.company   EQ g_company                                                                            */
/*              AND loadtag.item-type EQ NO                                                                                   */
/*              AND loadtag.tag-no    EQ w-fg-rctd.tag                                                                        */
/*              AND loadtag.i-no      EQ w-fg-rctd.i-no                                                                       */
/*              AND loadtag.job-no    EQ w-fg-rctd.job-no                                                                     */
/*            USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.                                                                          */
/*        IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').                                                     */
/*                                                                                                                            */
/*        IF AVAIL loadtag THEN DO:                                                                                           */
/*          FIND FIRST fg-bin                                                                                                 */
/*              WHERE fg-bin.company EQ g_company                                                                             */
/*                AND fg-bin.i-no    EQ loadtag.i-no                                                                          */
/*                AND fg-bin.tag     EQ loadtag.tag-no                                                                        */
/*              /*AND fg-bin.job-no = loadtag.job-no                                                                          */
/*                AND fg-bin.job-no2 = loadtag.job-no2*/                                                                      */
/*                AND fg-bin.qty     GT 0                                                                                     */
/*              USE-INDEX tag NO-LOCK NO-ERROR.                                                                               */
/*          IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/                                          */
/*             TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/*/
/*            ASSIGN                                                                                                          */
/*             loadtag.loc          = w-fg-rctd.loc2                                                                          */
/*             loadtag.loc-bin      = w-fg-rctd.loc-bin2                                                                      */
/*             loadtag.qty          = fg-bin.qty                                                                              */
/*             loadtag.pallet-count = fg-bin.qty                                                                              */
/*             loadtag.partial      = fg-bin.partial-count                                                                    */
/*             loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.                                     */
/*          ELSE /*partial transfer */                                                                                        */
/*            ASSIGN                                                                                                          */
/*             loadtag.loc     = w-fg-rctd.loc                                                                                */
/*             loadtag.loc-bin = w-fg-rctd.loc-bin.                                                                           */
/*                                                                                                                            */
/*          FIND CURRENT loadtag NO-LOCK NO-ERROR.                                                                            */
/*        END.                                                                                                                */
/*      END.                                                                                                                  */
/*    END.                                                                                                                    */
/*                                                                                                                            */
/*    FOR EACH w-inv:                                                                                                         */
/*      DELETE w-inv.                                                                                                         */
/*    END.                                                                                                                    */
/*                                                                                                                            */
/*    IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').                                        */
/*    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,                                                                            */
/*        FIRST itemfg                                                                                                        */
/*        WHERE itemfg.company EQ cocode                                                                                      */
/*          AND itemfg.i-no    EQ w-fg-rctd.i-no                                                                              */
/*        NO-LOCK:                                                                                                            */
/*                                                                                                                            */
/*      CREATE w-inv.                                                                                                         */
/*      w-inv.row-id = w-fg-rctd.row-id.                                                                                      */
/*    END.                                                                                                                    */
/*    IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').                                                      */
/*                                                                                                                            */
/*    IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').                                                            */
/*    RUN fg/invrecpt.p (?, 2).                                                                                               */
/*    IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').                                                              */
/*                                                                                                                            */
/*    IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').                                         */
/*    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",                                                                     */
/*        FIRST itemfg                                                                                                        */
/*        WHERE itemfg.company EQ cocode                                                                                      */
/*          AND itemfg.i-no    EQ w-fg-rctd.i-no                                                                              */
/*        NO-LOCK                                                                                                             */
/*        BREAK BY w-fg-rctd.i-no:                                                                                            */
/*                                                                                                                            */
/*      IF LAST-OF(w-fg-rctd.i-no) THEN DO:                                                                                   */
/*        IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).                                  */
/*        RUN fg/updfgcs1.p (RECID(itemfg), NO).                                                                              */
/*        IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).                                    */
/*                                                                                                                            */
/*        FOR EACH oe-ordl                                                                                                    */
/*            WHERE oe-ordl.company EQ cocode                                                                                 */
/*              AND oe-ordl.opened  EQ YES                                                                                    */
/*              AND oe-ordl.i-no    EQ w-fg-rctd.i-no                                                                         */
/*              AND oe-ordl.job-no  EQ ""                                                                                     */
/*              AND oe-ordl.cost    EQ 0                                                                                      */
/*            USE-INDEX opened NO-LOCK                                                                                        */
/*            BREAK BY oe-ordl.ord-no                                                                                         */
/*            TRANSACTION :                                                                                                   */
/*                                                                                                                            */
/*          DO i = 1 TO 1000:                                                                                                 */
/*            FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.                             */
/*            IF AVAIL b-oe-ordl THEN DO:                                                                                     */
/*              IF itemfg.prod-uom EQ "M" THEN                                                                                */
/*                b-oe-ordl.cost = itemfg.total-std-cost.                                                                     */
/*              ELSE                                                                                                          */
/*                RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0                                         */
/*                                        THEN "EA" ELSE itemfg.prod-uom),                                                    */
/*                                       "M", 0, 0, 0, 0,                                                                     */
/*                                       itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).                                       */
/*              LEAVE.                                                                                                        */
/*            END.                                                                                                            */
/*          END.                                                                                                              */
/*        END.                                                                                                                */
/*      END.                                                                                                                  */
/*    END.                                                                                                                    */
/*    IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').                                                       */
/*                                                                                                                            */
/*    IF v-fgpostgl NE "None" THEN DO TRANSACTION:                                                                            */
/*      /* gdm - 11050906 */                                                                                                  */
/*      REPEAT:                                                                                                               */
/*       FIND FIRST gl-ctrl EXCLUSIVE-LOCK                                                                                    */
/*         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.                                                                  */
/*       IF AVAIL gl-ctrl THEN DO:                                                                                            */
/*         ASSIGN v-trnum       = gl-ctrl.trnum + 1                                                                           */
/*                gl-ctrl.trnum = v-trnum.                                                                                    */
/*                                                                                                                            */
/*         FIND CURRENT gl-ctrl NO-LOCK.                                                                                      */
/*                                                                                                                            */
/*         IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').                                                      */
/*         RUN gl-from-work (1, v-trnum).                                                                                     */
/*         IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').                                              */
/*         RUN gl-from-work (2, v-trnum).                                                                                     */
/*         IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').                                                        */
/*                                                                                                                            */
/*         LEAVE.                                                                                                             */
/*        END. /* IF AVAIL gl-ctrl */                                                                                         */
/*      END. /* REPEAT */                                                                                                     */
/*      /* gdm - 11050906 */                                                                                                  */
/*    END.                                                                                                                    */
/*    FIND FIRST w-job NO-ERROR.                                                                                              */
/*    IF AVAIL w-job THEN DO:                                                                                                 */
/*      IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').                                                              */
/*      RUN jc/d-jclose.w.                                                                                                    */
/*      IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').                                                                */
/*    END.                                                                                                                    */
/*                                                                                                                            */
/*    IF v-adjustgl THEN DO TRANSACTION:                                                                                      */
/*      /** GET next G/L TRANS. POSTING # **/                                                                                 */
/*      FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.                                                    */
/*      ASSIGN                                                                                                                */
/*       v-trnum       = gl-ctrl.trnum + 1                                                                                    */
/*       gl-ctrl.trnum = v-trnum.                                                                                             */
/*      FIND CURRENT gl-ctrl NO-LOCK.                                                                                         */
/*      IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').                                                          */
/*      FOR EACH work-job BREAK BY work-job.actnum:                                                                           */
/*         CREATE gltrans.                                                                                                    */
/*        ASSIGN                                                                                                              */
/*         gltrans.company = cocode                                                                                           */
/*         gltrans.actnum  = work-job.actnum                                                                                  */
/*         gltrans.jrnl    = "ADJUST"                                                                                         */
/*         gltrans.tr-date = v-post-date                                                                                      */
/*         gltrans.period  = period.pnum                                                                                      */
/*         gltrans.trnum   = v-trnum.                                                                                         */
/*                                                                                                                            */
/*        IF work-job.fg THEN                                                                                                 */
/*          ASSIGN                                                                                                            */
/*           gltrans.tr-amt  = - work-job.amt                                                                                 */
/*           gltrans.tr-dscr = "ADJUSTMENT FG".                                                                               */
/*        ELSE                                                                                                                */
/*          ASSIGN                                                                                                            */
/*           gltrans.tr-amt  = work-job.amt                                                                                   */
/*           gltrans.tr-dscr = "ADJUSTMENT COGS".                                                                             */
/*                                                                                                                            */
/*        RELEASE gltrans.                                                                                                    */
/*      END. /* each work-job */                                                                                              */
/*      IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').                                                            */
/*    END.                                                                                                                    */
/*    IF v-got-fgemail THEN DO:                                                                                               */
/*      IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').                                                           */
/*      RUN send-fgemail (v-fgemail-file).                                                                                    */
/*      IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').                                                             */
/*    END.                                                                                                                    */
/*    IF fgPostLog THEN RUN fgPostLog ('End').                                                                                */
    
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    SESSION:SET-WAIT-STATE ("").
  
  RUN local-open-query.
    
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
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR browse-order-num AS INT NO-UNDO.

  RUN local-open-query.
  
  browse-order-num = INT(browse-order:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  IF lv-do-what <> "Delete" THEN
  CASE browse-order-num:

     WHEN 1 THEN
        FOR LAST b-fg-rctd WHERE
            b-fg-rctd.company EQ cocode AND
            b-fg-rctd.r-no GE lv-frst-rno AND
            INDEX("RE",b-fg-rctd.rita-code) > 0 AND
            b-fg-rctd.t-qty >= 0
            USE-INDEX fg-rctd NO-LOCK
            BY b-fg-rctd.r-no:
        END.
     WHEN 2 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty >= 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.tag:
         END.
      WHEN 3 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty >= 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.rct-date:
         END.
      WHEN 4 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty >= 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.po-no:
         END.
      WHEN 5 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty >= 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.i-no:
         END.
      WHEN 6 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty >= 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.job-no:
         END.
  END CASE.
  ELSE /*lv-do-what eq "Delete"*/
  CASE browse-order-num:

     WHEN 1 THEN
        FOR LAST b-fg-rctd WHERE
            b-fg-rctd.company EQ cocode AND
            b-fg-rctd.r-no GE lv-frst-rno AND
            INDEX("RE",b-fg-rctd.rita-code) > 0 AND
            b-fg-rctd.t-qty < 0
            USE-INDEX fg-rctd NO-LOCK
            BY b-fg-rctd.r-no:
        END.
     WHEN 2 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty < 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.tag:
         END.
      WHEN 3 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty < 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.rct-date:
         END.
      WHEN 4 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty < 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.po-no:
         END.
      WHEN 5 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty < 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.i-no:
         END.
      WHEN 6 THEN
         FOR LAST b-fg-rctd WHERE
             b-fg-rctd.company EQ cocode AND
             b-fg-rctd.r-no GE lv-frst-rno AND
             INDEX("RE",b-fg-rctd.rita-code) > 0 AND
             b-fg-rctd.t-qty < 0
             USE-INDEX fg-rctd NO-LOCK
             BY b-fg-rctd.job-no:
         END.
  END CASE.

  IF AVAILABLE b-fg-rctd THEN
     REPOSITION browser-table TO ROWID ROWID(b-fg-rctd) NO-ERROR.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN auto-add IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-fgemail B-table-Win 
PROCEDURE send-fgemail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-fgemail-file AS cha NO-UNDO.

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR NO-UNDO.

  FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2] + "\".
  ELSE
     v-dir = "c:\tmp\".

   FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = g_company
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "JOB#       FG Item#              Qty    " SKIP
                              "========== =============== ============ " SKIP.
       END.
       PUT STREAM st-email UNFORMATTED
                 tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN DO:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Receipts have been posted"
                    lv-mailbody = "Finished Goods Receipts have been posted"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-query B-table-Win 
PROCEDURE set-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT ll-set-parts THEN DO:
    RUN get-link-handle IN adm-broker-hdl
                         (THIS-PROCEDURE,'linker-target':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
      RUN dispatch IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("open-query").
  END.

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
         STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}) - ld).

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
 DEF VAR v-tag-seq AS INT NO-UNDO.
  DEF VAR v-locode AS cha NO-UNDO.
  DEF BUFFER xfg-rctd FOR fg-rctd.
  
  ASSIGN v-tag-seq = 0
         v-locode  = "".

  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    FIND FIRST xfg-rctd
        WHERE xfg-rctd.company EQ cocode
          AND xfg-rctd.loc     GT v-locode
        NO-LOCK NO-ERROR.

    IF AVAIL xfg-rctd THEN DO:
      v-locode = xfg-rctd.loc.

      FOR EACH xfg-rctd WHERE xfg-rctd.company EQ cocode
            AND xfg-rctd.loc     EQ v-locode
            AND xfg-rctd.tag     BEGINS string(int(fg-rctd.po-no:screen-value IN BROWSE {&browse-name}),"999999")
            USE-INDEX tag NO-LOCK
            BY xfg-rctd.tag DESC:

           IF int(substr(xfg-rctd.tag,7,2)) GT v-tag-seq THEN
           v-tag-seq = int(substr(xfg-rctd.tag,7,2)).
            LEAVE.
      END.
    END.

    ELSE LEAVE.
  END.  /* do while */
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
  ASSIGN
   v-tag-seq   = v-tag-seq + 1.
/*   fg-rctd.tag:screen-value in browse {&browse-name}
          = string(int(fg-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
*/          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-delete-tag B-table-Win 
PROCEDURE valid-delete-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

 FIND FIRST loadtag WHERE loadtag.company = cocode
                      AND loadtag.item-type = NO
                      AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
 IF AVAIL loadtag THEN
    FIND FIRST fg-bin WHERE fg-bin.company = g_company
                        AND fg-bin.i-no = loadtag.i-no
                        AND fg-bin.tag = loadtag.tag-no
                        AND fg-bin.qty > 0
                        NO-LOCK NO-ERROR.

 IF NOT AVAIL fg-bin THEN DO:
    MESSAGE "No Inventory On Hand Exists, Item cannot be deleted." 
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO fg-rctd.tag .
    op-error = YES.
    LEAVE.
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
  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

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
        APPLY "entry" TO fg-rctd.po-no IN BROWSE {&browse-name}.
        op-error = YES.
        LEAVE.
      END.
    END.

    ELSE DO:
      IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        MESSAGE "You may only enter a Job or a PO, Job No will be erased..."
            VIEW-AS ALERT-BOX ERROR.
      END.

      IF NOT CAN-FIND(FIRST job-hdr WHERE
         job-hdr.company EQ cocode AND
         job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
            op-error = YES.
            LEAVE.
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

  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
                                                         
  DO WITH FRAME {&frame-name}:
    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FOR EACH job-hdr
          WHERE job-hdr.company EQ cocode
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
          WHERE job.company EQ cocode
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

        /* gdm - */
        IF jobreopn-log EQ YES 
          THEN
           MESSAGE 
              "Job is CLOSED, would you like to reopen?"
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
          ELSE 
           ASSIGN lv-ans = jobreopn-log.
        /* gdm - */

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
      op-error = YES.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lot# B-table-Win 
PROCEDURE valid-lot# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DEF OUTPUT PARAM op-error AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""  THEN DO:
        MESSAGE TRIM(ip-focus:LABEL) + " may not be entered when tag# is blank".
        APPLY "entry" TO ip-focus.
        op-error = YES.
        LEAVE.
      END.
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
  DEF OUTPUT PARAM op-error AS LOG NO-UNDO.

  DO WITH FRAME {&frame-name}:
    IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        MESSAGE "You may only enter a Job or a PO, PO will be erased..." VIEW-AS ALERT-BOX ERROR.
        fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        RETURN.
      END.

      IF NOT CAN-FIND(FIRST po-ordl
          WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ NO
            AND (po-ordl.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                 fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")) THEN DO:
        IF ip-type NE 0 THEN DO:
          MESSAGE "Invalid PO#, try help..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.po-no IN BROWSE {&browse-name}.
        END.
        op-error = YES.
        LEAVE.
      END.
    END.
    ELSE DO:
      IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
        IF fgrecpt                                                AND
           fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
           fg-rctd.rita-code NE "E"                                  THEN DO:
          MESSAGE "You must enter a Job or a PO..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
          op-error = YES.
          LEAVE.
        END.
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
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.
  DEF OUTPUT PARAM op-error AS LOG NO-UNDO.
 
  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DEF VAR llValid AS LOG NO-UNDO.
  DEF VAR lcJobNo AS CHAR NO-UNDO.
  DEF VAR lcJobNo2 AS CHAR NO-UNDO.
  DEF VAR lcLoc AS CHAR NO-UNDO.
  DEF VAR lcLocBin AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF TRUE /* 08211410 ip-focus:SCREEN-VALUE NE "" */ THEN DO:
       IF ip-focus:SCREEN-VALUE EQ "" THEN 
         lv-msg = "Tag# number cannot be blank, please re-enter".
       ELSE DO:
       
         IF lv-do-what NE "Delete" AND NOT ll-set-parts THEN
         DO:
            IF (CAN-FIND(FIRST b-fg-rctd 
                           WHERE b-fg-rctd.company     EQ cocode 
                             AND b-fg-rctd.tag         EQ ip-focus:SCREEN-VALUE 
                             AND b-fg-rctd.rita-code   NE "P"
                             AND RECID(b-fg-rctd)      NE RECID(fg-rctd)) OR
             CAN-FIND(FIRST b-fg-rdtlh
                      WHERE b-fg-rdtlh.company   EQ cocode
                        AND b-fg-rdtlh.tag       EQ ip-focus:SCREEN-VALUE
                        AND b-fg-rdtlh.qty       GT 0
                        AND b-fg-rdtlh.rita-code NE "S")) THEN
             lv-msg = "Tag# has already been used, please re-enter".
        
         END. /*lv-do-what NE "Delete"*/
         ELSE IF NOT ll-set-parts THEN
         DO:
            IF CAN-FIND(FIRST b-fg-rctd 
                        WHERE b-fg-rctd.company     EQ cocode 
                          AND b-fg-rctd.tag         EQ ip-focus:SCREEN-VALUE 
                          AND b-fg-rctd.rita-code NE "P" 
                          AND RECID(b-fg-rctd)      NE RECID(fg-rctd)) THEN
               lv-msg = "Tag# has already been used, please re-enter".
         END. /*lv-do-what eq "Delete"*/

       END. /* If non-blank tag # */
      


      /* check for assembled and unassembled set parts on-hand or pending receipt*/
       IF lv-msg EQ "" AND ll-set-parts THEN 
          RUN fg/ValidFGRcptTagSP.p (INPUT ROWID(fg-rctd),
                                     INPUT ip-focus:SCREEN-VALUE,
                                     INPUT INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                                     INPUT cocode,
                                     INPUT NO,
                                     OUTPUT llValid,
                                     OUTPUT lv-msg,
                                     OUTPUT lcJobNo,
                                     OUTPUT lcJobNo2,
                                     OUTPUT lcLoc,
                                     OUTPUT lcLocBin
                                    ).
          IF llValid AND (adm-new-record OR ip-focus:MODIFIED) THEN
            ASSIGN 
                fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcJobNo
                fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcJobNo2
                fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcLoc
                fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcLocBin.
/*       IF lv-msg EQ "" AND ll-set-parts                                                             */
/*           AND int(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-NAME}) < 0 THEN DO:                */
/*             iTotalQty = 0.                                                                         */
/*         FOR EACH b-fg-rctd                                                                         */
/*             WHERE b-fg-rctd.company EQ cocode                                                      */
/*                  AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE                                    */
/*                  AND RECID(b-fg-rctd) NE RECID(fg-rctd) NO-LOCK:                                   */
/*             iTotalQty = iTotalQty + b-fg-rctd.t-qty.                                               */
/*         END.                                                                                       */
/*         FIND FIRST bf-fg-bin                                                                       */
/*             WHERE bf-fg-bin.company EQ cocode                                                      */
/*                 AND bf-fg-bin.tag   EQ ip-focus:SCREEN-VALUE NO-LOCK NO-ERROR.                     */
/*         IF AVAIL bf-fg-bin THEN                                                                    */
/*             ASSIGN                                                                                 */
/*                 iTotalQty = iTotalQty + bf-fg-bin.qty                                              */
/*                 fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-NAME} = bf-fg-bin.job-no            */
/*                 fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-NAME} = string(bf-fg-bin.job-no2). */
/*                                                                                                    */
/*         ELSE                                                                                       */
/*             lv-msg = "Invalid Tag#, try help or scan valid tag#".                                  */
/*         IF lv-msg EQ ""                                                                            */
/*             AND iTotalQty LT ABS(int(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-NAME})) THEN    */
/*             lv-msg = "Insufficient quantity in bin".                                               */
/*       END.                                                                                         */

      IF lv-msg EQ ""                                                   AND
         fgrecpt-int EQ 1                                               AND
         NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company   EQ cocode
                        AND loadtag.item-type EQ NO
                        AND loadtag.tag-no    EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = "Invalid Tag#, try help or scan valid tag#".

      IF lv-msg NE "" THEN DO:
         MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ip-focus.
         op-error = YES.
         LEAVE.
      END.
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

  DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.

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
      op-error = YES.
      LEAVE.
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
  DEF BUFFER bf-loadtag FOR loadtag.

  DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.

  FIND FIRST itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        op-error = YES.
        LEAVE.
     END.
     ELSE DO:
        MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no.
            op-error = YES.
            LEAVE.
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND FIRST itemfg {sys/look/itemfgrlW.i}
                       AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
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
            
    RUN fg/checksetb.p (ROWID(itemfg),
                       ROWID(fg-rctd),
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                       INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                       INPUT fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                       INPUT-OUTPUT li-max-qty).

    IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
      ll = NO.
      RUN fg/cmpQtyMsg.w (ROWID(itemfg),
                         ROWID(fg-rctd),
                         fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                         OUTPUT ll).      
       
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
        op-error = YES.
        LEAVE.
      END.
    END.
  END.
  
  IF NOT CAN-FIND(FIRST loc WHERE
     loc.company = cocode AND
     loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
     MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fg-rctd.loc.
     op-error = YES.
     LEAVE.
  END.
    IF NOT CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE
                  AND itemfg.prod-uom NE "") THEN DO:
    
       MESSAGE fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} + " has no cost UOM. Please correct the item master and try again."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.i-no.
       op-error = YES.
     LEAVE.
    END.   
  IF NOT CAN-FIND(FIRST fg-bin WHERE fg-bin.company = cocode 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino) THEN DO:
     MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fg-rctd.loc-bin.
     op-error = YES.
     LEAVE.
  END.
  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN
  DO:
  
    IF CAN-FIND(FIRST b-fg-rctd 
                WHERE b-fg-rctd.company = cocode 
                  AND b-fg-rctd.tag = fg-rctd.tag:SCREEN-VALUE
                  AND b-fg-rctd.rita-code <> "P"
                  AND RECID(b-fg-rctd) <> RECID(fg-rctd)) THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." SKIP
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.tag.
       op-error = YES.
       LEAVE.
    END.
    ELSE DO:
        IF lv-do-what <> "Delete" THEN
        IF CAN-FIND(FIRST b-fg-rdtlh
               WHERE b-fg-rdtlh.company   EQ cocode
                 AND b-fg-rdtlh.loc       EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                 AND b-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE
                 AND b-fg-rdtlh.qty       GT 0
                 AND b-fg-rdtlh.rita-code NE "S"
               USE-INDEX tag) THEN DO:
               MESSAGE "This Tag Number Has Already Been Used." SKIP
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               APPLY "entry" TO fg-rctd.tag.
               op-error = YES.
               LEAVE.
           END.
    END.
        /*validate the i-no matches the loadtag i-no*/
    FIND FIRST bf-loadtag WHERE bf-loadtag.company EQ g_company
        AND bf-loadtag.item-type EQ NO
        AND bf-loadtag.tag-no EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL bf-loadtag AND bf-loadtag.i-no NE fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
    
       MESSAGE "Item number does not match loadtag item number" SKIP
            "Please rescan and validate Item# field."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.tag.
       op-error = YES.
       LEAVE.
    END.
  END.

   RUN get-matrix (NO).

   RUN get-matrix-all (FALSE).
   
   IF INT(fg-rctd.cases-unit:SCREEN-VALUE) < 1 THEN DO:  /* task# 06200520*/
      MESSAGE "Unit/Pallet must be greater than or equal to 1." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.cases-unit .
      op-error = YES.
      LEAVE.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

