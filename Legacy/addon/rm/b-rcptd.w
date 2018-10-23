&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon\rm\b-rcptd.w

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

/* gdm - 09190805 */
&SCOPED-DEFINE yellowColumnsName b-rcptd

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED} 

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE BUFFER b-po-ord FOR po-ord.
DEFINE BUFFER b-company FOR company.
def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
DEF VAR lv-rowid AS ROWID NO-UNDO.
def var ll-qty-valid as LOG no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR lv-po-len LIKE po-ordl.s-len FORM ">>,>>9.9999" NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR ll-tag-meth AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR v-ssrmscan AS LOG NO-UNDO.
DEF VAR v-case-tag AS LOG NO-UNDO.
DEF VAR v-bin AS CHAR NO-UNDO.
DEF VAR ll-add-setup AS LOG NO-UNDO.
DEF VAR lv-save-fld AS CHAR EXTENT 2 NO-UNDO.
DEF VAR lv-setup AS DEC NO-UNDO.
DEF VAR v-new-mode AS LOG NO-UNDO.
DEF VAR lv-entry-qty AS DEC NO-UNDO.
DEF VAR lv-entry-qty-uom AS CHAR NO-UNDO.

{windows/l-poitmw.i NEW}

DEF BUFFER br-tmp FOR rm-rctd.  /* for tag validation */
DEF BUFFER xrm-rdtlh FOR rm-rdtlh. /* for tag validation */

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ g_company
                        AND sys-ctrl.name  EQ "SSRMSCAN" NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
   CREATE sys-ctrl.
   ASSIGN sys-ctrl.company = g_company
          sys-ctrl.NAME = "SSRMSCAN"
          sys-ctrl.descrip = "Prompt for the Warehouse/Bin?"
          sys-ctrl.log-fld = YES.
END.
v-ssrmscan = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE YES.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "RMWHSBIN"
   sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
   sys-ctrl.char-fld = "RMITEM".
  FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
  {sys/inc/rmunderover.i}
END.

DEFINE VARIABLE lv-do-what AS CHARACTER NO-UNDO.

DEFINE BUFFER bpo-ordl FOR po-ordl.

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
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.s-num rm-rctd.i-no rm-rctd.i-name rm-rctd.qty ~
rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom calc-ext-cost() @ ext-cost ~
display-dimension('W') @ lv-po-wid display-dimension('L') @ lv-po-len ~
rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.i-no rm-rctd.qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ "R" ~
AND rm-rctd.tag NE "" ~
AND ((lv-do-what EQ "Delete" AND rm-rctd.qty LT 0) ~
OR (lv-do-what NE "Delete" AND rm-rctd.qty GE 0)) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ "R" ~
AND rm-rctd.tag NE "" ~
AND ((lv-do-what EQ "Delete" AND rm-rctd.qty LT 0) ~
OR (lv-do-what NE "Delete" AND rm-rctd.qty GE 0)) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin B-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char )  FORWARD.

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
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 74 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(13)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.rct-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U WIDTH 9 LABEL-BGCOLOR 14
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->,>>>,>>9.9<<":U
            WIDTH 20 LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
  ENABLE
      rm-rctd.tag
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.i-no
      rm-rctd.qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 16.71 COL 93 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
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
         HEIGHT             = 17.14
         WIDTH              = 146.
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
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company EQ cocode
AND rm-rctd.rita-code EQ ""R""
AND rm-rctd.tag NE """"
AND ((lv-do-what EQ ""Delete"" AND rm-rctd.qty LT 0)
OR (lv-do-what NE ""Delete"" AND rm-rctd.qty GE 0))"
     _FldNameList[1]   > asi.rm-rctd.r-no
"rm-rctd.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" "x(13)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.po-no
"rm-rctd.po-no" ? "x(6)" "character" ? ? ? 14 ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.job-no
"rm-rctd.job-no" "Job" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = asi.rm-rctd.job-no2
     _FldNameList[9]   > asi.rm-rctd.s-num
"rm-rctd.s-num" "S" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.i-no
"rm-rctd.i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.i-name
"rm-rctd.i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" "->,>>>,>>9.9<<" "decimal" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "UOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.cost
"rm-rctd.cost" "Cost" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost-uom
"rm-rctd.cost-uom" "UOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > asi.rm-rctd.user-id
"rm-rctd.user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 DEF VAR help-recid AS RECID NO-UNDO.
 DEF VAR help-rowid AS ROWID NO-UNDO.

 ll-help-run = yes.

 case focus:name :
      when "po-no" then do:
           run windows/l-poordl.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign rm-rctd.po-no:screen-value in browse {&BROWSE-NAME} = entry(1,char-val)
                     rm-rctd.i-no:screen-value in browse {&BROWSE-NAME} = entry(2,char-val)
                     rm-rctd.i-name:screen-value in browse {&BROWSE-NAME} = entry(3,char-val)
                     rm-rctd.job-no:screen-value in browse {&BROWSE-NAME} = entry(4,char-val)
                     rm-rctd.job-no2:screen-value in browse {&BROWSE-NAME} = entry(5,char-val)
                     .
             find po-ordl where po-ordl.company = rm-rctd.company and
                                po-ordl.po-no = integer(entry(1,char-val)) and
                                po-ordl.line = integer(entry(6,char-val))
                                no-lock no-error.
             if avail po-ordl then RUN update-from-po-line.

             else do:
                find first item where item.company = rm-rctd.company and
                                      item.i-no = entry(2,char-val)
                                      no-lock no-error.
                assign rm-rctd.pur-uom:screen-value in browse {&BROWSE-NAME} = item.cons-uom
                       rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME} = item.cons-uom.
             end.
             if not avail item then find first item where item.company = rm-rctd.company and
                                                          item.i-no = entry(2,char-val)
                                      no-lock no-error.
             
             assign rm-rctd.loc:screen-value in browse {&BROWSE-NAME} =  item.loc
                    rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} =  item.loc-bin
                    .
             if rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} eq "" then do:
                  find first sys-ctrl where sys-ctrl.company eq rm-rctd.company
                                          and sys-ctrl.name    eq "AUTOISSU"
                                no-lock no-error.
                  if not avail sys-ctrl then do:
                        create sys-ctrl.
                        assign sys-ctrl.company = rm-rctd.company
                                   sys-ctrl.name    = "AUTOISSU"
                                   sys-ctrl.descrip = "Automatically Issue RM Receipts to asi"
                                   sys-ctrl.log-fld = yes.
                        message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                           update sys-ctrl.char-fld.
                  end.
                  assign rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} = sys-ctrl.char-fld.
             end.
             run tag-method (output ll-tag#).
             if ll-tag# and rm-rctd.po-no:screen-value in browse {&BROWSE-NAME} <> ""
             then do:
                 run tag-sequence.
             end.
             ext-cost = 0.
             disp ext-cost with browse {&BROWSE-NAME}.
           end.  /* char-val <> "" */
           return no-apply.   
     end.
     when "i-no" then do:
         IF DEC(rm-rctd.po-no:SCREEN-VALUE) NE 0 THEN DO:
           RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   YES, FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   ROWID(rm-rctd), OUTPUT help-rowid).
           FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
           IF AVAIL po-ordl THEN DO:
              ASSIGN 
               FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}           = po-ordl.i-no
               rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.i-name
               rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.job-no
               rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(po-ordl.job-no2).
              RUN update-from-po-line.
           END.
         END.

         ELSE
         IF rm-rctd.job-no:SCREEN-VALUE NE "" THEN DO:
                RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                        rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
         END.

         ELSE DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
            run windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val,OUTPUT help-recid).
            if char-val <> "" then do :
               ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                      rm-rctd.i-name:SCREEN-VALUE = ENTRY(2,char-val).
               RUN display-item(help-recid).
            END.
         END.
         return no-apply.   
     end.
     when "s-num" then do:
         IF rm-rctd.po-no:SCREEN-VALUE NE "" THEN DO:
           RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   ROWID(rm-rctd), OUTPUT help-rowid).
           FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
           IF AVAIL po-ordl THEN DO:
              ASSIGN 
               rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = po-ordl.i-no
               rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.i-name
               rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.job-no
               rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(po-ordl.job-no2).
              RUN update-from-po-line.
           END.
         END.
         return no-apply.   
     end.
     when "job-no" or when "job-no2" then do:
         IF DEC(rm-rctd.po-no:SCREEN-VALUE) EQ 0 THEN DO:
           run windows/l-jobno.w (rm-rctd.company,rm-rctd.job-no:screen-value, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val).
           end.
         END.
         ELSE DO:
           run windows/l-pojob.w (rm-rctd.company,rm-rctd.po-no:screen-value,rm-rctd.i-no:screen-value, output char-val).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val).
           end.
         END.
         return no-apply.   
     end.  
     when "loc" then do:
           run rm/l-loc.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&BROWSE-NAME}  = entry(1,char-val)
                     /*rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME} = entry(2,char-val) */
                     .
             
           end.
           return no-apply.   
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val)
                     rm-rctd.loc:screen-value = entry(2,char-val)
                    /* rm-rctd.qty:screen-value = entry(3,char-val)
                     rm-rctd.tag:screen-value = entry(4,char-val)*/
                     .
             
           end.
           return no-apply.   
     end.
   WHEN "tag" THEN DO:
         IF lv-do-what EQ 'Delete' THEN
         run windows/l-ldtag7.w (g_company,yes,focus:screen-value,output char-val,OUTPUT HELP-recid).
         ELSE
         run windows/l-ldtag.w (g_company,yes,focus:screen-value,output char-val,OUTPUT HELP-recid).
         if char-val <> "" then do :
            tag:SCREEN-VALUE = ENTRY(1,char-val).
            /*  ===*/
            FIND FIRST br-tmp WHERE br-tmp.company = g_company AND
                          br-tmp.tag = SELF:SCREEN-VALUE
                      AND ((lv-do-what EQ "delete" AND br-tmp.rita-code NE "P") OR lv-do-what NE "delete" )
                      AND RECID(br-tmp) <> RECID(rm-rctd)
                      NO-LOCK NO-ERROR.
            IF AVAIL br-tmp THEN DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               IF lv-do-what <> "Delete" THEN
               find first xrm-rdtlh where xrm-rdtlh.company   eq g_company
                    and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                    and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                    and xrm-rdtlh.qty       gt 0
                    and xrm-rdtlh.rita-code ne "S" 
                    use-index tag no-lock no-error.
               if avail xrm-rdtlh THEN  DO:
                  MESSAGE "This Tag Number Has Already Been Used." skip
                          "Please Enter A Unique Tag Number." 
                          VIEW-AS ALERT-BOX ERROR.
                  RETURN NO-APPLY.
               END.
            END.
            {addon/loadtags/disptagr.i "RMItem" rm-rctd.tag:SCREEN-VALUE}
            FIND FIRST bpo-ordl NO-LOCK
                 WHERE bpo-ordl.company EQ g_company
                   AND bpo-ordl.po-no EQ loadtag.po-no
                   AND bpo-ordl.job-no EQ loadtag.job-no
                   AND bpo-ordl.job-no2 EQ loadtag.job-no2
                   AND bpo-ordl.i-no EQ loadtag.i-no NO-ERROR.
            IF AVAILABLE bpo-ordl THEN
            rm-rctd.s-num:SCREEN-VALUE = STRING(bpo-ordl.s-num).
            IF lv-do-what EQ 'Delete' THEN
            rm-rctd.qty:SCREEN-VALUE = STRING(DEC(rm-rctd.qty:SCREEN-VALUE) * -1).
            RETURN NO-APPLY.
         END.
     END.
   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
 /*  {src/adm/template/brsleave.i}  */
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


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  /*
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF NOT v-ssrmscan THEN do:
          APPLY "row-leave" TO BROWSE {&browse-name}.
          RETURN NO-APPLY.
    END.
  END.
  */

    IF LASTKEY = -1 THEN RETURN.

    FIND FIRST br-tmp WHERE br-tmp.company = g_company 
                        AND br-tmp.tag = SELF:SCREEN-VALUE
                        AND SELF:SCREEN-VALUE <> ""
                        AND br-tmp.rita-code <> "P"
                        AND RECID(br-tmp) <> RECID(rm-rctd)
                        NO-LOCK NO-ERROR.
    IF AVAIL br-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
           VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        IF lv-do-what <> "Delete" THEN DO:
          find first xrm-rdtlh
                 where xrm-rdtlh.company   eq g_company
                   and xrm-rdtlh.loc       eq rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                   and xrm-rdtlh.tag       eq rm-rctd.tag:SCREEN-VALUE
                   AND xrm-rdtlh.tag <> ""
                   and xrm-rdtlh.qty       gt 0
                   and xrm-rdtlh.rita-code ne "S"
                 use-index tag no-lock no-error.
          if avail xrm-rdtlh THEN  DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
          END.
        END.
        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        IF lv-do-what = "delete" THEN DO:
           RUN valid-delete-tag NO-ERROR.
           IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        {addon/loadtags/disptagr.i "RMItem" rm-rctd.tag:SCREEN-VALUE}
      
        IF lv-do-what EQ "Delete" THEN DO:
            FIND FIRST rm-bin
                WHERE rm-bin.company EQ g_company
                  AND rm-bin.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                NO-LOCK NO-ERROR.
            IF AVAIL rm-bin THEN DO:
                ASSIGN            
                    rm-rctd.qty:SCREEN-VALUE = STRING(rm-bin.qty,"->,>>>,>>9.9<<")
                    rm-rctd.loc:SCREEN-VALUE = rm-bin.loc
                    rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin.
            END.
        END.

        FIND FIRST bpo-ordl NO-LOCK
             WHERE bpo-ordl.company EQ g_company
               AND bpo-ordl.po-no EQ loadtag.po-no
               AND bpo-ordl.job-no EQ loadtag.job-no
               AND bpo-ordl.job-no2 EQ loadtag.job-no2
               AND bpo-ordl.i-no EQ loadtag.i-no NO-ERROR.
        IF AVAILABLE bpo-ordl THEN
        rm-rctd.s-num:SCREEN-VALUE = STRING(bpo-ordl.s-num).
        IF lv-do-what EQ 'Delete' THEN
        rm-rctd.qty:SCREEN-VALUE = STRING(DEC(rm-rctd.qty:SCREEN-VALUE) * -1).
        APPLY "leave" TO rm-rctd.i-no IN BROWSE {&browse-name}.
        
        IF NOT v-ssrmscan THEN do:
          APPLY "row-leave" TO BROWSE {&browse-name}.
          RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
     IF LASTKEY = -1 THEN RETURN.

    DEF VAR v-locbin AS cha NO-UNDO.
    IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:

          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 rm-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).

          FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
          IF NOT AVAIL rm-bin THEN DO:
             MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO rm-rctd.loc .
             RETURN NO-APPLY.
          END.

          APPLY "leave" TO SELF.
          APPLY "tab" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
          /*APPLY "row-leave" TO BROWSE {&browse-name}.*/

          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
        FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL loc THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
        END.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  IF LASTKEY = -1 THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST rm-bin WHERE rm-bin.company = g_company
                           AND rm-bin.i-no = ""
                           AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:

      RUN display-po-job.
      IF lv-rowid EQ ? THEN RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(SELF:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  /*
  ll-warned = NO.
  IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
    {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "".
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-s-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
       {&self-name}:MODIFIED                                         THEN DO:
      RUN find-exact-po.

      FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

      IF NOT AVAIL po-ordl THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.i-no      EQ {&self-name}:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        lv-rowid = ROWID(po-ordl).
        RUN display-po-info.
      END.
    END.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  ll-warned = NO.
  FIND ITEM
      WHERE item.company EQ rm-rctd.company
        AND item.i-no    BEGINS rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.             
  IF AVAIL ITEM THEN DO:
    rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "0".
    RUN display-item (RECID(ITEM)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
   lv-entry-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
   IF lv-entry-qty NE DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
      RUN po-cost.

   IF LASTKEY NE -1 THEN DO WITH FRAME {&FRAME-NAME}:
      RUN valid-qty NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
      RUN get-matrix (NO).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
   lv-entry-qty-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF lv-entry-qty-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} THEN
     RUN po-cost.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (NO).

    IF NOT ll-qty-valid THEN DO:
      {rm/chkporun.i}
      ll-qty-valid = YES.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
   lv-save-fld[1] = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
   lv-save-fld[2] = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*ll-add-setup = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                       DEC(lv-save-fld[1]) OR
                   TRIM(rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                       TRIM(lv-save-fld[2]).*/

    RUN get-matrix (NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/tag#.i}
ll-tag-meth = v-tag#.

/* gdm - 09190805 */
{custom/yellowColumns.i}


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr B-table-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.
    
   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ po-ordl.company AND
        b-po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ po-ord.company AND
           vend.vend-no EQ po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ cocode AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
            DO:
               ip-cost = ip-cost * currency.ex-rate.

               RELEASE currency.
            END.
         END.

         RELEASE b-company.
         RELEASE vend.
      END.

      RELEASE b-po-ord.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-neg-rctd B-table-Win 
PROCEDURE create-neg-rctd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM in-recid AS RECID.

DEF BUFFER bf-rm-rctd FOR rm-rctd.

FIND rm-rctd WHERE RECID(rm-rctd) = in-recid NO-LOCK NO-ERROR.
IF AVAIL rm-rctd THEN DO:
    CREATE bf-rm-rctd.
    BUFFER-COPY rm-rctd EXCEPT r-no rec_key
             TO bf-rm-rctd.
    ASSIGN bf-rm-rctd.r-no = rm-rctd.r-no + 1
           bf-rm-rctd.qty = rm-rctd.qty * -1.
END.
RELEASE bf-rm-rctd.

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

DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.


FIND ITEM WHERE RECID(ITEM) EQ ip-recid NO-LOCK NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  IF AVAIL ITEM THEN DO:
    ASSIGN
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = item.i-name
     rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = item.loc
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.loc-bin
     rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom.

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN DO:
      {rm/avgcost.i}

      ASSIGN
       /*rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom */
       rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    =
           IF DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
             IF v-avgcost THEN STRING(item.avg-cost)
             ELSE STRING(item.last-cost)
           ELSE rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom. 
    END.

    ELSE RUN update-from-po-line.
  END.

  IF NOT checkWhsBin(cocode,rm-rctd.loc,rm-rctd.loc-bin) THEN
  DO:
    IF v-bin NE 'RMITEM' THEN
    ASSIGN
      rm-rctd.loc = SUBSTR(v-bin,1,5)
      rm-rctd.loc-bin= SUBSTR(v-bin,6).
    IF NOT checkWhsBin(cocode,rm-rctd.loc,rm-rctd.loc-bin) THEN
    DO:
      FIND FIRST loc NO-LOCK WHERE loc.company EQ cocode NO-ERROR.
      IF AVAILABLE loc THEN
      FIND FIRST rm-bin WHERE rm-bin.company EQ cocode
                          AND rm-bin.loc EQ loc.loc
                          AND rm-bin.i-no EQ '' NO-ERROR.
      ASSIGN
        rm-rctd.loc = IF AVAILABLE loc THEN loc.loc ELSE ''
        rm-rctd.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
    END.
  END.
  
  IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     EQ "" OR
     rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN DO:
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.active  EQ "X" 
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
        ASSIGN   
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = shipto.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = shipto.loc-bin.
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-jobmat B-table-Win 
PROCEDURE display-jobmat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAIL job-mat THEN DO:
      rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = job-mat.i-no.
      FIND FIRST item 
          WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.

      IF AVAIL item THEN RUN display-item (RECID(item)).

      FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

      rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-info B-table-Win 
PROCEDURE display-po-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
  DEF VAR lv-i-no LIKE rm-rctd.i-no NO-UNDO.
  DEF VAR lv-locode like locode.
  DEF VAR li-tag-seq as int.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

  FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      lv-i-no   = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = po-ordl.i-no
      rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = po-ordl.i-name
      rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = po-ordl.job-no
      rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = STRING(po-ordl.job-no2)
      rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = STRING(po-ordl.s-num)
      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = po-ordl.pr-uom
      lv-setup                                               = po-ordl.setup
      lv-cost                                                = po-ordl.cost
      ll-warned = NO.
    
     RUN po-cost.
    
     IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE lv-i-no THEN DO:
       FIND FIRST ITEM
           WHERE item.company EQ rm-rctd.company
             AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
           NO-LOCK NO-ERROR. 
       IF AVAIL ITEM THEN RUN display-item (RECID(ITEM)).
     END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-job B-table-Win 
PROCEDURE display-po-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-po-cnt AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    lv-rowid = ?.
    FOR EACH  po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) 
          AND po-ordl.item-type EQ YES
          AND ((po-ordl.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) AND
                po-ordl.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                    AND
                po-ordl.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) OR
               adm-adding-record)
        NO-LOCK:
      ASSIGN
       li-po-cnt = li-po-cnt + 1
       lv-rowid  = ROWID(po-ordl).
      IF li-po-cnt GT 1 THEN LEAVE.
    END.
 
    IF li-po-cnt GT 1 THEN DO:
      lv-rowid = ?.
      RUN windows/l-poitmw.w (YES, rm-rctd.company, rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                              YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                              ROWID(rm-rctd), OUTPUT lv-rowid).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-exact-po B-table-Win 
PROCEDURE find-exact-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ rm-rctd.company
          AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND po-ordl.item-type EQ YES
        NO-LOCK NO-ERROR.
    lv-rowid = IF AVAIL po-ordl THEN ROWID(po-ordl) ELSE ?.
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
  def var lv-out-qty LIKE rm-rctd.qty no-undo.
  def var lv-out-cost LIKE rm-rctd.cost no-undo.
  DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

  /* gdm - 07210901 */
  DEF VAR v-po-cost LIKE po-ordl.cost NO-UNDO.
  DEF VAR v-po-cuom LIKE lv-cost-uom  NO-UNDO. 
  
if avail rm-rctd then
if ip-first-disp AND
   rm-rctd.i-no ne "" then DO WITH FRAME {&FRAME-NAME}: /* for row-display */
  find item  where item.company eq cocode                           /* no screen-value used */
                     and item.i-no  eq rm-rctd.i-no /*:screen-value in browse {&browse-name}*/
                     use-index i-no no-error.
  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom.

  FIND CURRENT ITEM NO-LOCK.

  assign
   lv-qty-uom  = item.cons-uom
   lv-cost-uom = item.cons-uom
   v-dep       = item.s-dep.

  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rctd.i-no
                       and po-ordl.job-no = rm-rctd.job-no
                       and po-ordl.job-no2 = rm-rctd.job-no2
                       and po-ordl.item-type = yes 
                       and po-ordl.s-num = rm-rctd.s-num
                       and po-ordl.b-num = rm-rctd.b-num
                           no-lock no-error.
  /*if not avail po-ordl then return.  */
  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0.     
     {rm/pol-dims.i}
  end.
  else do:
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no
                         and job.job-no2 eq rm-rctd.job-no2
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company  eq cocode
                                  and job-mat.job      eq job.job
                                  and job-mat.i-no     eq rm-rctd.i-no
                                  and job-mat.frm      eq rm-rctd.s-num
                                  and job-mat.blank-no eq rm-rctd.b-num
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          lv-out-cost   = job-mat.std-cost
                                          lv-cost-uom   = job-mat.sc-uom.
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.    
  end.
  
  /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
 /* run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).
  
  /* convert cost pr-uom*/
  run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                    v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
  */
     /*SYS/REF/ */
  IF rm-rctd.pur-uom EQ lv-qty-uom THEN
    lv-out-qty = rm-rctd.qty.
  ELSE
    run custom/convquom.p (cocode,
                           rm-rctd.pur-uom,
                           lv-qty-uom,
                           v-bwt,
                           v-len,
                           input v-wid,
                           input v-dep,
                           input rm-rctd.qty,
                           output lv-out-qty).

  /* convert cost pr-uom*/
  /*sys/ref/convcuom.p*/
  IF rm-rctd.cost-uom EQ "L" THEN
     lv-out-cost = rm-rctd.cost / lv-out-qty.
  ELSE
  IF rm-rctd.cost-uom EQ lv-cost-uom THEN
     lv-out-cost = rm-rctd.cost.
  ELSE
    RUN custom/convcuom.p (cocode,
                           rm-rctd.cost-uom, lv-cost-uom,                    
                           v-bwt, v-len, v-wid, v-dep,
                           rm-rctd.cost, OUTPUT lv-out-cost).

  IF lv-out-qty  EQ ? THEN lv-out-qty  = 0.
  IF lv-out-cost EQ ? THEN lv-out-cost = 0.

  IF ll-add-setup AND lv-out-qty NE 0 THEN
     lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).

  ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
  
end.

ELSE
if NOT ip-first-disp                                        AND
   rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" then do: /* in update mode - use screen-value */
  find item  where item.company eq cocode
                and item.i-no  eq rm-rctd.i-no:screen-value in browse {&browse-name}
                      use-index i-no no-error.
  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

  FIND CURRENT ITEM NO-LOCK.

  assign
   lv-qty-uom  = item.cons-uom
   lv-cost-uom = item.cons-uom
   v-dep       = item.s-dep.

  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no:screen-value in browse {&browse-name})
                       and po-ordl.i-no  = rm-rctd.i-no:screen-value
                       and po-ordl.job-no = (rm-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(rm-rctd.job-no2:screen-value)
                       and po-ordl.item-type = yes
                       and po-ordl.s-num = integer(rm-rctd.s-num:screen-value)
                       
                           no-lock no-error.
  
  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0

     /* gdm - 07210901 */
            v-po-cost = po-ordl.cost
            v-po-cuom = po-ordl.pr-uom.

     {rm/pol-dims.i}
  end.
  else do:
        ASSIGN lv-qty-uom = item.cons-uom
               lv-cost-uom = ITEM.cons-uom .
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:screen-value
                         and job.job-no2 eq integer(rm-rctd.job-no2:screen-value)
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company  eq cocode
                                  and job-mat.job      eq job.job
                                  and job-mat.i-no     eq rm-rctd.i-no:screen-value
                                  and job-mat.frm      eq integer(rm-rctd.s-num:SCREEN-VALUE)
                   no-lock no-error.
             if avail job-mat then assign v-len         = job-mat.len
                                          v-wid         = job-mat.wid
                                          v-bwt         = job-mat.basis-w
                                          lv-out-cost   = job-mat.std-cost
                                          lv-cost-uom   = job-mat.sc-uom.
        end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.

     /* gdm - 07210901 */
     ASSIGN  v-po-cost = lv-out-cost 
             v-po-cuom = lv-cost-uom .
  end.
    
  /* convert qty */

  IF rm-rctd.pur-uom:screen-value in browse {&browse-name} EQ lv-qty-uom THEN
    lv-out-qty = DEC(rm-rctd.qty:screen-value in browse {&browse-name}).
  ELSE
     /*IF ITEM.mat-type <> "P" THEN*/
       run custom/convquom.p (cocode,
                              rm-rctd.pur-uom:screen-value in browse {&browse-name} ,
                              lv-qty-uom,
                              v-bwt,
                              v-len,
                              input v-wid,
                              input v-dep,
                              DEC(rm-rctd.qty:screen-value in browse {&browse-name}),
                              output lv-out-qty).
     /*ELSE
       run custom/convquom.p (cocode,
                              item.cons-uom ,
                              lv-qty-uom,
                              v-bwt,
                              v-len,
                              input v-wid,
                              input v-dep,
                              DEC(rm-rctd.qty:screen-value in browse {&browse-name}),
                              output lv-out-qty).*/

  /* convert cost */
  IF rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "L" THEN
    lv-out-cost = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) / lv-out-qty .
  ELSE
      /* gdm - 07210901 */
     IF v-po-cuom EQ "L" THEN
        lv-out-cost = DEC(v-po-cost) / lv-out-qty .
     ELSE
        IF rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ lv-cost-uom THEN
          lv-out-cost = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}).
     ELSE
        run custom/convcuom.p (cocode,
                              rm-rctd.cost-uom:screen-value in browse {&browse-name},
                              lv-cost-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              dec(rm-rctd.cost:screen-value in browse {&browse-name}),
                              output lv-out-cost).
 
  /*
  /*add in setup charge*/
  IF lv-setup NE 0 AND lv-out-qty NE 0 AND
    ((TRIM(rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NE
    TRIM(rm-rctd.cost-uom)) OR
    (DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) NE rm-rctd.cost)) THEN DO:

    lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).
  END. */

  IF lv-out-qty  EQ ? THEN lv-out-qty  = 0.
  IF lv-out-cost EQ ? THEN lv-out-cost = 0.

  IF ll-add-setup AND lv-out-qty NE 0 THEN
     lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).

  ASSIGN ext-cost = ROUND(lv-out-qty * lv-out-cost,2)
         rm-rctd.cost:SCREEN-VALUE = STRING(lv-out-cost)
         rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
         rm-rctd.qty:SCREEN-VALUE = STRING(lv-out-qty)
         rm-rctd.pur-uom:SCREEN-VALUE = lv-qty-uom /*
         lv-msf = display-msf-screen()
         lv-adder = display-adder-screen() */.

  /* DISPLAY ext-cost lv-setup lv-msf lv-adder WITH BROWSE {&browse-name}. */
end.

IF ll-add-setup THEN
  ASSIGN
   ll-add-setup   = NO
   lv-save-fld[1] = rm-rctd.cost:SCREEN-VALUE
   lv-save-fld[2] = rm-rctd.cost-uom:SCREEN-VALUE.

/* ======================================================================= */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-remain-qty B-table-Win 
PROCEDURE get-remain-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-remain-qty AS DEC NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.


  FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    FIND FIRST item
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    op-remain-qty = po-ordl.cons-qty.

    RUN windows/l-poitmw.w (NO, po-ordl.company, STRING(po-ordl.po-no),
                            YES, "", ROWID(rm-rctd), OUTPUT lv-rowid).

    FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(po-ordl) NO-ERROR.

    IF AVAIL tt-report THEN op-remain-qty = DEC(tt-report.key-01).

    IF po-ordl.cons-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND AVAIL item THEN
      RUN sys/ref/convquom.p(po-ordl.cons-uom, rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                             item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                             op-remain-qty, OUTPUT op-remain-qty).
  END.

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
  DEFINE OUTPUT PARAMETER op-in-update AS LOGICAL NO-UNDO.

  op-in-update = adm-brs-in-update OR adm-new-record.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win
PROCEDURE local-assign-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
        rm-rctd.enteredBy = USERID("asi")
        rm-rctd.enteredDT = DATETIME(TODAY, MTIME) 
        .

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
  ASSIGN
    rm-rctd.po-no = rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    rm-rctd.rct-date = DATE(rm-rctd.rct-date:SCREEN-VALUE)
    rm-rctd.job-no = rm-rctd.job-no:SCREEN-VALUE
    rm-rctd.job-no2 = INT(rm-rctd.job-no2:SCREEN-VALUE)
    rm-rctd.s-num = INT(rm-rctd.s-num:SCREEN-VALUE)
    rm-rctd.i-no = rm-rctd.i-no:SCREEN-VALUE
    rm-rctd.i-name = rm-rctd.i-name:SCREEN-VALUE
    rm-rctd.pur-uom = rm-rctd.pur-uom:SCREEN-VALUE
    rm-rctd.cost = DEC(rm-rctd.cost:SCREEN-VALUE)
    rm-rctd.cost-uom = rm-rctd.cost-uom:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li-nxt-r-no AS INT INIT 1 NO-UNDO.
 DEF BUFFER bf-rctd FOR rm-rctd.

  /* Code placed here will execute PRIOR to standard behavior. */
/*  FOR EACH bf-rctd NO-LOCK BY bf-rctd.r-no DESCENDING:                                 */
/*      li-nxt-r-no = bf-rctd.r-no.                                                      */
/*      LEAVE.                                                                           */
/*  END.                                                                                 */
/*  FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.                                  */
/*  IF AVAIL rm-rcpth AND rm-rcpth.r-no GT li-nxt-r-no THEN li-nxt-r-no = rm-rcpth.r-no. */
/*                                                                                       */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   v-new-mode    = YES
   rm-rctd.company   = cocode
   rm-rctd.r-no      = li-nxt-r-no
   rm-rctd.rita-code = "R".
  
  IF adm-adding-record THEN DO:
    ASSIGN
     rm-rctd.s-num = 0
     rm-rctd.s-num:screen-value in browse {&BROWSE-NAME} = "0"
     rm-rctd.rct-date = TODAY.

    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "RMWHSBIN"
                          no-lock no-error.
    if not avail sys-ctrl then do:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "RMWHSBIN"
                 sys-ctrl.descrip = "Default Location for RM Warehouse / Bin for RM Receipts"
                 sys-ctrl.log-fld = YES.
      message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                 update sys-ctrl.char-fld.
    end.
    IF sys-ctrl.char-fld NE 'RMITEM' THEN
    ASSIGN
      rm-rctd.loc = SUBSTR(sys-ctrl.char-fld,1,5)
      rm-rctd.loc-bin = SUBSTR(sys-ctrl.char-fld,6).
    disp rm-rctd.rct-date rm-rctd.loc rm-rctd.loc-bin with browse {&BROWSE-NAME}.
  END.

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
  v-new-mode = NO.

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
  def var li as int no-undo.
  def var hd-next as widget-handle no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  /*
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
    assign
     hd-post-child = hd-post:current-window
     hd-post-child = hd-post-child:first-child  /* frame */
     hd-post-child = hd-post-child:first-child /* field-group */
     hd-post-child = hd-post-child:first-child  /* field */
     hd-post-child:sensitive = no.
  end.
  */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:


    APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.

    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.

    {&BROWSE-NAME}:READ-ONLY = NO.
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
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN get-do-what IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-do-what).

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
  RUN get-matrix (NO).

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  IF lv-do-what = "delete" THEN DO:
     RUN valid-delete-tag NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
   
  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  DO li = 1 TO 2:
    RUN valid-uom (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  /* create a negative receipt for NON INVENTORY items item.inv-by-cust = yes */
  FIND ITEM NO-LOCK WHERE item.company = rm-rctd.company 
                      AND item.i-no = rm-rctd.i-no NO-ERROR.
  IF AVAIL ITEM AND ITEM.inv-by-cust AND v-new-mode THEN DO:
     RUN create-neg-rctd (INPUT RECID(rm-rctd)).                       
     ASSIGN v-new-mode = NO.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  RUN scan-next.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-cost B-table-Win 
PROCEDURE po-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     def var v-len like po-ordl.s-len no-undo.
     def var v-wid like po-ordl.s-len no-undo.
     def var v-dep like po-ordl.s-len no-undo. 
     def var v-bwt like po-ordl.s-len no-undo.
     def var lv-out-qty LIKE rm-rctd.qty no-undo.
     DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.
     DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

     FIND FIRST ITEM where
          item.company eq cocode AND
          item.i-no eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          use-index i-no
          NO-LOCK no-error.

     IF NOT AVAIL item THEN
        LEAVE.

     assign
        lv-qty-uom  = po-ordl.pr-qty-uom
        v-len = po-ordl.s-len
        v-wid = po-ordl.s-wid
        v-bwt = 0.

     {rm/pol-dims.i}

     IF rm-rctd.pur-uom:screen-value in browse {&browse-name} EQ lv-qty-uom THEN
        lv-out-qty = DEC(rm-rctd.qty:screen-value in browse {&browse-name}).
     ELSE
        run custom/convquom.p (INPUT cocode,
                             INPUT rm-rctd.pur-uom:screen-value in browse {&browse-name},
                             INPUT lv-qty-uom,
                             INPUT v-bwt,
                             INPUT v-len,
                             input v-wid,
                             input v-dep,
                             INPUT DEC(rm-rctd.qty:screen-value in browse {&browse-name}),
                             output lv-out-qty).

     FIND FIRST po-ord WHERE
          po-ord.company EQ cocode AND
          po-ord.po-no EQ po-ordl.po-no
          NO-LOCK NO-ERROR.

     ll-add-setup = NO.

     IF lv-out-qty LT po-ordl.ord-qty THEN
        lv-cost = po-ordl.cost +
                 (po-ordl.setup /
                 ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
     ELSE
        ASSIGN
           lv-cost = po-ordl.cost
           ll-add-setup = IF AVAIL po-ord AND po-ord.type NE "S" THEN YES
                          ELSE NO.
    
     rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom.

     RUN rm/getpocst.p (BUFFER po-ordl,
                        rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT-OUTPUT lv-cost).
    
     RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).
     RUN convert-vend-comp-curr(INPUT-OUTPUT lv-setup).     

     rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-cost).
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
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&BROWSE-NAME} TO RECID ip-recid NO-ERROR.
    RUN dispatch ("row-changed").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runWhichPost B-table-Win 
PROCEDURE runWhichPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opRunWhichPost AS CHARACTER NO-UNDO.

  opRunWhichPost = 'rm/r-rmtpst.p'.

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
  {src/adm/template/snd-list.i "rm-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-tag# as log no-undo.
  
  {rm/tag#.i}
  op-tag# = v-tag#.
  
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
  def buffer xrm-rctd for rm-rctd.
  
  assign v-tag-seq = 0
         v-locode  = "".

  do while true:
    find first xrm-rctd
        where xrm-rctd.company eq rm-rctd.company
          and xrm-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xrm-rctd then do:
      v-locode = xrm-rctd.loc.

      for each xrm-rctd where xrm-rctd.company eq rm-rctd.company
            and xrm-rctd.loc     eq v-locode
            and xrm-rctd.tag     begins string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999")
            use-index tag no-lock
            by xrm-rctd.tag desc:

           if int(substr(xrm-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xrm-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first rm-rctdh where rm-rctdh.company eq rm-rcth.company
          and rm-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail rm-rctdh then do:
      v-locode = rm-rctdh.loc.

      for each rm-rctdh
          where rm-rctdh.company eq cocode
            and rm-rctdh.loc     eq v-locode
            and rm-rctdh.tag     begins string(int(rm-rctd.po-no),"999999")
          use-index tag no-lock
          by rm-rctdh.tag desc:

        if int(substr(rm-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(rm-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign  v-tag-seq   = v-tag-seq + 1
         /* rm-rctd.tag:screen-value in browse {&BROWSE-NAME}
          = string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999") + string(v-tag-seq,"99").
           */
        .        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-from-po-line B-table-Win 
PROCEDURE update-from-po-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ord-qty LIKE po-ordl.ord-qty NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = STRING(po-ordl.s-num)
      rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = po-ordl.pr-qty-uom
      rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = STRING(po-ordl.cost)  /* po-ordl.cost*/
      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = po-ordl.pr-uom
      lv-po-wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}        = STRING(po-ordl.s-wid)
      lv-po-len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}        = STRING(po-ordl.s-len)
      ll-warned                                              = NO.

      /*RUN get-remain-qty (ROWID(po-ordl), OUTPUT lv-ord-qty).
      rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-ord-qty).*/
  END.

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
/*  FIND FIRST loadtag WHERE loadtag.company = g_company                                        */
/*                       AND loadtag.item-type = YES                                            */
/*                       AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} */
/*                       NO-LOCK NO-ERROR.                                                      */
/*  IF AVAIL loadtag THEN                                                                       */
/*     FIND FIRST rm-bin WHERE rm-bin.company = g_company                                       */
/*                         AND rm-bin.loc = loadtag.loc                                         */
/*                         AND rm-bin.i-no = loadtag.i-no                                       */
/*                         AND rm-bin.loc-bin = loadtag.loc-bin                                 */
/*                         AND rm-bin.tag = loadtag.tag-no                                      */
/*                         AND rm-bin.qty > 0                                                   */
/*                         NO-LOCK NO-ERROR.                                                    */
    FIND FIRST rm-bin 
        WHERE rm-bin.company = g_company
          AND rm-bin.tag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.qty > 0
        NO-LOCK NO-ERROR.
    IF NOT AVAIL rm-bin THEN DO:
        MESSAGE "Invalid Inventory Qty for the tag." 
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
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
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

        
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      RELEASE po-ord.

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN v-msg = "Invalid PO Line Item, try help".
       
      ELSE
      FIND FIRST po-ord WHERE
           po-ord.company EQ po-ordl.company AND
           po-ord.po-no   EQ po-ordl.po-no AND
           po-ord.type EQ "S"
           NO-LOCK NO-ERROR.

      IF AVAIL po-ord THEN DO:
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company   EQ po-ord.company
              AND rm-rcpth.vend-no   EQ po-ord.vend-no
              AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
              AND rm-rcpth.rita-code EQ "I"
            USE-INDEX vend NO-LOCK,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code
              /*AND SUBSTR(rm-rdtlh.BOL,1,30) EQ po-ordl.i-no
              AND SUBSTR(rm-rdtlh.BOL,31,3) EQ STRING(po-ordl.line,"999")*/
            NO-LOCK:
          LEAVE.
        END.
        IF NOT AVAIL rm-rdtlh THEN v-msg = "No RM issued to this Sheet PO".
      END.

      IF v-msg EQ "" AND po-ordl.stat EQ "C" THEN v-msg = "Closed".
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN v-msg = "This RM does not exist on Job, try help".
    END.

    ELSE DO:
      FIND FIRST ITEM
          WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND ITEM.i-code  EQ "R"
          NO-LOCK NO-ERROR.
      IF NOT AVAIL item THEN v-msg = "This item does not exist in RM file, try help".
    END.

    IF v-msg NE "" THEN DO:
      ll = NO.
      IF v-msg EQ "Closed" THEN DO:
        IF ll-warned THEN ll = YES.
        ELSE
          MESSAGE "PO Line is closed and may be re-opened during posting, continue?..."
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
      END.
      ELSE MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.

      IF ll THEN ll-warned = YES.
      ELSE DO:
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
      ASSIGN
       rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ""
       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = "".

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN ERROR-STATUS:ERROR = YES. 
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no2 IN BROWSE {&BROWSE-NAME}.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.po-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
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

  DO WITH FRAME {&FRAME-NAME}:
    ERROR-STATUS:ERROR = NO.

    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN
      ERROR-STATUS:ERROR = YES.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Receipt qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
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
  
  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN ERROR-STATUS:ERROR = YES.
    END.

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST job-mat
          WHERE job-mat.company EQ rm-rctd.company
            AND job-mat.job     EQ job.job
            AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mat THEN ERROR-STATUS:ERROR = YES.
    END.

    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
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
  IF rmrecpt-int EQ 1 THEN DO:
    FIND FIRST loadtag WHERE loadtag.company = g_company
                         AND loadtag.item-type = YES
                         AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''.
       APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
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
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = IF ip-int EQ 1 THEN rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                            ELSE rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

    IF AVAIL item AND INDEX("MOXY789@",ITEM.mat-type) GT 0 AND ip-int EQ 2 THEN
      lv-uom-list = lv-uom-list + ",L".

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO rm-rctd.pur-uom IN BROWSE {&BROWSE-NAME}.
      ELSE
        APPLY "entry" TO rm-rctd.cost-uom IN BROWSE {&BROWSE-NAME}.
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
 FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST rm-bin WHERE rm-bin.company = g_company
                      AND rm-bin.i-no = ""
                      AND rm-bin.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND rm-bin.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
  IF NOT AVAIL rm-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO rm-rctd.loc-bin.
          RETURN ERROR.
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

  RUN get-matrix (YES).
  return ext-cost.
  
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin B-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR ld-dim AS DEC NO-UNDO.
  DEF VAR v-wid-num AS DEC NO-UNDO.
  DEF VAR v-len-num AS DEC NO-UNDO.
  DEF BUFFER b-jm FOR job-mat.

  IF AVAIL rm-rctd THEN DO:
    FIND FIRST ITEM
        WHERE ITEM.company EQ cocode
          AND ITEM.i-no    EQ rm-rctd.i-no
          AND ITEM.i-code  EQ "R"
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
      ASSIGN
       v-wid-num = IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid
       v-len-num = ITEM.s-len.

    IF rm-rctd.po-no <> "" THEN DO:
      find first po-ordl where po-ordl.company   eq cocode
                           and po-ordl.po-no     eq int(rm-rctd.po-no)
                           and po-ordl.i-no      eq rm-rctd.i-no
                           and po-ordl.job-no    eq rm-rctd.job-no
                           and po-ordl.job-no2   eq rm-rctd.job-no2
                           and po-ordl.item-type eq yes
                           and po-ordl.s-num     eq rm-rctd.s-num
          no-lock no-error.
      if avail po-ordl then
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
      else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
      end.
    END.
      
    IF ip-dim = "W" THEN ld-dim = v-wid-num.
    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
  END.
  
  RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

