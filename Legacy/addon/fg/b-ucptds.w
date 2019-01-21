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
&SCOPED-DEFINE yellowColumnsName b-rcptd-fg
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
&scoped-define fld-name-1 fg-rctd.tag
&scoped-define SORTBY-1 BY {&fld-name-1}
&global-define IAMWHAT LOOKUP

DEF VAR ll-help-run AS LOG NO-UNDO.

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR v-msgreturn AS INT NO-UNDO.

DEF VAR v-post-date AS DATE INITIAL TODAY.
def var fg-uom-list  as char NO-UNDO.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-prgmname AS CHAR INIT "b-rcptd." NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

{fg/fg-post3.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/invrecpt.i NEW}

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO.

DEF TEMP-TABLE tt-email FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .



DEF STREAM logFile.
DEF STREAM st-email.

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
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit ~
fg-rctd.partial fg-rctd.t-qty fg-rctd.stack-code fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.po-no fg-rctd.i-no fg-rctd.i-name fg-rctd.std-cost ~
fg-rctd.cost-uom fg-rctd.ext-cost fg-rctd.r-no fg-rctd.created-by ~
fg-rctd.updated-by fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table fg-rctd.loc fg-rctd.loc-bin ~
fg-rctd.stack-code 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table fg-rctd
&Scoped-define QUERY-STRING-br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-br_table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby 

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
r-no|y|y|ASI.fg-rctd.r-no
company||y|ASI.fg-rctd.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "r-no",
     Keys-Supplied = "r-no,company"':U).

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
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sort By" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tag#", 1
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(13)":U WIDTH 7
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U WIDTH 9
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT "->>>,>>9":U
            WIDTH 9
      fg-rctd.cases-unit COLUMN-LABEL "Units/!Skid" FORMAT "->>9":U
            WIDTH 9
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
            WIDTH 10
      fg-rctd.t-qty COLUMN-LABEL "Total!Qty" FORMAT "->>>,>>>,>>9.99":U
      fg-rctd.stack-code COLUMN-LABEL "FG Lot#" FORMAT "x(20)":U
            WIDTH 21.8
      fg-rctd.job-no FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.po-no FORMAT "x(9)":U
      fg-rctd.i-no FORMAT "x(15)":U
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.std-cost COLUMN-LABEL "Cost/UOM" FORMAT ">>>,>>9.99<<":U
      fg-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      fg-rctd.ext-cost COLUMN-LABEL "Extended Cost" FORMAT "->>>,>>9.99<<":U
      fg-rctd.r-no COLUMN-LABEL "Seq No" FORMAT ">>>>>>>9":U COLUMN-BGCOLOR 14
      fg-rctd.created-by COLUMN-LABEL "User!Created" FORMAT "x(8)":U
      fg-rctd.updated-by COLUMN-LABEL "User!Updated" FORMAT "x(8)":U
      fg-rctd.rct-date COLUMN-LABEL "Receipt!Date" FORMAT "99/99/9999":U
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Receipt!Time"
  ENABLE
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.stack-code
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 115 BY 6.67
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     lv-search AT ROW 4.33 COL 35 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 4.33 COL 78 HELP
          "CLEAR AUTO FIND Value"
     browse-order AT ROW 4.57 COL 17 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 4.57 COL 120 COLON-ALIGNED WIDGET-ID 2
     RECT-4 AT ROW 4.81 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         HEIGHT             = 6.81
         WIDTH              = 144.
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

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN lv-search IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-search:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RECT-4:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "fg-rctd.company = g_company and
fg-rctd.rita-code = ""R"""
     _FldNameList[1]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whse" "x(13)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units/!Skid" "->>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.stack-code
"fg-rctd.stack-code" "FG Lot#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.fg-rctd.job-no
     _FldNameList[11]   = ASI.fg-rctd.job-no2
     _FldNameList[12]   = ASI.fg-rctd.po-no
     _FldNameList[13]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost/UOM" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.cost-uom
"fg-rctd.cost-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Extended Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.r-no
"fg-rctd.r-no" "Seq No" ? "integer" 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "User!Created" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.fg-rctd.updated-by
"fg-rctd.updated-by" "User!Updated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Receipt!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Receipt!Time" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   def var ll-tag# as log no-undo.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF BUFFER bf-tmp FOR fg-rctd.
   DEF BUFFER xfg-rdtlh FOR fg-rdtlh.

   ll-help-run = yes.
   case focus:name :
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
                         fg-rctd.cost-uom:screen-value in browse {&browse-name} = po-ordl.cons-uom /* pr-uom */
                         fg-rctd.std-cost:screen-value in browse {&browse-name} = string(po-ordl.cons-cost)  /* po-ordl.cost*/
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
               if fg-rctd.loc-bin:screen-value in browse {&browse-name} eq "" then do:
                    find first sys-ctrl where sys-ctrl.company eq fg-rctd.company
                                            and sys-ctrl.name    eq "AUTOISSU"
                                  no-lock no-error.
                    if not avail sys-ctrl then do:
                          create sys-ctrl.
                          assign sys-ctrl.company = fg-rctd.company
                                     sys-ctrl.name    = "AUTOISSU"
                                     sys-ctrl.descrip = "Automatically Issue RM Receipts to asi"
                                     sys-ctrl.log-fld = yes.
                          message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                             update sys-ctrl.char-fld.
                    end.
                    assign fg-rctd.loc-bin:screen-value in browse {&browse-name} = sys-ctrl.char-fld.
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
       /*
       when "i-no" then do:
             IF fg-rctd.po-no:SCREEN-VALUE <> "" THEN DO:
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
                  RUN windows/l-itemf2.w (fg-rctd.company, fg-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
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
       */
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
                            bf-tmp.tag = SELF:SCREEN-VALUE
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
              IF fg-rctd.loc:SCREEN-VALUE = "" THEN RUN get-def-values.
/*
              lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
              lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
*/  
              RETURN NO-APPLY.
           END.
       END.
   end case.
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
   /*{src/adm/template/brsleave.i}*/
    {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  RUN startsearch.
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


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE br_table /* Whse */
DO:
   IF LASTKEY = -1 THEN RETURN.

    DEF VAR v-locbin AS cha NO-UNDO.
    IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).

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

          APPLY "leave" TO SELF.
          APPLY "tab" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.      
          /*APPLY "row-leave" TO BROWSE {&browse-name}.*/

          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE br_table /* Bin */
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


&Scoped-define SELF-NAME fg-rctd.stack-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.stack-code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.stack-code IN BROWSE br_table /* FG Lot# */
DO:
  IF LASTKEY NE -1 THEN DO:    
    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" AND 
       fg-rctd.stack-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
        RUN custom/d-msg.w ("Error","","FG Lot# may not be entered when tag# is blank","",1,"OK", OUTPUT v-msgreturn).
        APPLY "entry" TO fg-rctd.stack-code.
    END.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search B-table-Win
ON LEAVE OF lv-search IN FRAME F-Main /* Auto Find */
or return of lv-search
DO:
        DEF VAR char-hdl AS cha NO-UNDO.

        assign browse-order
               lv-search.
        &scoped-define IAMWHAT Search
        &scoped-define where-statement begins lv-search
        case browse-order:
            {srtord2.i 1}
            /*{srtord2.i 2}  */
        end.     


        IF AVAIL fg-rctd OR NUM-RESULTS ("{&browse-name}") > 0 THEN DO:
           RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
           RUN run-update IN WIDGET-HANDLE(char-hdl).
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/closejob.i FGPost}
   {sys/inc/fgpostgl.i}
   {sys/inc/fgemails.i}
   {sys/inc/adjustgl.i}
   {sys/inc/ssmovefg.i}
END.

/*{src/adm/method/browser.i}*/
/* {src/adm/method/query.i} */
/* {methods/template/browser.i} */
 {custom/yellowcolumns.i} 
v-fgpostgl = fgpostgl.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'r-no':U THEN DO:
       &Scope KEY-PHRASE fg-rctd.r-no eq INTEGER(key-value)
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* r-no */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE can-exit B-table-Win 
PROCEDURE can-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-can-exit AS LOG NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-search B-table-Win 
PROCEDURE do-search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-search AS cha NO-UNDO.

   DEF VAR char-hdl AS cha NO-UNDO.

   lv-search = ip-search.

        &scoped-define IAMWHAT Search
        &scoped-define where-statement begins lv-search
        /*
        case browse-order:
            {srtord2.i 1}
            /*{srtord2.i 2}  */
        end.     
        */


  &scoped-define where-statement begins lv-search
  &scoped-define key-phrase ({&fld-name-1}) {&Where-statement}

  {&open-query-{&browse-name}}  

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? AND lv-search <> "" THEN
  DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  end.    
  IF NUM-RESULTS ("{&browse-name}") = 1 THEN DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
     RUN run-update IN WIDGET-HANDLE(char-hdl).
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
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF TRUE THEN RETURN "adm-error".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
     {custom/askdelss.i}
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO fg-rctd.loc IN BROWSE {&browse-name}.

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
  IF SSMoveFG-cha = "Whs/Bin" THEN DO:
     fg-rctd.stack-code:READ-ONLY IN BROWSE {&BROWSE-name} = YES.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-record NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  IF SSMoveFG-log THEN
     RUN post-finished-goods.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN scan-next.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-finished-goods B-table-Win 
PROCEDURE post-finished-goods :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-fg-rcpts for fg-rcpts.
  def buffer b-fg-rdtl for fg-rdtl.
  def buffer b-fg-bin for fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  def buffer b-itemfg1 for itemfg.
  def buffer ps-rctd for fg-rctd .
  def buffer b-po-ordl for po-ordl.
  def buffer b-oe-ordl for oe-ordl.

  def var v-one-item as log.
  def var v-dec as dec decimals 10.
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like fg-rctd.qty no-undo.
  def var v-i-qty like fg-rctd.qty no-undo.
  def var v-t-qty like fg-rctd.qty no-undo.
  def var v-overrun-qty like fg-rctd.qty no-undo.
  def var v-underrun-qty like fg-rctd.qty no-undo.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  def var v-recid as recid no-undo.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec DECIMALS 10 no-undo.
  def var v-autobin  as cha no-undo.
  def var v-newhdr as log no-undo. 
  def var v-fin-qty as dec no-undo.
  def var choice as log no-undo.
  def var v-trnum like gl-ctrl.trnum no-undo.
  def var uperiod as int no-undo.
  def var sysdate as date init today no-undo.    
  def var v-date like sysdate no-undo.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  SESSION:SET-WAIT-STATE ("general").
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  find first sys-ctrl  where sys-ctrl.company eq cocode
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
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

      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

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
    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

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

        IF AVAIL loadtag THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ g_company
                AND fg-bin.i-no    EQ loadtag.i-no
                AND fg-bin.tag     EQ loadtag.tag-no
              /*AND fg-bin.job-no = loadtag.job-no
                AND fg-bin.job-no2 = loadtag.job-no2*/
                AND fg-bin.qty     GT 0
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

    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

      CREATE w-inv.
      w-inv.row-id = w-fg-rctd.row-id.
    END.

    RUN fg/invrecpt.p (?, 2).

    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

      IF LAST-OF(w-fg-rctd.i-no) THEN DO:
        RUN fg/updfgcs1.p (RECID(itemfg), NO).

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

    IF v-fgpostgl NE "None" THEN DO TRANSACTION:
      /* gdm - 11050905 */
      REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN DO:
         ASSIGN v-trnum       = gl-ctrl.trnum + 1
                gl-ctrl.trnum = v-trnum.

         FIND CURRENT gl-ctrl NO-LOCK.
         
         RUN gl-from-work (1, v-trnum).
         RUN gl-from-work (2, v-trnum).
         
         LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050905*/
    END.
    find first w-job no-error.
    if avail w-job THEN DO:
      run jc/d-jclose.w.
    END.

    if v-adjustgl then do TRANSACTION:
      /** GET next G/L TRANS. POSTING # **/
      find first gl-ctrl where gl-ctrl.company eq cocode exclusive-lock.
      assign
       v-trnum       = gl-ctrl.trnum + 1
       gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      for each work-job break by work-job.actnum:
         create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "ADJUST"
         gltrans.tr-date = v-post-date
         gltrans.period  = period.pnum
         gltrans.trnum   = v-trnum.

        if work-job.fg then
          assign
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT FG".
        else
          assign
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
      end. /* each work-job */
    end.
    IF v-got-fgemail THEN DO:
      RUN send-fgemail (v-fgemail-file).
    END.
    SESSION:SET-WAIT-STATE ("").
  
  RUN local-open-query.
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
  DEF VAR v-done AS INT NO-UNDO.
  

  /*RUN custom/d-msg.w ("Warning","","Are you ready to post FG Receipts?","",2,"YES,NO", OUTPUT v-done).
  IF v-done >= 2 THEN RETURN.*/
  
  RUN fg/fgpstall.w (?, "R").
  
  RUN dispatch ('open-query').

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
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"srch-source",OUTPUT char-hdl).
  RUN apply-entry IN WIDGET-HANDLE(char-hdl).

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
  {src/adm/template/sndkycas.i "r-no" "fg-rctd" "r-no"}
  {src/adm/template/sndkycas.i "company" "fg-rctd" "company"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" AND 
      fg-rctd.stack-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
       RUN custom/d-msg.w ("Error","","FG Lot# may not be entered when tag# is blank","",1,"OK", OUTPUT v-msgreturn).
       APPLY "entry" TO fg-rctd.stack-code.
       RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

