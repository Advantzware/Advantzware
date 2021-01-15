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
DEF VAR v-post-date AS DATE INITIAL TODAY.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-prgmname AS CHAR INIT "b-rcptd." NO-UNDO.
DEFINE VARIABLE hInventoryProcs      AS HANDLE NO-UNDO.
DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFgEmails AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPromptForClose AS LOGICAL NO-UNDO INITIAL YES.

ASSIGN cocode = g_company
       locode = g_loc.

{fg/fg-post3.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/invrecpt.i NEW}
/* For fgpostBatch.p */
{fg/fgPostBatch.i}
{Inventory/ttInventory.i "NEW SHARED"}    

/*DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd                        */
/*    FIELD row-id   AS ROWID                                          */
/*    FIELD has-rec  AS LOG INIT NO                                    */
/*    FIELD invoiced AS LOG INIT NO.                                   */
/*                                                                     */
/*DEF TEMP-TABLE tt-email FIELD tt-recid AS RECID                      */
/*                        FIELD job-no LIKE job-hdr.job-no             */
/*                        FIELD job-no2 LIKE job-hdr.job-no2           */
/*                        FIELD i-no LIKE itemfg.i-no                  */
/*                        FIELD qty AS INT                             */
/*                        FIELD cust-no AS cha                         */
/*                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .*/

DEF STREAM logFile.
DEF STREAM st-email.

{sys/inc/ssmovefg.i}

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
fg-rctd.partial fg-rctd.t-qty fg-rctd.job-no fg-rctd.job-no2 fg-rctd.po-no ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.ext-cost fg-rctd.stack-code fg-rctd.created-by fg-rctd.updated-by ~
fg-rctd.rct-date STRING(fg-rctd.trans-time,'HH:MM') @ trans-time 
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
&Scoped-Define DISPLAYED-OBJECTS browse-order 

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
     SIZE 144 BY 1.43.

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
      fg-rctd.job-no FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.po-no FORMAT "x(9)":U
      fg-rctd.i-no FORMAT "x(15)":U
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.std-cost COLUMN-LABEL "Cost/UOM" FORMAT ">>>,>>9.99<<":U
      fg-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      fg-rctd.ext-cost COLUMN-LABEL "Extended Cost" FORMAT "->>>,>>9.99<<":U
      fg-rctd.stack-code COLUMN-LABEL "FG Lot#" FORMAT "x(20)":U
            WIDTH 21.8
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
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 15.48
         BGCOLOR 8 FONT 2 ROW-HEIGHT-CHARS .52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     browse-order AT ROW 15.05 COL 17 HELP
          "Select Browser Sort Order" NO-LABEL
     lv-search AT ROW 15.05 COL 84 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 15.05 COL 127 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 15.29 COL 1
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
         HEIGHT             = 15.71
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
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.cases
"cases" "Units" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.qty-case
"qty-case" "Unit!Count" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.cases-unit
"cases-unit" "Units/!Skid" "->>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.partial
"partial" "Partial" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.t-qty
"t-qty" "Total!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ASI.fg-rctd.job-no
     _FldNameList[10]   = ASI.fg-rctd.job-no2
     _FldNameList[11]   = ASI.fg-rctd.po-no
     _FldNameList[12]   > ASI.fg-rctd.i-no
"i-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.i-name
"i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.std-cost
"std-cost" "Cost/UOM" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.cost-uom
"cost-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.ext-cost
"ext-cost" "Extended Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.stack-code
"stack-code" "FG Lot#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.created-by
"created-by" "User!Created" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.updated-by
"updated-by" "User!Updated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.fg-rctd.rct-date
"rct-date" "Receipt!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
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
                  RUN windows/l-itemf2.w (fg-rctd.company, fg-rctd.i-no:SCREEN-VALUE,"", OUTPUT char-val, OUTPUT rec-val).
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
  /* {src/adm/template/brsleave.i} */
    {brsleave.i}
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

          RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lActiveBin).
          IF NOT lActiveBin THEN DO:
             MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
          RUN ValidateBin IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, 
               fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}, 
               OUTPUT lActiveBin ).
          IF NOT lActiveBin THEN DO:
               MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
          END.
          

          APPLY "leave" TO SELF.
          APPLY "tab" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.      
          /*APPLY "row-leave" TO BROWSE {&browse-name}.*/

          RETURN NO-APPLY.
       END.
    END.
    ELSE DO:
        RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lActiveBin).
        IF NOT lActiveBin THEN DO:
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
      RUN ValidateBin IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, 
          fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}, 
          OUTPUT lActiveBin ).

       IF NOT lActiveBin THEN DO: 
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
        MESSAGE "FG Lot# may not be entered when Tag# is blank" VIEW-AS ALERT-BOX ERROR.
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

DO TRANSACTION:
   {sys/inc/closejob.i FGPost}
   {sys/inc/fgpostgl.i}
   {sys/inc/fgemails.i}
   {sys/inc/adjustgl.i}
   
END.


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
     RUN GL_SpCreateGLHist(cocode,
                        work-gl.actnum,
                        "FGPOST",
                        (if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"),
                        v-post-date,
                        (debits - credits),
                        ip-trnum,
                        period.pnum,
                        "A",
                        v-post-date,
                        string(IF AVAIL fg-rctd THEN fg-rctd.i-no ELSE ""),
                        "FG").
      assign
       debits  = 0
       credits = 0.
    end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-in-update B-table-Win
PROCEDURE is-in-update:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM op-in-update AS LOG NO-UNDO.

    op-in-update = /*IF fg-rctd.tag:SENSITIVE IN BROWSE {&browse-name} THEN YES ELSE NO*/
        IF adm-brs-in-update OR adm-new-record THEN YES ELSE NO.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win
PROCEDURE local-exit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DELETE OBJECT hInventoryProcs.


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
  RUN Inventory/InventoryProcs.p PERSISTENT SET hInventoryProcs.
  
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
     RUN post-finish-goods-single.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN scan-next.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-finish-goods B-table-Win
PROCEDURE post-finish-goods:
    /*------------------------------------------------------------------------------
     Purpose:  This is the batch posting of several records using the rules defined
     in fgRecsByUser.p to create the workfiles to be processed in fgPostBatch.
     Notes:  This is called externally by the Post button
    ------------------------------------------------------------------------------*/
 

    SESSION:SET-WAIT-STATE ("general").


    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.
 
    /* Create  workfile records for the finished goods being posted */
    RUN fg/fgRecsByUser.p (INPUT cocode, INPUT "R", INPUT USERID("ASI"), INPUT TABLE w-fg-rctd BY-reference).
        
    RUN pPostMain.
    


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-finish-goods B-table-Win
PROCEDURE post-finish-goods-single:
    /*------------------------------------------------------------------------------
     Purpose:  This only posts the current active fg-rctd record
     Notes: This is only run if SSMoveFG is yes
    ------------------------------------------------------------------------------*/
 

    SESSION:SET-WAIT-STATE ("general").


    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.
 
      /* Create a single workfile record for the finished good being posted - originally removed with ticket 46268 */
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
    ASSIGN w-fg-rctd.row-id  = ROWID(fg-rctd)
         w-fg-rctd.has-rec = YES.
         
        
    RUN pPostMain.
    

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-finish-goods B-table-Win
PROCEDURE pPostMain PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main common post program
    ------------------------------------------------------------------------------*/
 
        
    ASSIGN
        v-post-date = TODAY
        .       
        
    RUN fg/fgpostBatch.p ( 
        INPUT v-post-date, /* Post date      */
        INPUT NO,          /* tg-recalc-cost */
        INPUT "R",         /* Receipts       */
        INPUT lFgEmails,   /* Send fg emails */
        INPUT YES,         /* create work-gl */
		INPUT lPromptForClose,  /* Executes .w closing orders logic */
        INPUT TABLE w-fg-rctd BY-reference,
        INPUT TABLE tt-fgemail BY-reference,
        INPUT TABLE tt-email BY-reference,
        INPUT TABLE tt-inv BY-reference).
            
    SESSION:SET-WAIT-STATE ("").
  
    RUN local-open-query.
    


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
       IF LAST-OF(tt-email.cust-no) THEN do:
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
       RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lActiveBin).
       IF NOT lActiveBin THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.
  
  RUN ValidateBin IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, 
      fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}, 
      OUTPUT lActiveBin ).
  IF NOT lActiveBin THEN DO: 
      MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.

  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" AND 
     fg-rctd.stack-code:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
     MESSAGE "FG Lot# may not be entered when Tag# is blank" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fg-rctd.stack-code.
     RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

