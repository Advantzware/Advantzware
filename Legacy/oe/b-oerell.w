&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: oe\b-oerell.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

SESSION:DEBUG-ALERT = FALSE. /* suppress F1 stack trace error*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

{oe/d-selbin.i NEW}

DEF BUFFER bf-rell FOR oe-rell.
DEF BUFFER bf-ordl FOR oe-ordl.

DEF TEMP-TABLE w-oe-rell NO-UNDO LIKE oe-rell.

def var v-do-all-items as log no-undo.
def var lv-item-imported as log no-undo.
def var lv-import-rejected as log no-undo.
DEF VAR v-ship-from AS LOG INIT YES NO-UNDO.
DEF VAR ll-ask-import AS LOG NO-UNDO.
DEF VAR lv-copy-rowid AS ROWID NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR lv-s-codes AS CHAR NO-UNDO.
DEF VAR lv-s-dscrs AS CHAR NO-UNDO.

RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).

def new shared var ord-ok as ch init ["R,I,S,P,A,N,U"].

&Scoped-define copyfields-IN-QUERY oe-rell.ord-no oe-rell.i-no ~
oe-rell.po-no oe-rell.qty oe-rell.tag oe-rell.loc oe-rell.loc-bin ~
oe-rell.job-no oe-rell.job-no2 oe-rell.cases oe-rell.qty-case ~
oe-rell.partial oe-rell.rel-no oe-rell.b-ord-no ~
oe-rell.s-code

DEF WORKFILE w-rowid FIELD w-rowid AS CHAR.

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

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
&Scoped-define EXTERNAL-TABLES oe-relh
&Scoped-define FIRST-EXTERNAL-TABLE oe-relh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-relh.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rell oe-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table oe-rell.ord-no oe-rell.i-no ~
oe-rell.po-no oe-rell.qty oe-rell.tag oe-rell.loc oe-rell.loc-bin ~
oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no oe-rell.cases ~
oe-rell.qty-case oe-rell.partial oe-rell.rel-no oe-rell.b-ord-no ~
oe-rell.s-code oe-ordl.part-no oe-rell.link-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table oe-rell.ord-no oe-rell.i-no ~
oe-rell.po-no oe-rell.qty oe-rell.tag oe-rell.loc oe-rell.loc-bin ~
oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no oe-rell.cases ~
oe-rell.qty-case oe-rell.partial oe-rell.s-code 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table oe-rell
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table oe-rell
&Scoped-define QUERY-STRING-br_table FOR EACH oe-rell WHERE oe-rell.company eq oe-relh.company and ~
asi.oe-rell.r-no    eq oe-relh.r-no ~
use-index r-no NO-LOCK, ~
      FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company and ~
oe-ordl.ord-no eq oe-rell.ord-no and ~
oe-ordl.i-no eq oe-rell.i-no and ~
oe-ordl.line eq oe-rell.line OUTER-JOIN NO-LOCK ~
    BY oe-rell.ord-no
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-rell WHERE oe-rell.company eq oe-relh.company and ~
asi.oe-rell.r-no    eq oe-relh.r-no ~
use-index r-no NO-LOCK, ~
      FIRST oe-ordl WHERE oe-ordl.company eq oe-rell.company and ~
oe-ordl.ord-no eq oe-rell.ord-no and ~
oe-ordl.i-no eq oe-rell.i-no and ~
oe-ordl.line eq oe-rell.line OUTER-JOIN NO-LOCK ~
    BY oe-rell.ord-no.
&Scoped-define TABLES-IN-QUERY-br_table oe-rell oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-br_table oe-rell
&Scoped-define SECOND-TABLE-IN-QUERY-br_table oe-ordl


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
company||y|ASI.oe-rell.company
r-no||y|ASI.oe-rell.r-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,r-no"':U).

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

DEFINE VARIABLE qty-total AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Total Qty" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE qty-balance AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 
     LABEL "Balance Qty" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 1.20.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      oe-rell, 
      oe-ordl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      oe-rell.ord-no FORMAT ">>>>>>":U WIDTH 8
      oe-rell.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 22
      oe-rell.po-no FORMAT "x(15)":U WIDTH 22
      oe-rell.qty COLUMN-LABEL "Qty" FORMAT "->>,>>>,>>>":U WIDTH 12
      oe-rell.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U WIDTH 28
      oe-rell.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 7
      oe-rell.loc-bin COLUMN-LABEL "Bin Loc." FORMAT "x(8)":U WIDTH 10
      oe-rell.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 9
      oe-rell.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 3
      oe-rell.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12
      oe-rell.cases COLUMN-LABEL "Units" FORMAT "->>>,>>>":U WIDTH 9
      oe-rell.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>>":U
            LABEL-FGCOLOR 0
      oe-rell.partial COLUMN-LABEL "Partial" FORMAT "->>,>>>,>>>":U
            WIDTH 12
      oe-rell.rel-no COLUMN-LABEL "Rel#" FORMAT ">>>":U WIDTH 5
      oe-rell.b-ord-no COLUMN-LABEL "" FORMAT ">>":U WIDTH 3
      oe-rell.s-code COLUMN-LABEL "Type" FORMAT "x(12)":U WIDTH 14
      VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "B-Both","B",
                     "S-Ship","S",
                     "I-Invoice","I",
                     "T-Transfer","T"
          DROP-DOWN-LIST
      oe-ordl.part-no FORMAT "x(15)":U WIDTH 22
      oe-rell.link-no COLUMN-LABEL "Rel. Seq. #" FORMAT ">>>>>>>>9":U
            WIDTH 15
  ENABLE
      oe-rell.ord-no
      oe-rell.i-no
      oe-rell.po-no
      oe-rell.qty
      oe-rell.tag
      oe-rell.loc
      oe-rell.loc-bin
      oe-rell.job-no
      oe-rell.job-no2
      oe-rell.cust-no
      oe-rell.cases
      oe-rell.qty-case
      oe-rell.partial
      oe-rell.s-code HELP "Enter (I)nvoice only, (S)hip only, (B)oth invoice & ship, or (T)ransfer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 140 BY 6.91
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     qty-total AT ROW 8.10 COL 80 COLON-ALIGNED
     qty-balance AT ROW 8.10 COL 110 COLON-ALIGNED
     RECT-1 AT ROW 8 COL 70
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.oe-relh
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
         HEIGHT             = 12.81
         WIDTH              = 151.
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

/* SETTINGS FOR FILL-IN qty-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-balance IN FRAME F-Main
   NO-ENABLE                                                            */

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.oe-rell WHERE ASI.oe-relh ...,ASI.oe-ordl WHERE ASI.oe-rell ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _OrdList          = "ASI.oe-rell.ord-no|yes"
     _JoinCode[1]      = "asi.oe-rell.company eq oe-relh.company and
asi.oe-rell.r-no    eq oe-relh.r-no
use-index r-no"
     _JoinCode[2]      = "oe-ordl.company eq oe-rell.company and
oe-ordl.ord-no eq oe-rell.ord-no and
oe-ordl.i-no eq oe-rell.i-no and
oe-ordl.line eq oe-rell.line"
     _FldNameList[1]   > ASI.oe-rell.ord-no
"oe-rell.ord-no" ? ">>>>>>" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-rell.i-no
"oe-rell.i-no" "FG Item#" ? "character" ? ? ? ? ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-rell.po-no
"oe-rell.po-no" ? ? "character" ? ? ? ? ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-rell.qty
"oe-rell.qty" "Qty" "->>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-rell.tag
"oe-rell.tag" "Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-rell.loc
"oe-rell.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-rell.loc-bin
"oe-rell.loc-bin" "Bin Loc." ? "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-rell.job-no
"oe-rell.job-no" "Job#" ? "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-rell.job-no2
"oe-rell.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-rell.cust-no
"oe-rell.cust-no" "Customer#" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-rell.cases
"oe-rell.cases" "Units" "->>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-rell.qty-case
"oe-rell.qty-case" "Qty/Unit" ">>>,>>>" "integer" ? ? ? ? 0 ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-rell.partial
"oe-rell.partial" "Partial" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-rell.rel-no
"oe-rell.rel-no" "Rel#" ">>>" "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-rell.b-ord-no
"oe-rell.b-ord-no" "" ">>" "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-rell.s-code
"oe-rell.s-code" "S/I" "!" "character" ? ? ? ? ? ? yes "Enter (I)nvoice only, (S)hip only, (B)oth invoice & ship, or (T)ransfer" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" ? ? "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-rell.link-no
"oe-rell.link-no" "Rel. Seq. #" ">>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   /*RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).*/
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
       THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
    def var char-val as cha no-undo.
    def var hlp-recid as recid no-undo.
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
    DEF VAR ll AS LOG INIT YES NO-UNDO.


    lw-focus = FOCUS.
    
    case lw-focus:name :
         when "ord-no" then do:
           MESSAGE "Press YES to select by Order or NO by FG Item"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL
               UPDATE ll.
           IF ll EQ YES THEN DO:
              run windows/l-ordlcs.w (g_company, oe-relh.cust-no,oe-relh.ship-id,lw-focus:screen-value, output char-val, output hlp-recid).     
              if hlp-recid <> ? then do:
                run display-orders (hlp-recid, char-val).
                ll-ask-import = NO.
              END.
           END.
           ELSE
           IF ll EQ NO THEN DO:

             lv-rowid = IF AVAIL oe-rell THEN ROWID(oe-rell) ELSE ?.
             RUN oe/l-binsel.w (ROWID(oe-relh), OUTPUT lv-rowid).

             /*RUN dispatch ('cancel-record').*/
             run dispatch ('open-query').  /* task 07141402 */
             RUN reopen-query.
             RUN repo-query (lv-rowid).
           END.
         end.
         when "po-no" then do:
              run windows/l-custpo.w (g_company, int(oe-rell.ord-no:screen-value in browse {&browse-name}),lw-focus:screen-value, output char-val, output hlp-recid).     
              if char-val <> "" then assign lw-focus:screen-value = entry(1,char-val)
                                            oe-rell.i-no:screen-value = entry(2,char-val).
         end.
        WHEN "i-no" THEN DO:
              run windows/l-orditm.w (g_company, int(oe-rell.ord-no:SCREEN-VALUE),lw-focus:screen-value, output char-val, output hlp-recid).     
              if hlp-recid <> ? then run display-orditm (hlp-recid).
        END.        
        when "loc" then do:
            run windows/l-loc.w (g_company,lw-focus:screen-value, output char-val).
            if char-val <> "" then do :
               assign lw-focus:screen-value  = entry(1,char-val)
                      /*oe-rell.loc-bin:screen-value in browse {&browse-name} = entry(2,char-val)*/
                      .

            end.  
       end.
       when "loc-bin" then do:
            run windows/l-locbin.w (g_company,oe-rell.loc:screen-value, lw-focus:SCREEN-VALUE, output char-val).
            if char-val <> "" then do :
               assign lw-focus:screen-value  = entry(1,char-val)
                      /*rm-rctd.qty:screen-value = entry(3,char-val)
                        rm-rctd.tag:screen-value = entry(4,char-val)*/
                      .

            end.   
      end.
      WHEN "s-code" THEN DO:
            RUN windows/l-cddscr.w ("Release Types", lv-s-codes, lv-s-dscrs, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
      END.

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
   /*{src/adm/template/brsleave.i} */
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
  
  /* to display line info on header viewer */
  def var char-hdl as cha no-undo.
  DEF VAR phandle AS HANDLE NO-UNDO.
  run get-link-handle in adm-broker-hdl(this-procedure,"record-source",output char-hdl).
  run display-items in widget-handle(char-hdl) (recid(oe-rell)).
  RUN display-qty .
  /*spec notes*/
  IF AVAIL oe-rell THEN
  DO:
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(oe-rell.rec_key,{methods/headers/oe-rell.i})"}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.ord-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-rell.ord-no IN BROWSE br_table /* Order# */
DO:
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
  ll-ask-import = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.ord-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.ord-no IN BROWSE br_table /* Order# */
DO:
  DEF VAR op-rowid AS ROWID NO-UNDO.
 
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   
    if adm-new-record then do:
      ASSIGN
       oe-rell.loc:SCREEN-VALUE IN BROWSE {&browse-name}    = g_loc
       oe-rell.s-code:SCREEN-VALUE IN BROWSE {&browse-name} = IF v-ship-from THEN "B" ELSE "I".

      if ll-ask-import and not v-do-all-items then do:
         run check-exist-ord (ROWID(oe-rell)).
         if return-value = "" then do:
             message "Import all items? " view-as alert-box question button yes-no
                      update ll-ans as log.
             if ll-ans then do:
                assign lv-item-imported = yes
                       lv-import-rejected = no.
                
                run import-order-items (recid(oe-ord), "order", OUTPUT op-rowid).

                RUN dispatch ('cancel-record').
                run dispatch ('open-query').
                RUN reopen-query2.
             end.
             else lv-import-rejected = yes.
         end.
                 
      end.
    end.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.i-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.i-no IN BROWSE br_table /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.po-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.po-no IN BROWSE br_table /* Customer PO */
DO:
  /* if lastkey = -1 then return.
   IF oe-rell.po-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
      find first oe-rel
          where oe-rel.company = g_company
            AND oe-rel.ord-no = int(oe-rell.ord-no:screen-value in browse {&browse-name} )
            and oe-rel.po-no = self:SCREEN-VALUE
          no-lock no-error.
     if NOT avail oe-rel then do:
        message "Invalid PO#. Try Help. " view-as alert-box error.
        return no-apply.
     end.               
     assign oe-rell.po-no:screen-value = oe-rel.po-no
            oe-rell.i-no:screen-value = oe-rel.i-no
            .         
   END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.qty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.qty IN BROWSE br_table /* Qty */
DO:
  IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN RUN calc-cases.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.job-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.job-no IN BROWSE br_table /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.job-no2 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.job-no2 IN BROWSE br_table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.cust-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.cust-no IN BROWSE br_table /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.cases br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-rell.cases IN BROWSE br_table /* Units */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.qty-case br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-rell.qty-case IN BROWSE br_table /* Qty/Unit */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.partial br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-rell.partial IN BROWSE br_table /* Partial */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.s-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.s-code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-rell.s-code IN BROWSE br_table /* S/I */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
session:data-entry-return = yes.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN v-ship-from = oe-ctrl.ship-from.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-line B-table-Win 
PROCEDURE add-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN add-line IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "oe-relh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-relh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-cases B-table-Win 
PROCEDURE calc-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-cases LIKE oe-rell.cases NO-UNDO.
  DEF VAR lv-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEF VAR lv-partial LIKE oe-rell.partial NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-qty      = DEC(oe-rell.qty:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-qty-case = DEC(oe-rell.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-partial  = DEC(oe-rell.partial:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF lv-qty-case EQ 0 THEN lv-qty-case = lv-qty.

    ASSIGN
     lv-cases   = TRUNC((lv-qty - lv-partial) / lv-qty-case,0)
     lv-partial = lv-qty - (lv-cases * lv-qty-case)

     oe-rell.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(lv-cases)
     oe-rell.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-qty-case)
     oe-rell.partial:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(lv-partial).
  END.

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
    oe-rell.qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(oe-rell.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
               INT(oe-rell.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) +
               INT(oe-rell.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
               oe-rell.qty:FORMAT IN BROWSE {&browse-name}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-exist-ord B-table-Win 
PROCEDURE check-exist-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     return "" to import  or any string  not to import items
------------------------------------------------------------------------------*/
  def input param ip-rowid as ROWID no-undo.

  IF CAN-FIND(FIRST bf-rell where
     bf-rell.company = oe-relh.company AND
     bf-rell.r-no = oe-relh.r-no AND
     int(oe-rell.ord-no:screen-value in browse {&browse-name}) = bf-rell.ord-no AND
     ROWID(bf-rell) <> ip-rowid) then
     return "Order Exist".
  
  return "".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-navigation B-table-Win 
PROCEDURE Disable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_item B-table-Win 
PROCEDURE delete_item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-loc LIKE rm-rctd.loc NO-UNDO.
  DEF VAR ll-renumber AS LOG NO-UNDO.
  DEF BUFFER b-po-ordl FOR po-ordl.

   RUN local-delete-record .
  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-orders B-table-Win 
PROCEDURE display-orders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF INPUT PARAM ip-char AS CHAR NO-UNDO.

  DEF VAR op-rowid AS ROWID NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.


  FIND bf-ordl WHERE RECID(bf-ordl) EQ ip-recid NO-LOCK NO-ERROR.

  IF AVAIL bf-ordl THEN DO:
    RUN import-order-items (ip-recid, ip-char, OUTPUT op-rowid).
    RUN reopen-query.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
    RUN cancel-line IN WIDGET-HANDLE(char-hdl).
    RUN dispatch ("open-query").
    RUN repo-query (op-rowid).
    RUN update-line IN WIDGET-HANDLE(char-hdl).
  END.

  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-orditm B-table-Win 
PROCEDURE display-orditm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-recid as recid no-undo.

  
  find bf-ordl where recid(bf-ordl) = ip-recid no-lock no-error.
  if not avail bf-ordl then return.

  FIND FIRST oe-ord OF bf-ordl NO-LOCK.

  find first oe-rel where oe-rel.company = bf-ordl.company
                          and oe-rel.ord-no = bf-ordl.ord-no
                          and oe-rel.ship-id = oe-relh.ship-id
                          and oe-rel.rel-date = oe-relh.rel-date
                          and oe-rel.i-no = bf-ordl.i-no
                          and oe-rel.link-no = 0
                          NO-LOCK no-error.  
  assign oe-rell.i-no:screen-value in browse {&browse-name} = string(bf-ordl.i-no)
         oe-rell.qty:SCREEN-VALUE = IF AVAIL oe-rel THEN string(oe-rel.qty) ELSE STRING(bf-ordl.qty)
         oe-rell.qty-case:SCREEN-VALUE = string(bf-ordl.cas-cnt)
         oe-rell.po-no:SCREEN-VALUE = IF AVAIL oe-rel THEN oe-rel.po-no
                                      ELSE
                                      IF bf-ordl.po-no NE "" THEN bf-ordl.po-no
                                      ELSE oe-ord.po-no.

  find fg-bin where fg-bin.company = bf-ordl.company
                    and fg-bin.i-no = bf-ordl.i-no
                    and fg-bin.loc =  oe-rell.loc
                    and fg-bin.qty = int(oe-rell.qty:SCREEN-VALUE)
                    no-lock no-error.
  if avail fg-bin then  do:
         assign oe-rell.tag:SCREEN-VALUE = fg-bin.tag
                oe-rell.loc-bin:SCREEN-VALUE = fg-bin.loc-bin.
         if fg-bin.case-count > 0 then oe-rell.qty-case:SCREEN-VALUE = string(fg-bin.case-count).         
  end.

  RUN calc-cases.

  /** Find last actual release for this order number and add 1 to
     the get the next release. **/
  /* === rel-no logic moved to line (oe-rell) ========*/
  DEF BUFFER bf-rell FOR oe-rell .
  DEF VAR li-nxt-rel-no AS INT NO-UNDO.
  for each bf-rell where bf-rell.company eq g_company
      and bf-rell.ord-no  eq INT(oe-rell.ord-no:SCREEN-VALUE)
      AND ROWID(bf-rell) NE ROWID(oe-rell) no-lock 
      by bf-rell.rel-no desc:
      li-nxt-rel-no =  bf-rell.rel-no.
      leave.  
  end.
  li-nxt-rel-no = li-nxt-rel-no + 1.
  oe-rell.rel-no:SCREEN-VALUE = STRING(li-nxt-rel-no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-navigation B-table-Win 
PROCEDURE Enable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-order-items B-table-Win 
PROCEDURE import-order-items :
/*------------------------------------------------------------------------------
  Purpose:    oe/allrel.p  oe/oe-rellu.p  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF INPUT PARAM ip-char AS CHAR NO-UNDO.
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-oe-rell FOR oe-rell.
  DEF BUFFER b-oe-rell-new FOR oe-rell.

  FIND bf-ordl WHERE RECID(bf-ordl) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL bf-ordl THEN
    FIND FIRST oe-ord OF bf-ordl NO-LOCK NO-ERROR.
  ELSE
    FIND oe-ord WHERE RECID(oe-ord) EQ ip-recid NO-LOCK NO-ERROR.

  FIND FIRST b-oe-rell NO-LOCK
      WHERE b-oe-rell.r-no   EQ oe-relh.r-no
        AND b-oe-rell.s-code NE ""
        AND ROWID(b-oe-rell) NE ROWID(oe-rell)
      NO-ERROR.

  IF AVAIL oe-ord THEN
  FOR EACH bf-ordl OF oe-ord NO-LOCK:
    IF RECID(bf-ordl) EQ ip-recid OR ip-char EQ "order" THEN DO:
      find first oe-rel where oe-rel.company = oe-ord.company
                          and oe-rel.ord-no = oe-ord.ord-no
                          and oe-rel.ship-id = oe-relh.ship-id
                          and oe-rel.rel-date = oe-relh.rel-date
                          and oe-rel.i-no = bf-ordl.i-no
                          and oe-rel.link-no = 0
                          no-error.
      
      create b-oe-rell-new.
      assign b-oe-rell-new.company = oe-relh.company
             b-oe-rell-new.loc = /* g_loc - 10021210 */ IF AVAIL oe-rel THEN oe-rel.spare-char-1
                                ELSE g_loc
             b-oe-rell-new.r-no = oe-relh.r-no
             b-oe-rell-new.po-no = IF AVAIL oe-rel THEN oe-rel.po-no
                                   ELSE
                                   IF bf-ordl.po-no NE "" THEN bf-ordl.po-no
                                   ELSE oe-ord.po-no 
             b-oe-rell-new.line = bf-ordl.line
             b-oe-rell-new.qty = if avail oe-rel then oe-rel.qty else bf-ordl.qty
             b-oe-rell-new.i-no = bf-ordl.i-no
             b-oe-rell-new.link-no = if avail oe-rel then oe-rel.r-no else 0
             b-oe-rell-new.s-code = IF AVAIL b-oe-rell THEN b-oe-rell.s-code ELSE "B"
             b-oe-rell-new.qty-case = bf-ordl.cas-cnt
             b-oe-rell-new.ord-no = oe-ord.ord-no
             b-oe-rell-new.job-no = bf-ordl.job-no
             b-oe-rell-new.job-no2 = bf-ordl.job-no2.

      /*{oe/rel-no.i}*/
      DEF BUFFER bf-rell FOR oe-rell .
      DEF VAR li-nxt-rel-no AS INT NO-UNDO.
      for each bf-rell where bf-rell.company eq g_company
          and bf-rell.ord-no  eq b-oe-rell-new.ord-no no-lock 
          by bf-rell.rel-no desc:
          li-nxt-rel-no =  bf-rell.rel-no.
          leave.  
       end.

      ASSIGN
       li-nxt-rel-no = li-nxt-rel-no + 1
       b-oe-rell-new.rel-no = li-nxt-rel-no.

      if avail oe-rel then oe-rel.rel-no = b-oe-rell-new.rel-no. 

      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ b-oe-rell-new.company
            AND itemfg.i-no    EQ b-oe-rell-new.i-no
          NO-ERROR.
                   
      ASSIGN
      b-oe-rell-new.qty-case = if avail bf-ordl and
                               bf-ordl.cas-cnt gt 0 then bf-ordl.cas-cnt
                               else
                               if avail itemfg           and
                                  itemfg.case-count gt 0 then itemfg.case-count
                               else 1

      
       b-oe-rell-new.cases   = TRUNC((b-oe-rell-new.qty - b-oe-rell-new.partial) /
                               b-oe-rell-new.qty-case,0)
       b-oe-rell-new.partial = b-oe-rell-new.qty - (b-oe-rell-new.cases * b-oe-rell-new.qty-case).

      IF RECID(bf-ordl) EQ ip-recid THEN op-rowid = ROWID(b-oe-rell-new).
    END.                 
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-rell FOR oe-rell.
 
  DEF VAR old-po-no LIKE oe-rell.po-no NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  old-po-no = IF AVAIL oe-rell THEN oe-rell.po-no ELSE "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */    

  IF adm-new-record AND NOT adm-adding-record THEN DO:
    EMPTY TEMP-TABLE w-oe-rell.

    FIND b-oe-rell WHERE ROWID(b-oe-rell) EQ lv-copy-rowid NO-LOCK NO-ERROR.
    IF AVAIL b-oe-rell THEN DO:

      CREATE w-oe-rell.
      BUFFER-COPY b-oe-rell TO w-oe-rell.
      BUFFER-COPY oe-rell USING {&copyfields-in-query} rec_key
               TO w-oe-rell.
      BUFFER-COPY w-oe-rell TO oe-rell.
    END.
  END.

  IF oe-rell.line eq 0 THEN DO:
    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ oe-rell.company
          AND bf-ordl.ord-no  EQ oe-rell.ord-no
          AND bf-ordl.i-no    EQ oe-rell.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL bf-ordl THEN oe-rell.line = bf-ordl.line.
  END.

  FIND FIRST oe-rel
      WHERE oe-rel.company EQ oe-rell.company
        AND oe-rel.ord-no  EQ oe-rell.ord-no
        AND oe-rel.ship-id EQ oe-relh.ship-id
        AND oe-rel.i-no    EQ oe-rell.i-no
        AND oe-rel.line    EQ oe-rell.line
        AND oe-rel.po-no   EQ old-po-no
        AND oe-rel.link-no EQ 0
        AND NOT CAN-FIND(FIRST b-oe-rell
                         WHERE b-oe-rell.company  EQ oe-rel.company
                           AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                           AND b-oe-rell.i-no     EQ oe-rel.i-no
                           AND b-oe-rell.line     EQ oe-rel.line
                           AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                           AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND b-oe-rell.po-no    EQ oe-rel.po-no
                           AND ROWID(b-oe-rell)   NE ROWID(oe-rell))
      NO-ERROR.
  IF AVAIL oe-rel THEN 
      ASSIGN 
        oe-rel.qty = oe-rell.qty
        oe-rel.rel-no = oe-rell.rel-no.

  IF oe-rell.po-no NE old-po-no THEN
  FOR EACH oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.po-no    EQ old-po-no
        AND oe-rel.link-no  EQ 0:
    oe-rel.po-no = oe-rell.po-no.
  END.

  RELEASE oe-rel.

  lv-rowid = ROWID(oe-rell).

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
  IF oe-relh.posted THEN DO:
      MESSAGE "This release has already been posted, no updates allowed."
          VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  ASSIGN oe-rell.rel-no = int(oe-rell.rel-no:SCREEN-VALUE IN BROWSE {&browse-name})
         oe-rell.b-ord-no = INT(oe-rell.b-ord-no:SCREEN-VALUE IN BROWSE {&browse-name}).
  
  
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
  assign lv-item-imported = no
         lv-import-rejected = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-rell FOR oe-rell.

  /* Code placed here will execute PRIOR to standard behavior. */
  if lv-item-imported then do:
     lv-item-imported = no.
     return.
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign oe-rell.company = oe-relh.company
         oe-rell.r-no = oe-relh.r-no.

  FIND FIRST b-oe-rell NO-LOCK
      WHERE b-oe-rell.r-no   EQ oe-relh.r-no
        AND b-oe-rell.s-code NE ""
        AND ROWID(b-oe-rell) NE ROWID(oe-rell)
      NO-ERROR.
  IF AVAIL b-oe-rell THEN oe-rell.s-code = b-oe-rell.s-code.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-ord-no LIKE oe-boll.ord-no NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  li-ord-no = oe-rell.ord-no.
     
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF li-ord-no NE 0 THEN
  FOR EACH oe-rel
      WHERE oe-rel.company EQ cocode
        AND oe-rel.ord-no  EQ li-ord-no:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  END.

  RUN reopen-query2.

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
  lv-copy-rowid = ROWID(oe-rell).

  APPLY "entry" TO oe-rell.ord-no IN BROWSE {&browse-name}.

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

  find first sys-ctrl where sys-ctrl.company = g_company
                        and sys-ctrl.name = "ADDRELSE" no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     assign sys-ctrl.company = g_company
            sys-ctrl.name = "ADDRELSE"
            sys-ctrl.descrip = "Method for Adding Releases"
            sys-ctrl.char-fld = "ASI".
     message "Sys Ctrl record not found. " 
             "Enter default method for adding releases."
             update sys-ctrl.char-fld.
  end.
  v-do-all-items = sys-ctrl.char-fld = "ROYAL".
  
  RUN display-qty .
  APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.

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
 
  RUN display-qty .
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
  def var ll-new-record as log no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.

  RUN valid-ord-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  if adm-new-record and not v-do-all-items and
     not lv-item-imported AND NOT lv-import-rejected then do:
    run check-exist-ord (ROWID(oe-rell)).  
    if return-value = "" then do:
             message "Import all items? " view-as alert-box question button yes-no
                      update ll-ans as log.
             if ll-ans then do:
                lv-item-imported = yes.
                run import-order-items (recid(oe-ord), "order", OUTPUT lv-rowid).  
                /*run new-state ("update-complete"). 
                run reopen-query.
                */
                RUN dispatch ('cancel-record').
                RUN dispatch ('open-query').

                return.  /* no update-record, assign-record running */  
             end.
             ELSE DO:
                  APPLY "entry" TO oe-rell.ord-no IN BROWSE {&browse-name}.
                  RETURN NO-APPLY.
             END.
    end.                 
  end.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-s-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  lv-rowid = ?.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN DO:
    RUN reopen-query.
    RUN repo-query (lv-rowid).
  END.

  assign lv-item-imported = no
         lv-import-rejected = no.
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF AVAIL oe-rell THEN
  DO: /*spec notes*/
    DEF VAR char-hdl AS CHAR NO-UNDO.
    DEF VAR phandle AS HANDLE NO-UNDO.
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(oe-rell.rec_key,{methods/headers/oe-rell.i})"}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh-browse B-table-Win 
PROCEDURE Refresh-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-action AS CHARACTER NO-UNDO. 
    def var char-hdl as cha no-undo.
     run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).  /* viewer*/
     run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl). /* first page browser */
     
     IF ip-action NE "update" THEN
         RUN record-added IN WIDGET-HANDLE(char-hdl).
     run reopen-query in widget-handle(char-hdl) (ROWID(oe-relh)) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query2 B-table-Win 
PROCEDURE reopen-query2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var char-hdl as cha no-undo.
   run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).  /* viewer*/
   run get-link-handle in adm-broker-hdl(widget-handle(char-hdl),"record-source", output char-hdl). /* first page browser */
   
   run reopen-query in widget-handle(char-hdl) (ROWID(oe-relh)) .
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
  END.
  RUN display-qty .
 APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bintags B-table-Win 
PROCEDURE select-bintags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-all-or-one AS cha NO-UNDO.
 DEF VAR lv-rowids AS CHAR NO-UNDO.
 DEF VAR li AS INT NO-UNDO.
 DEF VAR ll AS LOG NO-UNDO.


 IF AVAIL oe-rell THEN DO:
   FIND FIRST bf-ordl
       WHERE bf-ordl.company  EQ cocode
         AND bf-ordl.ord-no   EQ oe-rell.ord-no
         AND bf-ordl.i-no     EQ oe-rell.i-no
         AND bf-ordl.line     EQ oe-rell.line
       NO-LOCK NO-ERROR.

   IF relmerge-int NE 0 OR NOT AVAIL bf-ordl OR TRIM(bf-ordl.job-no) EQ "" THEN
     MESSAGE "Select Bins for All Jobs?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll.

   lv-all-or-one = IF ll THEN "ALL" ELSE "ONE".

   RUN oe/d-selbin.w (1, ROWID(oe-rell), lv-all-or-one, oe-rell.i-no,
                      OUTPUT lv-rowids).

   IF lv-rowids NE "" THEN DO:
     RUN dispatch ('open-query').

     FOR EACH w-rowid:
       DELETE w-rowid.
     END.

     DO li = 1 TO NUM-ENTRIES(lv-rowids):
       IF ENTRY(li,lv-rowids) NE "" THEN DO:
         CREATE w-rowid.
         w-rowid = ENTRY(li,lv-rowids).
       END.
     END.

     li = 0.
     DO WHILE AVAIL oe-rell AND CAN-FIND(FIRST w-rowid):
       IF li GE 500 THEN LEAVE.
       FIND FIRST w-rowid WHERE w-rowid EQ STRING(ROWID(oe-rell)) NO-ERROR.
       IF AVAIL w-rowid THEN DELETE w-rowid.
       RUN dispatch ('get-next').
       li = li + 1.
     END.
   END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SELECT_notes B-table-Win 
PROCEDURE SELECT_notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAIL oe-rell THEN DO:

  FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.link-no  EQ oe-rell.r-no
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.po-no    EQ oe-rell.po-no        
        USE-INDEX link-ord NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-rel THEN 
      
      FIND  FIRST oe-rel
        WHERE oe-rel.company  EQ oe-rell.company
          AND oe-rel.ord-no   EQ oe-rell.ord-no
          AND oe-rel.i-no     EQ oe-rell.i-no
          AND oe-rel.line     EQ oe-rell.line
          AND oe-rel.rel-no   EQ oe-rell.rel-no
          AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
          AND oe-rel.po-no    EQ oe-rell.po-no      
        USE-INDEX ord-item NO-LOCK NO-ERROR.
END.

/* IF AVAIL oe-rel THEN                                                    */
/*   RUN windows/specnot5.w (INPUT oe-rel.rec_key, INPUT PROGRAM-NAME(1)). */
IF AVAIL oe-rel THEN
 RUN windows/datenote.w (INPUT oe-rel.rec_key, INPUT PROGRAM-NAME(1), "RDC", "R").
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
  {src/adm/template/sndkycas.i "company" "oe-rell" "company"}
  {src/adm/template/sndkycas.i "r-no" "oe-rell" "r-no"}

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
  {src/adm/template/snd-list.i "oe-relh"}
  {src/adm/template/snd-list.i "oe-rell"}
  {src/adm/template/snd-list.i "oe-ordl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNavigation B-table-Win 
PROCEDURE setNavigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var char-hdl as cha no-undo.
DEF VAR phandle AS HANDLE NO-UNDO.
/*run get-link-handle in adm-broker-hdl(this-procedure,"tableIO",output char-hdl).


IF avail(oe-relh)  THEN DO:
  IF oe-relh.posted = YES THEN 
    run set-buttons in widget-handle(char-hdl) ("disable-all").
  ELSE 
    run set-buttons in widget-handle(char-hdl) ("INITIAL").

END.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no B-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF oe-rell.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       (oe-rell.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE oe-bolh.cust-no OR
        NOT CAN-FIND(FIRST fg-bin 
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.i-no    EQ oe-rell.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.job-no  EQ oe-rell.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.job-no2 EQ INT(oe-rell.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                       AND fg-bin.loc     EQ oe-rell.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.loc-bin EQ oe-rell.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.tag     EQ oe-rell.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.cust-no EQ oe-rell.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}))
        THEN DO:
      MESSAGE "Invalid Customer#, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.ord-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
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

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ cocode
          AND bf-ordl.ord-no  EQ INT(oe-rell.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND bf-ordl.i-no    EQ oe-rell.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-ordl THEN DO:
      MESSAGE TRIM(oe-rell.i-no:LABEL IN BROWSE {&browse-name}) +
              " not on Order, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.i-no IN BROWSE {&browse-name}.
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
  DEF VAR lv-job-no LIKE bf-ordl.job-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-job-no = TRIM(oe-rell.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-job-no = FILL(" ",6 - LENGTH(lv-job-no)) + lv-job-no
     oe-rell.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = lv-job-no.

    IF relmerge-int EQ 0 THEN DO:
      FIND FIRST bf-ordl
          WHERE bf-ordl.company  EQ cocode
            AND bf-ordl.ord-no   EQ INT(oe-rell.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND bf-ordl.i-no     EQ oe-rell.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND (TRIM(bf-ordl.job-no) EQ "" OR
                 (bf-ordl.job-no EQ lv-job-no AND
                  (bf-ordl.job-no2 EQ INT(oe-rell.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                   FOCUS:NAME EQ "job-no")))
          NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-ordl THEN DO:
        MESSAGE TRIM(oe-rell.job-no:LABEL IN BROWSE {&browse-name}) +
              " not for Order/FG, try again..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-rell.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no B-table-Win 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ord-no LIKE oe-rell.ord-no NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-msg    = ""
     lv-ord-no = INT(oe-rell.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF oe-rell.loc:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      oe-rell.loc:SCREEN-VALUE IN BROWSE {&browse-name}    = g_loc.

    IF oe-rell.s-code:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      oe-rell.s-code:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF v-ship-from THEN "B" ELSE "I".

    IF lv-ord-no NE 0 THEN
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ g_company
          AND oe-ord.ord-no  EQ lv-ord-no
        NO-LOCK NO-ERROR.

    IF lv-msg EQ "" THEN
      IF NOT AVAIL oe-ord THEN lv-msg = "invalid".

    IF AVAIL oe-ord AND oe-ord.cust-no <> oe-relh.cust-no THEN do:
         MESSAGE "Sorry, order must match current customer. Try help..." 
                VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-rell.ord-no IN BROWSE {&browse-name}.
         RETURN ERROR.
    END.

    IF lv-msg EQ "" THEN
      IF oe-ord.stat EQ "H" THEN "on hold".

    IF lv-msg EQ "" THEN
      IF LOOKUP(oe-ord.stat,ord-ok) LE 0 THEN lv-msg = "not available for release".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(oe-rell.ord-no:LABEL IN BROWSE {&browse-name}) + " is " +
              TRIM(lv-msg) + ", try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.ord-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-code B-table-Win 
PROCEDURE valid-s-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-rell FOR oe-rell.


  DO WITH FRAME {&FRAME-NAME}:
    IF LOOKUP(oe-rell.s-code:SCREEN-VALUE IN BROWSE {&browse-name},lv-s-codes) LE 0 OR
       CAN-FIND(FIRST b-oe-rell
                WHERE b-oe-rell.r-no   EQ oe-relh.r-no
                  AND b-oe-rell.s-code NE oe-rell.s-code:SCREEN-VALUE IN BROWSE {&browse-name}
                  AND ROWID(b-oe-rell) NE ROWID(oe-rell))
    THEN DO:
      MESSAGE "Invalid " + TRIM(oe-rell.s-code:LABEL IN BROWSE {&browse-name}) +
              ", try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO oe-rell.s-code IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-qty B-table-Win 
PROCEDURE display-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iordered AS INTEGER NO-UNDO .
  DEFINE VARIABLE iqty-rel AS INTEGER NO-UNDO .
  DEFINE VARIABLE ibal-rel AS INTEGER NO-UNDO .
  DEFINE BUFFER bf-oe-ordl-2 FOR oe-ordl.
  DEFINE BUFFER bf-oe-rell FOR oe-rell .

IF AVAIL oe-rell THEN DO:
find first bf-oe-ordl-2 NO-LOCK
       WHERE bf-oe-ordl-2.company = cocode 
         AND bf-oe-ordl-2.ord-no = oe-rell.ord-no
         AND bf-oe-ordl-2.i-no = oe-rell.i-no NO-ERROR.

  if avail bf-oe-ordl-2 then
     assign
        iordered = bf-oe-ordl-2.qty
        iqty-rel = bf-oe-ordl-2.t-rel-qty
        .
  else assign iordered = 0
              iqty-rel = 0 .
  IF iqty-rel < 0  THEN iqty-rel = 0.

     ibal-rel = 0 .
     FOR EACH bf-oe-rell WHERE bf-oe-rell.company eq oe-relh.company and 
         bf-oe-rell.r-no    eq oe-relh.r-no 
         use-index r-no NO-LOCK, 
         FIRST oe-ordl WHERE oe-ordl.company eq bf-oe-rell.company and 
         oe-ordl.ord-no eq bf-oe-rell.ord-no and 
         oe-ordl.i-no eq bf-oe-rell.i-no and 
         oe-ordl.line eq bf-oe-rell.line  NO-LOCK 
         BY bf-oe-rell.ord-no :
        ibal-rel = ibal-rel + bf-oe-rell.qty .
     END.
     
   ASSIGN qty-total = ibal-rel
          qty-balance = iordered - ibal-rel .

  display qty-total qty-balance  with frame {&frame-name}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
