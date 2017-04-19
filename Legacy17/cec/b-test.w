&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: cec\b-test.w

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
{custom/gcompany.i}
{custom/gloc.i}
{custom/persist.i}
def var ls-add-what as cha no-undo.
def var li-new-estnum as int no-undo.
def var ll-new-record as log no-undo.
def var ll-is-copy-record as log no-undo.
def var char-val as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.
def new shared var formule as int extent 12 no-undo.
def var lv-part-no-prev like eb.part-no no-undo.
def var lv-eb-recid as recid no-undo.
def var lv-ef-recid as recid no-undo.
def var is-item-copied-from-est as log no-undo.
def var li-form# like ef.form-no no-undo.
def var li-est-form-qty like est.form-qty no-undo.
def var ls-cust-no as cha no-undo.
def var ls-ship-id as cha no-undo.
def var ls-set-part-no as cha no-undo.  /* set part-no from local-create-record*/

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

{sys/inc/var.i "new" "shared"}

DEF BUFFER recalc-mr FOR reftable.

def new shared temp-table formule field formule as dec extent 12.

def var ls-prev-val as cha no-undo.
def var lv-copy-qty as int extent 20 no-undo.
def var lv-copy-pr as dec extent 20 no-undo.
def var lv-copy-uom as cha extent 20 no-undo.
def var lv-copy-date as date extent 20 no-undo.
def var lv-estqty-recid as recid no-undo.
def var lv-foam as log no-undo.
{cec/descalc.i "new"}
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
def var k_frac as dec init 6.25 no-undo.
def var ll-is-add-from-tool as log no-undo.
def var ll-crt-itemfg as log no-undo.

{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Corr
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est est-qty
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, est-qty.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table est.est-no est.est-date eb.cust-no ~
eb.ship-id eb.part-no eb.part-dscr1 eb.stock-no est-qty.eqty eb.style ~
eb.flute eb.test ef.board ef.cal eb.procat ~
display-cw-dim(yes,eb.len) @ eb.len eb.len ~
display-cw-dim(yes,eb.len) @ eb.len display-cw-dim(yes,eb.wid) @ eb.wid ~
eb.wid display-cw-dim(yes,eb.wid) @ eb.wid ~
display-cw-dim(yes,eb.dep) @ eb.dep eb.dep ~
display-cw-dim(yes,eb.dep) @ eb.dep eb.tab-in eb.i-col eb.i-coat eb.yld-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.form-no = ef.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table ef eb
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ef
&Scoped-define SECOND-TABLE-IN-QUERY-br_table eb


/* Definitions for FRAME Corr                                           */

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
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ef, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      est.est-no FORMAT "99999":U COLUMN-FONT 2
      est.est-date FORMAT "99/99/9999":U COLUMN-FONT 2
      eb.cust-no FORMAT "x(8)":U COLUMN-FONT 2
      eb.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U COLUMN-FONT 2
      eb.part-no FORMAT "x(15)":U COLUMN-FONT 2
      eb.part-dscr1 FORMAT "x(30)":U COLUMN-FONT 2
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U COLUMN-FONT 2
      est-qty.eqty COLUMN-LABEL "Est Qty" FORMAT ">>>>>>>9":U
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U COLUMN-FONT 2
      eb.flute FORMAT "XXX":U COLUMN-FONT 2
      eb.test FORMAT "x(6)":U COLUMN-FONT 2
      ef.board FORMAT "x(12)":U COLUMN-FONT 2
      ef.cal FORMAT ">9.99999<":U COLUMN-FONT 2
      eb.procat FORMAT "x(5)":U COLUMN-FONT 2
      display-cw-dim(yes,eb.len) @ eb.len
      eb.len FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.len) @ eb.len
      display-cw-dim(yes,eb.wid) @ eb.wid
      eb.wid FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.wid) @ eb.wid
      display-cw-dim(yes,eb.dep) @ eb.dep
      eb.dep FORMAT ">>9.99":U COLUMN-FONT 2
      display-cw-dim(yes,eb.dep) @ eb.dep
      eb.tab-in FORMAT "In/Out":U
      eb.i-col FORMAT ">9":U
      eb.i-coat FORMAT ">9":U
      eb.yld-qty COLUMN-LABEL "Qty/Set" FORMAT "->>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 16.43
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Corr
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.est,ASI.est-qty
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
         HEIGHT             = 17.24
         WIDTH              = 148.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME Corr
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 Corr */
ASSIGN 
       FRAME Corr:SCROLLABLE       = FALSE
       FRAME Corr:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.ef WHERE ASI.est-qty ...,ASI.eb WHERE ASI.ef ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.ef.company = ASI.est-qty.company
  AND ASI.ef.est-no = ASI.est-qty.est-no
  AND ASI.ef.eqty = ASI.est-qty.eqty"
     _JoinCode[2]      = "ASI.eb.company = ASI.ef.company
  AND ASI.eb.est-no = ASI.ef.est-no
  AND ASI.eb.form-no = ASI.ef.form-no"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" ? "99999" "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.est.est-date
"est.est-date" ? ? "date" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.cust-no
"eb.cust-no" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.eb.ship-id
"eb.ship-id" "Ship To" ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.eb.part-dscr1
"eb.part-dscr1" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.est-qty.eqty
"est-qty.eqty" "Est Qty" ">>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.eb.flute
"eb.flute" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.eb.test
"eb.test" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.ef.board
"ef.board" ? "x(12)" "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.ef.cal
"ef.cal" ? ">9.99999<" "decimal" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.eb.procat
"eb.procat" ? ? "character" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"display-cw-dim(yes,eb.len) @ eb.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.eb.len
"eb.len" ? ">>9.99" "decimal" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"display-cw-dim(yes,eb.len) @ eb.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-cw-dim(yes,eb.wid) @ eb.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.eb.wid
"eb.wid" ? ">>9.99" "decimal" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"display-cw-dim(yes,eb.wid) @ eb.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"display-cw-dim(yes,eb.dep) @ eb.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.eb.dep
"eb.dep" ? ">>9.99" "decimal" ? ? 2 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"display-cw-dim(yes,eb.dep) @ eb.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   = ASI.eb.tab-in
     _FldNameList[25]   = ASI.eb.i-col
     _FldNameList[26]   = ASI.eb.i-coat
     _FldNameList[27]   > ASI.eb.yld-qty
"eb.yld-qty" "Qty/Set" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Corr
/* Query rebuild information for FRAME Corr
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME Corr */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME Corr
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME Corr
DO:
     def var ls-cur-val as cha no-undo.
     def var lv-eb-tmpid as recid no-undo.
     def var lv-handle as handle no-undo.          
     def var char-val2 as cha no-undo.        
     def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     
     case focus:name :
     when "part-no" then do: 
           run est/l-ebrfqP.w (gcompany, gloc, focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return no-apply.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef of xeb where xef.company = xeb.company and
                                 xef.est-no = xeb.est-no  
                no-lock no-error.
           run copy-from-est.
           /*run copy-from-est2.*/
           lv-part-no-prev = eb.part-no.          
           return no-apply.           
      end.
      when "stock-no" then do:
        /* run windows/l-itemfg.w  (gcompany, output char-val). */
           run est/l-ebrfq.w (gcompany, gloc,focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef of xeb where xef.company = xeb.company and
                                 xef.est-no = xeb.est-no
                          no-lock no-error.
   
           run copy-from-est.
           /*run copy-from-est2. */
  
           return no-apply.
      end.
      when "style" then do:
     
           ls-cur-val = focus:screen-value.
           run windows/l-stylec.w (gcompany,ls-cur-val, output char-val).
           if char-val <> "" then do:
              focus:screen-value in browse {&browse-name} =  entry(1,char-val).
              find style where style.company = gcompany and
                               style.style = eb.style:screen-value in browse {&browse-name}
                         no-lock no-error.            
              if avail style then do:
                 assign ef.board:screen-value in browse {&browse-name} = style.material[1]
                        .
                 find item where item.company = gcompany and
                                 item.i-no = style.material[1] no-lock no-error.
                 ef.cal:screen-value in browse {&browse-name} = if avail item then string(item.cal) else "".
        
              end.          
           end.  
           return no-apply.
      end.
      when "procat" then do:
           ls-cur-val = focus:screen-value.
           run windows/l-fgcat.w (gcompany,ls-cur-val,output char-val).
           if char-val <> "" then
              focus:screen-value in browse {&browse-name} =  entry(1,char-val).
           return no-apply.

       end.
       when "flute" then do:
           ls-cur-val = focus:screen-value.
           run windows/l-flute.w (gcompany,output char-val).
           if char-val <> "" then
              focus:screen-value in browse {&browse-name} =  entry(1,char-val).
           return no-apply.
       end.
       when "test" then do:
           ls-cur-val = eb.flute:screen-value.
           run windows/l-test.w (gcompany,gloc,ls-cur-val,output char-val).
           if char-val <> "" then
              focus:screen-value in browse {&browse-name} =  entry(1,char-val).
           return no-apply.       
       end.
       when "Board" then do:
           def var lv-ind like style.industry no-undo.
           ls-cur-val = focus:screen-value.
           find style where style.company = gcompany and
                            style.style = eb.style:screen-value in browse {&browse-name}
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (gcompany,lv-ind,ls-cur-val,output char-val).
           else run windows/l-board.w (gcompany,lv-ind, ls-cur-val,output char-val).
           if char-val <> "" then 
              assign ef.board:screen-value in browse {&browse-name} = entry(1,char-val)
                     ef.cal:screen-value in browse {&browse-name} = entry(2,char-val)
                     .
           return no-apply.   
       end.
       when "cust-no" then do:
           /* ==================
           lv-handle = focus:handle.
           run applhelp.p.             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
              if avail cust then do:
                    find first shipto where shipto.company = gcompany
                                        and shipto.cust-no = cust.cust-no
                                        no-lock no-error.
                    eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
              end.    
           end.   /* g_lookup-var <> "" */
           g_lookup-var = "".
           ========================== */
           ls-cur-val = focus:screen-value.
           run windows/l-cust.w (gcompany,ls-cur-val, output char-val).
           if char-val <> "" then do:
              focus:screen-value in browse {&browse-name} =  ENTRY(1,char-val).
              find first shipto where shipto.company = gcompany
                                  and shipto.cust-no = focus:screen-value
                                  no-lock no-error.
               eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
           end.
           return no-apply.
       end.  /* cust-no*/
       when "ship-id" then do:
           ls-cur-val = focus:screen-value.
           run windows/l-shipto.w (gcompany,gloc,eb.cust-no:screen-value,ls-cur-val, output char-val).
           if char-val <> "" then 
              focus:screen-value in browse {&browse-name} =  ENTRY(1,char-val).
           return no-apply.
       end.  /* cust-no*/
       when "eqty" then do:
             lv-estqty-recid = if avail est-qty then recid(est-qty) else ?.
             run est/estqtyd.w (lv-estqty-recid, recid(eb),est-qty.eqty:screen-value in browse {&browse-name}, output char-val, output char-val2, output date-val, output date-val2) .
             if char-val <> "?" 
                then assign est-qty.eqty:screen-value = entry(1,char-val)
                            lv-copy-qty[2] = integer(entry(2,char-val))
                            lv-copy-qty[3] = integer(entry(3,char-val))
                            lv-copy-qty[4] = integer(entry(4,char-val))
                            lv-copy-qty[5] = integer(entry(5,char-val))
                            lv-copy-qty[6] = integer(entry(6,char-val))
                            lv-copy-qty[7] = integer(entry(7,char-val))
                            lv-copy-qty[8] = integer(entry(8,char-val))
                            lv-copy-qty[9] = integer(entry(9,char-val))
                            lv-copy-qty[10] = integer(entry(10,char-val))
                         /*   lv-copy-pr[1] = decimal(entry(11,char-val))
                            lv-copy-pr[2] = decimal(entry(12,char-val))
                            lv-copy-pr[3] = decimal(entry(13,char-val))
                            lv-copy-pr[4] = decimal(entry(14,char-val))
                            lv-copy-pr[5] = decimal(entry(15,char-val))
                            lv-copy-pr[6] = decimal(entry(16,char-val))
                            lv-copy-pr[7] = decimal(entry(17,char-val))
                            lv-copy-pr[8] = decimal(entry(18,char-val))
                            lv-copy-pr[9] = decimal(entry(19,char-val))
                            lv-copy-pr[10] = decimal(entry(20,char-val))
                            lv-copy-uom[1] = entry(21,char-val)
                            lv-copy-uom[2] = entry(22,char-val)
                            lv-copy-uom[3] = entry(23,char-val)
                            lv-copy-uom[4] = entry(24,char-val)
                            lv-copy-uom[5] = entry(25,char-val)
                            lv-copy-uom[6] = entry(26,char-val)
                            lv-copy-uom[7] = entry(27,char-val)
                            lv-copy-uom[8] = entry(28,char-val)
                            lv-copy-uom[9] = entry(29,char-val)
                            lv-copy-uom[10] = entry(30,char-val)
                            lv-copy-date[1] = date(entry(1,date-val))
                            lv-copy-date[2] = date(entry(2,date-val))
                            lv-copy-date[3] = date(entry(3,date-val))
                            lv-copy-date[4] = date(entry(4,date-val))
                            lv-copy-date[5] = date(entry(5,date-val))
                            lv-copy-date[6] = date(entry(6,date-val))
                            lv-copy-date[7] = date(entry(7,date-val))
                            lv-copy-date[8] = date(entry(8,date-val))
                            lv-copy-date[9] = date(entry(9,date-val))
                            lv-copy-date[10] = date(entry(1,date-val))
                            */
                            .
             if char-val2 <> "?" 
                then assign lv-copy-qty[11] = integer(entry(1,char-val2))
                            lv-copy-qty[12] = integer(entry(2,char-val2))
                            lv-copy-qty[13] = integer(entry(3,char-val2))
                            lv-copy-qty[14] = integer(entry(4,char-val2))
                            lv-copy-qty[15] = integer(entry(5,char-val2))
                            lv-copy-qty[16] = integer(entry(6,char-val2))
                            lv-copy-qty[17] = integer(entry(7,char-val2))
                            lv-copy-qty[18] = integer(entry(8,char-val2))
                            lv-copy-qty[19] = integer(entry(9,char-val2))
                            lv-copy-qty[20] = integer(entry(10,char-val2))
                            /*lv-copy-pr[11] = decimal(entry(11,char-val2))
                            lv-copy-pr[12] = decimal(entry(12,char-val2))
                            lv-copy-pr[13] = decimal(entry(13,char-val2))
                            lv-copy-pr[14] = decimal(entry(14,char-val2))
                            lv-copy-pr[15] = decimal(entry(15,char-val2))
                            lv-copy-pr[16] = decimal(entry(16,char-val2))
                            lv-copy-pr[17] = decimal(entry(17,char-val2))
                            lv-copy-pr[18] = decimal(entry(18,char-val2))
                            lv-copy-pr[19] = decimal(entry(19,char-val2))
                            lv-copy-pr[20] = decimal(entry(20,char-val2))
                            lv-copy-uom[11] = entry(21,char-val2)
                            lv-copy-uom[12] = entry(22,char-val2)
                            lv-copy-uom[13] = entry(23,char-val2)
                            lv-copy-uom[14] = entry(24,char-val2)
                            lv-copy-uom[15] = entry(25,char-val2)
                            lv-copy-uom[16] = entry(26,char-val2)
                            lv-copy-uom[17] = entry(27,char-val2)
                            lv-copy-uom[18] = entry(28,char-val2)
                            lv-copy-uom[19] = entry(29,char-val2)
                            lv-copy-uom[20] = entry(30,char-val2)
                            lv-copy-date[11] = date(entry(1,date-val2))
                            lv-copy-date[12] = date(entry(2,date-val2))
                            lv-copy-date[13] = date(entry(3,date-val2))
                            lv-copy-date[14] = date(entry(4,date-val2))
                            lv-copy-date[15] = date(entry(5,date-val2))
                            lv-copy-date[16] = date(entry(6,date-val2))
                            lv-copy-date[17] = date(entry(7,date-val2))
                            lv-copy-date[18] = date(entry(8,date-val2))
                            lv-copy-date[19] = date(entry(9,date-val2))
                            lv-copy-date[20] = date(entry(1,date-val2))
                            */.
             return no-apply.
       end.
       otherwise do:
        /* ==========================  
           lv-handle = focus:handle.
           run applhelp.p.
             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
                 if avail cust then do:
                    find first shipto where shipto.company = gcompany
                                        and shipto.cust-no = cust.cust-no
                                        no-lock no-error.
                    eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
                 end.    
              end.  /* cust-no */
           end.   /* g_lookup-var <> "" */
           g_lookup-var = "".
           =======================  */
           return no-apply.
        end.  /* otherwise */
  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON return OF br_table IN FRAME Corr
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME Corr
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
  /* not to have error no eb record avail when add new set item */
  if not avail eb then find eb where recid(eb)  = lv-eb-recid no-lock.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME Corr
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
   if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:
      return no-apply.
   end.
 
     {est/brsleave.i}   /* same but update will be like add 
                           need to run set-attribute-list ("adm-new-record = 'no' ")
                           in local-update-record  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME Corr
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
    
  if not adm-new-record /*and not adm-adding-record */ then   
     assign lv-eb-recid = recid(eb)
            lv-ef-recid = recid(ef).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est.est-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.est-date br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est.est-date IN BROWSE br_table /* Est Date */
DO:
  if eb.cust-no:screen-value in browse {&browse-name} eq "" then do:
    apply "tab" to {&self-name} in browse {&browse-name}.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.cust-no IN BROWSE br_table /* Cust. # */
DO:
    if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock.
    if eb.est-type = 6 and eb.form-no > 1 then do:
       apply "tab" to self.
       return no-apply.    
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.cust-no IN BROWSE br_table /* Cust. # */
DO:
   if lastkey <> -1 and /*eb.cust-no:screen-value in browse {&browse-name} <> "" and */
      not can-find(cust where cust.company = gcompany and cust.cust-no = eb.cust-no:screen-value in browse {&browse-name} )
   then do:
       if eb.cust-no:screen-value = "" then do:
           message "Invalid Customer Number. Try Help." view-as alert-box error. 
           return no-apply.
       end.
       message "Customer " eb.cust-no:screen-value "does not exist. Do you want to add it?"
               view-as alert-box question button yes-no update ll-ans as log.
       if not ll-ans then  return no-apply.
       
       run est/custfly.w (eb.cust-no:screen-value). 
       
   end.
   else do:  
       find cust where cust.company = gcompany and
                              cust.cust-no = self:screen-value 
                              no-lock no-error.
       if avail cust then do:
         find first shipto
             where shipto.company eq cust.company
               and shipto.cust-no eq cust.cust-no
               and (if cust.cust-no eq "TEMP" then shipto.ship-id eq "TEMP"
                    else shipto.ship-no eq 1)
             no-lock no-error.
         eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
       end.                    
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.ship-id IN BROWSE br_table /* Ship To */
DO:
  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock.
  if eb.est-type = 6 and eb.form-no > 1 then do:
       apply "tab" to self.
       return no-apply.    
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.part-no IN BROWSE br_table /* Cust Part # */
DO:
   def var lv-eb-tmpid as recid no-undo.
   
   if lastkey <> -1 and eb.part-no:screen-value in browse {&browse-name} = "" then do:
      message "Customer Part # can not be blank!" view-as alert-box error.
      return no-apply.
   end.
   
/* =========???
      /* lv-part-no-prev ; assigned from entry and help */  
   if lv-part-no-prev <> self:screen-value in browse {&browse-name} and
      lastkey <> -1
   then do:
       /* find estimate rec first */
       find first xeb where xeb.company = gcompany and
                            xeb.loc = gloc and
                            xeb.part-no = self:screen-value
                           use-index part no-lock no-error.  
       if avail xeb then do:
          find next xeb where xeb.company = gcompany and
                           xeb.loc = gloc and
                           xeb.part-no = self:screen-value
                           use-index part no-lock no-error.  
          if avail xeb then do:  /* multi estimates */
             run est/l-ebrfqP.w (gcompany, gloc,self:screen-value, output lv-eb-tmpid) .
             if lv-eb-tmpid = ?  then return no-apply.
             find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
             find xef of xeb where xef.company = xeb.company and
                                   xef.est-no = xeb.est-no
                             no-lock no-error.
             run copy-from-est.
             run copy-from-est2.
            /* return no-apply. */
          end.
          else do:  /* only one estimate exists for the part# */
             find first xeb where xeb.company = gcompany and
                              xeb.loc = gloc and
                              xeb.part-no = self:screen-value
                              use-index part no-lock no-error.  
             find first xef of xeb where xef.company = xeb.company and
                                   xef.est-no = xeb.est-no
                  no-lock no-error.
             run copy-from-est.
             run copy-from-est2.
             /*return no-apply.          */
          end.
       end.
       else do:  /* no estimate record exist */
          find first itemfg where itemfg.company = gcompany and
                               itemfg.part-no = self:screen-value in browse {&browse-name}
                               no-lock no-error.
          if avail itemfg then find style where style.company = gcompany and
                        style.style = itemfg.style
                        no-lock no-error.
          if avail itemfg then
             assign eb.part-dscr1:screen-value in browse {&browse-name} = itemfg.i-name
                 eb.style:screen-value in browse {&browse-name} = itemfg.style
                 eb.stock-no:screen-value in browse {&browse-name} = itemfg.i-no
                 eb.procat:screen-value in browse {&browse-name} = itemfg.procat
                 ef.board:screen-value in browse {&browse-name} = if avail style then style.material[1]
                                                                       else ef.board
                 eb.len:screen-value = string(itemfg.l-score[50])
                 eb.wid:screen-value = string(itemfg.w-score[50])
                 eb.dep:screen-value = string(itemfg.d-score[50])
                 .
          
    
        end.
    end.  /* modified */    
===============*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.stock-no IN BROWSE br_table /* FG Item# */
DO:
  def var lv-eb-tmpid as recid no-undo.
        /* 1. get values from itemfg if itemfg.est-no <> ""
           2. else get estimate info window */
  if adm-new-record /*AND xef.part-no:screen-value in browse {&browse-name} = "" */
     and self:screen-value <> ""
  then do:
    /* copy from estimate */
    find first itemfg where itemfg.company = gcompany and
                            itemfg.i-no = eb.stock-no:screen-value in browse {&browse-name}
                      no-lock no-error.
    if avail itemfg and itemfg.est-no <> "" then do:
       find xest where xest.company  = itemfg.company and
                      xest.loc = itemfg.loc and
                      xest.est-no = itemfg.est-no
                      no-lock no-error.
       if avail xest then do:
          find first xeb where xeb.company = xest.company and
                               xeb.est-no = xest.est-no and
                               xeb.stock-no = itemfg.i-no
                              no-lock no-error.
          if avail xeb then do:
             find first xef of xeb where xef.company = xeb.company and
                                         xef.est-no = xeb.est-no
                  no-lock no-error.
             assign eb.len:screen-value in browse {&browse-name} = string(xeb.len)
                               eb.wid:screen-value in browse {&browse-name} = string(xeb.wid)
                               eb.dep:screen-value in browse {&browse-name} = string(xeb.dep)
                               eb.procat:screen-value in browse {&browse-name} = xeb.procat
                               eb.style:screen-value in browse {&browse-name} = xeb.style
                               eb.part-no:screen-value in browse {&browse-name} = xeb.part-no
                               eb.part-dscr1:screen-value in browse {&browse-name} = xeb.part-dscr1                               
                               ef.cal:screen-value in browse {&browse-name} = if avail xef then string(xef.cal)  else ""
                               ef.board:screen-value in browse {&browse-name} = if avail xef then xef.board else ""
                               eb.i-col:screen-value in browse {&browse-name} = string(xeb.i-col)                               
                               eb.i-coat:screen-value in browse {&browse-name} = string(xeb.i-coat)
                               .
  
             run  copy-from-est.                  
           end.
       end.    
    end.  /* avail itemfg */
    else do:  /* get it from estimate */                             
          /* 
           run est/l-ebrfq.w (gcompany, gloc, self:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return no-apply.
           find xeb where recid(xeb) = lv-eb-tmpid no-lock no-error.
           find xef where xef.company = xeb.company and
                          xef.est-no = xeb.est-no 
                no-lock no-error.
           
           run copy-from-est.
           run copy-from-est2.
          */  
       if not avail itemfg and eb.stock-no:screen-value <> "" then do:
          message "This item does not exist, would you like to add it?" view-as alert-box question
                    button yes-no update ll-ans1 as log.  
          if ll-ans1 then do:
           /* run from assign-record   
             find xest where recid(xest) = recid(est) no-lock no-error.
             find xeb where recid(xeb) = recid(eb) no-lock no-error.
             find xef where recid(xef) = recid(ef) no-lock no-error.
             run crt-itemfg (input self:screen-value).
           */
           ll-crt-itemfg = yes.  
           return.
          end.   
          return no-apply.        
       end.    
    end.            /* else */  
  end. /* adm-new-record */ 
 
  else if not adm-new-record and lastkey <> -1 then do:  /* update existing records */
  
    find first itemfg where itemfg.company = gcompany and
                            itemfg.i-no = eb.stock-no:screen-value in browse {&browse-name}
                      no-lock no-error.

    if not avail itemfg and eb.stock-no:screen-value <> "" then do:
    /*   message "Invalid FG Item#. Try Help.".
       return no-apply.
     */
       message "This item does not exist, would you like to add it?" view-as alert-box question
               button yes-no update ll-ans as log.  
       if ll-ans then do:
        /*  find xest where recid(xest) = recid(est) no-lock no-error.
          find xeb where recid(xeb) = recid(eb) no-lock no-error.
          find xef where recid(xef) = recid(ef) no-lock no-error.
          run crt-itemfg (input self:screen-value).
        */
          ll-crt-itemfg = yes.  
          return.
       end.   
       return no-apply.        
    end.  
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-qty.eqty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-qty.eqty IN BROWSE br_table /* Est Qty */
DO: 
  if eb.est-type = 6 and eb.form-no > 1 then do:
       apply "tab" to self.
       return no-apply.    
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-qty.eqty IN BROWSE br_table /* Est Qty */
DO:
    if lastkey = -1 then return.
    
    if int(est-qty.eqty:screen-value in browse {&browse-name} ) <= 0 then do:
       message "Quantity must be entered. " view-as alert-box error.
       return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.style IN BROWSE br_table /* Style */
DO:
    ls-prev-val = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.style IN BROWSE br_table /* Style */
DO:
   if is-item-copied-from-est then return.
   
   if lastkey <> -1 and /*eb.style:screen-value in browse {&browse-name} <> "" and */
      not can-find(style where style.company = gcompany and
                               style.style = eb.style:screen-value in browse {&browse-name} )
   then do:
        if eb.style:screen-value = "" then message "Valid Style must be entered. Try Help." view-as alert-box error.
        else message "Invalid Style. Try Help. " view-as alert-box error.
        return no-apply.
   end. 

   if /*eb.style:modified in browse {&browse-name}*/ 
      self:screen-value <> ls-prev-val 
   then do:
     find style where style.company = gcompany and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.   
     if avail style then do:
        assign ef.board:screen-value in browse {&browse-name} = style.material[1].
        lv-foam = if style.type = "F" then  yes else no.
     end.   
   end.
   self:screen-value = caps(self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.flute br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.flute IN BROWSE br_table /* Flute */
DO:
   def var ls-board as cha no-undo.
   
   if lastkey <> -1 and 
      not can-find(first flute where flute.code = eb.flute:screen-value in browse {&browse-name})
   then do:
      message "Invalid Flute Code." view-as alert-box error.
      return no-apply.
   end.
  
   if adm-new-record and eb.flute:modified in browse {&browse-name}
      /*self:screen-value <> ls-prev-val  */
   then do:
     find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = eb.style:screen-value
                           and (reftable.loc = eb.flute:screen-value or lv-foam)
                           and reftable.code = "BOARD"
                        no-lock no-error.
     if avail reftable and reftable.dscr <> "" then do:
        find first item where item.company = gcompany and
                              item.i-no = reftable.dscr
                              no-lock no-error.
        if avail item then
                 assign ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr */
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/ string(item.cal)
                        ls-board = item.i-no
                        .                      
                          
     end.                                         
  /*   if not avail item or (avail item and item.i-no = "" ) then   */
     if ls-board = "" then
     for each item  where item.company  eq gcompany
                  and item.mat-type eq "B"
                  and item.i-code   eq "E"
                  and item.flute    eq eb.flute:screen-value in browse {&browse-name}
                  and item.reg-no   eq eb.test:screen-value
                  use-index mat-type no-lock
                  by item.i-no:
       assign ef.board:screen-value    = item.i-no
              ef.cal:screen-value = string(item.cal).
     /* ====
      xef.brd-dscr = item.est-dscr
      xef.i-code   = item.i-code
      xef.flute    = item.flute
      xef.test     = item.reg-no
      xef.cal      = item.cal
      xef.weight   = item.basis-w.

     if item.i-code eq "R" then
       assign
        xef.lsh-len = item.s-len
        xef.lsh-wid = item.s-wid
        xef.gsh-wid = item.s-wid.
     if item.r-wid ne 0 then
       assign
        xef.roll     = true
        xef.roll-wid = item.r-wid
        xef.lsh-wid  = item.r-wid
        xef.gsh-wid  = item.r-wid.
     ============*/   
     leave.
   end.



   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.test br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.test IN BROWSE br_table /* Test */
DO:
  def var ls-board as cha no-undo.
  
   /*if ef.board:screen-value in browse {&browse-name} = "" then */
  if adm-new-record and eb.test:modified in browse {&browse-name} then
  do:  
     find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = eb.style:screen-value in browse {&browse-name}
                           and (reftable.loc = eb.flute:screen-value or lv-foam)
                           and reftable.code = "BOARD"
                        no-lock no-error.
     if avail reftable and reftable.dscr <> "" then do:
        find first item where item.company = gcompany and
                              item.i-no = reftable.dscr
                              no-lock no-error.
        if avail item then
                 assign ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr 
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no */
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/  string(item.cal)
                        ls-board = item.i-no.
                        .                      
                          
     end.                                         
     /*if not avail item or (avail item and item.i-no = "" ) then   */
     if ls-board = "" then
     for each item  where item.company  eq gcompany
                  and item.mat-type eq "B"
                  and item.i-code   eq "E"
                  and item.flute    eq eb.flute:screen-value in browse {&browse-name}
                  and item.reg-no   eq eb.test:screen-value
                  use-index mat-type no-lock
                  by item.i-no:
       assign ef.board:screen-value    = item.i-no
              ef.cal:screen-value = string(item.cal).
     /* ====
      xef.brd-dscr = item.est-dscr
      xef.i-code   = item.i-code
      xef.flute    = item.flute
      xef.test     = item.reg-no
      xef.cal      = item.cal
      xef.weight   = item.basis-w.

     if item.i-code eq "R" then
       assign
        xef.lsh-len = item.s-len
        xef.lsh-wid = item.s-wid
        xef.gsh-wid = item.s-wid.
     if item.r-wid ne 0 then
       assign
        xef.roll     = true
        xef.roll-wid = item.r-wid
        xef.lsh-wid  = item.r-wid
        xef.gsh-wid  = item.r-wid.
     ============*/   
     leave.
   end.

   end.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.board IN BROWSE br_table /* Board */
DO:
   if lastkey = -1 then return.
   
   find item where item.company = gcompany
                and item.i-no = self:screen-value in browse {&browse-name}
                    no-lock no-error.
   if not avail item then do:
      message "Invalid Board. Try Help. " view-as alert-box error.
      return no-apply.
   end.
   else assign ef.cal:screen-value in browse {&browse-name} = string(item.cal)
              .               

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.procat IN BROWSE br_table /* Category */
DO:
    if lastkey <> -1 and 
      /* eb.procat:screen-value in browse {&browse-name} <> "" and */
       not can-find(first fgcat where fgcat.company = gcompany and
                    fgcat.procat = eb.procat:screen-value in browse {&browse-name})
    then do:
         message "Invalid Category. Try Help." view-as alert-box error.
         return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON PAGE-DOWN OF eb.procat IN BROWSE br_table /* Category */
DO:
   find first fgcat where fgcat.company = gcompany and
                          fgcat.procat > eb.procat:screen-value in browse {&browse-name}
                          no-lock no-error.
   if avail fgcat then eb.procat:screen-value in browse {&browse-name} = fgcat.procat.
   
                          
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON PAGE-UP OF eb.procat IN BROWSE br_table /* Category */
DO:
   find last fgcat where fgcat.company = gcompany and
                          fgcat.procat < eb.procat:screen-value in browse {&browse-name}
                          no-lock no-error.
   if avail fgcat then eb.procat:screen-value in browse {&browse-name} = fgcat.procat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.len IN BROWSE br_table /* Length */
DO:
   if lastkey = -1 then return.
   if decimal(eb.len:screen-value in browse {&browse-name}) = 0 then do:
        message "Length can not be 0. " view-as alert-box error.
        return no-apply.
   end.

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to self.
             return no-apply.
   end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.wid IN BROWSE br_table /* Width */
DO:
   if lastkey = -1 then return.
   if decimal(eb.wid:screen-value in browse {&browse-name}) = 0 then do:
        message "Width can not be 0. " view-as alert-box error.
        return no-apply.
   end.

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to self.
             return no-apply.
   end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dep br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.dep IN BROWSE br_table /* Depth */
DO:
    if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= 0.16 
   then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to self.
             return no-apply.
   end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
RUN Get-Company  (OUTPUT gcompany).
RUN Get-location (OUTPUT gloc).
assign cocode = gcompany
       locode = gloc.

DO TRANSACTION:
   {sys/inc/graphic.i}
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-estimate B-table-Win 
PROCEDURE add-estimate :
/*------------------------------------------------------------------------------
  Purpose:     run from select_add in w-estc.w and 
               called from button select_add in optionse.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-is-add-from-tool = yes.  /* add from option button not from add button */
  ls-add-what = "set" .   /* new estimate */
  run est/d-addset.w (output ls-add-what). /* one item or set cec/est-add.p */
  if ls-add-what = "" then return no-apply.  /* cancel */
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
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
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "est-qty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "est-qty"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-qty B-table-Win 
PROCEDURE assign-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*== Qty assignment ==*/
  def buffer bf-est for est.
  def buffer bf-est-qty for est-qty.
  find bf-est-qty where recid(bf-est-qty) = recid(est-qty).
  assign      bf-est-qty.qty[1] = est-qty.eqty
              bf-est-qty.qty[2] = lv-copy-qty[2]
              bf-est-qty.qty[3] = lv-copy-qty[3]
              bf-est-qty.qty[4] = lv-copy-qty[4]
              bf-est-qty.qty[5] = lv-copy-qty[5]
              bf-est-qty.qty[6] = lv-copy-qty[6]
              bf-est-qty.qty[7] = lv-copy-qty[7]
              bf-est-qty.qty[8] = lv-copy-qty[8]
              bf-est-qty.qty[9] = lv-copy-qty[9]
              bf-est-qty.qty[10] = lv-copy-qty[10]
              bf-est-qty.qty-price[1] = lv-copy-pr[1]
              bf-est-qty.qty-price[2] = lv-copy-pr[2]
              bf-est-qty.qty-price[3] = lv-copy-pr[3]
              bf-est-qty.qty-price[4] = lv-copy-pr[4]
              bf-est-qty.qty-price[5] = lv-copy-pr[5]
              bf-est-qty.qty-price[6] = lv-copy-pr[6]
              bf-est-qty.qty-price[7] = lv-copy-pr[7]
              bf-est-qty.qty-price[8] = lv-copy-pr[8]
              bf-est-qty.qty-price[9] = lv-copy-pr[9]
              bf-est-qty.qty-price[10] = lv-copy-pr[10]
              bf-est-qty.qty-uom[1] = lv-copy-uom[1]
              bf-est-qty.qty-uom[2] = lv-copy-uom[2]
              bf-est-qty.qty-uom[3] = lv-copy-uom[3]
              bf-est-qty.qty-uom[4] = lv-copy-uom[4]
              bf-est-qty.qty-uom[5] = lv-copy-uom[5]
              bf-est-qty.qty-uom[6] = lv-copy-uom[6]
              bf-est-qty.qty-uom[7] = lv-copy-uom[7]
              bf-est-qty.qty-uom[8] = lv-copy-uom[8]
              bf-est-qty.qty-uom[9] = lv-copy-uom[9]
              bf-est-qty.qty-uom[10] = lv-copy-uom[10]
              bf-est-qty.qty-date[1] = lv-copy-date[1]  
              bf-est-qty.qty-date[2] = lv-copy-date[2]  
              bf-est-qty.qty-date[3] = lv-copy-date[3]  
              bf-est-qty.qty-date[4] = lv-copy-date[4]  
              bf-est-qty.qty-date[5] = lv-copy-date[5]  
              bf-est-qty.qty-date[6] = lv-copy-date[6]  
              bf-est-qty.qty-date[7] = lv-copy-date[7]  
              bf-est-qty.qty-date[8] = lv-copy-date[8]  
              bf-est-qty.qty-date[9] = lv-copy-date[9]  
              bf-est-qty.qty-date[10] = lv-copy-date[10]  
              .  
       assign bf-est-qty.qty[11] = lv-copy-qty[11]
              bf-est-qty.qty[12] = lv-copy-qty[12]
              bf-est-qty.qty[13] = lv-copy-qty[13]
              bf-est-qty.qty[14] = lv-copy-qty[14]
              bf-est-qty.qty[15] = lv-copy-qty[15]
              bf-est-qty.qty[16] = lv-copy-qty[16]
              bf-est-qty.qty[17] = lv-copy-qty[17]
              bf-est-qty.qty[18] = lv-copy-qty[18]
              bf-est-qty.qty[19] = lv-copy-qty[19]
              bf-est-qty.qty[20] = lv-copy-qty[20]
              bf-est-qty.qty-price[11] = lv-copy-pr[11]
              bf-est-qty.qty-price[12] = lv-copy-pr[12]
              bf-est-qty.qty-price[13] = lv-copy-pr[13]
              bf-est-qty.qty-price[14] = lv-copy-pr[14]
              bf-est-qty.qty-price[15] = lv-copy-pr[15]
              bf-est-qty.qty-price[16] = lv-copy-pr[16]
              bf-est-qty.qty-price[17] = lv-copy-pr[17]
              bf-est-qty.qty-price[18] = lv-copy-pr[18]
              bf-est-qty.qty-price[19] = lv-copy-pr[19]
              bf-est-qty.qty-price[20] = lv-copy-pr[20]
              bf-est-qty.qty-uom[11] = lv-copy-uom[11]
              bf-est-qty.qty-uom[12] = lv-copy-uom[12]
              bf-est-qty.qty-uom[13] = lv-copy-uom[13]
              bf-est-qty.qty-uom[14] = lv-copy-uom[14]
              bf-est-qty.qty-uom[15] = lv-copy-uom[15]
              bf-est-qty.qty-uom[16] = lv-copy-uom[16]
              bf-est-qty.qty-uom[17] = lv-copy-uom[17]
              bf-est-qty.qty-uom[18] = lv-copy-uom[18]
              bf-est-qty.qty-uom[19] = lv-copy-uom[19]
              bf-est-qty.qty-uom[20] = lv-copy-uom[20]
              bf-est-qty.qty-date[11] = lv-copy-date[11]  
              bf-est-qty.qty-date[12] = lv-copy-date[12]  
              bf-est-qty.qty-date[13] = lv-copy-date[13]  
              bf-est-qty.qty-date[14] = lv-copy-date[14]  
              bf-est-qty.qty-date[15] = lv-copy-date[15]  
              bf-est-qty.qty-date[16] = lv-copy-date[16]  
              bf-est-qty.qty-date[17] = lv-copy-date[17]  
              bf-est-qty.qty-date[18] = lv-copy-date[18]  
              bf-est-qty.qty-date[19] = lv-copy-date[19]  
              bf-est-qty.qty-date[20] = lv-copy-date[20]  
              .  
      find bf-est where bf-est.company = bf-est-qty.company and
                        bf-est.est-no = bf-est-qty.est-no.
      assign bf-est.est-qty[1] = est-qty.eqty
             bf-est.est-qty[2] = bf-est-qty.qty[2]
             bf-est.est-qty[3] = bf-est-qty.qty[3]
             bf-est.est-qty[4] = bf-est-qty.qty[4]
             .                  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size B-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* calc blank W,L SqIn */

   def buffer bf-eb for eb .
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var K_FRAC as dec init 6.25 no-undo.
   def var v-score-char like v-lscore-c extent 12.
   DEF VAR v-index AS INT NO-UNDO.
   DEF VAR v-str AS CHAR NO-UNDO.

   find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.
   lv-panels = sys-ctrl.log-fld.

   find xest where recid(xest) = recid(est) no-lock.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
       if style.type <> "F" then run calc-blank-size2. 

      {cec/msfcalc.i}
      run est/u2kinc1c.p (recid(eb)).
      run est/u2kinc2c.p (recid(eb)).
      find first formule no-lock no-error.
      find bf-eb of eb exclusive-lock.    
      assign bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          .
      /*bf-eb.t-sqin = if v-corr then bf-eb.t-sqin * .007 else bf-eb.t-sqin / 144.
      */
   
      if not lv-panels or style.type = "F" then 
         assign bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      else do:
         run cec/descalc.p (recid(xest),recid(xeb)).

         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" then
            assign  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         else do:
           i = 0.
           for each w-box-design-line:
             i = i + 1.
             xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
             {sys/inc/k16bb.i xeb.k-wid-array2[i]}
           end.

           assign  v-score-char    = ""
                   j               = 1.
           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt 12 then leave.
           end.
           do i = 1 to 12:

              IF v-cecscrn-dec AND v-score-char[i] NE "" THEN
                 ASSIGN
                    v-index = INDEX(v-score-char[i],".")
                    v-str = SUBSTRING(v-score-char[i],v-index + 1)
                    v-str = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))
                    SUBSTRING(v-score-char[i],v-index) = v-str.

              xeb.k-len-array[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i xeb.k-len-array[i]}.
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */
       
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 B-table-Win 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}
   find first item where item.company = est.company
                    and item.i-no eq eb.adhesive
                  no-lock no-error.
   if avail item then do:
 
            if item.mat-type eq "G" then do:
                    if eb.tab-in  then do:
                       {est/u2estc.i eb.k-len 3}
                    end.
                    else do:
                       {est/u2estc.i eb.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if eb.tab-in  then do:
                       {est/u2estc.i eb.k-len 5}
                    end.
                    else do:
                       {est/u2estc.i eb.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    eb.tab-in = no.
                    {est/u2estc.i eb.k-len 7}
            end. 
    end.
    else do:
                 eb.tab-in = no.
                 {est/u2estc.i eb.k-len 7}
    end.

    if eb.len eq eb.wid
    then do:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    end.
    else do:
                 {est/u2estc.i eb.k-wid 2}
    end.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pass B-table-Win 
PROCEDURE calc-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* *********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.
      def buffer bf-eb for eb.
      
      find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         IF style.material[2] NE "" THEN
         find first item where item.company = eb.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).

         IF style.material[6] NE "" THEN 
         find first alt-item where alt-item.company  = eb.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.
 
      save_id = recid(item). save_id2 = recid(alt-item).
      j = (integer(eb.i-col:screen-value in browse {&browse-name})
          + integer(eb.i-coat:screen-value in browse {&browse-name})  ) 
          .
      {sys/inc/roundup.i j}
      ASSIGN
      counter = 1
      choice = true.
  
      find bf-eb of eb exclusive-lock.    
      if eb.i-col > 0 then assign bf-eb.i-pass = 1.
      if eb.i-coat > 0 then assign bf-eb.i-coat-p = 1.
      if choice then do i = 1 to 10:
         if i le integer(eb.i-col) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
              assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.est-dscr
                     bf-eb.i-%[i]    = k.
         end.
         else if (i > integer(eb.i-col)) and
                 (i <= (integer(eb.i-col) + 
                       integer(eb.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = if avail alt-item then alt-item.i-no else ""
                     bf-eb.i-dscr[i] = if avail alt-item then alt-item.est-dscr else ""
                     bf-eb.i-%[i]    = 100.
         end.
         else if (i >  (eb.i-col + eb.i-coat) )
         then do:
            assign bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  
        
         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (eb.i-pass) then counter = eb.i-pass.         
      end. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est B-table-Win 
PROCEDURE copy-from-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-new-eb for eb.
  def buffer bf-new-ef for ef.

  is-item-copied-from-est  = yes.

  find bf-new-eb where recid(bf-new-eb) = if recid(eb) <> ? then recid(eb) else lv-eb-recid .
  find bf-new-ef where recid(bf-new-ef) = if recid(eb) <> ? then recid(ef) else lv-ef-recid.
  
  buffer-copy xeb except xeb.company xeb.est-no xeb.form-no xeb.blank-no to bf-new-eb .
  buffer-copy xef except xef.company xef.est-no xef.form-no to bf-new-ef.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-from-est2 B-table-Win 
PROCEDURE copy-from-est2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       assign eb.len:screen-value in browse {&browse-name} = string(xeb.len)
              eb.wid:screen-value in browse {&browse-name} = string(xeb.wid)
              eb.dep:screen-value in browse {&browse-name} = string(xeb.dep)
              eb.procat:screen-value in browse {&browse-name} = xeb.procat
              eb.style:screen-value in browse {&browse-name} = xeb.style
              eb.stock-no:screen-value in browse {&browse-name} = xeb.stock-no
              eb.part-dscr1:screen-value in browse {&browse-name} = xeb.part-dscr1
              eb.part-no:screen-value in browse {&browse-name} = xeb.part-no
              ef.board:screen-value in browse {&browse-name} = xef.board
              ef.cal:screen-value in browse {&browse-name} = string(xef.cal)
              eb.i-col:screen-value in browse {&browse-name} = string(xeb.i-col)
              eb.i-coat:screen-value in browse {&browse-name} = string(xeb.i-coat)
              .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-inst B-table-Win 
PROCEDURE create-inst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-notes for notes.
  
  
  find cust where cust.cust-no eq eb.cust-no no-lock no-error.
  
  if avail cust then
  for each notes
      where notes.rec_key   eq cust.rec_key
        and notes.note_type eq "D"
        and notes.note_code ne ""
      no-lock:

    find first bf-notes
        where bf-notes.rec_key   eq est.rec_key
          and bf-notes.note_type eq notes.note_type
          and bf-notes.note_code eq notes.note_code
      no-lock no-error.
      
    if not avail bf-notes then do:
      create bf-notes.
      buffer-copy notes except notes.note_form_no to bf-notes
      assign
       bf-notes.rec_key   = est.rec_key
       bf-notes.note_date = today
       bf-notes.note_time = time.
    end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-prep B-table-Win 
PROCEDURE create-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN est/BuildDefaultPreps.p(BUFFER est,
                              BUFFER ef,
                              INPUT eb.form-no,
                              INPUT 0).
/*   def var i as int no-undo.                                                                                                                          */
/*   find last est-prep where est-prep.company = gcompany and                                                                                           */
/*                            est-prep.est-no = est.est-no and                                                                                          */
/*                            est-prep.eqty = est-qty.eqty                                                                                              */
/*                            no-lock no-error.                                                                                                         */
/*   i = if avail est-prep then est-prep.line + 1 else 1.                                                                                               */
/*                                                                                                                                                      */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:                                                                        */
/*       create est-prep.                                                                                                                               */
/*       assign est-prep.e-num  = est.e-num                                                                                                             */
/*              est-prep.company = est.company                                                                                                          */
/*              est-prep.est-no = est.est-no                                                                                                            */
/*              est-prep.eqty = est-qty.eqty                                                                                                            */
/*              est-prep.line   = i                                                                                                                     */
/*              est-prep.s-num  = eb.form-no                                                                                                            */
/*              est-prep.b-num  = 0 /*1 */                                                                                                              */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in                                                                   */
/*                                else if prep.mat-type eq "b" and  avail ef then ef.nsh-wid * ef.nsh-len /* ef.adh-sqin is 0 in Corrware - die inch */ */
/*                                else 1  /* mat-type eq "m" */                                                                                         */
/*             est-prep.code   = prep.code                                                                                                              */
/*             est-prep.dscr   = prep.dscr                                                                                                              */
/*             est-prep.cost   = prep.cost                                                                                                              */
/*             est-prep.ml     = prep.ml                                                                                                                */
/*             est-prep.simon  = prep.simon                                                                                                             */
/*             est-prep.mkup   = prep.mkup                                                                                                              */
/*             est-prep.amtz   = prep.amtz                                                                                                              */
/*             est-prep.mat-type = prep.mat-type.                                                                                                       */
/*             if lookup(est-prep.mat-type, "p,f") gt 0 then                                                                                            */
/*                run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty).                                                              */
/*             i = i + 1.                                                                                                                               */
/* /*message "prep crt: " i est-prep.qty prep.mat-type view-as alert-box.                                                                               */
/* */                                                                                                                                                   */
/*   end.                                                                                                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-est-childrecord B-table-Win 
PROCEDURE crt-est-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def buffer bb for eb.
  def buffer bf for ef.
 
  create est-qty.
  assign est-qty.company = gcompany
         est-qty.est-no =  est.est-no
         est-qty.eqty = 0
         est-qty.qty-date = est.est-date
         .
  create ef.
  assign
   ef.est-type  = est.est-type
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
  /*  ASI character
   ef.lsh-len   = ce-ctrl.ls-length
   ef.lsh-wid   = ce-ctrl.ls-width.
  */
   ef.lsh-wid   = ce-ctrl.ls-length
   ef.lsh-len   = ce-ctrl.ls-width.

  create eb.
  assign  eb.est-type = est.est-type
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = integer(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.tr-cas = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0
   eb.yld-qty = 1
   .

  /* ???? bugs : 2 records are created  , delete one ========== */
  for each bb where bb.est-no = "" :
      delete bb.
  end.
  for each bf where bf.est-no = "" :
      delete bf.
  end.
  /*========*/
  find first item where item.company = gcompany
                    and item.mat-type = "C"  /* Case/Bundle */
                    and item.i-no eq eb.cas-no
      no-lock no-error.
  if avail item then do:
     find first e-item where e-item.company eq item.company
                         and e-item.loc     eq item.loc
                         and e-item.i-no    eq item.i-no
        no-lock no-error.
     find first itemfg  where itemfg.company eq gcompany
                          and itemfg.i-no    eq eb.stock-no
        no-lock no-error.
     if avail e-item then
        assign  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = if avail itemfg then itemfg.case-count else e-item.box-case
                .
     if eb.cas-len eq 0 then eb.cas-len = item.case-l.
     if eb.cas-wid eq 0 then eb.cas-wid = item.case-w.
     if eb.cas-dep eq 0 then eb.cas-dep = item.case-d.
     if eb.cas-wt  eq 0 then eb.cas-wt  = item.avg-w.
     if eb.cas-pal eq 0 then eb.cas-pal = item.case-pall.
     if eb.cas-cnt eq 0 then eb.cas-cnt =
              if avail itemfg then itemfg.case-count else item.box-case.
  end.  /* avail item */

  i = 1.

  assign lv-eb-recid = recid(eb) 
         lv-ef-recid = recid(ef).
/* create after assign est,ef,eb records 
  for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:
      create est-prep.
      assign est-prep.e-num  = est.e-num
             est-prep.company = est.company
             est-prep.est-no = est.est-no
             est-prep.line   = i
             est-prep.s-num  = 1
             est-prep.b-num  = 1
             est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in
                               else if prep.mat-type eq "b" and  avail ef   
                               then ef.adh-sqin
                               else 1  /* mat-type eq "m" */
            est-prep.code   = prep.code
            est-prep.dscr   = prep.dscr
            est-prep.cost   = prep.cost
            est-prep.ml     = prep.ml
            est-prep.simon  = prep.simon
            est-prep.mkup   = prep.mkup
            est-prep.amtz   = prep.amtz
            est-prep.mat-type = prep.mat-type.
            if lookup(est-prep.mat-type, "p,f") gt 0 then
               run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty).
            i = i + 1.
message "prep crt: " i est-prep.qty prep.mat-type view-as alert-box.            
  end.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg B-table-Win 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

def input parameter v-item like itemfg.i-no.
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
*/
def var cocode as cha no-undo.
def var locode as cha no-undo.
def var v-alloc like itemfg.alloc init yes.
def var tmpstore as cha no-undo.
def var i as int no-undo.

assign cocode = gcompany
       locode = gloc
       .

{ce/msfcalc.i}
{oe/fgfreight.i}
/*
{sys/inc/setprint.i}
*/
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "SETPRINT"
    no-lock no-error.
if avail sys-ctrl then v-alloc = sys-ctrl.log-fld.
       
find first cust  where cust.company eq gcompany
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.
create itemfg.
assign
 itemfg.company    = gcompany
 itemfg.loc        = gloc
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = xeb.part-dscr1
 itemfg.part-dscr1 = xeb.part-dscr2
 itemfg.sell-uom   = "M"
 itemfg.part-no    = xeb.part-no
 itemfg.cust-no    = xeb.cust-no
 itemfg.cust-name  = if avail cust then cust.name else ""
 itemfg.pur-uom    = "M"
 itemfg.prod-uom   = "M"
 itemfg.alloc      = v-alloc
 itemfg.stocked    = yes
 itemfg.die-no     = xeb.die-no
 itemfg.plate-no   = xeb.plate-no
 itemfg.style      = xeb.style
 itemfg.procat     = xeb.procat
 itemfg.cad-no     = xeb.cad-no
 itemfg.upc-no     = xeb.upc-no
 itemfg.spc-no     = xeb.spc-no
 itemfg.isaset     = (xest.est-type eq 2 or xest.est-type eq 6) and
                     xeb.form-no eq 0
 itemfg.pur-man    = xeb.pur-man      
 itemfg.alloc      = itemfg.isaset.

 {oe/fgfreighta.i xeb}


IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

{sys/inc/fgcascnt.i itemfg xeb}

{sys/inc/updfgdim.i "xeb"}
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-est B-table-Win 
PROCEDURE crt-new-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var cocode as cha no-undo.
  def buffer bf-est for est.
  def buffer bb for eb.
  def var char-hdl as cha no-undo.
  def var lv-crt-est-rowid as rowid no-undo.
  def var ll-dumb as log no-undo.
    
  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */
  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc.
                          
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = yes.
  ce-ctrl.e-num = li-new-estnum.
  
  assign est.est-type = 5 + (if ls-add-what = "SET-SET" then 1 else 0)
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = string(li-new-estnum,">>>>>>>>")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         .
   {sys/ref/est-add.i est}     
   
   run crt-est-childrecord.  /* create ef,eb,est-prep */
   lv-crt-est-rowid = rowid(est).

   release est.
   release est-qty.
   release ef.
   release eb.

   /* refresh browser for new record */
   RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
/* may need to refresh  */
   RUN Refreshrow in widget-handle(char-hdl) ("NewRecord", lv-crt-est-rowid).
   ll-dumb =  br_table:refresh() in frame {&frame-name}.
   assign cocode = gcompany .      

/*   find est where rowid(est) = lv-crt-est-rowid no-lock no-error.
   display est.est-no est.est-date with browse {&browse-name}.
   disp eb.yld-qty with browse {&browse-name}.
 */

  release ce-ctrl.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-item B-table-Win 
PROCEDURE crt-new-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var cocode as cha no-undo.
  def buffer bf-est for est.
  def buffer bb for eb.
  
  message "new-item  1 "  "{&adm-first-enabled-table}" skip
          "2:" "{&adm-second-enabled-table}" " ,3:" "{&adm-third-enabled-table}"
          skip "gourp: " group-assign-target
           view-as alert-box.

  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */

  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           no-lock.
  li-new-estnum = ce-ctrl.e-num .
  ll-new-record = yes.
    
  assign est.est-type = 5
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = string(li-new-estnum,">>>>>>>>")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         .
  
   display est.est-no est.est-date with browse {&browse-name}.
            
   assign cocode = gcompany
         .      
   {sys/ref/est-add.i est}     

   run crt-est-childrecord.  /* create ef,eb,est-prep */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-set B-table-Win 
PROCEDURE crt-new-set :
/*------------------------------------------------------------------------------
  Purpose:  new item for set   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def buffer bb for eb.
  def buffer bf for ef.
  def buffer best for est.
  def var ls-part-no as cha no-undo.  /* set partno */
     
  /* not create est, est-qty, est-prep */
  
  ll-new-record = yes.
  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           no-lock.
          
  create ef. 
  assign
   ef.est-type  = 6
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = li-form# + 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-wid   = ce-ctrl.ls-length
   ef.lsh-len   = ce-ctrl.ls-width.

  find first bb where bb.company = est.company and
                      bb.est-no = est.est-no and
                      bb.form-no = 0 and
                      bb.blank-no = 0
                      no-lock no-error.
  if avail bb then ls-part-no = bb.part-no.
  else ls-part-no = "".

  find last bb where bb.company = est.company and
                      bb.est-no = est.est-no 
                      no-lock no-error.                      
  create eb. 
  assign  eb.est-type = 6
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = integer(est.est-no)
   eb.form-no  = li-form# + 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.tr-cas = 1
   eb.i-pass   = 0
   eb.cust-no = ls-cust-no
   eb.ship-id = ls-ship-id
   eb.yld-qty = 1
   eb.part-no = ls-part-no + "-" + string(eb.form-no)
   eb.tab-in = yes
   eb.len = 0
   eb.wid = 0
   eb.dep = 0
   eb.procat = if avail bb then bb.procat else ""
   .


  /* ???? bugs : 2 records are created  , delete one ========== */
  for each bb where bb.est-no = "" :
      delete bb.
  end.
  for each bf where bf.est-no = "" :
      delete bf.
  end.
  /*  ========*/
  find first item where item.company = gcompany
                    and item.mat-type = "C"  /* Case/Bundle */
                    and item.i-no eq eb.cas-no
      no-lock no-error.
  if avail item then do:
     find first e-item where e-item.company eq item.company
                         and e-item.loc     eq item.loc
                         and e-item.i-no    eq item.i-no
        no-lock no-error.
     find first itemfg  where itemfg.company eq gcompany
                          and itemfg.i-no    eq eb.stock-no
        no-lock no-error.
     if avail e-item then
        assign  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = if avail itemfg then itemfg.case-count else e-item.box-case
                .
     if eb.cas-len eq 0 then eb.cas-len = item.case-l.
     if eb.cas-wid eq 0 then eb.cas-wid = item.case-w.
     if eb.cas-dep eq 0 then eb.cas-dep = item.case-d.
     if eb.cas-wt  eq 0 then eb.cas-wt  = item.avg-w.
     if eb.cas-pal eq 0 then eb.cas-pal = item.case-pall.
     if eb.cas-cnt eq 0 then eb.cas-cnt =
              if avail itemfg then itemfg.case-count else item.box-case.
  end.  /* avail item */

  find best where recid(best) = recid(est) exclusive-lock.
  assign  best.form-qty = est.form-qty + 1
          best.est-type = 6.
    
  display eb.cust-no eb.ship-id eb.part-no eb.tab-in 
          eb.len eb.wid eb.dep eb.procat
          eb.yld-qty 
          with browse {&browse-name}.

  assign lv-eb-recid = recid(eb) 
         lv-ef-recid = recid(ef).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-est-childrecord B-table-Win 
PROCEDURE delete-est-childrecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  for each est-qty where est-qty.company = est.company 
                     and est-qty.est-no = est.est-no
                     :
     for each eb where eb.company = est.company 
                   and eb.est-no = est.est-no
                   and eb.eqty = est-qty.eqty:
         delete eb.             
     end.
     for each ef where ef.company = est.company 
                   and ef.est-no = est.est-no
                   and ef.eqty = est-qty.eqty:
         delete ef.             
     end.
     delete est-qty.
  end.   
  for each est-prep where est-prep.company = est.company
                      and est-prep.est-no = est.est-no:
      delete est-prep.
  end.
  for each est-op where est-op.company = est.company
                    and est-op.est-no = est.est-no:
      delete est-op.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  HIDE FRAME Corr.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  if not ll-is-add-from-tool then do:
     run est/d-addwh2.w  (output ls-add-what).  /*for combo estimate only */
     if ls-add-what = "set"    /* new estimate */
        then run est/d-addset.w (output ls-add-what). /* one item or set cec/est-add.p */
   
     if ls-add-what = "" then return no-apply.  /* cancel from dialog box */
  end.
  
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
  def var is-first-record as log init no no-undo.
  def buffer bf-eb for eb.  
  def buffer bf-ef for ef.
  def buffer bf-est for est.
  def var i as int no-undo.
  def var xx as dec no-undo.
  def var lv-hld-cust like eb.cust-no no-undo.
  def var lv-hld-ship like eb.ship-id no-undo.
  DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
  
  {sys/inc/ceroute.i C}
  
  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock no-error.
  if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock no-error.
  
  assign
   lv-hld-cust = eb.cust-no
   lv-hld-ship = eb.ship-id.
  IF AVAIL ef THEN
    lv-hld-board = ef.board.

  run get-attribute in adm-broker-hdl ('Is-First-Est').
  if return-value = "Yes" then do:
       is-first-record = yes.
       run set-attribute-list in adm-broker-hdl ('Is-First-Est=No'). /* reset */
  end.     
  else is-first-record = no.
  /* Change 4th enabled record lock status PROGRESS only does upto 3 records 
     est,est-qty,eb,ef (ef has still no-lock status) */
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.  
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */    
  /* convert to decimal from 1/16 */
  {sys/inc/k16bb.i eb.wid  } 
  {sys/inc/k16bb.i eb.len  } 
  {sys/inc/k16bb.i eb.dep  } 

  /*== update all eb,ef eqty field ==*/
  for each bf-eb where bf-eb.company = est-qty.company and
                       bf-eb.est-no = est-qty.est-no:
      assign bf-eb.eqty = est-qty.eqty.
  end.  
  for each bf-ef where bf-ef.company = est-qty.company and
                       bf-ef.est-no = est-qty.est-no:
         bf-ef.eqty = est-qty.eqty.
  end.

  find bf-est where bf-est.company = est-qty.company and
                    bf-est.est-no = est-qty.est-no.
  bf-est.est-qty[1] = est-qty.eqty.                  
  if not ll-is-copy-record and (ll-new-record or is-first-record)
  then do:
     if eb.stock-no = "" then do:
        find first ce-ctrl where ce-ctrl.company = gcompany and
                                 ce-ctrl.loc = gloc
                                 no-lock no-error.
        eb.cas-no = ce-ctrl.def-case.
        eb.tr-no = ce-ctrl.def-pal.      
     end.
     find cust where cust.company = gcompany and
                     cust.cust-no = eb.cust-no
                     no-lock no-error.
     eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
     eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.      
     /* get default values from rm table */
     find item where item.company = eb.company and
                     item.i-no = eb.cas-no
              no-lock no-error.
     if avail item then assign /*eb.cas-cost:Screen-value = */
                              eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     find item where item.company = eb.company and
                     item.i-no = eb.tr-no
              no-lock no-error.
     if avail item then assign /*eb.cas-cost:Screen-value = */
                              eb.tr-len = (item.case-l)
                              eb.tr-wid = (item.case-w)
                              eb.tr-dep = (item.case-d)
                              .
     find first shipto where shipto.company = est.company and
                             shipto.cust-no = eb.cust-no
                             no-lock no-error.
     if avail shipto then assign eb.ship-id = shipto.ship-id
                                 eb.carrier = shipto.carrier
                                 eb.ship-name = shipto.ship-name
                                 eb.ship-addr[1] = shipto.ship-addr[1]
                                 eb.ship-addr[2] = shipto.ship-addr[2]
                                 eb.ship-city = shipto.ship-city
                                 eb.ship-state = shipto.ship-state
                                 eb.ship-zip = shipto.ship-zip
                                 .
     find style where style.company = est.company and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.
     if avail style then do:
        assign eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.k-len = style.dim-dkl
               eb.k-wid = style.dim-dkw
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.                 
               .
     end.  /* avail style */ 
     run calc-pass.
     run calc-blank-size.
     if eb.gluelap <> 0 then eb.lin-in = eb.dep.    
     if not avail cust then find cust where cust.company = eb.company and
                                 cust.cust-no = eb.cust-no
                                 no-lock no-error.
     eb.sman = cust.sman.   
     find first sman where sman.company eq cust.company
                       and sman.sman    eq cust.sman
                     no-lock no-error.
     find first custype where custype.custype eq cust.type
              no-lock no-error.
     if avail sman then do:
        find first sman-mtx where sman-mtx.company eq eb.company
                    and sman-mtx.sman    eq sman.sman
                    and sman-mtx.custype eq cust.type
                  no-lock no-error.
        eb.comm = if avail sman-mtx then sman-mtx.type-comm
                  else sman.scomm.           
     end.
     else do:
          if avail custype then eb.comm = custype.commrate.
          else do:
                find ce-ctrl where ce-ctrl.company = eb.company and
                                   ce-ctrl.loc = eb.loc
                             no-lock no-error.
                eb.comm = ce-ctrl.comm-mrkup.
          end.
     end.
     find first shipto where shipto.company = est.company and
                             shipto.cust-no = eb.cust-no
                             no-lock no-error.
     if avail shipto then assign eb.ship-id = shipto.ship-id
                                 eb.carrier = shipto.carrier
                                 eb.ship-name = shipto.ship-name
                                 eb.ship-addr[1] = shipto.ship-addr[1]
                                 eb.ship-addr[2] = shipto.ship-addr[2]
                                 eb.ship-city = shipto.ship-city
                                 eb.ship-state = shipto.ship-state
                                 eb.ship-zip = shipto.ship-zip
                                 .
     if caps(cust.cust-no) ne "TEMP" then
        assign
             eb.sman          = cust.sman
             eb.ship-id       = shipto.ship-id
             eb.ship-name     = shipto.ship-name
             eb.ship-addr[1]  = shipto.ship-addr[1]
             eb.ship-addr[2]  = shipto.ship-addr[2]
             eb.ship-city     = shipto.ship-city
             eb.ship-state    = shipto.ship-state
             eb.ship-zip      = shipto.ship-zip.
      find first ce-ctrl where ce-ctrl.company eq eb.company
                           and ce-ctrl.loc     eq eb.loc
                no-lock no-error.
      assign ef.lsh-wid = ce-ctrl.ls-length
             ef.lsh-len = ce-ctrl.ls-width
             ef.gsh-len = ef.lsh-len
             ef.gsh-wid = ef.lsh-wid
             ef.nsh-len = ef.lsh-len
             ef.nsh-wid = ef.lsh-wid
             ef.n-out   = 1
             ef.n-out-l = 1
             ef.n-out-d = 1
             ef.lam-dscr = "S".

     find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = eb.style
                           and (reftable.loc = eb.flute or lv-foam)
                           and reftable.code = "BOARD"
                        no-lock no-error.
     if avail reftable and reftable.dscr <> "" then do:
        find first item where item.company = gcompany and
                              item.i-no = reftable.dscr
                              no-lock no-error.
        if avail item then do:
           assign /*ef.board = item.i-no */
                  ef.brd-dscr = item.est-dscr 
                  ef.i-code = item.i-code
                  ef.flute = item.flute
                  ef.test = item.reg-no
                  ef.weight = item.basis-w
                  .
           if item.i-code = "R" then assign ef.lsh-len = item.s-len
                                            ef.lsh-wid = item.s-wid
                                            ef.gsh-wid = item.s-wid.
           if item.r-wid <> 0 then assign ef.roll = true
                                          ef.roll-wid = item.r-wid
                                          ef.lsh-wid = item.r-wid
                                          ef.gsh-wid = item.r-wid.                                        
        end.          
     end.  /* avail reftable */
     if style.material[4] ne "" then do:  /* adder*/ 
               find first item  where item.company eq cocode
                     and item.i-no    eq style.material[4]
                   no-lock no-error.
               if avail item then
                  do i = 1 to 6:  
                     if ef.adder[i] = "" then do:
                         assign ef.adder[i]     = item.i-no
                                ef.adder[i + 6] = item.est-dscr
                                ef.weight       = ef.weight + item.basis-w.
                         leave.       
                     end.           
                  end.
            end.
      if style.material[5] ne "" then do:  /* leaf label */
               find first item  where item.company eq cocode
                                  and item.i-no    eq style.material[5]
                   no-lock no-error.
               if avail item then /*leaf-block:
               for each ef where ef.company eq xest.company and
                                 ef.est-no = xest.est-no,
                   first eb of ef no-lock:  */
                  do i = 1 to 2:
                     if ef.leaf[i] = "" then do:
                        assign ef.leaf-snum[i] = ef.form-no
                               ef.leaf-bnum[i] = 1
                               ef.leaf[i]      = item.i-no
                               ef.leaf-dscr[i] = item.est-dscr
                               ef.leaf-l[i] = eb.t-len
                               ef.leaf-w[i] = eb.t-wid
                               .
                         leave.
                     end.   
                  end.
            /*   end. */
     end.
     ef.xgrain = "N".
     find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).  
     if lv-foam then assign xeb.t-wid = xeb.wid
                         xeb.t-len = xeb.len
                         xeb.t-dep = xeb.dep
                         .
     run cec/calc-dim.p.
     run create-inst.
     run create-prep.
     def var lv-cas-pal as dec no-undo.
     def var lv-tr-cnt as int no-undo.
     def var lv-numstack as int no-undo.
     def var lv-stackcode as cha no-undo.
     def var lv-error as log no-undo.
     run cec/kpallet.p (recid(xeb), output lv-cas-pal, output lv-tr-cnt,
      output lv-numstack, output lv-stackcode, output lv-error).
     if lv-error then do:
        message "An error occured while attempting to calculate the number of pallets. "
                skip
                "Please review any previous error messages for more information." 
                 view-as alert-box error.
     end.
     else assign eb.cas-pal = lv-cas-pal
                 eb.tr-cnt = lv-tr-cnt.
                 
     if not lv-foam then do:
       {sys/inc/ceroute1.i w id l en} 
     end.
   end.  /* new not copy */
   
   {ce/uship-id.i ll-new-record}

   if not ll-is-copy-record and ceroute-log then do:
     find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.line    ge 500:
       delete est-op.
     end.
    
     if can-find(first est-op where est-op.company eq xest.company
                                and est-op.est-no  eq xest.est-no
                                and est-op.s-num   eq xef.form-no) then
     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.s-num   eq xef.form-no
         no-lock:
     end.
  
     else do:
        /* Protect existing est-op records */
       for each est-op
           where est-op.company eq xest.company
           and est-op.est-no    eq xest.est-no:
         if not est-op.auto then est-op.line = est-op.line + 500.
                            else est-op.auto = no.
       end.
                                 
       xx = dec(xef.form-no).
    
       run cec/mach-seq.p (xef.form-no).
    
       for each est-op
           where est-op.company eq xest.company
             and est-op.est-no  eq xest.est-no
             and est-op.s-num   ne int(xx):
          
         if est-op.auto then delete est-op.
         else
         if est-op.line ge 500 then est-op.line = est-op.line - 500.
                               else est-op.auto = yes.
       end.
     end.
   end.                         
  
  if ll-crt-itemfg then do:
     find xest where recid(xest) = recid(est) no-lock no-error.
     find xeb where recid(xeb) = recid(eb) no-lock no-error.
     find xef where recid(xef) = recid(ef) no-lock no-error.
     run crt-itemfg (xeb.stock-no).
     ll-crt-itemfg = no.
  end.
  /*== Qty assignment ==*/
  run assign-qty.

  /* Reset cust-no ship-id ... for set */
  if eb.est-type = 6 then 
     for each bf-eb where bf-eb.company = eb.company and
                          bf-eb.est-no = eb.est-no :
         assign bf-eb.cust-no = eb.cust-no
                bf-eb.ship-id = eb.ship-id
                .              
   end.        
   if est.est-type = 6 then do:
        def var li-num-eb as int no-undo.
        li-num-eb = 0.
        for each bf-eb where bf-eb.company = est.company and
                             bf-eb.est-no = est.est-no and
                             bf-eb.form-no > 0 and
                             bf-eb.blank-no > 0 no-lock:
            li-num-eb = li-num-eb + 1. 
        end.
        if li-num-eb <= 1 then do:
           assign est.est-type = 5
                  eb.est-type = est.est-type
                  ef.est-type = est.est-type
                  .
        end.    
   end.
     
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   

    /* Code placed here will execute PRIOR to standard behavior. */

    if avail est then li-form# = est.form-qty. /* for set creation on crt-new-set */
   
    if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock no-error.
    if not avail eb then find eb where recid(eb) =  lv-eb-recid no-lock no-error.

    ls-cust-no = if avail eb then eb.cust-no else "" .  /* for new item record */
    ls-ship-id = if avail eb then eb.ship-id else "" .  /* for crt-new-set */

  /* Dispatch standard ADM method.                             */
  if ls-add-what begins "set" then do: /* new estimate */
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  end.   
  else do:  /* item */
   /*    create eb.
       create ef.
    */   
  end.
  /* Code placed here will execute AFTER standard behavior.    */
  ls-set-part-no = "".
  if ls-add-what begins "set" then do:  /* new estimate: set-set or set-item */
     run crt-new-est.
     if ls-add-what = "set-set" then do:
        run est/crt-set.w (rowid(est), output ls-set-part-no).
        /*def var phandle as widget-handle no-undo.
        def var char-hdl as cha no-undo.   
        RUN get-link-handle IN adm-broker-hdl
            (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
        phandle = WIDGET-HANDLE(char-hdl).
        RUN new-state in phandle ('update-begin':U). */
        if ls-set-part-no <> "" then do:
           find xeb where recid(xeb) = recid(eb).
           assign xeb.part-no = entry(1,ls-set-part-no) + "-1"
                  xeb.part-dscr1 = entry(2,ls-set-part-no)
                  xeb.part-dscr2 = entry(3,ls-set-part-no)
                  xeb.stock-no = if entry(4,ls-set-part-no) <> "" then (entry(4,ls-set-part-no) + "-1") else ""
                  .
           release xeb.
        end.
     end.       

     RUN get-link-handle IN adm-broker-hdl
            (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
     phandle = WIDGET-HANDLE(char-hdl). 
     RUN new-state in phandle ('update-begin':U).  /* to have save button */

     display est.est-no est.est-date with browse {&browse-name}.          
     disp eb.yld-qty with browse {&browse-name}.

  end.
  else if ls-add-what = "item" then do:
      run crt-new-set.
      if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock.
      apply "entry" to eb.part-no in browse {&browse-name}.
      
      return no-apply.
      
  /*  not working
  assign est.est-no:sensitive in browse {&browse-name} = no
         est.est-date:sensitive = no
         eb.cust-no:sensitive = no
         eb.ship-id:sensitive = no.
  */       
  end.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-comp like est.company no-undo.
  def var lv-est-no like est.est-no no-undo.
  def buffer bb for eb.
  def buffer bf for ef.  
  def buffer best for est.
  def var li-num-of-efrec as int no-undo.
  def var ll-dum as log no-undo.
  def var li-est-type as int no-undo.
  def var char-hdl as cha no-undo.  
  
  li-num-of-efrec = 0.
  for each bf where bf.company = est.company
                and bf.est-no = est.est-no
              no-lock:
      li-num-of-efrec = li-num-of-efrec + 1. 
  end.
  
  /* Code placed here will execute PRIOR to standard behavior. */
   MESSAGE "Delete Currently Selected Record?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
   IF not response  THEN RETURN "ADM-ERROR":U.

   assign lv-comp = est.company
          lv-est-no = est.est-no
          li-est-type = est.est-type.
   if not avail eb then find eb where recid(eb) = lv-eb-recid.
   if not avail ef then find ef where recid(ef) = lv-ef-recid.             

  /* Dispatch standard ADM method.                             */
  if est.est-type = 5 then 
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  else do:  /* type 6 */
     /* =======if need to delete all items
     find best where best.company = lv-comp 
                 and best.est-no = lv-est-no .
     for each bf where bf.company = lv-comp and
                       bf.est-no = lv-est-no :
         for each bb where bb.company = lv-comp
                       and bb.est-no = lv-est-no:
             delete bb.          
         end.          
         delete bf.
             
     end.                  
     =========     */

     /* single item delete */                  
     find bb where recid(bb) = recid(eb).
     delete bb.
     find bf where recid(bf) = recid(ef).

     for each est-op where est-op.company = lv-comp
                       and est-op.est-no = lv-est-no 
                       and est-op.s-num = bf.form-no:
         delete est-op.
     end.
     for each est-prep where est-prep.company = lv-comp
                       and est-prep.est-no = lv-est-no 
                       and est-prep.s-num = bf.form-no:
         delete est-prep.
     end.

     delete bf.

     ll-dum = browse {&browse-name}:delete-current-row().
     find best where recid(best) = recid(est).
     best.form-qty = best.form-qty - 1.
     /* after delete */
     find first bf where bf.company = lv-comp and
                         bf.est-no = lv-est-no no-lock no-error.
     if not avail bf then do:
        for each est-qty where est-qty.company = lv-comp 
                        and est-qty.est-no = lv-est-no :
            delete est-qty.
        end.
        for each est-prep where est-prep.company = lv-comp
                         and est-prep.est-no = lv-est-no:
            delete est-prep.
        end.
        for each est-op where est-op.company = lv-comp
                          and est-op.est-no = lv-est-no:
            delete est-op.
        end.
        for each est where est.company = lv-comp and
                           est.est-no = lv-est-no:
            delete est.                        
        end.
        run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl). 
        run dispatch in widget-handle(char-hdl) ('get-next').
     end.   
  end.
  
  /* Code placed here will execute AFTER standard behavior.    */
  /* run delete-est-childrecord.  not working */

  if not avail est /* = est.est-type = 5*/  or li-num-of-efrec <= 1 then do:
     for each est-qty where est-qty.company = lv-comp 
                        and est-qty.est-no = lv-est-no :
         delete est-qty.
     end.
     for each est-prep where est-prep.company = lv-comp
                         and est-prep.est-no = lv-est-no:
         delete est-prep.
     end.
     for each est-op where est-op.company = lv-comp
                       and est-op.est-no = lv-est-no:
         delete est-op.
     end. 
     for each est where est.company = lv-comp and
                        est.est-no = lv-est-no:
         delete est.                        
     end.
  end. 
  if li-est-type = 5 then do:
     run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl). 
     run dispatch in widget-handle(char-hdl) ('get-next').
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
   if not avail eb and lv-eb-recid <> ? then find eb where recid(eb) = lv-eb-recid.
   if not avail ef and lv-ef-recid <> ? then find ef where recid(ef) = lv-ef-recid.             

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     if not avail eb then return.
     find style where style.company = gcompany and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.   
     lv-foam = if avail style and style.type = "F" then yes else no.


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
  if not avail eb and lv-eb-recid <> ? then find eb where recid(eb) = lv-eb-recid.
  if not avail ef and lv-ef-recid <> ? then find ef where recid(ef) = lv-ef-recid.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
message "local exit".
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
  assign lv-eb-recid = recid(eb)
         lv-ef-recid = recid(ef).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-rowid as rowid no-undo.
  def var ll-dumb as log no-undo.
  def var char-hdl as cha no-undo.
  def var li-row-num as int no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  /* == validation ===== */
     if eb.part-no:screen-value in browse {&browse-name} = "" then do:
        message "Customer Part # can not be blank!" view-as alert-box error.
        apply "entry" to eb.part-no.
        return no-apply.
     end.
     if int(est-qty.eqty:screen-value ) <= 0 then do:
        message "Quantity must be entered. " view-as alert-box error.
        apply "entry" to est-qty.eqty.
        return no-apply.
     end.

     if /*(eb.style:screen-value in browse {&browse-name} <> "" and */
        not can-find(style where style.company = gcompany and
                               style.style = eb.style:screen-value in browse {&browse-name})
     then do:
        if eb.style:screen-value = "" then message "Valid Style must be entered. Try Help." view-as alert-box error.
        else message "Invalid Style. Try Help. " view-as alert-box error.
        apply "entry" to eb.style.
        return no-apply.
     end. 
     if /* eb.cust-no:screen-value <> "" and */
        not can-find(cust where cust.company = gcompany and cust.cust-no = eb.cust-no:screen-value)
     then do:
        message "Invalid Customer Number. Try Help." view-as alert-box error.
        apply "Entry" to eb.cust-no.
        return no-apply.
     end.

     if not can-find(first flute where flute.code = eb.flute:screen-value in browse {&browse-name})
     then do:
        message "Invalid Flute Code." view-as alert-box error.
        apply "entry" to eb.flute.
        return no-apply.
     end.

     if ef.board:screen-value in browse {&browse-name} <> "" and
        not can-find(item where item.company = gcompany
                     and item.i-no = ef.board:screen-value in browse {&browse-name} )
     then do:
        message "Invalid Board. Try Help. " view-as alert-box error.
        apply "entry" to ef.board.
        return no-apply.
     end.
     if /*eb.procat:screen-value in browse {&browse-name} <> "" and */
       not can-find(first fgcat where fgcat.company = gcompany and
                    fgcat.procat = eb.procat:screen-value )
     then do:
         message "Invalid Category. Try Help." view-as alert-box error.
         apply "entry" to eb.procat.
         return no-apply.
     end.
     if decimal(eb.len:screen-value) = 0 then do:
        message "Length can not be 0. " view-as alert-box error.
        apply "entry" to eb.len.
        return no-apply.
     end.
     if decimal(eb.wid:screen-value) = 0 then do:
        message "Width can not be 0. " view-as alert-box error.
        apply "entry" to eb.wid.
        return no-apply.
     end.
     if decimal(eb.wid:screen-value) - trunc(decimal(eb.wid:screen-value),0) >= 0.16 
     then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to eb.wid.
             return no-apply.
     end.
     if decimal(eb.len:screen-value) - trunc(decimal(eb.len:screen-value),0) >= 0.16 
     then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to eb.len.
             return no-apply.
     end.
     if decimal(eb.dep:screen-value) - trunc(decimal(eb.dep:screen-value),0) >= 0.16 
     then do:
             message "Can not have more than .15 as decimal, field is (inches.16ths) "
                      view-as alert-box error.
             apply "entry" to eb.dep.
             return no-apply.
     end.
     

  /* ====== end validation =======*/
  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock no-error.
  if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock no-error.
  lv-rowid = rowid(est).
  li-row-num = browse {&browse-name}:focused-row.

  /* === check record locked ==== */
  FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  if not avail est then do:
     message "Estimate Record is being changed by someone else. Wait a few minutes and try again. " view-as alert-box error.
     return no-apply.
  end.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* assign next number when record get created 
  if ll-new-record and ls-add-what begins "set" then do:
     find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           exclusive-lock.
     ce-ctrl.e-num = li-new-estnum.
  end.
  */

/*if ll-new-record then do:  refresh for eqty in update mode   */
      RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).

/*  redundant procedure I think 
     run dispatch in widget-handle(char-hdl) ('open-query').    
     run reposition-query in widget-handle(char-hdl) (this-procedure).
     run dispatch in widget-handle(char-hdl) ("row-changed"). /* tell to parent */
 */  
     run dispatch ('open-query').
     reposition {&browse-name} to rowid rowid(ef).
     run notify ('row-available').
     
     run New_Record in widget-handle(char-hdl) (rowid(est)). 
     
    /* ll-dumb = browse {&browse-name}:select-focused-row().*/
/*  end. */

ll-is-add-from-tool = no.  /* reset */
run get-attribute ('adm-new-record') .
if return-value = "Yes" then run set-attribute-list ("ADM-NEW-RECORD = 'NO'").

session:set-wait-state("").
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
  run dispatch ('apply-entry').
  
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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as DEC DECIMALS 6 no-undo.
  
  if ip-is-corr-style and ip-dim <> 0 AND v-cecscrn-char NE "Decimal" then 
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.

  RETURN out-dim.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

