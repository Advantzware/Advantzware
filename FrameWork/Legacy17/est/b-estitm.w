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
{custom/globdefs.i}

{custom/gcompany.i}
{custom/gloc.i}
def var ls-add-what as cha no-undo.
def var li-new-estnum as int no-undo.
def var ll-new-record as log no-undo.
def var ll-is-copy-record as log no-undo.
def var char-val as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.
def new shared var formule as de extent 12 .
def var lv-part-no-prev like eb.part-no no-undo.
def var lv-eb-recid as recid no-undo.
def var lv-ef-recid as recid no-undo.
def buffer bf-eb for eb.
def buffer bf-ef for ef.
def buffer bf-est for est.
def var is-item-copied-from-est as log no-undo.
define new shared var cocode as cha no-undo.  /* for sup-programs */
define new shared var locode as cha no-undo.  /* for sup-programs */
/*def new shared temp-table formule field formule as dec extent 12. */
def var ls-prev-val as cha no-undo.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
def var lv-copy-qty as int extent 20 no-undo.
def var lv-copy-pr as dec extent 20 no-undo.
def var lv-copy-uom as cha extent 20 no-undo.
def var lv-copy-date as date extent 20 no-undo.
def var lv-estqty-recid as recid no-undo.
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
def var k_frac as dec init 6.25 no-undo.

ASSIGN cocode = g_company
       locode = g_loc.
{sys/inc/f16to32.i}

DEF BUFFER recalc-mr FOR reftable.

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
&Scoped-define EXTERNAL-TABLES est est-qty
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, est-qty.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table est.est-no est.est-date ~
eb.cust-no eb.ship-id eb.part-no eb.part-dscr1 eb.stock-no est-qty.eqty ~
eb.style ef.board ef.cal eb.procat eb.wid eb.len eb.dep eb.i-col eb.i-coat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table est.est-date ~
eb.cust-no eb.ship-id eb.part-no eb.part-dscr1 eb.stock-no est-qty.eqty ~
eb.style ef.board ef.cal eb.procat eb.wid eb.len eb.dep eb.i-col eb.i-coat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table est eb est-qty ef
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table est
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table eb
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-Browser-Table est-qty
&Scoped-define FOURTH-ENABLED-TABLE-IN-QUERY-Browser-Table ef
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.eqty = ef.eqty NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ef WHERE ef.company = est-qty.company ~
  AND ef.est-no = est-qty.est-no ~
  AND ef.eqty = est-qty.eqty NO-LOCK, ~
      EACH eb WHERE eb.company = ef.company ~
  AND eb.est-no = ef.est-no ~
  AND eb.eqty = ef.eqty NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ef eb
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ef
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table eb


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
     SIZE 49 BY 1 NO-UNDO.

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
      ef, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      est.est-no COLUMN-LABEL "Est #" FORMAT "x(5)":U
      est.est-date FORMAT "99/99/9999":U
      eb.cust-no FORMAT "x(8)":U
      eb.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U
      eb.part-no FORMAT "x(15)":U
      eb.part-dscr1 COLUMN-LABEL "Description" FORMAT "x(30)":U
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U
      est-qty.eqty FORMAT ">>>>>>9":U
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U
      ef.board FORMAT "x(10)":U
      ef.cal FORMAT "9.99999":U
      eb.procat COLUMN-LABEL "Cat" FORMAT "x(5)":U
      eb.wid FORMAT ">9.99999":U
      eb.len FORMAT ">9.99999":U
      eb.dep FORMAT ">9.99999":U
      eb.i-col FORMAT ">9":U
      eb.i-coat FORMAT ">9":U
  ENABLE
      est.est-date
      eb.cust-no
      eb.ship-id
      eb.part-no
      eb.part-dscr1
      eb.stock-no
      est-qty.eqty
      eb.style
      ef.board
      ef.cal
      eb.procat
      eb.wid
      eb.len
      eb.dep
      eb.i-col
      eb.i-coat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 147 BY 14.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 15.52 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 15.52 COL 80 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 15.52 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 15.52 COL 2
     RECT-4 AT ROW 15.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.est,ASI.est-qty
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
         HEIGHT             = 15.71
         WIDTH              = 147.6.
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
     _TblList          = "ASI.ef WHERE ASI.est-qty ...,ASI.eb WHERE ASI.ef ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _JoinCode[1]      = "ASI.ef.company = ASI.est-qty.company
  AND ASI.ef.est-no = ASI.est-qty.est-no
  AND ASI.ef.eqty = ASI.est-qty.eqty"
     _JoinCode[2]      = "ASI.eb.company = ASI.ef.company
  AND ASI.eb.est-no = ASI.ef.est-no
  AND ASI.eb.eqty = ASI.ef.eqty"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" "Est #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.est.est-date
"est.est-date" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.cust-no
"eb.cust-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.eb.ship-id
"eb.ship-id" "Ship To" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.eb.part-dscr1
"eb.part-dscr1" "Description" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.est-qty.eqty
"est-qty.eqty" ? ">>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ef.board
"ef.board" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ef.cal
"ef.cal" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.eb.procat
"eb.procat" "Cat" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.eb.wid
"eb.wid" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.eb.len
"eb.len" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.eb.dep
"eb.dep" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.eb.i-col
"eb.i-col" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.eb.i-coat
"eb.i-coat" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
     def var ls-cur-val as cha no-undo.
     def var lv-eb-tmpid as recid no-undo.
     def var lv-handle as handle no-undo.          
     def var char-val2 as cha no-undo.        
     def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-rowid as rowid no-undo.


   case focus:name :
     when "part-no" then do: 
           run est/l-ebrfqP.w (gcompany, gloc, focus:screen-value, output lv-eb-tmpid) .
           if lv-eb-tmpid = ?  then return no-apply.
           find xeb where recid(eb) = lv-eb-tmpid no-lock no-error.
           find xef where xef.company = xeb.company and
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
           run windows/l-style.w (gcompany,ls-cur-val, output char-val).
           if char-val <> "" then do:
              focus:screen-value in browse {&browse-name} =  entry(1,char-val).
              find style where style.company = gcompany and
                               style.style = eb.style:screen-value in browse {&browse-name}
                         no-lock no-error.            
              if avail style then 
                 assign ef.board:screen-value in browse {&browse-name} = style.material[1].

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
           else run windows/l-board1.w (eb.company,lv-ind,focus:screen-value, output lv-rowid).
           FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN 
              assign ef.board:screen-value in browse {&browse-name} = item.i-no
                     ef.cal:screen-value in browse {&browse-name}   = string(item.cal).

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
                            lv-copy-pr[1] = decimal(entry(11,char-val))
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
                            lv-copy-pr[11] = decimal(entry(11,char-val2))
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
                            .
             return no-apply.
       end.
       otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.
             
           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
   /*===================
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
                 /*assign eb.ship-id:screen-value = cust.name
                     rfq.ship-addr[1]:screen-value = cust.addr[1]
                     rfq.ship-addr[2]:screen-value = cust.addr[2]
                     rfq.ship-city:screen-value =    cust.city
                     rfq.ship-state:screen-value =   cust.state
                     rfq.ship-zip:screen-value =     cust.zip
                     rfq.sman:screen-value in frame {&frame-name} = if avail cust then cust.sman else ""
                     rfq.fob-code:screen-value in frame {&frame-name} = if cust.fob-code = "Dest" then "D"
                                                                        else if cust.fob-code = "orig" then "O"
                                                                        else ""
                     rfq.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "B" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else ""
                     .                
   
                 find sman where sman.company = gcompany and
                              sman.sman = cust.sman:screen-value
                              no-lock no-error.
                 assign sman_sname:screen-value = if avail sman then sman.sname else ""
                     rfq.comm:screen-value = if avail sman then string(sman.scomm) else "0"
                     .
                     */
                 if avail cust then do:
                    find first shipto where shipto.company = gcompany
                                        and shipto.cust-no = cust.cust-no
                                        no-lock no-error.
                    eb.ship-id:screen-value = if avail shipto then shipto.ship-id else "".
                 end.    
              end.  /* cust-no */
       ======================*/
           end.   /* g_lookup-var <> "" */
           return no-apply.
        end.  /* otherwise */
  end case.  
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
     /*{src/adm/template/brsleave.i} */
     {est/brsleave.i}   /* same but update will be like add */

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


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.part-no IN BROWSE Browser-Table /* Cust Part # */
DO:
   def var lv-eb-tmpid as recid no-undo.

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

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.stock-no IN BROWSE Browser-Table /* FG Item# */
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
             find xest where recid(xest) = recid(est) no-lock no-error.
             find xeb where recid(xeb) = recid(eb) no-lock no-error.
             find xef where recid(xef) = recid(ef) no-lock no-error.
             run crt-itemfg (input self:screen-value).
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
          find xest where recid(xest) = recid(est) no-lock no-error.
          find xeb where recid(xeb) = recid(eb) no-lock no-error.
          find xef where recid(xef) = recid(ef) no-lock no-error.
          run crt-itemfg (input self:screen-value).
       end.   
       return no-apply.        
    end.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.style IN BROWSE Browser-Table /* Style */
DO:
   if is-item-copied-from-est then return.
   
   if eb.style:modified in browse {&browse-name} then do:
     find style where style.company = gcompany and
                      style.style = eb.style:screen-value in browse {&browse-name}
                      no-lock no-error.   
     if avail style then 
        assign ef.board:screen-value in browse {&browse-name} = style.material[1].
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.board IN BROWSE Browser-Table /* Board */
DO:

   find item where item.company = gcompany
                and item.i-no = self:screen-value in browse {&browse-name}
                    no-lock no-error.
    if avail item then 
       assign ef.cal:screen-value in browse {&browse-name} = string(item.cal)
              .               

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
assign cocode = gcompany
       locode = gloc.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size B-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* calc blank W,L SqIn */
   def buffer bf-eb for eb .
   
   find xest where recid(xest) = recid(est) no-lock.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   if avail style then do:
      run est/u2kinc1.p .
      run est/u2kinc2.p .
      
      find bf-eb of eb exclusive-lock.    
      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          .
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
      counter = 1.
      choice = true.
/*    do i = 1 to 10:
       if eb.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
 
      find bf-eb of eb exclusive-lock.    
      if choice then do i = 1 to 10:
         if i le integer(eb.i-col:screen-value) then do with frame {&frame-name}:
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
                     bf-eb.i-code[i] = alt-item.i-no
                     bf-eb.i-dscr[i] = alt-item.est-dscr
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

message "copy-from-est     " 
        recid(xef) "," recid(ef) skip
        recid(xeb) "," recid(eb) .
          
  find bf-new-eb where recid(bf-new-eb) = recid(eb).
  find bf-new-ef where recid(bf-new-ef) = recid(ef).
  
  buffer-copy xeb except xeb.company xeb.est-no to bf-new-eb .
  buffer-copy xef except xef.company xef.est-no to bf-new-ef.
  
  
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
         est-qty.eqty = 0.
          
  create ef.
  assign
   ef.est-type  = 1
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-len   = ce-ctrl.ls-length
   ef.lsh-wid   = ce-ctrl.ls-width.

  create eb.
  assign  eb.est-type = 1
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = integer(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0.

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

    RUN est/BuildDefaultPreps.p(BUFFER est,
                              BUFFER ef,
                              INPUT 1,
                              INPUT 1).
/*   i = 1.                                                                                */
/*                                                                                         */
/*                                                                                         */
/*   for each prep where prep.company = gcompany and prep.dfault eq yes no-lock:           */
/*       create est-prep.                                                                  */
/*       assign est-prep.e-num  = est.e-num                                                */
/*              est-prep.company = gcompany                                                */
/*              est-prep.est-no = est.est-no                                               */
/*              est-prep.line   = i                                                        */
/*              est-prep.s-num  = 1                                                        */
/*              est-prep.b-num  = 1                                                        */
/*              est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in      */
/*                                else if prep.mat-type eq "b" and  avail ef               */
/*                                then ef.adh-sqin                                         */
/*                         else 1  /* mat-type eq "m" */                                   */
/*             est-prep.code   = prep.code                                                 */
/*             est-prep.dscr   = prep.dscr                                                 */
/*             est-prep.cost   = prep.cost                                                 */
/*             est-prep.ml     = prep.ml                                                   */
/*             est-prep.simon  = prep.simon                                                */
/*             est-prep.mkup   = prep.mkup                                                 */
/*             est-prep.amtz   = prep.amtz                                                 */
/*             est-prep.mat-type = prep.mat-type.                                          */
/*             if lookup(est-prep.mat-type, "p,f") gt 0 then                               */
/*                run sys/inc/flm-prep.p(recid(est), est-prep.s-num, output est-prep.qty). */
/*             i = i + 1.                                                                  */
/*   end.                                                                                  */

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
 itemfg.alloc      = v-alloc.

 /* Create an itemfg-loc for the default warehouse */
 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

IF v-FGFreightClass AND xeb.dest-code GT "" AND itemfg.frt-class = "" THEN
     itemfg.frt-class = xeb.dest-code.

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

  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */
  find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           no-lock.
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = yes.
    
  assign est.est-type = 1
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-new-item B-table-Win 
PROCEDURE crt-new-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
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
  def input parameter ip-est-no like est.est-no no-undo.
  def input parameter ip-comp like est.company no-undo.
  def buffer d-est-qty for est-qty.
    
message "del-ch   " ip-est-no ip-comp.

  for each d-est-qty where est-qty.company = ip-comp 
                     and est-qty.est-no = ip-est-no
                     :
       message "in del" d-est-qty.est-no.
       
     for each eb where eb.company = d-est-qty.company 
                   and eb.est-no = d-est-qty.est-no
                   and eb.eqty = d-est-qty.eqty:
         delete eb.             
     end.
     for each ef where ef.company = d-est-qty.company 
                   and ef.est-no = d-est-qty.est-no
                   and ef.eqty = d-est-qty.eqty:
         delete ef.             
     end.
     message "delete est=qty" est-qty.est-no .
     delete d-est-qty.
  end.   
  for each est-prep where est-prep.company = ip-comp
                      and est-prep.est-no = ip-est-no:
      delete est-prep.
      message "del prep".
  end.
  for each est-op where est-op.company = ip-comp
                    and est-op.est-no = ip-est-no:
      delete est-op.
  end.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
 
  /*  run est/d-addwht.w  (output ls-add-what).  for combo estimate only */
  ls-add-what = "est".
  if ls-add-what = "" then return no-apply.  /* cancel from dialog box */
    
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

  /* Code placed here will execute PRIOR to standard behavior. */

  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock.
  if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock.
  
  /* Change 4th enabled record lock status PROGRESS only does upto 3 records 
     est,est-qty,eb,ef (ef has still no-lock status) */
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR NO-WAIT.  
        
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* ======= update all eb,ef eqty field ==========*/
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

   if not ll-is-copy-record and ll-new-record then do:
     if eb.stock-no:screen-value in browse {&browse-name} = "" then do:
        find first ce-ctrl no-lock no-error.
        eb.cas-no = ce-ctrl.def-case.
        eb.tr-no = ce-ctrl.def-pal.      
     end.
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
                      style.style = eb.style:screen-value
                      no-lock no-error.
     if avail style then assign eb.adhesive = style.material[7]
                                eb.gluelap = style.dim-gl
                                eb.k-wid = style.dim-dkw
                                eb.fpanel = style.dim-pan5
                                eb.lock = style.dim-fit
                                eb.tuck = style.dim-tk.                 
     if eb.gluelap <> 0 then eb.lin-in = eb.dep.
 
     run calc-pass.
     run calc-blank-size.

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

  /* Code placed here will execute PRIOR to standard behavior. */
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if ls-add-what = "est" then run crt-new-est.
  else if ls-add-what = "item" then run crt-new-item.
 
  assign lv-eb-recid = recid(eb) 
         lv-ef-recid = recid(ef).
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-est-no like est.est-no no-undo.
  def var lv-comp like est.company no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
    MESSAGE "Delete Currently Selected Record?"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
    IF not response  THEN RETURN "ADM-ERROR":U.

    assign lv-est-no = est.est-no
           lv-comp = est.company.
    if not avail eb then find eb where recid(eb) = lv-eb-recid.
    if not avail ef then find ef where recid(ef) = lv-ef-recid.         
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* run delete-est-childrecord (input lv-comp, lv-est-no).  not working */  
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
  apply "entry" to est.est-date in browse {&browse-name}.
  return no-apply.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-row-num as int no-undo.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  /* == validation ===== */
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

     if ef.board:screen-value in browse {&browse-name} <> "" and
        not can-find(item where item.company = gcompany
                     and item.i-no = ef.board:screen-value in browse {&browse-name} )
     then do:
        message "Invalid Board. Try Help. " view-as alert-box error.
        apply "entry" to ef.board.
        return no-apply.
     end.
     if eb.procat:screen-value in browse {&browse-name} <> "" and
       not can-find(first fgcat where fgcat.company = gcompany and
                    fgcat.procat = eb.procat:screen-value )
     then do:
         message "Invalid Category. Try Help." view-as alert-box error.
         apply "entry" to eb.procat.
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

  if not avail eb then find eb where recid(eb) = lv-eb-recid no-lock.
  if not avail ef then find ef where recid(ef) = lv-ef-recid no-lock.

/*  lv-rowid = rowid(est). */
  li-row-num = browse {&browse-name}:focused-row.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if ll-new-record then do:
     find first ce-ctrl where ce-ctrl.company = gcompany and
                           ce-ctrl.loc = gloc
                           exclusive-lock.
     ce-ctrl.e-num = li-new-estnum.
  end.
 
 /*if ll-new-record then do:  refresh for eqty in update mode   */
      RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).

/*  redundant procedure I think
     run dispatch in widget-handle(char-hdl) ('open-query').    
     run reposition-query in widget-handle(char-hdl) (this-procedure).
     run dispatch in widget-handle(char-hdl) ("row-changed"). /* tell to parent */
*/     
     run dispatch ('open-query').
     reposition {&browse-name} to row li-row-num.

    /* ll-dumb = browse {&browse-name}:select-focused-row().*/
/*  end. */


session:set-wait-state("").

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

