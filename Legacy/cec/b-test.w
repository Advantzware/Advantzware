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
DEF VAR ls-add-what AS cha NO-UNDO.
DEF VAR li-new-estnum AS INT NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.
DEF VAR ll-is-copy-record AS LOG NO-UNDO.
DEF VAR char-val AS cha NO-UNDO.
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR ef.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF NEW SHARED BUFFER xqty FOR est-qty.
DEF NEW SHARED VAR formule AS INT EXTENT 12 NO-UNDO.
DEF VAR lv-part-no-prev LIKE eb.part-no NO-UNDO.
DEF VAR lv-eb-recid AS RECID NO-UNDO.
DEF VAR lv-ef-recid AS RECID NO-UNDO.
DEF VAR is-item-copied-from-est AS LOG NO-UNDO.
DEF VAR li-form# LIKE ef.form-no NO-UNDO.
DEF VAR li-est-form-qty LIKE est.form-qty NO-UNDO.
DEF VAR ls-cust-no AS cha NO-UNDO.
DEF VAR ls-ship-id AS cha NO-UNDO.
DEF VAR ls-set-part-no AS cha NO-UNDO.  /* set part-no from local-create-record*/
DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
 
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

{sys/inc/var.i "new" "shared"}

DEF BUFFER recalc-mr FOR reftable.

DEF NEW SHARED TEMP-TABLE formule FIELD formule AS DEC EXTENT 12.

DEF VAR ls-prev-val AS cha NO-UNDO.
DEF VAR lv-copy-qty AS INT EXTENT 20 NO-UNDO.
DEF VAR lv-copy-pr AS DEC EXTENT 20 NO-UNDO.
DEF VAR lv-copy-uom AS cha EXTENT 20 NO-UNDO.
DEF VAR lv-copy-date AS DATE EXTENT 20 NO-UNDO.
DEF VAR lv-estqty-recid AS RECID NO-UNDO.
DEF VAR lv-foam AS LOG NO-UNDO.
{cec/descalc.i "new"}
DEF NEW SHARED VAR xcal AS DEC NO-UNDO.
DEF NEW SHARED VAR sh-wid AS DEC NO-UNDO.
DEF NEW SHARED VAR sh-len AS DEC NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR ll-is-add-from-tool AS LOG NO-UNDO.
DEF VAR ll-crt-itemfg AS LOG NO-UNDO.

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
display-cw-dim(yes,eb.dep) @ eb.dep eb.tab-in eb.i-col eb.i-coat eb.yld-qty ~
eb.quantityPerSet 
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
  ( INPUT ip-is-corr-style AS LOG, INPUT  ip-dim AS DECIMAL )  FORWARD.

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
      eb.yld-qty COLUMN-LABEL "Yield Quantity" FORMAT "->>>>>9":U
      eb.quantityPerSet FORMAT ">>>>9.9<<<":U
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
"eb.yld-qty" "Yield Quantity" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.eb.quantityPerSet
"eb.quantityPerSet" ? ? "decimal" ? ? ? ? ? ? no "Quantity Per Set" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state IN phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME Corr
DO:
     DEF VAR ls-cur-val AS cha NO-UNDO.
     DEF VAR lv-eb-tmpid AS RECID NO-UNDO.
     DEF VAR lv-handle AS HANDLE NO-UNDO.          
     DEF VAR char-val2 AS cha NO-UNDO.        
     DEF VAR date-val AS cha NO-UNDO.
     DEF VAR date-val2 AS cha NO-UNDO.
     
     CASE FOCUS:NAME :
     WHEN "part-no" THEN DO: 
           RUN est/l-ebrfqP.w (gcompany, gloc, FOCUS:SCREEN-VALUE, OUTPUT lv-eb-tmpid) .
           IF lv-eb-tmpid = ?  THEN RETURN NO-APPLY.
           FIND xeb WHERE RECID(xeb) = lv-eb-tmpid NO-LOCK NO-ERROR.
           FIND xef OF xeb WHERE xef.company = xeb.company AND
                                 xef.est-no = xeb.est-no  
                NO-LOCK NO-ERROR.
           RUN copy-from-est.
           /*run copy-from-est2.*/
           lv-part-no-prev = eb.part-no.          
           RETURN NO-APPLY.           
      END.
      WHEN "stock-no" THEN DO:
        /* run windows/l-itemfg.w  (gcompany, output char-val). */
           RUN est/l-ebrfq.w (gcompany, gloc,FOCUS:SCREEN-VALUE, OUTPUT lv-eb-tmpid) .
           IF lv-eb-tmpid = ?  THEN RETURN.
           FIND xeb WHERE RECID(xeb) = lv-eb-tmpid NO-LOCK NO-ERROR.
           FIND xef OF xeb WHERE xef.company = xeb.company AND
                                 xef.est-no = xeb.est-no
                          NO-LOCK NO-ERROR.
   
           RUN copy-from-est.
           /*run copy-from-est2. */
  
           RETURN NO-APPLY.
      END.
      WHEN "style" THEN DO:
     
           ls-cur-val = FOCUS:SCREEN-VALUE.
           RUN windows/l-stylec.w (gcompany,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(1,char-val).
              FIND style WHERE style.company = gcompany AND
                               style.style = eb.style:screen-value IN BROWSE {&browse-name}
                         NO-LOCK NO-ERROR.            
              IF AVAIL style THEN DO:
                 ASSIGN ef.board:screen-value IN BROWSE {&browse-name} = style.material[1]
                        .
                 FIND item WHERE item.company = gcompany AND
                                 item.i-no = style.material[1] NO-LOCK NO-ERROR.
                 ef.cal:screen-value IN BROWSE {&browse-name} = IF AVAIL item THEN STRING(item.cal) ELSE "".
        
              END.          
           END.  
           RETURN NO-APPLY.
      END.
      WHEN "procat" THEN DO:
           ls-cur-val = FOCUS:SCREEN-VALUE.
           RUN windows/l-fgcat.w (gcompany,ls-cur-val,OUTPUT char-val).
           IF char-val <> "" THEN
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(1,char-val).
           RETURN NO-APPLY.

       END.
       WHEN "flute" THEN DO:
           ls-cur-val = FOCUS:SCREEN-VALUE.
           RUN windows/l-flute.w (gcompany,OUTPUT char-val).
           IF char-val <> "" THEN
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(1,char-val).
           RETURN NO-APPLY.
       END.
       WHEN "test" THEN DO:
           ls-cur-val = eb.flute:screen-value.
           RUN windows/l-test.w (gcompany,gloc,ls-cur-val,OUTPUT char-val).
           IF char-val <> "" THEN
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  entry(1,char-val).
           RETURN NO-APPLY.       
       END.
       WHEN "Board" THEN DO:
           DEF VAR lv-ind LIKE style.industry NO-UNDO.
           ls-cur-val = FOCUS:SCREEN-VALUE.
           FIND style WHERE style.company = gcompany AND
                            style.style = eb.style:screen-value IN BROWSE {&browse-name}
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  /* foam */
                 RUN windows/l-boardf.w (gcompany,lv-ind,ls-cur-val,OUTPUT char-val).
           ELSE RUN windows/l-board.w (gcompany,lv-ind, ls-cur-val,OUTPUT char-val).
           IF char-val <> "" THEN 
              ASSIGN ef.board:screen-value IN BROWSE {&browse-name} = ENTRY(1,char-val)
                     ef.cal:screen-value IN BROWSE {&browse-name} = ENTRY(2,char-val)
                     .
           RETURN NO-APPLY.   
       END.
       WHEN "cust-no" THEN DO:
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
           ls-cur-val = FOCUS:SCREEN-VALUE.
           RUN windows/l-cust.w (gcompany,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  ENTRY(1,char-val).
              FIND FIRST shipto WHERE shipto.company = gcompany
                                  AND shipto.cust-no = FOCUS:SCREEN-VALUE
                                  NO-LOCK NO-ERROR.
               eb.ship-id:screen-value = IF AVAIL shipto THEN shipto.ship-id ELSE "".
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
       WHEN "ship-id" THEN DO:
           ls-cur-val = FOCUS:SCREEN-VALUE.
           RUN windows/l-shipto.w (gcompany,gloc,eb.cust-no:screen-value,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN 
              FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =  ENTRY(1,char-val).
           RETURN NO-APPLY.
       END.  /* cust-no*/
       WHEN "eqty" THEN DO:
             lv-estqty-recid = IF AVAIL est-qty THEN RECID(est-qty) ELSE ?.
             RUN est/estqtyd.w (lv-estqty-recid, RECID(eb),est-qty.eqty:screen-value IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2) .
             IF char-val <> "?" 
                THEN ASSIGN est-qty.eqty:screen-value = ENTRY(1,char-val)
                            lv-copy-qty[2] = INTEGER(ENTRY(2,char-val))
                            lv-copy-qty[3] = INTEGER(ENTRY(3,char-val))
                            lv-copy-qty[4] = INTEGER(ENTRY(4,char-val))
                            lv-copy-qty[5] = INTEGER(ENTRY(5,char-val))
                            lv-copy-qty[6] = INTEGER(ENTRY(6,char-val))
                            lv-copy-qty[7] = INTEGER(ENTRY(7,char-val))
                            lv-copy-qty[8] = INTEGER(ENTRY(8,char-val))
                            lv-copy-qty[9] = INTEGER(ENTRY(9,char-val))
                            lv-copy-qty[10] = INTEGER(ENTRY(10,char-val))
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
             IF char-val2 <> "?" 
                THEN ASSIGN lv-copy-qty[11] = INTEGER(ENTRY(1,char-val2))
                            lv-copy-qty[12] = INTEGER(ENTRY(2,char-val2))
                            lv-copy-qty[13] = INTEGER(ENTRY(3,char-val2))
                            lv-copy-qty[14] = INTEGER(ENTRY(4,char-val2))
                            lv-copy-qty[15] = INTEGER(ENTRY(5,char-val2))
                            lv-copy-qty[16] = INTEGER(ENTRY(6,char-val2))
                            lv-copy-qty[17] = INTEGER(ENTRY(7,char-val2))
                            lv-copy-qty[18] = INTEGER(ENTRY(8,char-val2))
                            lv-copy-qty[19] = INTEGER(ENTRY(9,char-val2))
                            lv-copy-qty[20] = INTEGER(ENTRY(10,char-val2))
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
             RETURN NO-APPLY.
       END.
       OTHERWISE DO:
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
           RETURN NO-APPLY.
        END.  /* otherwise */
  END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON RETURN OF br_table IN FRAME Corr
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME Corr
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
  /* not to have error no eb record avail when add new set item */
  IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb)  = lv-eb-recid NO-LOCK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME Corr
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
   IF KEYFUNCTION(LASTKEY) = "page-up" OR 
      keyfunction(LASTKEY) = "page-down" OR
      keyfunction(LASTKEY) = "cursor-up" OR
      keyfunction(LASTKEY) = "cursor-down" 
   THEN DO:
      RETURN NO-APPLY.
   END.
 
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
    
  IF NOT adm-new-record /*and not adm-adding-record */ THEN   
     ASSIGN lv-eb-recid = RECID(eb)
            lv-ef-recid = RECID(ef).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est.est-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est.est-date br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est.est-date IN BROWSE br_table /* Est Date */
DO:
  IF eb.cust-no:screen-value IN BROWSE {&browse-name} EQ "" THEN DO:
    APPLY "tab" TO {&self-name} in browse {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.cust-no IN BROWSE br_table /* Cust. # */
DO:
    IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK.
    IF eb.est-type = 6 AND eb.form-no > 1 THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.cust-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.cust-no IN BROWSE br_table /* Cust. # */
DO:
   IF LASTKEY <> -1 AND /*eb.cust-no:screen-value in browse {&browse-name} <> "" and */
      NOT CAN-FIND(cust WHERE cust.company = gcompany AND cust.cust-no = eb.cust-no:screen-value IN BROWSE {&browse-name} )
   THEN DO:
       IF eb.cust-no:screen-value = "" THEN DO:
           MESSAGE "Invalid Customer Number. Try Help." VIEW-AS ALERT-BOX ERROR. 
           RETURN NO-APPLY.
       END.
       MESSAGE "Customer " eb.cust-no:screen-value "does not exist. Do you want to add it?"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF NOT ll-ans THEN  RETURN NO-APPLY.
       
       RUN est/custfly.w (eb.cust-no:screen-value). 
       
   END.
   ELSE DO:  
       FIND cust WHERE cust.company = gcompany AND
                              cust.cust-no = SELF:screen-value 
                              NO-LOCK NO-ERROR.
       IF AVAIL cust THEN DO:
         FIND FIRST shipto
             WHERE shipto.company EQ cust.company
               AND shipto.cust-no EQ cust.cust-no
               AND (IF cust.cust-no EQ "TEMP" THEN shipto.ship-id EQ "TEMP"
                    ELSE shipto.ship-no EQ 1)
             NO-LOCK NO-ERROR.
         eb.ship-id:screen-value = IF AVAIL shipto THEN shipto.ship-id ELSE "".
       END.                    
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.ship-id br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.ship-id IN BROWSE br_table /* Ship To */
DO:
  IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK.
  IF eb.est-type = 6 AND eb.form-no > 1 THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.    
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.part-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.part-no IN BROWSE br_table /* Cust Part # */
DO:
   DEF VAR lv-eb-tmpid AS RECID NO-UNDO.
   
   IF LASTKEY <> -1 AND eb.part-no:screen-value IN BROWSE {&browse-name} = "" THEN DO:
      MESSAGE "Customer Part # can not be blank!" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   
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
  DEF VAR lv-eb-tmpid AS RECID NO-UNDO.
        /* 1. get values from itemfg if itemfg.est-no <> ""
           2. else get estimate info window */
  IF adm-new-record /*AND xef.part-no:screen-value in browse {&browse-name} = "" */
     AND SELF:screen-value <> ""
  THEN DO:
    /* copy from estimate */
    FIND FIRST itemfg WHERE itemfg.company = gcompany AND
                            itemfg.i-no = eb.stock-no:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
    IF AVAIL itemfg AND itemfg.est-no <> "" THEN DO:
       FIND xest WHERE xest.company  = itemfg.company AND
                      xest.loc = itemfg.loc AND
                      xest.est-no = itemfg.est-no
                      NO-LOCK NO-ERROR.
       IF AVAIL xest THEN DO:
          FIND FIRST xeb WHERE xeb.company = xest.company AND
                               xeb.est-no = xest.est-no AND
                               xeb.stock-no = itemfg.i-no
                              NO-LOCK NO-ERROR.
          IF AVAIL xeb THEN DO:
             FIND FIRST xef OF xeb WHERE xef.company = xeb.company AND
                                         xef.est-no = xeb.est-no
                  NO-LOCK NO-ERROR.
             ASSIGN eb.len:screen-value IN BROWSE {&browse-name} = STRING(xeb.len)
                               eb.wid:screen-value IN BROWSE {&browse-name} = STRING(xeb.wid)
                               eb.dep:screen-value IN BROWSE {&browse-name} = STRING(xeb.dep)
                               eb.procat:screen-value IN BROWSE {&browse-name} = xeb.procat
                               eb.style:screen-value IN BROWSE {&browse-name} = xeb.style
                               eb.part-no:screen-value IN BROWSE {&browse-name} = xeb.part-no
                               eb.part-dscr1:screen-value IN BROWSE {&browse-name} = xeb.part-dscr1                               
                               ef.cal:screen-value IN BROWSE {&browse-name} = IF AVAIL xef THEN STRING(xef.cal)  ELSE ""
                               ef.board:screen-value IN BROWSE {&browse-name} = IF AVAIL xef THEN xef.board ELSE ""
                               eb.i-col:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-col)                               
                               eb.i-coat:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-coat)
                               .
  
             RUN  copy-from-est.                  
           END.
       END.    
    END.  /* avail itemfg */
    ELSE DO:  /* get it from estimate */                             
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
       IF NOT AVAIL itemfg AND eb.stock-no:screen-value <> "" THEN DO:
          MESSAGE "This item does not exist, would you like to add it?" VIEW-AS ALERT-BOX QUESTION
                    BUTTON YES-NO UPDATE ll-ans1 AS LOG.  
          IF ll-ans1 THEN DO:
           /* run from assign-record   
             find xest where recid(xest) = recid(est) no-lock no-error.
             find xeb where recid(xeb) = recid(eb) no-lock no-error.
             find xef where recid(xef) = recid(ef) no-lock no-error.
             run crt-itemfg (input self:screen-value).
           */
           ll-crt-itemfg = YES.  
           RETURN.
          END.   
          RETURN NO-APPLY.        
       END.    
    END.            /* else */  
  END. /* adm-new-record */ 
 
  ELSE IF NOT adm-new-record AND LASTKEY <> -1 THEN DO:  /* update existing records */
  
    FIND FIRST itemfg WHERE itemfg.company = gcompany AND
                            itemfg.i-no = eb.stock-no:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.

    IF NOT AVAIL itemfg AND eb.stock-no:screen-value <> "" THEN DO:
    /*   message "Invalid FG Item#. Try Help.".
       return no-apply.
     */
       MESSAGE "This item does not exist, would you like to add it?" VIEW-AS ALERT-BOX QUESTION
               BUTTON YES-NO UPDATE ll-ans AS LOG.  
       IF ll-ans THEN DO:
        /*  find xest where recid(xest) = recid(est) no-lock no-error.
          find xeb where recid(xeb) = recid(eb) no-lock no-error.
          find xef where recid(xef) = recid(ef) no-lock no-error.
          run crt-itemfg (input self:screen-value).
        */
          ll-crt-itemfg = YES.  
          RETURN.
       END.   
       RETURN NO-APPLY.        
    END.  
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-qty.eqty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF est-qty.eqty IN BROWSE br_table /* Est Qty */
DO: 
  IF eb.est-type = 6 AND eb.form-no > 1 THEN DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.    
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-qty.eqty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF est-qty.eqty IN BROWSE br_table /* Est Qty */
DO:
    IF LASTKEY = -1 THEN RETURN.
    
    IF int(est-qty.eqty:screen-value IN BROWSE {&browse-name} ) <= 0 THEN DO:
       MESSAGE "Quantity must be entered. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.style IN BROWSE br_table /* Style */
DO:
    ls-prev-val = SELF:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.style br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.style IN BROWSE br_table /* Style */
DO:
   IF is-item-copied-from-est THEN RETURN.
   
   IF LASTKEY <> -1 AND /*eb.style:screen-value in browse {&browse-name} <> "" and */
      NOT CAN-FIND(style WHERE style.company = gcompany AND
                               style.style = eb.style:screen-value IN BROWSE {&browse-name} )
   THEN DO:
        IF eb.style:screen-value = "" THEN MESSAGE "Valid Style must be entered. Try Help." VIEW-AS ALERT-BOX ERROR.
        ELSE MESSAGE "Invalid Style. Try Help. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
   END. 

   IF /*eb.style:modified in browse {&browse-name}*/ 
      SELF:screen-value <> ls-prev-val 
   THEN DO:
     FIND style WHERE style.company = gcompany AND
                      style.style = eb.style:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.   
     IF AVAIL style THEN DO:
        ASSIGN ef.board:screen-value IN BROWSE {&browse-name} = style.material[1].
        lv-foam = IF style.type = "F" THEN  YES ELSE NO.
     END.   
   END.
   SELF:screen-value = CAPS(SELF:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.flute br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.flute IN BROWSE br_table /* Flute */
DO:
   DEF VAR ls-board AS cha NO-UNDO.
   
   IF LASTKEY <> -1 AND 
      NOT CAN-FIND(FIRST flute WHERE flute.code = eb.flute:screen-value IN BROWSE {&browse-name})
   THEN DO:
      MESSAGE "Invalid Flute Code." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
  
   IF adm-new-record AND eb.flute:modified IN BROWSE {&browse-name}
      /*self:screen-value <> ls-prev-val  */
   THEN DO:
     FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                           AND reftable.company = eb.style:screen-value
                           AND (reftable.loc = eb.flute:screen-value OR lv-foam)
                           AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN DO:
        FIND FIRST item WHERE item.company = gcompany AND
                              item.i-no = reftable.dscr
                              NO-LOCK NO-ERROR.
        IF AVAIL item THEN
                 ASSIGN ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr */
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/ STRING(item.cal)
                        ls-board = item.i-no
                        .                      
                          
     END.                                         
  /*   if not avail item or (avail item and item.i-no = "" ) then   */
     IF ls-board = "" THEN
     FOR EACH item  WHERE item.company  EQ gcompany
                  AND item.mat-type EQ "B"
                  AND item.i-code   EQ "E"
                  AND item.flute    EQ eb.flute:screen-value IN BROWSE {&browse-name}
                  AND item.reg-no   EQ eb.test:screen-value
                  USE-INDEX mat-type NO-LOCK
                  BY item.i-no:
       ASSIGN ef.board:screen-value    = item.i-no
              ef.cal:screen-value = STRING(item.cal).
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
     LEAVE.
   END.



   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.test br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.test IN BROWSE br_table /* Test */
DO:
  DEF VAR ls-board AS cha NO-UNDO.
  
   /*if ef.board:screen-value in browse {&browse-name} = "" then */
  IF adm-new-record AND eb.test:modified IN BROWSE {&browse-name} THEN
  DO:  
     FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                           AND reftable.company = eb.style:screen-value IN BROWSE {&browse-name}
                           AND (reftable.loc = eb.flute:screen-value OR lv-foam)
                           AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN DO:
        FIND FIRST item WHERE item.company = gcompany AND
                              item.i-no = reftable.dscr
                              NO-LOCK NO-ERROR.
        IF AVAIL item THEN
                 ASSIGN ef.board:screen-value = item.i-no
                       /* ef.brd-dscr:screen-value = item.est-dscr 
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no */
                        ef.cal:screen-value = /*if item.s-dep <> 0 then string(item.s-dep)
                                              else*/  STRING(item.cal)
                        ls-board = item.i-no.
                        .                      
                          
     END.                                         
     /*if not avail item or (avail item and item.i-no = "" ) then   */
     IF ls-board = "" THEN
     FOR EACH item  WHERE item.company  EQ gcompany
                  AND item.mat-type EQ "B"
                  AND item.i-code   EQ "E"
                  AND item.flute    EQ eb.flute:screen-value IN BROWSE {&browse-name}
                  AND item.reg-no   EQ eb.test:screen-value
                  USE-INDEX mat-type NO-LOCK
                  BY item.i-no:
       ASSIGN ef.board:screen-value    = item.i-no
              ef.cal:screen-value = STRING(item.cal).
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
     LEAVE.
   END.

   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.board br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef.board IN BROWSE br_table /* Board */
DO:
   IF LASTKEY = -1 THEN RETURN.
   
   FIND item WHERE item.company = gcompany
                AND item.i-no = SELF:screen-value IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
   IF NOT AVAIL item THEN DO:
      MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   ELSE ASSIGN ef.cal:screen-value IN BROWSE {&browse-name} = STRING(item.cal)
              .               

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.procat IN BROWSE br_table /* Category */
DO:
    IF LASTKEY <> -1 AND 
      /* eb.procat:screen-value in browse {&browse-name} <> "" and */
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = gcompany AND
                    fgcat.procat = eb.procat:screen-value IN BROWSE {&browse-name})
    THEN DO:
         MESSAGE "Invalid Category. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON PAGE-DOWN OF eb.procat IN BROWSE br_table /* Category */
DO:
   FIND FIRST fgcat WHERE fgcat.company = gcompany AND
                          fgcat.procat > eb.procat:screen-value IN BROWSE {&browse-name}
                          NO-LOCK NO-ERROR.
   IF AVAIL fgcat THEN eb.procat:screen-value IN BROWSE {&browse-name} = fgcat.procat.
   
                          
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.procat br_table _BROWSE-COLUMN B-table-Win
ON PAGE-UP OF eb.procat IN BROWSE br_table /* Category */
DO:
   FIND LAST fgcat WHERE fgcat.company = gcompany AND
                          fgcat.procat < eb.procat:screen-value IN BROWSE {&browse-name}
                          NO-LOCK NO-ERROR.
   IF AVAIL fgcat THEN eb.procat:screen-value IN BROWSE {&browse-name} = fgcat.procat.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.len br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.len IN BROWSE br_table /* Length */
DO:
   IF LASTKEY = -1 THEN RETURN.
   IF DECIMAL(eb.len:screen-value IN BROWSE {&browse-name}) = 0 THEN DO:
        MESSAGE "Length can not be 0. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
   END.

   IF LASTKEY <> -1 AND
      decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= 0.16 
   THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO SELF.
             RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.wid br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.wid IN BROWSE br_table /* Width */
DO:
   IF LASTKEY = -1 THEN RETURN.
   IF DECIMAL(eb.wid:screen-value IN BROWSE {&browse-name}) = 0 THEN DO:
        MESSAGE "Width can not be 0. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
   END.

   IF LASTKEY <> -1 AND
      decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= 0.16 
   THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO SELF.
             RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.dep br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.dep IN BROWSE br_table /* Depth */
DO:
    IF LASTKEY <> -1 AND
      decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= 0.16 
   THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO SELF.
             RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
RUN Get-Company  (OUTPUT gcompany).
RUN Get-location (OUTPUT gloc).
ASSIGN cocode = gcompany
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

  ll-is-add-from-tool = YES.  /* add from option button not from add button */
  ls-add-what = "set" .   /* new estimate */
  RUN est/d-addset.w (OUTPUT ls-add-what). /* one item or set cec/est-add.p */
  IF ls-add-what = "" THEN RETURN NO-APPLY.  /* cancel */
  
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
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bf-est-qty FOR est-qty.
  FIND bf-est-qty WHERE RECID(bf-est-qty) = recid(est-qty).
  ASSIGN      bf-est-qty.qty[1] = est-qty.eqty
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
       ASSIGN bf-est-qty.qty[11] = lv-copy-qty[11]
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
      FIND bf-est WHERE bf-est.company = bf-est-qty.company AND
                        bf-est.est-no = bf-est-qty.est-no.
      ASSIGN bf-est.est-qty[1] = est-qty.eqty
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

   DEF BUFFER bf-eb FOR eb .
   DEF VAR lv-panels AS LOG NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   DEF VAR j AS INT NO-UNDO.
   DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
   DEF VAR v-score-char LIKE v-lscore-c EXTENT 12.
   DEF VAR v-index AS INT NO-UNDO.
   DEF VAR v-str AS CHAR NO-UNDO.

   FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                           AND sys-ctrl.name    EQ "PANELS"
        NO-LOCK NO-ERROR.
   IF NOT AVAIL sys-ctrl THEN DO:
      CREATE sys-ctrl.
      ASSIGN  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = YES.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   END.
   lv-panels = sys-ctrl.log-fld.

   FIND xest WHERE RECID(xest) = recid(est) NO-LOCK.
   FIND xef WHERE RECID(xef) = recid(ef) NO-LOCK.
   FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK.
   
   FIND style WHERE style.company = eb.company AND
                    style.style = eb.style
                    NO-LOCK NO-ERROR.
   IF AVAIL style THEN DO:
       IF style.type <> "F" THEN RUN calc-blank-size2. 

      {cec/msfcalc.i}
      RUN est/u2kinc1c.p (RECID(eb)).
      RUN est/u2kinc2c.p (RECID(eb)).
      FIND FIRST formule NO-LOCK NO-ERROR.
      FIND bf-eb OF eb EXCLUSIVE-LOCK.    
      ASSIGN bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          .
      /*bf-eb.t-sqin = if v-corr then bf-eb.t-sqin * .007 else bf-eb.t-sqin / 144.
      */
   
      IF NOT lv-panels OR style.type = "F" THEN 
         ASSIGN bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      ELSE DO:
         RUN cec/descalc.p (RECID(xest),RECID(xeb)).

         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         IF v-lscore-c BEGINS "No" THEN
            ASSIGN  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         ELSE DO:
           i = 0.
           FOR EACH w-box-design-line:
             i = i + 1.
             xeb.k-wid-array2[i] = w-box-design-line.wscore-d.
             {sys/inc/k16bb.i xeb.k-wid-array2[i]}
           END.

           ASSIGN  v-score-char    = ""
                   j               = 1.
           DO i = 1 TO 80:
             IF substr(v-lscore-c,i,1) NE "" THEN DO:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                IF substr(v-lscore-c,i + 1,1) EQ "" THEN
                   ASSIGN  v-score-char[j] = TRIM(v-score-char[j])
                           j = j + 1.
             END.
             IF j GT 12 THEN LEAVE.
           END.
           DO i = 1 TO 12:

              IF v-cecscrn-dec AND v-score-char[i] NE "" THEN
                 ASSIGN
                    v-index = INDEX(v-score-char[i],".")
                    v-str = SUBSTRING(v-score-char[i],v-index + 1)
                    v-str = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))
                    SUBSTRING(v-score-char[i],v-index) = v-str.

              xeb.k-len-array[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i xeb.k-len-array[i]}.
           END.
         END.  /* else v-lscore */
       END. /* panels or not foam */
       
   END.

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
   FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK.
   
   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}
   FIND FIRST item WHERE item.company = est.company
                    AND item.i-no EQ eb.adhesive
                  NO-LOCK NO-ERROR.
   IF AVAIL item THEN DO:
 
            IF item.mat-type EQ "G" THEN DO:
                    IF eb.tab-in  THEN DO:
                       {est/u2estc.i eb.k-len 3}
                    END.
                    ELSE DO:
                       {est/u2estc.i eb.k-len 4}
                    END.
            END.
            ELSE IF item.mat-type EQ "S" THEN DO:
                    IF eb.tab-in  THEN DO:
                       {est/u2estc.i eb.k-len 5}
                    END.
                    ELSE DO:
                       {est/u2estc.i eb.k-len 6}
                    END.
            END.
            ELSE IF item.mat-type EQ "T" THEN DO:
                    eb.tab-in = NO.
                    {est/u2estc.i eb.k-len 7}
            END. 
    END.
    ELSE DO:
                 eb.tab-in = NO.
                 {est/u2estc.i eb.k-len 7}
    END.

    IF eb.len EQ eb.wid
    THEN DO:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    END.
    ELSE DO:
                 {est/u2estc.i eb.k-wid 2}
    END.




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

      DEF VAR k AS INT NO-UNDO.
      DEF VAR counter AS INT NO-UNDO.
      DEF VAR i AS INT NO-UNDO.
      DEF VAR j AS INT NO-UNDO.
      DEF VAR save_id AS RECID NO-UNDO.
      DEF VAR save_id2 AS RECID NO-UNDO.
      DEF BUFFER alt-item FOR item .
      DEF VAR choice AS LOG NO-UNDO.
      DEF BUFFER bf-eb FOR eb.
      
      FIND FIRST style WHERE style.company = eb.company AND
                 style.style = eb.style NO-LOCK NO-ERROR.
      IF AVAIL style THEN DO:
         IF k = 0 THEN k = INTEGER(style.material[3]).

         IF style.material[2] NE "" THEN
         FIND FIRST item WHERE item.company = eb.company AND
                    item.i-no = style.material[2] NO-LOCK NO-ERROR.
         IF AVAIL item THEN k = INTEGER(style.material[3]).

         IF style.material[6] NE "" THEN 
         FIND FIRST alt-item WHERE alt-item.company  = eb.company  AND
                                   alt-item.mat-type = "V"     AND
                                   alt-item.i-no     = style.material[6]
                                   NO-LOCK NO-ERROR.
      END.
      IF NOT AVAIL item OR NOT AVAIL alt-item OR (k = 0) THEN DO:
         FIND FIRST ce-ctrl WHERE ce-ctrl.company = eb.company AND
                                  ce-ctrl.loc = eb.loc
                                   NO-LOCK NO-ERROR.
         IF k = 0 THEN k = ce-ctrl.def-inkcov.
         IF NOT AVAIL item THEN DO:
            FIND FIRST item WHERE item.company = eb.company AND
                       item.i-no = ce-ctrl.def-ink NO-LOCK NO-ERROR.
         END.
         IF NOT AVAIL alt-item THEN
            FIND FIRST alt-item WHERE alt-item.company  = eb.company  AND
                                      alt-item.mat-type = "V"     AND
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      NO-LOCK NO-ERROR.
      END.
 
      save_id = RECID(item). save_id2 = RECID(alt-item).
      j = (INTEGER(eb.i-col:screen-value IN BROWSE {&browse-name})
          + integer(eb.i-coat:screen-value IN BROWSE {&browse-name})  ) 
          .
      {sys/inc/roundup.i j}
      ASSIGN
      counter = 1
      choice = TRUE.
  
      FIND bf-eb OF eb EXCLUSIVE-LOCK.    
      IF eb.i-col > 0 THEN ASSIGN bf-eb.i-pass = 1.
      IF eb.i-coat > 0 THEN ASSIGN bf-eb.i-coat-p = 1.
      IF choice THEN DO i = 1 TO 10:
         IF i LE integer(eb.i-col) THEN DO WITH FRAME {&frame-name}:
              FIND item WHERE RECID(item) = save_id NO-LOCK NO-ERROR.
              ASSIGN bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.est-dscr
                     bf-eb.i-%[i]    = k.
         END.
         ELSE IF (i > integer(eb.i-col)) AND
                 (i <= (INTEGER(eb.i-col) + 
                       integer(eb.i-coat)) )
         THEN DO:
              FIND alt-item WHERE RECID(alt-item) = save_id2 NO-LOCK NO-ERROR.
              ASSIGN bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = IF AVAIL alt-item THEN alt-item.i-no ELSE ""
                     bf-eb.i-dscr[i] = IF AVAIL alt-item THEN alt-item.est-dscr ELSE ""
                     bf-eb.i-%[i]    = 100.
         END.
         ELSE IF (i >  (eb.i-col + eb.i-coat) )
         THEN DO:
            ASSIGN bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  
        
         END.
         IF j <> 0 AND i MODULO j = 0 THEN counter = counter + 1.
         IF counter > (eb.i-pass) THEN counter = eb.i-pass.         
      END. 
   
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
  DEF BUFFER bf-new-eb FOR eb.
  DEF BUFFER bf-new-ef FOR ef.

  is-item-copied-from-est  = YES.

  FIND bf-new-eb WHERE RECID(bf-new-eb) = IF RECID(eb) <> ? THEN RECID(eb) ELSE lv-eb-recid .
  FIND bf-new-ef WHERE RECID(bf-new-ef) = IF RECID(eb) <> ? THEN RECID(ef) ELSE lv-ef-recid.
  
  BUFFER-COPY xeb EXCEPT xeb.company xeb.est-no xeb.form-no xeb.blank-no TO bf-new-eb .
  BUFFER-COPY xef EXCEPT xef.company xef.est-no xef.form-no TO bf-new-ef.
  
  
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
       ASSIGN eb.len:screen-value IN BROWSE {&browse-name} = STRING(xeb.len)
              eb.wid:screen-value IN BROWSE {&browse-name} = STRING(xeb.wid)
              eb.dep:screen-value IN BROWSE {&browse-name} = STRING(xeb.dep)
              eb.procat:screen-value IN BROWSE {&browse-name} = xeb.procat
              eb.style:screen-value IN BROWSE {&browse-name} = xeb.style
              eb.stock-no:screen-value IN BROWSE {&browse-name} = xeb.stock-no
              eb.part-dscr1:screen-value IN BROWSE {&browse-name} = xeb.part-dscr1
              eb.part-no:screen-value IN BROWSE {&browse-name} = xeb.part-no
              ef.board:screen-value IN BROWSE {&browse-name} = xef.board
              ef.cal:screen-value IN BROWSE {&browse-name} = STRING(xef.cal)
              eb.i-col:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-col)
              eb.i-coat:screen-value IN BROWSE {&browse-name} = STRING(xeb.i-coat)
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
  DEF BUFFER bf-notes FOR notes.
  
  
  FIND cust WHERE cust.cust-no EQ eb.cust-no NO-LOCK NO-ERROR.
  
  IF AVAIL cust THEN
  FOR EACH notes
      WHERE notes.rec_key   EQ cust.rec_key
        AND notes.note_type EQ "D"
        AND notes.note_code NE ""
      NO-LOCK:

    FIND FIRST bf-notes
        WHERE bf-notes.rec_key   EQ est.rec_key
          AND bf-notes.note_type EQ notes.note_type
          AND bf-notes.note_code EQ notes.note_code
      NO-LOCK NO-ERROR.
      
    IF NOT AVAIL bf-notes THEN DO:
      CREATE bf-notes.
      BUFFER-COPY notes EXCEPT notes.note_form_no TO bf-notes
      ASSIGN
       bf-notes.rec_key   = est.rec_key
       bf-notes.note_date = TODAY
       bf-notes.note_time = TIME.
    END.
  END.
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
  DEF VAR i AS INT NO-UNDO.
  DEF BUFFER bb FOR eb.
  DEF BUFFER bf FOR ef.
 
  CREATE est-qty.
  ASSIGN est-qty.company = gcompany
         est-qty.est-no =  est.est-no
         est-qty.eqty = 0
         est-qty.qty-date = est.est-date
         .
  CREATE ef.
  ASSIGN
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

  CREATE eb.
  ASSIGN  eb.est-type = est.est-type
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = INTEGER(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.tr-cas = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0
   eb.yld-qty = 1
   eb.quantityPerSet = 1
   .
   
   RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
   IF cPackCodeOverride GT "" THEN 
       eb.cas-no = cPackCodeOverride.
  /* ???? bugs : 2 records are created  , delete one ========== */
  FOR EACH bb WHERE bb.est-no = "" :
      DELETE bb.
  END.
  FOR EACH bf WHERE bf.est-no = "" :
      DELETE bf.
  END.
  /*========*/
  FIND FIRST item WHERE item.company = gcompany
                    AND item.mat-type = "C"  /* Case/Bundle */
                    AND item.i-no EQ eb.cas-no
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN DO:
     FIND FIRST e-item WHERE e-item.company EQ item.company
                         AND e-item.loc     EQ item.loc
                         AND e-item.i-no    EQ item.i-no
        NO-LOCK NO-ERROR.
     FIND FIRST itemfg  WHERE itemfg.company EQ gcompany
                          AND itemfg.i-no    EQ eb.stock-no
        NO-LOCK NO-ERROR.
     IF AVAIL e-item THEN
        ASSIGN  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = IF AVAIL itemfg THEN itemfg.case-count ELSE e-item.box-case
                .
     IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
     IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
     IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
     IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
     IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
     IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
              IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
  END.  /* avail item */

  i = 1.

  ASSIGN lv-eb-recid = RECID(eb) 
         lv-ef-recid = RECID(ef).
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

DEF INPUT PARAMETER v-item LIKE itemfg.i-no.
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
*/
DEF VAR cocode AS cha NO-UNDO.
DEF VAR locode AS cha NO-UNDO.
DEF VAR v-alloc LIKE itemfg.alloc INIT YES.
DEF VAR tmpstore AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.

ASSIGN cocode = gcompany
       locode = gloc
       .

{ce/msfcalc.i}
{oe/fgfreight.i}
/*
{sys/inc/setprint.i}
*/
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SETPRINT"
    NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN v-alloc = sys-ctrl.log-fld.
       
FIND FIRST cust  WHERE cust.company EQ gcompany
                   AND cust.cust-no EQ xeb.cust-no
    NO-LOCK NO-ERROR.
CREATE itemfg.
ASSIGN
 itemfg.company    = gcompany
 itemfg.loc        = gloc
 itemfg.i-no       = v-item
 itemfg.i-name     = xeb.part-dscr1
 itemfg.part-dscr1 = xeb.part-dscr2
 itemfg.part-no    = xeb.part-no
 itemfg.cust-no    = xeb.cust-no
 itemfg.cust-name  = IF AVAIL cust THEN cust.name ELSE ""
 itemfg.alloc      = v-alloc
 itemfg.die-no     = xeb.die-no
 itemfg.plate-no   = xeb.plate-no
 itemfg.style      = xeb.style
 itemfg.procat     = xeb.procat
 itemfg.cad-no     = xeb.cad-no
 itemfg.upc-no     = xeb.upc-no
 itemfg.spc-no     = xeb.spc-no
 itemfg.isaset     = (xest.est-type EQ 2 OR xest.est-type EQ 6) AND
                     xeb.form-no EQ 0
 itemfg.pur-man    = xeb.pur-man      
 itemfg.alloc      = itemfg.isaset
 .

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
  DEF VAR cocode AS cha NO-UNDO.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bb FOR eb.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-crt-est-rowid AS ROWID NO-UNDO.
  DEF VAR ll-dumb AS LOG NO-UNDO.
    
  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */
  FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                           ce-ctrl.loc = gloc.
                          
  li-new-estnum = ce-ctrl.e-num + 1.
  ll-new-record = YES.
  ce-ctrl.e-num = li-new-estnum.
  
  ASSIGN est.est-type = 5 + (IF ls-add-what = "SET-SET" THEN 1 ELSE 0)
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = STRING(li-new-estnum,">>>>>>>>")
         est.form-qty = 1
         est.est-date = TODAY
         est.mod-date = ?
         .
   {sys/ref/est-add.i est}     
   
   RUN crt-est-childrecord.  /* create ef,eb,est-prep */
   lv-crt-est-rowid = ROWID(est).

   RELEASE est.
   RELEASE est-qty.
   RELEASE ef.
   RELEASE eb.

   /* refresh browser for new record */
   RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'Record-source':U,OUTPUT char-hdl).
/* may need to refresh  */
   RUN Refreshrow IN WIDGET-HANDLE(char-hdl) ("NewRecord", lv-crt-est-rowid).
   ll-dumb =  br_table:refresh() IN FRAME {&frame-name}.
   ASSIGN cocode = gcompany .      

/*   find est where rowid(est) = lv-crt-est-rowid no-lock no-error.
   display est.est-no est.est-date with browse {&browse-name}.
   disp eb.yld-qty with browse {&browse-name}.
 */

  RELEASE ce-ctrl.
   
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
  DEF VAR cocode AS cha NO-UNDO.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bb FOR eb.
  
  MESSAGE "new-item  1 "  "{&adm-first-enabled-table}" SKIP
          "2:" "{&adm-second-enabled-table}" " ,3:" "{&adm-third-enabled-table}"
          SKIP "gourp: " group-assign-target
           VIEW-AS ALERT-BOX.

  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */

  FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                           ce-ctrl.loc = gloc
                           NO-LOCK.
  li-new-estnum = ce-ctrl.e-num .
  ll-new-record = YES.
    
  ASSIGN est.est-type = 5
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = STRING(li-new-estnum,">>>>>>>>")
         est.form-qty = 1
         est.est-date = TODAY
         est.mod-date = ?
         .
  
   DISPLAY est.est-no est.est-date WITH BROWSE {&browse-name}.
            
   ASSIGN cocode = gcompany
         .      
   {sys/ref/est-add.i est}     

   RUN crt-est-childrecord.  /* create ef,eb,est-prep */

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
  DEF VAR i AS INT NO-UNDO.
  DEF BUFFER bb FOR eb.
  DEF BUFFER bf FOR ef.
  DEF BUFFER best FOR est.
  DEF VAR ls-part-no AS cha NO-UNDO.  /* set partno */
     
  /* not create est, est-qty, est-prep */
  
  ll-new-record = YES.
  FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                           ce-ctrl.loc = gloc
                           NO-LOCK.
          
  CREATE ef. 
  ASSIGN
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

  FIND FIRST bb WHERE bb.company = est.company AND
                      bb.est-no = est.est-no AND
                      bb.form-no = 0 AND
                      bb.blank-no = 0
                      NO-LOCK NO-ERROR.
  IF AVAIL bb THEN ls-part-no = bb.part-no.
  ELSE ls-part-no = "".

  FIND LAST bb WHERE bb.company = est.company AND
                      bb.est-no = est.est-no 
                      NO-LOCK NO-ERROR.                      
  CREATE eb. 
  ASSIGN  eb.est-type = 6
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = INTEGER(est.est-no)
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
   eb.quantityPerSet = 1
   eb.part-no = ls-part-no + "-" + string(eb.form-no)
   eb.tab-in = YES
   eb.len = 0
   eb.wid = 0
   eb.dep = 0
   eb.procat = IF AVAIL bb THEN bb.procat ELSE ""
   .


  /* ???? bugs : 2 records are created  , delete one ========== */
  FOR EACH bb WHERE bb.est-no = "" :
      DELETE bb.
  END.
  FOR EACH bf WHERE bf.est-no = "" :
      DELETE bf.
  END.
  /*  ========*/
  FIND FIRST item WHERE item.company = gcompany
                    AND item.mat-type = "C"  /* Case/Bundle */
                    AND item.i-no EQ eb.cas-no
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN DO:
     FIND FIRST e-item WHERE e-item.company EQ item.company
                         AND e-item.loc     EQ item.loc
                         AND e-item.i-no    EQ item.i-no
        NO-LOCK NO-ERROR.
     FIND FIRST itemfg  WHERE itemfg.company EQ gcompany
                          AND itemfg.i-no    EQ eb.stock-no
        NO-LOCK NO-ERROR.
     IF AVAIL e-item THEN
        ASSIGN  eb.cas-len = e-item.case-l
                eb.cas-wid = e-item.case-w
                eb.cas-dep = e-item.case-d
                eb.cas-wt  = e-item.avg-w
                eb.cas-pal = e-item.case-pall
                eb.cas-cnt = IF AVAIL itemfg THEN itemfg.case-count ELSE e-item.box-case
                .
     IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
     IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
     IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
     IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
     IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
     IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
              IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
  END.  /* avail item */

  FIND best WHERE RECID(best) = recid(est) EXCLUSIVE-LOCK.
  ASSIGN  best.form-qty = est.form-qty + 1
          best.est-type = 6.
    
  DISPLAY eb.cust-no eb.ship-id eb.part-no eb.tab-in 
          eb.len eb.wid eb.dep eb.procat
          eb.yld-qty eb.quantityPerSet
          WITH BROWSE {&browse-name}.

  ASSIGN lv-eb-recid = RECID(eb) 
         lv-ef-recid = RECID(ef).

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
  FOR EACH est-qty WHERE est-qty.company = est.company 
                     AND est-qty.est-no = est.est-no
                     :
     FOR EACH eb WHERE eb.company = est.company 
                   AND eb.est-no = est.est-no
                   AND eb.eqty = est-qty.eqty:
         DELETE eb.             
     END.
     FOR EACH ef WHERE ef.company = est.company 
                   AND ef.est-no = est.est-no
                   AND ef.eqty = est-qty.eqty:
         DELETE ef.             
     END.
     DELETE est-qty.
  END.   
  FOR EACH est-prep WHERE est-prep.company = est.company
                      AND est-prep.est-no = est.est-no:
      DELETE est-prep.
  END.
  FOR EACH est-op WHERE est-op.company = est.company
                    AND est-op.est-no = est.est-no:
      DELETE est-op.
  END.
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
  IF NOT ll-is-add-from-tool THEN DO:
     RUN est/d-addwh2.w  (OUTPUT ls-add-what).  /*for combo estimate only */
     IF ls-add-what = "set"    /* new estimate */
        THEN RUN est/d-addset.w (OUTPUT ls-add-what). /* one item or set cec/est-add.p */
   
     IF ls-add-what = "" THEN RETURN NO-APPLY.  /* cancel from dialog box */
  END.
  
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
  DEF VAR is-first-record AS LOG INIT NO NO-UNDO.
  DEF BUFFER bf-eb FOR eb.  
  DEF BUFFER bf-ef FOR ef.
  DEF BUFFER bf-est FOR est.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR xx AS DEC NO-UNDO.
  DEF VAR lv-hld-cust LIKE eb.cust-no NO-UNDO.
  DEF VAR lv-hld-ship LIKE eb.ship-id NO-UNDO.
  DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
  
  {sys/inc/ceroute.i C}
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL ef THEN FIND ef WHERE RECID(ef) = lv-ef-recid NO-LOCK NO-ERROR.
  
  ASSIGN
   lv-hld-cust = eb.cust-no
   lv-hld-ship = eb.ship-id.
  IF AVAIL ef THEN
    lv-hld-board = ef.board.

  RUN get-attribute IN adm-broker-hdl ('Is-First-Est').
  IF RETURN-VALUE = "Yes" THEN DO:
       is-first-record = YES.
       RUN set-attribute-list IN adm-broker-hdl ('Is-First-Est=No'). /* reset */
  END.     
  ELSE is-first-record = NO.
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
  FOR EACH bf-eb WHERE bf-eb.company = est-qty.company AND
                       bf-eb.est-no = est-qty.est-no:
      ASSIGN bf-eb.eqty = est-qty.eqty.
  END.  
  FOR EACH bf-ef WHERE bf-ef.company = est-qty.company AND
                       bf-ef.est-no = est-qty.est-no:
         bf-ef.eqty = est-qty.eqty.
  END.

  FIND bf-est WHERE bf-est.company = est-qty.company AND
                    bf-est.est-no = est-qty.est-no.
  bf-est.est-qty[1] = est-qty.eqty.                  
  IF NOT ll-is-copy-record AND (ll-new-record OR is-first-record)
  THEN DO:
     IF eb.stock-no = "" THEN DO:
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany AND
                                 ce-ctrl.loc = gloc
                                 NO-LOCK NO-ERROR.
        eb.cas-no = ce-ctrl.def-case.
        eb.tr-no = ce-ctrl.def-pal.      
     END.
     FIND cust WHERE cust.company = gcompany AND
                     cust.cust-no = eb.cust-no
                     NO-LOCK NO-ERROR.
     eb.cas-no = IF AVAIL cust AND cust.case-bundle <> "" THEN cust.case-bundle ELSE eb.cas-no.
     eb.tr-no = IF AVAIL cust AND cust.pallet <> "" THEN cust.pallet ELSE eb.tr-no.      
     /* get default values from rm table */
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.cas-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN /*eb.cas-cost:Screen-value = */
                              eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)         
                              .
     FIND item WHERE item.company = eb.company AND
                     item.i-no = eb.tr-no
              NO-LOCK NO-ERROR.
     IF AVAIL item THEN ASSIGN /*eb.cas-cost:Screen-value = */
                              eb.tr-len = (item.case-l)
                              eb.tr-wid = (item.case-w)
                              eb.tr-dep = (item.case-d)
                              .
     FIND FIRST shipto WHERE shipto.company = est.company AND
                             shipto.cust-no = eb.cust-no
                             NO-LOCK NO-ERROR.
     IF AVAIL shipto THEN ASSIGN eb.ship-id = shipto.ship-id
                                 eb.carrier = shipto.carrier
                                 eb.ship-name = shipto.ship-name
                                 eb.ship-addr[1] = shipto.ship-addr[1]
                                 eb.ship-addr[2] = shipto.ship-addr[2]
                                 eb.ship-city = shipto.ship-city
                                 eb.ship-state = shipto.ship-state
                                 eb.ship-zip = shipto.ship-zip
                                 .
     FIND style WHERE style.company = est.company AND
                      style.style = eb.style:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.
     IF AVAIL style THEN DO:
        ASSIGN eb.adhesive = style.material[7]
               eb.gluelap = style.dim-gl
               eb.k-len = style.dim-dkl
               eb.k-wid = style.dim-dkw
               eb.fpanel = style.dim-pan5
               eb.lock = style.dim-fit
               eb.tuck = style.dim-tk.                 
               .
     END.  /* avail style */ 
     RUN calc-pass.
     RUN calc-blank-size.
     IF eb.gluelap <> 0 THEN eb.lin-in = eb.dep.    
     IF NOT AVAIL cust THEN FIND cust WHERE cust.company = eb.company AND
                                 cust.cust-no = eb.cust-no
                                 NO-LOCK NO-ERROR.
     eb.sman = cust.sman.   
     FIND FIRST sman WHERE sman.company EQ cust.company
                       AND sman.sman    EQ cust.sman
                     NO-LOCK NO-ERROR.
     FIND FIRST custype WHERE custype.custype EQ cust.type
              NO-LOCK NO-ERROR.
     IF AVAIL sman THEN DO:
        FIND FIRST sman-mtx WHERE sman-mtx.company EQ eb.company
                    AND sman-mtx.sman    EQ sman.sman
                    AND sman-mtx.custype EQ cust.type
                  NO-LOCK NO-ERROR.
        eb.comm = IF AVAIL sman-mtx THEN sman-mtx.type-comm
                  ELSE sman.scomm.           
     END.
     ELSE DO:
          IF AVAIL custype THEN eb.comm = custype.commrate.
          ELSE DO:
                FIND ce-ctrl WHERE ce-ctrl.company = eb.company AND
                                   ce-ctrl.loc = eb.loc
                             NO-LOCK NO-ERROR.
                eb.comm = ce-ctrl.comm-mrkup.
          END.
     END.
     FIND FIRST shipto WHERE shipto.company = est.company AND
                             shipto.cust-no = eb.cust-no
                             NO-LOCK NO-ERROR.
     IF AVAIL shipto THEN ASSIGN eb.ship-id = shipto.ship-id
                                 eb.carrier = shipto.carrier
                                 eb.ship-name = shipto.ship-name
                                 eb.ship-addr[1] = shipto.ship-addr[1]
                                 eb.ship-addr[2] = shipto.ship-addr[2]
                                 eb.ship-city = shipto.ship-city
                                 eb.ship-state = shipto.ship-state
                                 eb.ship-zip = shipto.ship-zip
                                 .
     IF CAPS(cust.cust-no) NE "TEMP" THEN
        ASSIGN
             eb.sman          = cust.sman
             eb.ship-id       = shipto.ship-id
             eb.ship-name     = shipto.ship-name
             eb.ship-addr[1]  = shipto.ship-addr[1]
             eb.ship-addr[2]  = shipto.ship-addr[2]
             eb.ship-city     = shipto.ship-city
             eb.ship-state    = shipto.ship-state
             eb.ship-zip      = shipto.ship-zip.
      FIND FIRST ce-ctrl WHERE ce-ctrl.company EQ eb.company
                           AND ce-ctrl.loc     EQ eb.loc
                NO-LOCK NO-ERROR.
      ASSIGN ef.lsh-wid = ce-ctrl.ls-length
             ef.lsh-len = ce-ctrl.ls-width
             ef.gsh-len = ef.lsh-len
             ef.gsh-wid = ef.lsh-wid
             ef.nsh-len = ef.lsh-len
             ef.nsh-wid = ef.lsh-wid
             ef.n-out   = 1
             ef.n-out-l = 1
             ef.n-out-d = 1
             ef.lam-dscr = "S".

     FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                           AND reftable.company = eb.style
                           AND (reftable.loc = eb.flute OR lv-foam)
                           AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.dscr <> "" THEN DO:
        FIND FIRST item WHERE item.company = gcompany AND
                              item.i-no = reftable.dscr
                              NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO:
           ASSIGN /*ef.board = item.i-no */
                  ef.brd-dscr = item.est-dscr 
                  ef.i-code = item.i-code
                  ef.flute = item.flute
                  ef.test = item.reg-no
                  ef.weight = item.basis-w
                  .
           IF item.i-code = "R" THEN ASSIGN ef.lsh-len = item.s-len
                                            ef.lsh-wid = item.s-wid
                                            ef.gsh-wid = item.s-wid.
           IF item.r-wid <> 0 THEN ASSIGN ef.roll = TRUE
                                          ef.roll-wid = item.r-wid
                                          ef.lsh-wid = item.r-wid
                                          ef.gsh-wid = item.r-wid.                                        
        END.          
     END.  /* avail reftable */
     IF style.material[4] NE "" THEN DO:  /* adder*/ 
               FIND FIRST item  WHERE item.company EQ cocode
                     AND item.i-no    EQ style.material[4]
                   NO-LOCK NO-ERROR.
               IF AVAIL item THEN
                  DO i = 1 TO 6:  
                     IF ef.adder[i] = "" THEN DO:
                         ASSIGN ef.adder[i]     = item.i-no
                                ef.adder[i + 6] = item.est-dscr
                                ef.weight       = ef.weight + item.basis-w.
                         LEAVE.       
                     END.           
                  END.
            END.
      IF style.material[5] NE "" THEN DO:  /* leaf label */
               FIND FIRST item  WHERE item.company EQ cocode
                                  AND item.i-no    EQ style.material[5]
                   NO-LOCK NO-ERROR.
               IF AVAIL item THEN /*leaf-block:
               for each ef where ef.company eq xest.company and
                                 ef.est-no = xest.est-no,
                   first eb of ef no-lock:  */
                  DO i = 1 TO 2:
                     IF ef.leaf[i] = "" THEN DO:
                        ASSIGN ef.leaf-snum[i] = ef.form-no
                               ef.leaf-bnum[i] = 1
                               ef.leaf[i]      = item.i-no
                               ef.leaf-dscr[i] = item.est-dscr
                               ef.leaf-l[i] = eb.t-len
                               ef.leaf-w[i] = eb.t-wid
                               .
                         LEAVE.
                     END.   
                  END.
            /*   end. */
     END.
     ef.xgrain = "N".
     FIND xest WHERE RECID(xest) = recid(est).
     FIND xef WHERE RECID(xef) = recid(ef).
     FIND xeb WHERE RECID(xeb) = recid(eb).  
     IF lv-foam THEN ASSIGN xeb.t-wid = xeb.wid
                         xeb.t-len = xeb.len
                         xeb.t-dep = xeb.dep
                         .
     RUN cec/calc-dim.p.
     RUN create-inst.
     RUN create-prep.
     DEF VAR lv-cas-pal AS DEC NO-UNDO.
     DEF VAR lv-tr-cnt AS INT NO-UNDO.
     DEF VAR lv-numstack AS INT NO-UNDO.
     DEF VAR lv-stackcode AS cha NO-UNDO.
     DEF VAR lv-error AS LOG NO-UNDO.
     RUN cec/kpallet.p (RECID(xeb), OUTPUT lv-cas-pal, OUTPUT lv-tr-cnt,
      OUTPUT lv-numstack, OUTPUT lv-stackcode, OUTPUT lv-error).
     IF lv-error THEN DO:
        MESSAGE "An error occured while attempting to calculate the number of pallets. "
                SKIP
                "Please review any previous error messages for more information." 
                 VIEW-AS ALERT-BOX ERROR.
     END.
     ELSE ASSIGN eb.cas-pal = lv-cas-pal
                 eb.tr-cnt = lv-tr-cnt.
                 
     IF NOT lv-foam THEN DO:
       {sys/inc/ceroute1.i w id l en} 
     END.
   END.  /* new not copy */
   
   {ce/uship-id.i ll-new-record}

   IF NOT ll-is-copy-record AND ceroute-log THEN DO:
     FIND xest WHERE RECID(xest) = recid(est).
     FIND xef WHERE RECID(xef) = recid(ef).
     FIND xeb WHERE RECID(xeb) = recid(eb).

     FOR EACH est-op
         WHERE est-op.company EQ xest.company
           AND est-op.est-no  EQ xest.est-no
           AND est-op.line    GE 500:
       DELETE est-op.
     END.
    
     IF CAN-FIND(FIRST est-op WHERE est-op.company EQ xest.company
                                AND est-op.est-no  EQ xest.est-no
                                AND est-op.s-num   EQ xef.form-no) THEN
     FOR EACH est-op
         WHERE est-op.company EQ xest.company
           AND est-op.est-no  EQ xest.est-no
           AND est-op.s-num   EQ xef.form-no
         NO-LOCK:
     END.
  
     ELSE DO:
        /* Protect existing est-op records */
       FOR EACH est-op
           WHERE est-op.company EQ xest.company
           AND est-op.est-no    EQ xest.est-no:
         IF NOT est-op.auto THEN est-op.line = est-op.line + 500.
                            ELSE est-op.auto = NO.
       END.
                                 
       xx = dec(xef.form-no).
    
       RUN cec/mach-seq.p (xef.form-no).
    
       FOR EACH est-op
           WHERE est-op.company EQ xest.company
             AND est-op.est-no  EQ xest.est-no
             AND est-op.s-num   NE int(xx):
          
         IF est-op.auto THEN DELETE est-op.
         ELSE
         IF est-op.line GE 500 THEN est-op.line = est-op.line - 500.
                               ELSE est-op.auto = YES.
       END.
     END.
   END.                         
  
  IF ll-crt-itemfg THEN DO:
     FIND xest WHERE RECID(xest) = recid(est) NO-LOCK NO-ERROR.
     FIND xeb WHERE RECID(xeb) = recid(eb) NO-LOCK NO-ERROR.
     FIND xef WHERE RECID(xef) = recid(ef) NO-LOCK NO-ERROR.
     RUN crt-itemfg (xeb.stock-no).
     ll-crt-itemfg = NO.
  END.
  /*== Qty assignment ==*/
  RUN assign-qty.

  /* Reset cust-no ship-id ... for set */
  IF eb.est-type = 6 THEN 
     FOR EACH bf-eb WHERE bf-eb.company = eb.company AND
                          bf-eb.est-no = eb.est-no :
         ASSIGN bf-eb.cust-no = eb.cust-no
                bf-eb.ship-id = eb.ship-id
                .              
   END.        
   IF est.est-type = 6 THEN DO:
        DEF VAR li-num-eb AS INT NO-UNDO.
        li-num-eb = 0.
        FOR EACH bf-eb WHERE bf-eb.company = est.company AND
                             bf-eb.est-no = est.est-no AND
                             bf-eb.form-no > 0 AND
                             bf-eb.blank-no > 0 NO-LOCK:
            li-num-eb = li-num-eb + 1. 
        END.
        IF li-num-eb <= 1 THEN DO:
           ASSIGN est.est-type = 5
                  eb.est-type = est.est-type
                  ef.est-type = est.est-type
                  .
        END.    
   END.
     
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
    DEF VAR char-hdl AS cha NO-UNDO.   

    /* Code placed here will execute PRIOR to standard behavior. */

    IF AVAIL est THEN li-form# = est.form-qty. /* for set creation on crt-new-set */
   
    IF NOT AVAIL ef THEN FIND ef WHERE RECID(ef) = lv-ef-recid NO-LOCK NO-ERROR.
    IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) =  lv-eb-recid NO-LOCK NO-ERROR.

    ls-cust-no = IF AVAIL eb THEN eb.cust-no ELSE "" .  /* for new item record */
    ls-ship-id = IF AVAIL eb THEN eb.ship-id ELSE "" .  /* for crt-new-set */

  /* Dispatch standard ADM method.                             */
  IF ls-add-what BEGINS "set" THEN DO: /* new estimate */
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  END.   
  ELSE DO:  /* item */
   /*    create eb.
       create ef.
    */   
  END.
  /* Code placed here will execute AFTER standard behavior.    */
  ls-set-part-no = "".
  IF ls-add-what BEGINS "set" THEN DO:  /* new estimate: set-set or set-item */
     RUN crt-new-est.
     IF ls-add-what = "set-set" THEN DO:
        RUN est/crt-set.w (ROWID(est), OUTPUT ls-set-part-no).
        /*def var phandle as widget-handle no-undo.
        def var char-hdl as cha no-undo.   
        RUN get-link-handle IN adm-broker-hdl
            (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
        phandle = WIDGET-HANDLE(char-hdl).
        RUN new-state in phandle ('update-begin':U). */
        IF ls-set-part-no <> "" THEN DO:
           FIND xeb WHERE RECID(xeb) = recid(eb).
           ASSIGN xeb.part-no = ENTRY(1,ls-set-part-no) + "-1"
                  xeb.part-dscr1 = ENTRY(2,ls-set-part-no)
                  xeb.part-dscr2 = ENTRY(3,ls-set-part-no)
                  xeb.stock-no = IF ENTRY(4,ls-set-part-no) <> "" THEN (ENTRY(4,ls-set-part-no) + "-1") ELSE ""
                  .
           RELEASE xeb.
        END.
     END.       

     RUN get-link-handle IN adm-broker-hdl
            (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
     phandle = WIDGET-HANDLE(char-hdl). 
     RUN new-state IN phandle ('update-begin':U).  /* to have save button */

     DISPLAY est.est-no est.est-date WITH BROWSE {&browse-name}.          
     DISP eb.yld-qty eb.quantityPerSet WITH BROWSE {&browse-name}.

  END.
  ELSE IF ls-add-what = "item" THEN DO:
      RUN crt-new-set.
      IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK.
      APPLY "entry" TO eb.part-no IN BROWSE {&browse-name}.
      
      RETURN NO-APPLY.
      
  /*  not working
  assign est.est-no:sensitive in browse {&browse-name} = no
         est.est-date:sensitive = no
         eb.cust-no:sensitive = no
         eb.ship-id:sensitive = no.
  */       
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
  DEF VAR lv-comp LIKE est.company NO-UNDO.
  DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
  DEF BUFFER bb FOR eb.
  DEF BUFFER bf FOR ef.  
  DEF BUFFER best FOR est.
  DEF VAR li-num-of-efrec AS INT NO-UNDO.
  DEF VAR ll-dum AS LOG NO-UNDO.
  DEF VAR li-est-type AS INT NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.  
  
  li-num-of-efrec = 0.
  FOR EACH bf WHERE bf.company = est.company
                AND bf.est-no = est.est-no
              NO-LOCK:
      li-num-of-efrec = li-num-of-efrec + 1. 
  END.
  
  /* Code placed here will execute PRIOR to standard behavior. */
   MESSAGE "Delete Currently Selected Record?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
   IF NOT response  THEN RETURN "ADM-ERROR":U.

   ASSIGN lv-comp = est.company
          lv-est-no = est.est-no
          li-est-type = est.est-type.
   IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid.
   IF NOT AVAIL ef THEN FIND ef WHERE RECID(ef) = lv-ef-recid.             

  /* Dispatch standard ADM method.                             */
  IF est.est-type = 5 THEN 
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  ELSE DO:  /* type 6 */
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
     FIND bb WHERE RECID(bb) = recid(eb).
     DELETE bb.
     FIND bf WHERE RECID(bf) = recid(ef).

     FOR EACH est-op WHERE est-op.company = lv-comp
                       AND est-op.est-no = lv-est-no 
                       AND est-op.s-num = bf.form-no:
         DELETE est-op.
     END.
     FOR EACH est-prep WHERE est-prep.company = lv-comp
                       AND est-prep.est-no = lv-est-no 
                       AND est-prep.s-num = bf.form-no:
         DELETE est-prep.
     END.

     DELETE bf.

     ll-dum = BROWSE {&browse-name}:delete-current-row().
     FIND best WHERE RECID(best) = recid(est).
     best.form-qty = best.form-qty - 1.
     /* after delete */
     FIND FIRST bf WHERE bf.company = lv-comp AND
                         bf.est-no = lv-est-no NO-LOCK NO-ERROR.
     IF NOT AVAIL bf THEN DO:
        FOR EACH est-qty WHERE est-qty.company = lv-comp 
                        AND est-qty.est-no = lv-est-no :
            DELETE est-qty.
        END.
        FOR EACH est-prep WHERE est-prep.company = lv-comp
                         AND est-prep.est-no = lv-est-no:
            DELETE est-prep.
        END.
        FOR EACH est-op WHERE est-op.company = lv-comp
                          AND est-op.est-no = lv-est-no:
            DELETE est-op.
        END.
        FOR EACH est WHERE est.company = lv-comp AND
                           est.est-no = lv-est-no:
            DELETE est.                        
        END.
        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl). 
        RUN dispatch IN WIDGET-HANDLE(char-hdl) ('get-next').
     END.   
  END.
  
  /* Code placed here will execute AFTER standard behavior.    */
  /* run delete-est-childrecord.  not working */

  IF NOT AVAIL est /* = est.est-type = 5*/  OR li-num-of-efrec <= 1 THEN DO:
     FOR EACH est-qty WHERE est-qty.company = lv-comp 
                        AND est-qty.est-no = lv-est-no :
         DELETE est-qty.
     END.
     FOR EACH est-prep WHERE est-prep.company = lv-comp
                         AND est-prep.est-no = lv-est-no:
         DELETE est-prep.
     END.
     FOR EACH est-op WHERE est-op.company = lv-comp
                       AND est-op.est-no = lv-est-no:
         DELETE est-op.
     END. 
     FOR EACH est WHERE est.company = lv-comp AND
                        est.est-no = lv-est-no:
         DELETE est.                        
     END.
  END. 
  IF li-est-type = 5 THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl). 
     RUN dispatch IN WIDGET-HANDLE(char-hdl) ('get-next').
  END.

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
   IF NOT AVAIL eb AND lv-eb-recid <> ? THEN FIND eb WHERE RECID(eb) = lv-eb-recid.
   IF NOT AVAIL ef AND lv-ef-recid <> ? THEN FIND ef WHERE RECID(ef) = lv-ef-recid.             

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     IF NOT AVAIL eb THEN RETURN.
     FIND style WHERE style.company = gcompany AND
                      style.style = eb.style:screen-value IN BROWSE {&browse-name}
                      NO-LOCK NO-ERROR.   
     lv-foam = IF AVAIL style AND style.type = "F" THEN YES ELSE NO.


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
  IF NOT AVAIL eb AND lv-eb-recid <> ? THEN FIND eb WHERE RECID(eb) = lv-eb-recid.
  IF NOT AVAIL ef AND lv-ef-recid <> ? THEN FIND ef WHERE RECID(ef) = lv-ef-recid.
  
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
MESSAGE "local exit".
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
  ASSIGN lv-eb-recid = RECID(eb)
         lv-ef-recid = RECID(ef).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR li-row-num AS INT NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  /* == validation ===== */
     IF eb.part-no:screen-value IN BROWSE {&browse-name} = "" THEN DO:
        MESSAGE "Customer Part # can not be blank!" VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.part-no.
        RETURN NO-APPLY.
     END.
     IF int(est-qty.eqty:screen-value ) <= 0 THEN DO:
        MESSAGE "Quantity must be entered. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO est-qty.eqty.
        RETURN NO-APPLY.
     END.

     IF /*(eb.style:screen-value in browse {&browse-name} <> "" and */
        NOT CAN-FIND(style WHERE style.company = gcompany AND
                               style.style = eb.style:screen-value IN BROWSE {&browse-name})
     THEN DO:
        IF eb.style:screen-value = "" THEN MESSAGE "Valid Style must be entered. Try Help." VIEW-AS ALERT-BOX ERROR.
        ELSE MESSAGE "Invalid Style. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.style.
        RETURN NO-APPLY.
     END. 
     IF /* eb.cust-no:screen-value <> "" and */
        NOT CAN-FIND(cust WHERE cust.company = gcompany AND cust.cust-no = eb.cust-no:screen-value)
     THEN DO:
        MESSAGE "Invalid Customer Number. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "Entry" TO eb.cust-no.
        RETURN NO-APPLY.
     END.

     IF NOT CAN-FIND(FIRST flute WHERE flute.code = eb.flute:screen-value IN BROWSE {&browse-name})
     THEN DO:
        MESSAGE "Invalid Flute Code." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.flute.
        RETURN NO-APPLY.
     END.

     IF ef.board:screen-value IN BROWSE {&browse-name} <> "" AND
        NOT CAN-FIND(item WHERE item.company = gcompany
                     AND item.i-no = ef.board:screen-value IN BROWSE {&browse-name} )
     THEN DO:
        MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ef.board.
        RETURN NO-APPLY.
     END.
     IF /*eb.procat:screen-value in browse {&browse-name} <> "" and */
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = gcompany AND
                    fgcat.procat = eb.procat:screen-value )
     THEN DO:
         MESSAGE "Invalid Category. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO eb.procat.
         RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.len:screen-value) = 0 THEN DO:
        MESSAGE "Length can not be 0. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.len.
        RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.wid:screen-value) = 0 THEN DO:
        MESSAGE "Width can not be 0. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO eb.wid.
        RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.wid:screen-value) - trunc(DECIMAL(eb.wid:screen-value),0) >= 0.16 
     THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO eb.wid.
             RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.len:screen-value) - trunc(DECIMAL(eb.len:screen-value),0) >= 0.16 
     THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO eb.len.
             RETURN NO-APPLY.
     END.
     IF DECIMAL(eb.dep:screen-value) - trunc(DECIMAL(eb.dep:screen-value),0) >= 0.16 
     THEN DO:
             MESSAGE "Can not have more than .15 as decimal, field is (inches.16ths) "
                      VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO eb.dep.
             RETURN NO-APPLY.
     END.
     

  /* ====== end validation =======*/
  IF NOT AVAIL eb THEN FIND eb WHERE RECID(eb) = lv-eb-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL ef THEN FIND ef WHERE RECID(ef) = lv-ef-recid NO-LOCK NO-ERROR.
  lv-rowid = ROWID(est).
  li-row-num = BROWSE {&browse-name}:focused-row.

  /* === check record locked ==== */
  FIND CURRENT est EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAIL est THEN DO:
     MESSAGE "Estimate Record is being changed by someone else. Wait a few minutes and try again. " VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  
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
     RUN dispatch ('open-query').
     REPOSITION {&browse-name} TO ROWID ROWID(ef).
     RUN notify ('row-available').
     
     RUN New_Record IN WIDGET-HANDLE(char-hdl) (ROWID(est)). 
     
    /* ll-dumb = browse {&browse-name}:select-focused-row().*/
/*  end. */

ll-is-add-from-tool = NO.  /* reset */
RUN get-attribute ('adm-new-record') .
IF RETURN-VALUE = "Yes" THEN RUN set-attribute-list ("ADM-NEW-RECORD = 'NO'").

SESSION:SET-WAIT-STATE("").
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
  RUN dispatch ('apply-entry').
  
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
  ( INPUT ip-is-corr-style AS LOG, INPUT  ip-dim AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR out-dim AS DEC DECIMALS 6 NO-UNDO.
  
  IF ip-is-corr-style AND ip-dim <> 0 AND v-cecscrn-char NE "Decimal" THEN 
     out-dim = ROUND(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  ELSE out-dim = ip-dim.

  RETURN out-dim.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

