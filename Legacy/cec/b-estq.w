&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  cec\b-estq.w

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
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc
 gcompany = g_company
 gloc = g_loc   .

DEF VAR li-new-estnum LIKE  ce-ctrl.e-num NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.

DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-first-run AS LOG INIT YES NO-UNDO.
DEF VAR ll-initial AS LOG INIT YES NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "est-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Estimate" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR lv-est-date-entered AS LOG NO-UNDO.
DEF VAR ll-shipto AS LOG NO-UNDO.

DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-est-no AS cha NO-UNDO.
DEF VAR lv-first-show-est-no AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
{sys/inc/f16to32.i}

DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR lActive AS LOG NO-UNDO.
DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""EC"" }
END.

&SCOPED-DEFINE key-phrase  YES
/*
    
&SCOPED-DEFINE for-est                            ~
         FOR EACH ASI.est WHERE ~{&KEY-PHRASE}      ~
                     AND est.company = g_company    ~
                     AND ASI.est.est-type >= 5 AND est.est-date >= vi_est-date
             
                    

&SCOPED-DEFINE for-eqty                            ~
          EACH ASI.est-qty WHERE  est-qty.company = ASI.est.company ~
                          AND ASI.est-qty.est-no = ASI.est.est-no OUTER-JOIN  

&SCOPED-DEFINE for-ef                     ~
           each ASI.ef OUTER-JOIN WHERE ASI.ef.company = ASI.est.company ~
                          AND ASI.ef.est-no = ASI.est.est-no AND ef.form-no > 0 

&SCOPED-DEFINE for-eb                     ~
        each ASI.eb OUTER-JOIN WHERE ASI.eb.company = ASI.est.company  ~
                 AND ASI.eb.est-no = ASI.est.est-no          ~
                 and eb.form-no = ef.FORM-no and eb.blank-no > 0      ~
                 AND eb.cust-no BEGINS begin_cust-no            ~
                 AND eb.part-no BEGINS vi_part-no            ~
                 AND eb.stock-no BEGINS vi_stock-no          ~
                 AND eb.style BEGINS vi_style                 ~
                 AND eb.len >= vi_len                   

/*
&SCOPED-DEFINE for-eb                     ~
        FIRST ASI.eb WHERE ASI.eb.company = ASI.est.company  ~
                 AND ASI.eb.est-no = ASI.est.est-no          ~
                 and eb.form-no > 0 and eb.blank-no > 0      ~
*/

&SCOPED-DEFINE sortby-log                             ~
    IF lv-sort-by EQ "est-no"  THEN est.est-no   ELSE     ~
    IF lv-sort-by EQ "est-date"  THEN STRING(est.est-date)  ELSE ~
    IF lv-sort-by EQ "eqty"  THEN STRING(est-qty.eqty)  ELSE ~
    IF lv-sort-by EQ "ord-no"  THEN STRING(eb.ord-no)  ELSE ~
    IF lv-sort-by EQ "cust-no" THEN eb.cust-no  ELSE ~
    IF lv-sort-by EQ "part-no"  THEN eb.part-no ELSE ~
    IF lv-sort-by EQ "stock-no"  THEN eb.stock-no  ELSE ~
    IF lv-sort-by EQ "style"  THEN eb.style  ELSE ~
    IF lv-sort-by EQ "part-dscr1"  THEN eb.part-dscr1  ELSE ~
    IF lv-sort-by EQ "flute"  THEN eb.flute  ELSE ~
    IF lv-sort-by EQ "len"  THEN STRING(eb.len)  ELSE ~
    IF lv-sort-by EQ "wid"  THEN STRING(eb.wid)  ELSE ~
    IF lv-sort-by EQ "dep"  THEN STRING(eb.dep)  ELSE ~
    IF lv-sort-by EQ "yld-qty"  THEN STRING(eb.yld-qty)  ELSE ~
    IF lv-sort-by EQ "die-no"  THEN eb.die-no  ELSE ~
    eb.plate-no  ~
    



&SCOPED-DEFINE sortby BY est.est-no BY eb.form-no BY eb.blank-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

*/
          
&SCOPED-DEFINE for-est                            ~
         EACH ASI.est WHERE ~{&KEY-PHRASE}      ~
                     AND est.company = g_company    ~
                     AND est.est-no = eb.est-no ~
                     AND ASI.est.est-type >= 5 ~
                     AND ((est.est-type EQ 5 AND tb_single) OR ~
                          (est.est-type EQ 6 AND tb_set)    OR ~
                          (est.est-type GT 6 AND tb_tancom))  
             
                    
&SCOPED-DEFINE for-eqty                            ~
          EACH ASI.est-qty WHERE  est-qty.company = ASI.est.company ~
                             AND ASI.est-qty.est-no = ASI.est.est-no ~
                             AND ASI.est-qty.eqty = eb.eqty 

&SCOPED-DEFINE for-ef                     ~
           each ASI.ef  WHERE ASI.ef.company = ASI.est.company ~
                          AND ASI.ef.est-no = ASI.est.est-no AND ef.form-no = eb.form-no  

&SCOPED-DEFINE for-eb                     ~
       FOR EACH ASI.eb WHERE ASI.eb.company = g_company  ~
                 AND eb.form-no > 0 AND eb.blank-no > 0                     ~
                 AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")             ~
                 AND (IF begin_cust-no BEGINS '*' THEN  eb.cust-no MATCHES begin_cust-no     ~
                     ELSE eb.cust-no BEGINS begin_cust-no )                ~
                 AND (IF begin_ship BEGINS '*' THEN  eb.ship-id MATCHES begin_ship     ~
                     ELSE eb.ship-id BEGINS begin_ship)                     ~
                 AND (eb.ship-id EQ begin_ship OR NOT ll-shipto)            ~
                 AND (IF vi_part-no BEGINS '*' THEN eb.part-no MATCHES vi_part-no     ~
                     ELSE eb.part-no BEGINS vi_part-no )                    ~
                 AND (IF vi_stock-no BEGINS '*' THEN eb.stock-no MATCHES vi_stock-no   ~
                     ELSE eb.stock-no BEGINS vi_stock-no)                    ~
                 AND (IF vi_part-dscr1 BEGINS '*' THEN eb.part-dscr1 MATCHES vi_part-dscr1  ~
                     ELSE eb.part-dscr1 BEGINS vi_part-dscr1)                  ~
                 AND (IF vi_style BEGINS '*' THEN eb.style MATCHES vi_style ~
                     ELSE eb.style BEGINS vi_style)                         ~
                 AND eb.die-no MATCHES vi_die-no                            ~
                 AND eb.cad-no BEGINS vi_cad-no                             ~
                 AND eb.plate-no BEGINS vi_plate-no                         ~
                 AND eb.len GE TRUNC(vi_len,0) + ((vi_len - TRUNC(vi_len,0)) * k_frac) ~
                 AND eb.len LE TRUNC(vi_len-2,0) + ((vi_len-2 - TRUNC(vi_len-2,0)) * k_frac) ~
                 AND eb.wid GE TRUNC(vi_wid,0) + ((vi_wid - TRUNC(vi_wid,0)) * k_frac) ~
                 AND eb.wid LE TRUNC(vi_wid-2,0) + ((vi_wid-2 - TRUNC(vi_wid-2,0)) * k_frac) ~
                 AND eb.dep GE TRUNC(vi_dep,0) + ((vi_dep - TRUNC(vi_dep,0)) * k_frac) ~
                 AND eb.dep LE TRUNC(vi_dep-2,0) + ((vi_dep-2 - TRUNC(vi_dep-2,0)) * k_frac) 


&SCOPED-DEFINE sortby-log                             ~
    IF lv-sort-by EQ "est-no"  THEN est.est-no   ELSE     ~
    IF lv-sort-by EQ "est-date"  THEN STRING((YEAR(est.est-date) * 10000) + (MONTH(est.est-date) * 100) + DAY(est.est-date))  ELSE ~
    IF lv-sort-by EQ "eqty"  THEN STRING(IF eb.est-type EQ 8 THEN eb.bl-qty ELSE est-qty.eqty,"9999999999")  ELSE ~
    IF lv-sort-by EQ "ord-no"  THEN STRING(eb.ord-no,"9999999999")  ELSE ~
    IF lv-sort-by EQ "cust-no" THEN eb.cust-no  ELSE ~
    IF lv-sort-by EQ "part-no"  THEN eb.part-no ELSE ~
    IF lv-sort-by EQ "stock-no"  THEN eb.stock-no  ELSE ~
    IF lv-sort-by EQ "style"  THEN eb.style  ELSE ~
    IF lv-sort-by EQ "part-dscr1"  THEN eb.part-dscr1  ELSE ~
    IF lv-sort-by EQ "flute"  THEN eb.flute  ELSE ~
    IF lv-sort-by EQ "Test"  THEN eb.test  ELSE ~
    IF lv-sort-by EQ "len"  THEN STRING(eb.len,">>>>>>9.99")  ELSE ~
    IF lv-sort-by EQ "wid"  THEN STRING(eb.wid,">>>>>>9.99")  ELSE ~
    IF lv-sort-by EQ "dep"  THEN STRING(eb.dep,">>>>>>9.99")  ELSE ~
    IF lv-sort-by EQ "yld-qty"  THEN STRING(9000000000 + eb.yld-qty,"9999999999")  ELSE ~
    IF lv-sort-by EQ "die-no"  THEN eb.die-no  ELSE ~
    IF lv-sort-by EQ "cad-no" THEN eb.cad-no ELSE ~
    IF lv-sort-by EQ "plate-no" THEN eb.plate-no ELSE ~
    IF lv-sort-by EQ "entered-id"  THEN est.entered-id  ELSE ~
    IF lv-sort-by EQ "updated-id" THEN est.updated-id ELSE ~
    IF lv-sort-by EQ "ship-id"  THEN eb.ship-id  ELSE ""
    



&SCOPED-DEFINE sortby BY est.est-no BY eb.form-no BY eb.blank-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DO TRANSACTION:
  {sys/inc/browser.i "CEBROWSE"}
  {sys/inc/cefgitem.i}
END.
ll-initial = browser-log.

DEF BUFFER blast-eb FOR eb.
DEF VAR lv-last-est-no AS cha NO-UNDO.
DEF VAR lv-persistent-handle AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,Tableio-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb est est-qty ef

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table est.est-no eb.cust-no ~
eb.part-no display-combo-qty () @ est-qty.eqty est-qty.eqty ~
display-combo-qty () @ est-qty.eqty eb.ord-no eb.stock-no eb.style ~
eb.part-dscr1 eb.flute eb.test eb.yld-qty eb.quantityPerSet ~
display-cw-dim(yes,eb.len) @ eb.len display-cw-dim(yes,eb.wid) @ eb.wid ~
display-cw-dim(yes,eb.dep) @ eb.dep eb.die-no eb.cad-no eb.plate-no ~
est.est-date est.updated-id eb.rec_key eb.pur-man eb.ship-id est.entered-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table est.est-no eb.cust-no ~
eb.part-no est-qty.eqty eb.ord-no eb.stock-no eb.style eb.part-dscr1 ~
eb.flute eb.test eb.yld-qty eb.quantityPerSet eb.die-no eb.cad-no ~
eb.plate-no est.est-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table est eb est-qty
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table est
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table eb
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-Browser-Table est-qty
&Scoped-define QUERY-STRING-Browser-Table FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = gcompany and ~
eb.est-no = lv-last-est-no NO-LOCK, ~
      FIRST est WHERE est.company = eb.company ~
  AND est.est-no = eb.est-no ~
  AND est.est-type >= 5 NO-LOCK, ~
      FIRST est-qty WHERE est-qty.company = eb.company and est-qty.est-no = eb.est-no ~
and est-qty.eqty = eb.eqty  ~
 NO-LOCK, ~
      FIRST ef WHERE ef.company = eb.company ~
  AND ef.est-no = eb.est-no ~
  AND ef.form-no = eb.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = gcompany and ~
eb.est-no = lv-last-est-no NO-LOCK, ~
      FIRST est WHERE est.company = eb.company ~
  AND est.est-no = eb.est-no ~
  AND est.est-type >= 5 NO-LOCK, ~
      FIRST est-qty WHERE est-qty.company = eb.company and est-qty.est-no = eb.est-no ~
and est-qty.eqty = eb.eqty  ~
 NO-LOCK, ~
      FIRST ef WHERE ef.company = eb.company ~
  AND ef.est-no = eb.est-no ~
  AND ef.form-no = eb.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table eb est est-qty ef
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table eb
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table est
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table est-qty
&Scoped-define FOURTH-TABLE-IN-QUERY-Browser-Table ef


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS vi_est-no begin_cust-no vi_part-no ~
vi_stock-no vi_part-dscr1 vi_style vi_len vi_len-2 vi_wid vi_wid-2 vi_dep ~
vi_dep-2 vi_die-no TG_exact-match vi_cad-no vi_plate-no tb_single tb_set ~
tb_tancom btn_go btn_prev Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS vi_est-no begin_cust-no begin_ship ~
vi_part-no vi_stock-no vi_part-dscr1 vi_style vi_len vi_len-2 vi_wid ~
vi_wid-2 vi_dep vi_dep-2 vi_die-no TG_exact-match vi_cad-no vi_plate-no ~
tb_single tb_set tb_tancom fi_sort-by FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-combo-qty B-table-Win 
FUNCTION display-combo-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
    ( INPUT ip-is-corr-style AS LOG, INPUT  ip-dim AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-yld-qty B-table-Win 
FUNCTION display-yld-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 14 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 16 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE begin_ship AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE vi_cad-no AS CHARACTER FORMAT "x(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_dep AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_dep-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 9999.9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_die-no AS CHARACTER FORMAT "x(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_est-date AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE vi_est-no AS CHARACTER FORMAT "x(8)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE vi_len AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_len-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 9999.9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_part-dscr1 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_part-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_plate-no AS CHARACTER FORMAT "x(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_stock-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_style AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_wid AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_wid-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 9999.9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 151 BY 5.24.

DEFINE VARIABLE tb_set AS LOGICAL INITIAL yes 
     LABEL "Set" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .86 NO-UNDO.

DEFINE VARIABLE tb_single AS LOGICAL INITIAL yes 
     LABEL "Single" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .86 NO-UNDO.

DEFINE VARIABLE tb_tancom AS LOGICAL INITIAL yes 
     LABEL "Tandem/Combo" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .86 NO-UNDO.

DEFINE VARIABLE TG_exact-match AS LOGICAL INITIAL no 
     LABEL "Exact" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.6 BY .81 TOOLTIP "Exact Match" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      eb
    FIELDS(eb.cust-no
      eb.part-no
      eb.ord-no
      eb.stock-no
      eb.style
      eb.part-dscr1
      eb.flute
      eb.test
      eb.yld-qty
      eb.quantityPerSet
      eb.len
      eb.len
      eb.wid
      eb.wid
      eb.dep
      eb.dep
      eb.die-no
      eb.cad-no
      eb.plate-no
      eb.rec_key
      eb.pur-man
      eb.ship-id), 
      est, 
      est-qty
    FIELDS(est-qty.eqty
      est-qty.eqty
      est-qty.eqty), 
      ef
    FIELDS() SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      est.est-no FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      eb.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U LABEL-BGCOLOR 14
      eb.part-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      display-combo-qty () @ est-qty.eqty
      est-qty.eqty FORMAT "->,>>>,>>9":U LABEL-BGCOLOR 14
      display-combo-qty () @ est-qty.eqty
      eb.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      eb.stock-no COLUMN-LABEL "FG Item #" FORMAT "x(15)":U LABEL-BGCOLOR 14
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U WIDTH 9 LABEL-BGCOLOR 14
      eb.part-dscr1 COLUMN-LABEL "Item Name" FORMAT "x(30)":U LABEL-BGCOLOR 14
      eb.flute FORMAT "XXX":U LABEL-BGCOLOR 14
      eb.test FORMAT "x(6)":U LABEL-BGCOLOR 14
      eb.yld-qty COLUMN-LABEL "Yield Quantity" FORMAT "->>>>>>9":U
            LABEL-BGCOLOR 14
      eb.quantityPerSet FORMAT ">>>>9.9<<<":U
      display-cw-dim(yes,eb.len) @ eb.len FORMAT ">>9.99":U LABEL-BGCOLOR 14
      display-cw-dim(yes,eb.wid) @ eb.wid FORMAT ">>9.99":U LABEL-BGCOLOR 14
      display-cw-dim(yes,eb.dep) @ eb.dep FORMAT ">>9.99":U LABEL-BGCOLOR 14
      eb.die-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      eb.cad-no COLUMN-LABEL "Cad #" FORMAT "x(15)":U LABEL-BGCOLOR 14
      eb.plate-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      est.est-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      est.updated-id COLUMN-LABEL "Modified By" FORMAT "X(8)":U
            WIDTH 15 LABEL-BGCOLOR 14
      eb.rec_key FORMAT "X(20)":U
      eb.pur-man FORMAT "P/M":U
      eb.ship-id FORMAT "x(8)":U LABEL-BGCOLOR 14
      est.entered-id COLUMN-LABEL "Created By" FORMAT "X(8)":U
            WIDTH 15 LABEL-BGCOLOR 14
  ENABLE
      est.est-no
      eb.cust-no
      eb.part-no
      est-qty.eqty
      eb.ord-no
      eb.stock-no
      eb.style
      eb.part-dscr1
      eb.flute
      eb.test
      eb.yld-qty
      eb.quantityPerSet
      eb.die-no
      eb.cad-no
      eb.plate-no
      est.est-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 151 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vi_est-no AT ROW 1.95 COL 2 NO-LABEL
     begin_cust-no AT ROW 1.95 COL 15 COLON-ALIGNED NO-LABEL
     begin_ship AT ROW 3 COL 15 COLON-ALIGNED NO-LABEL
     vi_part-no AT ROW 1.95 COL 29 COLON-ALIGNED NO-LABEL
     vi_stock-no AT ROW 1.95 COL 49 COLON-ALIGNED NO-LABEL
     vi_part-dscr1 AT ROW 3.14 COL 49 COLON-ALIGNED NO-LABEL
     vi_style AT ROW 1.95 COL 71 COLON-ALIGNED NO-LABEL
     vi_len AT ROW 1.95 COL 86 COLON-ALIGNED NO-LABEL
     vi_len-2 AT ROW 1.95 COL 103 COLON-ALIGNED NO-LABEL
     vi_wid AT ROW 2.91 COL 86 COLON-ALIGNED NO-LABEL
     vi_wid-2 AT ROW 2.91 COL 103 COLON-ALIGNED NO-LABEL
     vi_dep AT ROW 3.86 COL 86 COLON-ALIGNED NO-LABEL
     vi_dep-2 AT ROW 3.86 COL 103 COLON-ALIGNED NO-LABEL
     vi_die-no AT ROW 1.95 COL 116 COLON-ALIGNED NO-LABEL
     TG_exact-match AT ROW 2 COL 140.4 WIDGET-ID 4
     vi_cad-no AT ROW 2.91 COL 116 COLON-ALIGNED NO-LABEL
     vi_plate-no AT ROW 3.86 COL 116 COLON-ALIGNED NO-LABEL
     tb_single AT ROW 4.19 COL 3
     tb_set AT ROW 4.19 COL 17
     tb_tancom AT ROW 4.19 COL 30
     btn_go AT ROW 5.05 COL 3
     btn_prev AT ROW 5.05 COL 18
     btn_next AT ROW 5.05 COL 39
     vi_est-date AT ROW 5.05 COL 54 COLON-ALIGNED NO-LABEL
     fi_sort-by AT ROW 5.05 COL 81 COLON-ALIGNED NO-LABEL
     FI_moveCol AT ROW 5.05 COL 136.2 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     Browser-Table AT ROW 6.24 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "to" VIEW-AS TEXT
          SIZE 3 BY 1 AT ROW 2.91 COL 101
     "Estimate" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 3
          FGCOLOR 9 FONT 6
     "Customer" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 17
          FGCOLOR 9 FONT 6
     "FG Item# / Name" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.24 COL 51
          FGCOLOR 9 FONT 6
     "Style" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 73
          FGCOLOR 9 FONT 6
     "to" VIEW-AS TEXT
          SIZE 3 BY 1 AT ROW 1.95 COL 101
     "Customer Part#" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.24 COL 31
          FGCOLOR 9 FONT 6
     "Ship To" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.14 COL 6
          FGCOLOR 9 FONT 6
     "Match" VIEW-AS TEXT
          SIZE 6.8 BY .62 AT ROW 2.76 COL 144 WIDGET-ID 6
     "Sort By:" VIEW-AS TEXT
          SIZE 9.4 BY 1 AT ROW 5.05 COL 72.8
          FONT 6
     "to" VIEW-AS TEXT
          SIZE 3 BY 1 AT ROW 3.86 COL 101
     "Die # / Cad # / Plate #" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.24 COL 118
          FGCOLOR 9 FONT 6
     "L x W x D" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 96
          FGCOLOR 9 FONT 6
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 5.29 COL 115.2 WIDGET-ID 10
          FONT 6
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Design Page: 1
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
         HEIGHT             = 20.14
         WIDTH              = 151.8.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_ship IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

ASSIGN 
       eb.rec_key:VISIBLE IN BROWSE Browser-Table = FALSE
       eb.pur-man:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vi_est-date IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN vi_est-no IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.eb,ASI.est WHERE ASI.eb  ...,ASI.est-qty WHERE ASI.est ...,ASI.ef WHERE ASI.eb ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST, FIRST USED, FIRST USED"
     _Where[1]         = "eb.company = gcompany and
eb.est-no = lv-last-est-no"
     _JoinCode[2]      = "ASI.est.company = ASI.eb.company
  AND ASI.est.est-no = ASI.eb.est-no
  AND ASI.est.est-type >= 5"
     _JoinCode[3]      = "est-qty.company = eb.company and est-qty.est-no = eb.est-no
and est-qty.eqty = eb.eqty 
"
     _JoinCode[4]      = "ASI.ef.company = ASI.eb.company
  AND ASI.ef.est-no = ASI.eb.est-no
  AND ASI.ef.form-no = ASI.eb.form-no"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" ? "x(8)" "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.eb.cust-no
"eb.cust-no" "Cust#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"display-combo-qty () @ est-qty.eqty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.est-qty.eqty
"est-qty.eqty" ? "->,>>>,>>9" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"display-combo-qty () @ est-qty.eqty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.eb.ord-no
"eb.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.eb.stock-no
"eb.stock-no" "FG Item #" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.eb.part-dscr1
"eb.part-dscr1" "Item Name" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.eb.flute
"eb.flute" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.eb.test
"eb.test" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.eb.yld-qty
"eb.yld-qty" "Yield Quantity" "->>>>>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.eb.quantityPerSet
"eb.quantityPerSet" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"display-cw-dim(yes,eb.len) @ eb.len" ? ">>9.99" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"display-cw-dim(yes,eb.wid) @ eb.wid" ? ">>9.99" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"display-cw-dim(yes,eb.dep) @ eb.dep" ? ">>9.99" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.eb.die-no
"eb.die-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.eb.cad-no
"eb.cad-no" "Cad #" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.eb.plate-no
"eb.plate-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.est.est-date
"est.est-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.est.updated-id
"est.updated-id" "Modified By" ? "character" ? ? ? 14 ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.eb.rec_key
"eb.rec_key" ? ? "character" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.eb.pur-man
"eb.pur-man" ? ? "logical" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.eb.ship-id
"eb.ship-id" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.est.entered-id
"est.entered-id" "Created By" ? "character" ? ? ? 14 ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no B-table-Win
ON HELP OF begin_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-cust2.w (INPUT g_company, INPUT FOCUS:SCREEN-VALUE,"", OUTPUT char-val).
          IF char-val <> "" THEN
          DO:
            /* 11121503 Was moving to previous field for some reason */
            APPLY 'entry' TO begin_cust-no IN FRAME F-MAIN.
            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).

          END.
          /* return no-apply. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no B-table-Win
ON LEAVE OF begin_cust-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF {&self-name}:SCREEN-VALUE <> "" THEN DO:
     begin_ship:SENSITIVE = YES.
     APPLY "entry" TO begin_ship.
     RETURN NO-APPLY.
  END.
  ELSE begin_ship:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship B-table-Win
ON HELP OF begin_ship IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.


  RUN windows/l-shipto.w (g_company, g_loc, begin_cust-no:SCREEN-VALUE,
                          FOCUS:SCREEN-VALUE, OUTPUT char-val).
  IF char-val NE "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship B-table-Win
ON LEAVE OF begin_ship IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  APPLY "entry" TO vi_part-no .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   DEF VAR char-hdl AS cha NO-UNDO.
   DEF VAR phandle AS HANDLE NO-UNDO. 
 
   {methods/run_link.i "CONTAINER-SOURCE" "select-page" "(2)" }  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
   DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
   IF AVAIL est AND est.highlight THEN
      est.est-no:BGCOLOR IN BROWSE {&browse-name} = 14.
   ELSE
   IF AVAIL est AND est.mod-date = 01/01/1900 THEN
      est.est-no:bgcolor IN BROWSE {&browse-name} = 12.
   ELSE est.est-no:bgcolor = ?.

   IF v-cefgitem-log THEN
   DO:
/*                                                        */
/*       FIND FIRST reftable WHERE                        */
/*            reftable.reftable EQ "FGSTATUS" AND         */
/*            reftable.company  EQ cocode AND             */
/*            reftable.loc      EQ "" AND                 */
/*            reftable.code     EQ eb.stock-no            */
/*            NO-LOCK NO-ERROR.                           */
/*                                                        */
/*       IF AVAIL reftable AND reftable.code2 EQ "I" THEN */
      RUN fg/GetItemfgActInact.p (INPUT cocode,
                                  INPUT eb.stock-no,
                                  OUTPUT lActive).
      IF NOT lActive THEN
         eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = 11.
      ELSE
         eb.stock-no:BGCOLOR IN BROWSE {&browse-name} = ?.

      RELEASE reftable.
   END.

   IF v-cecscrn-char = "Decimal" THEN
     ASSIGN
        eb.wid:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.len:FORMAT IN BROWSE {&browse-name} = ">>9.999999"
        eb.dep:FORMAT IN BROWSE {&browse-name} = ">>9.999999".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN DO:
    APPLY 'ENTRY' TO vi_est-no IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
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
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.


  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /*{methods/template/local/setvalue.i}     */
  
  IF AVAIL est AND AVAIL eb THEN DO:
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(est.rec_key,'ESTIMATE:' + eb.rec_key + ' ' + {methods/headers/est.i})"}
  
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = est.rec_key))"}
  
    {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
       "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = est.rec_key))"}

     RUN paper-clip-image-proc(INPUT est.rec_key).

     RUN dept-image-proc.

    IF eb.stock-no NE "" THEN
    DO:
       FIND FIRST itemfg WHERE
            itemfg.company EQ eb.company AND
            itemfg.i-no EQ eb.stock-no
            NO-LOCK NO-ERROR.

       IF AVAIL itemfg THEN
       DO:
          RUN spec-image-proc(INPUT itemfg.rec_key, INPUT YES).
          RELEASE itemfg.
       END.
       ELSE
          RUN spec-image-proc(INPUT "", INPUT NO).
    END.
    ELSE
       RUN spec-image-proc(INPUT "", INPUT NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  SESSION:SET-WAIT-STATE("general").
  DEF VAR v-cust-no AS CHAR NO-UNDO .
  DEF BUFFER bf-eb  FOR eb .
  DEF BUFFER bf-est FOR est .
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "leave" TO FOCUS.

    ASSIGN
     begin_cust-no
     begin_ship
     vi_part-no
     vi_est-no
     vi_stock-no
     vi_part-dscr1
     vi_style
     vi_len 
     vi_wid 
     vi_dep
     vi_len-2 
     vi_wid-2 
     vi_dep-2  
     vi_est-date
     vi_die-no  
     vi_cad-no
     vi_plate-no
     tb_single
     tb_set
     tb_tancom
     .

    RUN dispatch ("open-query").

    GET FIRST Browser-Table .
            
     IF NOT AVAIL est THEN DO:
         IF begin_cust-no <> "" THEN DO:
             v-cust-no =begin_cust-no  .
         END.
         ELSE DO:
             FIND FIRST bf-eb WHERE bf-eb.company = cocode 
                 AND (bf-eb.cust-no BEGINS begin_cust-no  OR begin_cust-no = "" )
                 AND ( bf-eb.ship-id BEGINS begin_ship  OR begin_ship = "" )
                 AND  (bf-eb.part-no BEGINS vi_part-no OR vi_part-no = "")
                 AND (bf-eb.est-no = vi_est-no OR vi_est-no = "")
                 AND ( bf-eb.stock-no BEGINS vi_stock-no OR vi_stock-no = "")
                 AND  (bf-eb.style BEGINS vi_style OR vi_style = "")
                 AND (bf-eb.part-dscr1 BEGINS vi_part-dscr1 OR vi_part-dscr1 = "")
                 NO-LOCK NO-ERROR. 
             IF AVAIL bf-eb THEN
                 v-cust-no = bf-eb.cust-no .
             ELSE v-cust-no = "".
         END.
         FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = v-cust-no NO-LOCK NO-ERROR.
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
         ELSE
         MESSAGE "No Estimate Found, please update your Search Criteria."
                VIEW-AS ALERT-BOX ERROR.
     END.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Show Next */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     vi_est-no:SCREEN-VALUE = ""
     begin_cust-no:SCREEN-VALUE = ""
     vi_part-no:SCREEN-VALUE = ""
     vi_stock-no:SCREEN-VALUE    = ""
     vi_style:SCREEN-VALUE   = ""
     vi_len:SCREEN-VALUE  = ""
     vi_wid:SCREEN-VALUE  = ""
     vi_dep:SCREEN-VALUE  = ""
     vi_len-2:SCREEN-VALUE  = "99,999.99"
     vi_wid-2:SCREEN-VALUE  = "99,999.99"
     vi_dep-2:SCREEN-VALUE  = "99,999.99"
     vi_die-no:SCREEN-VALUE  = "*"
     
    /* vi_est-date:SCREEN-VALUE = ?  /*string(date(1,1,year(today)))*/ */
        .
    lv-show-next = YES.

    APPLY "choose" TO btn_go.
  END.
   SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_prev B-table-Win
ON CHOOSE OF btn_prev IN FRAME F-Main /* Show Previous */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     vi_est-no:SCREEN-VALUE = ""
     begin_cust-no:SCREEN-VALUE = ""
     vi_part-no:SCREEN-VALUE = ""
     vi_stock-no:SCREEN-VALUE    = ""
     vi_style:SCREEN-VALUE   = ""
     vi_len:SCREEN-VALUE  = ""
     vi_wid:SCREEN-VALUE  = ""
     vi_dep:SCREEN-VALUE  = ""
     vi_len-2:SCREEN-VALUE  = "99,999.99"
     vi_wid-2:SCREEN-VALUE  = "99,999.99"
     vi_dep-2:SCREEN-VALUE  = "99,999.99"
     vi_die-no:SCREEN-VALUE  = "*"
    /* vi_est-date:SCREEN-VALUE = ?  /*string(date(1,1,year(today)))*/ */
        
    lv-show-prev = YES.
  
    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.
   SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG_exact-match
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_exact-match B-table-Win
ON VALUE-CHANGED OF TG_exact-match IN FRAME F-Main /* Exact */
DO:
   ASSIGN {&self-name}.
   IF TG_exact-match = NO THEN DO:
      IF INDEX(vi_die-no:SCREEN-VALUE,"*") = 0 THEN
         vi_die-no:SCREEN-VALUE = CAPS(vi_die-no:SCREEN-VALUE + "*"). 
   END.
   ELSE
      IF INDEX(vi_die-no:SCREEN-VALUE,"*", 1) > 0 THEN
         vi_die-no:SCREEN-VALUE = REPLACE(vi_die-no:SCREEN-VALUE, "*", "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_cad-no B-table-Win
ON LEAVE OF vi_cad-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_dep B-table-Win
ON LEAVE OF vi_dep IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_dep-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_dep-2 B-table-Win
ON LEAVE OF vi_dep-2 IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_die-no B-table-Win
ON LEAVE OF vi_die-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
   IF TG_exact-match:SCREEN-VALUE = "NO" THEN
   DO:
      IF INDEX({&self-name}:SCREEN-VALUE,"*") = 0 THEN
         {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE + "*").
   END.
   ELSE DO:
      IF INDEX(vi_die-no:SCREEN-VALUE,"*", 1) > 0 THEN
         vi_die-no:SCREEN-VALUE = REPLACE(vi_die-no:SCREEN-VALUE, "*", "").
      ASSIGN 
       {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_est-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_est-date B-table-Win
ON LEAVE OF vi_est-date IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  lv-est-date-entered = SELF:MODIFIED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_est-no B-table-Win
ON HELP OF vi_est-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_est-no B-table-Win
ON LEAVE OF vi_est-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_len B-table-Win
ON LEAVE OF vi_len IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_len-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_len-2 B-table-Win
ON LEAVE OF vi_len-2 IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-dscr1 B-table-Win
ON LEAVE OF vi_part-dscr1 IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-no B-table-Win
ON HELP OF vi_part-no IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","part",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(2,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-no B-table-Win
ON LEAVE OF vi_part-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_plate-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_plate-no B-table-Win
ON LEAVE OF vi_plate-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_stock-no B-table-Win
ON HELP OF vi_stock-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-itemfg.w (g_company,"","", OUTPUT char-val).
    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_stock-no B-table-Win
ON LEAVE OF vi_stock-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_style B-table-Win
ON HELP OF vi_style IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   
   RUN windows/l-stylec.w (gcompany,FOCUS:SCREEN-VALUE, OUTPUT char-val).
   IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_style B-table-Win
ON LEAVE OF vi_style IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_wid B-table-Win
ON LEAVE OF vi_wid IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_wid-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_wid-2 B-table-Win
ON LEAVE OF vi_wid-2 IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{sys/inc/f3help.i}
 RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EC',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""EC""}
FIND LAST blast-eb WHERE blast-eb.company = gcompany 
                     AND blast-eb.est-type > 4 NO-LOCK NO-ERROR.
lv-last-est-no = IF AVAIL blast-eb THEN blast-eb.est-no ELSE "".

&SCOPED-DEFINE cellColumnDat cecb-estq

{methods/browsers/setCellColumns.i}

SESSION:DATA-ENTRY-RETURN = YES.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilterValues B-table-Win 
PROCEDURE clearFilterValues :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/clearFilterValues.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-est B-table-Win 
PROCEDURE create-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cocode AS cha NO-UNDO.
  DEF BUFFER bf-est FOR est.
  DEF BUFFER bb FOR eb.
  DEF BUFFER recalc-mr FOR reftable.


  /*  don't use e-num any more as key index
  find last bf-est use-index e-num no-lock no-error.
  li-enum = if avail bf-est then bf-est.e-num else 0.
  */

  REPEAT:
  
    FIND FIRST ce-ctrl WHERE
         ce-ctrl.company = gcompany AND
         ce-ctrl.loc = gloc
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   
    IF AVAIL ce-ctrl THEN
    DO:
       ASSIGN
       li-new-estnum = ce-ctrl.e-num + 1
       ce-ctrl.e-num = li-new-estnum.
       FIND CURRENT ce-ctrl NO-LOCK.
       LEAVE.
    END.
  END.

  CREATE est.  
  ASSIGN 
         ll-new-record = YES
         est.est-type = 5
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = STRING(li-new-estnum,">>>>>>>9")
         est.form-qty = 1
         est.est-date = TODAY
         est.mod-date = ?
         cocode = gcompany.      

   {sys/ref/est-add.i est}     

   RUN crt-est-childrecord.  /* create ef,eb,est-prep */

  RUN local-open-query.  
  RUN set-attribute-list IN adm-broker-hdl ('Is-First-Est = Yes').

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
  DEFINE VARIABLE cPackCodeOverride AS CHARACTER NO-UNDO.
  DEF BUFFER bb FOR eb.
 
  CREATE est-qty.
  ASSIGN est-qty.company = gcompany
         est-qty.est-no =  est.est-no
         est-qty.eqty = 0.
          
  CREATE ef.
  ASSIGN
   ef.est-type  = 5
   ef.company   = gcompany
   ef.loc       = gloc
   ef.e-num     = est.e-num
   ef.est-no    = est.est-no
   ef.form-no   = 1
   ef.cust-seq  = 1
   ef.blank-qty = 1
   ef.lsh-len   = ce-ctrl.ls-length
   ef.lsh-wid   = ce-ctrl.ls-width.

  CREATE eb.
  ASSIGN  eb.est-type = 5
          eb.company  = gcompany
   eb.loc      = gloc
   eb.e-num    = est.e-num
   eb.est-no   = est.est-no
   eb.est-int  = INTEGER(est.est-no)
   eb.form-no  = 1
   eb.cust-seq = 1
   eb.blank-no = 1
   eb.cas-no   = ce-ctrl.def-case
   eb.tr-no    = ce-ctrl.def-pal
   eb.i-pass   = 0.
    
  RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
  IF cPackCodeOverride GT "" THEN 
      eb.cas-no = cPackCodeOverride.
      
  /* ???? bugs : 2 records are created  , delete one ========== 
  for each bb where bb.e-num = 0 :
      delete bb.
  end.
  ========*/
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
/*              est-prep.company = est.company                                             */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-image-proc B-table-Win 
PROCEDURE dept-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
  
   FIND FIRST notes WHERE notes.rec_key = est.rec_key
       NO-LOCK NO-ERROR.
   
   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-note B-table-Win 
PROCEDURE disable-note :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER op-enable-note AS LOG  NO-UNDO.
 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR FromEstNo AS CHAR NO-UNDO.
DEF VAR ToEstNo AS CHAR NO-UNDO.

IF est.est-no NE "" THEN
    ASSIGN
    FromEstNo = est.est-no
    ToEstNo   = est.est-no . 

RUN fg/EstC-exp.w (FromEstNo,ToEstNo).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-run B-table-Win 
PROCEDURE first-run :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-est-no AS CHAR INIT "" NO-UNDO.

&SCOPED-DEFINE where-first1           ~
      WHERE est.company  EQ g_company ~
        AND est.est-type GE 5         ~
      USE-INDEX est-no2


RUN set-defaults.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "CEBROWSE"
                        NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        /*message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                update sys-ctrl.log-fld. */
END.
  
IF ll-initial THEN DO:
  li = 0.
  /*FOR EACH est {&where-first1} NO-LOCK BY est.est-no DESC:

     li = li + 1.
     lv-est-no = est.est-no.
     IF li >= sys-ctrl.int-fld THEN LEAVE.
  END.*/

  RELEASE est.
  IF custcount <> "" THEN  DO:
      
      FOR EACH est {&where-first1} NO-LOCK,
          FIRST eb WHERE eb.company = g_company
          AND eb.est-no = est.est-no 
          AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "") NO-LOCK BY est.est-no DESC:
          IF est.est-no NE lv-est-no THEN li = li + 1.
          lv-est-no = est.est-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
  END.
  ELSE DO:
      FIND LAST est {&where-first1} NO-LOCK NO-ERROR.
      DO WHILE AVAIL est:
          IF est.est-no NE lv-est-no THEN li = li + 1.
          lv-est-no = est.est-no.
          IF li GE sys-ctrl.int-fld THEN LEAVE.
          
          FIND PREV est {&where-first1} NO-LOCK NO-ERROR.
      END.
  END.

  &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                   FOR EACH eb WHERE eb.company = g_company AND eb.est-no >= lv-est-no  ~
                              AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")        ~
                              AND eb.form-no > 0 AND eb.blank-no > 0 NO-LOCK USE-INDEX est-no,  ~
                   {&for-est}  NO-LOCK, ~
                   {&for-eqty} NO-LOCK, ~
                   {&for-ef}  NO-LOCK
            
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE lv-first-run = NO.

ll-initial = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-records B-table-Win 
PROCEDURE get-num-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-num-record AS INT.


  op-num-record = NUM-RESULTS("{&browse-name}") - 1.
  IF op-num-record LT 0 THEN op-num-record = 0.
  IF op-num-record NE 0 AND lv-last-rowid EQ ip-rowid THEN op-num-record = ?.

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
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

  IF v-cecscrn-char = "Decimal" THEN
     ASSIGN
        eb.wid:WIDTH IN BROWSE {&browse-name} = 13
        eb.len:WIDTH IN BROWSE {&browse-name} = 13
        eb.dep:WIDTH IN BROWSE {&browse-name} = 13
        vi_len:FORMAT = "->>,>>9.999999"
        vi_len-2:FORMAT = "->>,>>9.999999"
        vi_wid:FORMAT = "->>,>>9.999999"
        vi_wid-2:FORMAT = "->>,>>9.999999"
        vi_dep:FORMAT = "->>,>>9.999999"
        vi_dep-2:FORMAT = "->>,>>9.999999".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide B-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-first FOR est.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  FIND FIRST bf-first WHERE bf-first.company = gcompany AND
                            bf-first.loc = gloc AND
                            bf-first.est-type >= 5 
                            NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-first THEN RUN create-est.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  RUN setCellColumns.

  ASSIGN 
   est.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
   est.est-date:READ-ONLY IN BROWSE {&browse-name} = YES
   est-qty.eqty:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.part-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   eb.stock-no:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.style:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.part-dscr1:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.flute:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.test:READ-ONLY IN BROWSE {&browse-name} = YES
 /*  eb.len:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.wid:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.dep:READ-ONLY IN BROWSE {&browse-name} = YES */
   eb.yld-qty:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.die-no:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.cad-no:READ-ONLY IN BROWSE {&browse-name} = YES
   eb.plate-no:READ-ONLY IN BROWSE {&browse-name} = YES
   vi_die-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*"
   FI_moveCol = "Sort".

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

  /*RUN set-focus.*/
  ENABLE {&browse-name} WITH FRAME {&FRAME-NAME}. 

  APPLY "entry" TO  vi_est-no IN FRAME {&FRAME-NAME}. 
  
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

  IF lv-first-run THEN DO:
     RUN first-run.
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN DO:
       RUN show-all.
  END.
  ELSE DO:
    {cec/j-esteb.i}
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").
    RUN dispatch ("row-changed").
    APPLY "value-changed" TO BROWSE {&browse-name}.

    /*RUN dispatch ('get-last':U).*/
    GET LAST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-est-no = est.est-no.

    /*RUN dispatch ('get-first':U).*/
    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-first-show-est-no = est.est-no.

    lv-first-run = NO.
    lv-show-prev = NO.
    lv-show-next = NO.
  END.

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
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

  {methods/run_link.i "CONTAINER-SOURCE" "disable-enable-farm" "(NO)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         Browser-Table:COLUMN-MOVABLE = v-col-move
         Browser-Table:COLUMN-RESIZABLE = v-col-move
         v-col-move = NOT v-col-move
         FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
      DISPLAY FI_moveCol.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.


  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN RUN dispatch ('get-next':U).
    WHEN "P" THEN RUN dispatch ('get-prev':U).
  END CASE.
    
  IF ROWID(ar-invl) EQ lv-last-rowid THEN
    op-nav-type = "L".
      
  IF ROWID(ar-invl) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

    APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser2 B-table-Win 
PROCEDURE navigate-browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.
  
  DEF VAR hld-rowid AS ROWID NO-UNDO.


  hld-rowid = ROWID(ar-inv).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(ar-inv) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END.
    WHEN "P" THEN DO WHILE ROWID(ar-inv) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
                  END.
  END CASE.
    
  IF ROWID(ar-inv) EQ lv-last-rowid2 THEN
    op-nav-type = "L".
      
  IF ROWID(ar-inv) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".
    
    APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New_record B-table-Win 
PROCEDURE New_record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  /*vi_est-no = "".
  lv-first-run = YES.*/
  /* Set the lv-est-no value so that search will be faster */
  FIND FIRST eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAILABLE eb THEN 
    RUN setEstNoSearch (INPUT eb.est-no).

  RUN local-open-query.

  DO WITH FRAME {&frame-name}:      

    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ('row-changed').

    APPLY "value-changed" TO {&browse-name}.
    RETURN NO-APPLY.  
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New_record-user B-table-Win 
PROCEDURE New_record-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
 DEF VAR v-custcount AS CHAR NO-UNDO . 
 /* Set the lv-est-no value so that search will be faster */ 
 FIND FIRST eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAILABLE eb THEN 
    RUN setEstNoSearch (INPUT eb.est-no).

  ASSIGN
       v-custcount = custcount 
       custcount   = "" .
  RUN local-open-query.
 
ASSIGN custcount = v-custcount .
  DO WITH FRAME {&frame-name}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.  
    RUN dispatch ('row-changed').
    APPLY "value-changed" TO {&browse-name}.
    RETURN NO-APPLY.  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   {sys/ref/attachlogic.i}
  
   IF v-est-no <> "" AND v-i-no <> "" THEN
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              (LOOKUP(attach.rec_key,v-rec-key-list) GT 0 AND
              (TRIM(attach.est-no) = trim(v-est-no)) OR 
               (INDEX(v-i-no,attach.i-no) > 0))).
   ELSE
      IF v-est-no <> "" /*AND v-i-no EQ ""*/ THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              LOOKUP(attach.rec_key,v-rec-key-list) GT 0 AND
              trim(attach.est-no) = trim(v-est-no)).
   ELSE
      IF v-est-no EQ "" AND v-i-no <> "" THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              index(v-i-no,attach.i-no) > 0).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN paper-clip-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
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
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "est-qty"}
  {src/adm/template/snd-list.i "ef"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN vi_est-no = ""
          begin_cust-no = ""
          begin_ship = ""
          vi_part-no = ""
          vi_stock-no = ""
          vi_plate-no = ""
          vi_cad-no = ""
          vi_style = ""
          vi_len = 0
          vi_wid = 0
          vi_dep = 0
          vi_len-2 = 99999.99
          vi_wid-2 = 99999.99
          vi_dep-2 = 99999.99
          vi_die-no = "*"
          vi_est-date = ? /* DATE(1,1,YEAR(TODAY)) */.

   DISP  vi_est-no 
          begin_cust-no
          begin_ship
          vi_part-no 
          vi_stock-no
          vi_style
          vi_wid
          vi_dep
          vi_len-2
          vi_wid-2
          vi_dep-2 
          vi_die-no 
          vi_est-date
          vi_plate-no
          vi_cad-no
          tb_single
          tb_set
          tb_tancom
          WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {methods/setfocus.i vi_est-no}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstNoSearch B-table-Win 
PROCEDURE setEstNoSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ipiEstNo LIKE eb.est-no NO-UNDO.

ASSIGN vi_est-no = ipiEstNo
       tb_single = YES 
       tb_set    = YES 
       tb_tancom = YES   
       begin_cust-no = ""
       begin_ship    = ""
       vi_part-no    = ""
       vi_stock-no   = ""
       vi_part-dscr1 = ""
       vi_style      = ""
       vi_len        = 0.00
       vi_wid        = 0.00
       vi_dep        = 0.00
       vi_len-2      = 99999.99
       vi_wid-2      = 99999.99
       vi_dep-2      = 99999.99
       vi_die-no     = "*" 
       vi_cad-no     = ""
       vi_plate-no   = ""  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-all B-table-Win 
PROCEDURE show-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-est-no AS cha NO-UNDO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "CEBROWSE"
                        NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        /*message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                update sys-ctrl.log-fld. */
END.

RUN set-defaults.
  
IF lv-show-prev THEN DO:
   li = 0.
  FOR EACH est WHERE est.company = g_company      
                 AND est.est-no <= lv-last-show-est-no
                AND est.est-type >= 5  NO-LOCK,
      FIRST eb WHERE eb.company = g_company
          AND eb.est-no = est.est-no 
          AND ( (LOOKUP(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "") NO-LOCK BY est.est-no DESC:

     li = li + 1.
     lv-est-no = est.est-no.
     IF li >= sys-ctrl.int-fld THEN LEAVE.
  END.
  /*MESSAGE lv-last-show-est-no lv-first-show-est-no lv-est-no VIEW-AS ALERT-BOX.*/

  &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                   FOR EACH eb WHERE eb.company = g_company AND eb.est-no >= lv-est-no  ~
                                 AND eb.est-no <= lv-last-show-est-no   ~
                              AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")        ~
                              AND eb.form-no > 0 AND eb.blank-no > 0 NO-LOCK USE-INDEX est-no,  ~
                   {&for-est}  NO-LOCK, ~
                   {&for-eqty} NO-LOCK, ~
                   {&for-ef}  NO-LOCK
            
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:
    li = 0.   

    FOR EACH est WHERE est.company = g_company
                  AND est.est-no >= lv-first-show-est-no
                  AND est.est-type >= 5  NO-LOCK,
        FIRST eb WHERE eb.company = g_company
          AND eb.est-no = est.est-no 
          AND ( (LOOKUP(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "") NO-LOCK BY est.est-no  :

       li = li + 1.
       lv-est-no = est.est-no.
       IF li >= sys-ctrl.int-fld THEN LEAVE.
    END.
   /*MESSAGE "NEXT:" lv-last-show-est-no "," lv-first-show-est-no "," lv-est-no VIEW-AS ALERT-BOX.*/

    &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
                     FOR EACH eb WHERE eb.company = g_company AND eb.est-no <= lv-est-no  ~
                                   AND eb.est-no >= lv-first-show-est-no   ~
                                AND ( (lookup(eb.cust-no,custcount) <> 0 AND eb.cust-no <> "") OR custcount = "")        ~
                                AND eb.form-no > 0 AND eb.blank-no > 0 NO-LOCK USE-INDEX est-no,  ~
                     {&for-est}  NO-LOCK, ~
                     {&for-eqty} NO-LOCK, ~
                     {&for-ef}  NO-LOCK



            IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                           ELSE {&open-query} {&sortby-phrase-desc}. 

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-image-proc B-table-Win 
PROCEDURE spec-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-search AS LOG NO-UNDO.

   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
  
   IF ip-search THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = ip-rec_key AND
               notes.note_type = "S").

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-combo-qty B-table-Win 
FUNCTION display-combo-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-qty LIKE est-qty.eqty NO-UNDO.

  DEF BUFFER b-eb FOR eb.


  IF AVAIL est-qty AND AVAIL eb THEN DO:
    lv-qty = est-qty.eqty.

    FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
    IF AVAIL b-eb AND b-eb.est-type EQ 8 THEN lv-qty = b-eb.bl-qty.
  END.

  RETURN lv-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-yld-qty B-table-Win 
FUNCTION display-yld-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR out-qty LIKE eb.yld-qty NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF eb.yld-qty LT 0 THEN
      ASSIGN
       out-qty = eb.yld-qty * -1
       eb.yld-qty:FORMAT IN BROWSE {&browse-name} = "/>>>>>>".
    ELSE
      ASSIGN
       out-qty = eb.yld-qty
       eb.yld-qty:FORMAT IN BROWSE {&browse-name} = "->>>>>>". 
  END.

  RETURN out-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

