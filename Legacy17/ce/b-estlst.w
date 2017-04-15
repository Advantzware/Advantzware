&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p on 04.07.2017 @  2:08:00 pm */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ce\b-estlst.w

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
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}
def var li-new-estnum like  ce-ctrl.e-num no-undo.
def var ll-new-record as log no-undo.
def var lv-frst-rowid2 as rowid no-undo.
def var lv-last-rowid2 as rowid no-undo.

DEF BUFFER recalc-mr FOR reftable.

/* for customized browsing */
&scoped-define item-key-phrase  est

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est est-qty ef eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table est.est-no est.est-date ~
est-qty.eqty eb.ord-no eb.cust-no eb.part-no eb.stock-no eb.style ~
eb.part-dscr1 eb.flute eb.test eb.len eb.yld-qty eb.wid eb.die-no eb.dep ~
eb.plate-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH est WHERE ~{&KEY-PHRASE} ~
      AND est.company = gcompany and ~
ASI.est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      FIRST est-qty WHERE est-qty.company ~
 = est.company ~
  AND est-qty.est-no = est.est-no OUTER-JOIN NO-LOCK, ~
      FIRST ef WHERE ef.company = est.company ~
  AND ef.est-no = est.est-no NO-LOCK, ~
      FIRST eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no ~
  and eb.form-no > 0 and eb.blank-no > 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH est WHERE ~{&KEY-PHRASE} ~
      AND est.company = gcompany and ~
ASI.est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      FIRST est-qty WHERE est-qty.company ~
 = est.company ~
  AND est-qty.est-no = est.est-no OUTER-JOIN NO-LOCK, ~
      FIRST ef WHERE ef.company = est.company ~
  AND ef.est-no = est.est-no NO-LOCK, ~
      FIRST eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no ~
  and eb.form-no > 0 and eb.blank-no > 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table est est-qty ef eb
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table est
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table est-qty
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table ef
&Scoped-define FOURTH-TABLE-IN-QUERY-Browser-Table eb


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

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
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 97 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      est
    FIELDS(est.est-no
      est.est-date), 
      est-qty
    FIELDS(est-qty.eqty), 
      ef
    FIELDS(), 
      eb
    FIELDS(eb.ord-no
      eb.cust-no
      eb.part-no
      eb.stock-no
      eb.style
      eb.part-dscr1
      eb.flute
      eb.test
      eb.len
      eb.yld-qty
      eb.wid
      eb.die-no
      eb.dep
      eb.plate-no) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      est.est-no FORMAT "x(8)":U WIDTH 12
      est.est-date FORMAT "99/99/9999":U
      est-qty.eqty FORMAT "->>,>>9.99":U
      eb.ord-no FORMAT ">>>>>9":U
      eb.cust-no FORMAT "x(8)":U
      eb.part-no FORMAT "x(15)":U
      eb.stock-no COLUMN-LABEL "FG Item #" FORMAT "x(15)":U
      eb.style COLUMN-LABEL "Style" FORMAT "x(6)":U WIDTH 9
      eb.part-dscr1 COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      eb.flute FORMAT "XXX":U
      eb.test FORMAT "x(6)":U
      eb.len
      eb.yld-qty COLUMN-LABEL "Qty/Set" FORMAT ">>>>>>9":U
      eb.wid
      eb.die-no FORMAT "x(15)":U
      eb.dep
      eb.plate-no FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 113 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 136 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     RECT-4 AT ROW 19.1 COL 1
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
         WIDTH              = 149.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.est,ASI.est-qty WHERE ASI.est ...,ASI.ef WHERE ASI.est ...,ASI.eb WHERE ASI.est ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST OUTER USED, FIRST USED, FIRST USED"
     _Where[1]         = "est.company = gcompany and
ASI.est.est-type >= 1 and est.est-type <= 4"
     _JoinCode[2]      = "ASI.est-qty.company
 = ASI.est.company
  AND ASI.est-qty.est-no = ASI.est.est-no"
     _JoinCode[3]      = "ASI.ef.company = ASI.est.company
  AND ASI.ef.est-no = ASI.est.est-no"
     _JoinCode[4]      = "ASI.eb.company = ASI.est.company
  AND ASI.eb.est-no = ASI.est.est-no
  and eb.form-no > 0 and eb.blank-no > 0"
     _FldNameList[1]   > ASI.est.est-no
"est.est-no" ? "x(8)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ASI.est.est-date
     _FldNameList[3]   = ASI.est-qty.eqty
     _FldNameList[4]   = ASI.eb.ord-no
     _FldNameList[5]   = ASI.eb.cust-no
     _FldNameList[6]   = ASI.eb.part-no
     _FldNameList[7]   > ASI.eb.stock-no
"eb.stock-no" "FG Item #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.eb.style
"eb.style" "Style" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.eb.part-dscr1
"eb.part-dscr1" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.eb.flute
     _FldNameList[11]   = ASI.eb.test
     _FldNameList[12]   > "_<CALC>"
"eb.len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.eb.yld-qty
"eb.yld-qty" "Qty/Set" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"eb.wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = ASI.eb.die-no
     _FldNameList[16]   > "_<CALC>"
"eb.dep" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = ASI.eb.plate-no
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
   {src/adm/template/brsleave.i}
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/f3help.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-est B-table-Win 
PROCEDURE create-est :
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

  REPEAT:

    find first ce-ctrl where
         ce-ctrl.company = gcompany and
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

  create est.  
  assign ll-new-record = yes
         est.est-type = 1
         est.company = gcompany
         est.loc = gloc
       /*  est.e-num = li-enum + 1 */
         est.est-no = string(li-new-estnum,">>>>>>>9")
         est.form-qty = 1
         est.est-date = today
         est.mod-date = ?
         cocode = gcompany.



   {sys/ref/est-add.i est}     

   run crt-est-childrecord.  /* create ef,eb,est-prep */


   run local-open-query.  
   RUN set-attribute-list in adm-broker-hdl ('Is-First-Est = Yes').

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

  /* ???? bugs : 2 records are created  , delete one ========== 
  for each bb where bb.e-num = 0 :
      delete bb.
  end.
  ========*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide B-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-first for est.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  find first bf-first where bf-first.company = gcompany and
                            bf-first.loc = gloc and
                            bf-first.est-type >= 1 AND
                            bf-first.est-type <= 4
                            no-lock no-error.
  if not avail bf-first then run create-est.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  apply "value-changed" to browse-order in frame {&frame-name}.

  RUN dispatch ('get-last':U).
  IF AVAIL eb THEN
    ASSIGN lv-last-rowid2 = ROWID(ef).

  RUN dispatch ('get-first':U).
  IF AVAIL eb THEN
    ASSIGN lv-frst-rowid2 = ROWID(ef).

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
  def input  parameter ip-nav-type as char.
  def output parameter op-nav-type as char.

  def var lv-rowid      as rowid no-undo.
  def var lv-frst-rowid as rowid no-undo.
  def var lv-last-rowid as rowid no-undo.


  lv-rowid = if avail est then rowid(est) else ?.

  run dispatch ('get-last':U).
  if avail est then lv-last-rowid = rowid(est).

  run dispatch ('get-first':U).
  if avail est then lv-frst-rowid = rowid(est).

  if lv-rowid ne ? then
    reposition {&browse-name} to rowid lv-rowid.

  case ip-nav-type:
    when "F" then run dispatch ('get-first':U).
    when "L" then run dispatch ('get-last':U).
    when "N" then run dispatch ('get-next':U).
    when "P" then run dispatch ('get-prev':U).
  end case.

  if rowid(est) eq lv-last-rowid then
    op-nav-type = "L".

  if rowid(est) eq lv-frst-rowid then
    op-nav-type = if op-nav-type eq "L" then "B" else "F".

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


  hld-rowid = ROWID(ef).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(ef) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END.
    WHEN "P" THEN DO WHILE ROWID(ef) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
                  END.
    WHEN "G" THEN RUN lookup-eb.
  END CASE.

  IF ROWID(ef) EQ lv-last-rowid2 THEN
    op-nav-type = "L".

  IF ROWID(ef) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New_Record B-table-Win 
PROCEDURE New_Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-rowid as rowid no-undo.


  run local-open-query.

  do with frame {&frame-name}:
    reposition {&browse-name} to rowid ip-rowid no-error.
    run dispatch ('row-changed').

    apply "value-changed" to {&browse-name}.
    return no-apply.  
  end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRow B-table-Win 
PROCEDURE RefreshRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter pcMode     as character no-undo.
  define input parameter prRowIdent as rowid     no-undo.

  if pcMode = 'newRecord':U then do:

    run local-open-query.

    reposition browser-table to rowid prrowident.

    /*do while true:
      if available est then
         if (prrowident <> ? and rowid(est) = prRowIdent) /*or
            (currowident = rowid(est) ) */
          then leave.
      browse browser-Table:select-next-row().
      browse browser-Table:fetch-selected-row(1).
    end.*/

    RUN dispatch ('row-changed':U).

  end.
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
  Parameters:" <none>
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
  def var out-dim as dec no-undo.
  def var k_frac as dec init 6.25 no-undo.

  if ip-is-corr-style and ip-dim <> 0 then 
     /*round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC),2)   sys/inc/k16.i */
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.
  RETURN out-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

