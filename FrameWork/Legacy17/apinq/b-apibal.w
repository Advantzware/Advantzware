&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  apinq\b-apibal.w

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

&SCOPED-DEFINE yellowColumnsName b-apibal
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF BUFFER b-vend FOR vend.
DEF BUFFER b-ap-inv FOR ap-inv.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var phandle as widget-handle no-undo.

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-report NO-UNDO /*LIKE report*/
    FIELD key-01 LIKE report.key-01
    FIELD key-02 LIKE report.key-02
    FIELD check-no   AS   CHAR
    FIELD trans-date LIKE ap-inv.inv-date
    FIELD dscr       AS   CHAR
    FIELD credits    AS   DEC
    FIELD debits     AS   DEC
    FIELD balance    AS   DEC
    FIELD cr-db      AS   LOG
    FIELD add-sub    AS   LOG
    FIELD po-no      AS   CHAR
    FIELD ap-inv-rec-key AS CHAR
    FIELD vend-no AS CHAR
    FIELD inv-no AS CHAR
  INDEX key-01 IS PRIMARY key-01
  INDEX key-02 key-02
  INDEX check-no check-no
  INDEX trans-date trans-date
  INDEX dscr dscr
  INDEX credits credits
  INDEX debits debits
  INDEX balance balance
  INDEX po-no po-no.

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

{sa/sa-sls01.i}

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
&Scoped-define INTERNAL-TABLES tt-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-report.key-02 tt-report.check-no tt-report.trans-date tt-report.dscr tt-report.credits tt-report.debits tt-report.balance tt-report.po-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-report


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_vend fi_finv fi_tinv fi_days tb_open ~
btn_go btn_print Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_vend fi_name fi_finv fi_tinv fi_days ~
tb_open fi_sortby 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_print 
     LABEL "&Print" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_days AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Up to" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_finv AS CHARACTER FORMAT "x(12)" 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_tinv AS CHARACTER FORMAT "x(12)" INITIAL "zzzzzzzzzzzz" 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 2.62.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Invoices Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-report.key-02     FORMAT "x(12)"          COLUMN-LABEL "Invoice#"    LABEL-BGCOLOR 14
      tt-report.check-no                           COLUMN-LABEL "Check#"      LABEL-BGCOLOR 14
      tt-report.trans-date                         COLUMN-LABEL "Date"        LABEL-BGCOLOR 14
      tt-report.dscr       FORMAT "x(8)"           COLUMN-LABEL "Description" LABEL-BGCOLOR 14
      tt-report.credits    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Credits"     LABEL-BGCOLOR 14
      tt-report.debits     FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Debits"      LABEL-BGCOLOR 14
      tt-report.balance    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Balance"     LABEL-BGCOLOR 14
      tt-report.po-no      FORMAT "x(65)"          COLUMN-LABEL "PO#s"        LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 17.38
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_vend AT ROW 1.24 COL 17 COLON-ALIGNED
     fi_name AT ROW 1.24 COL 37 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     fi_finv AT ROW 2.43 COL 20 COLON-ALIGNED
     fi_tinv AT ROW 2.43 COL 55 COLON-ALIGNED
     fi_days AT ROW 1.24 COL 99 COLON-ALIGNED
     tb_open AT ROW 2.43 COL 74
     btn_go AT ROW 1.24 COL 122
     btn_print AT ROW 1.24 COL 136
     Browser-Table AT ROW 3.62 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortby AT ROW 2.43 COL 108 COLON-ALIGNED
     "Days Old" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 1.24 COL 111
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
         HEIGHT             = 20
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table btn_print F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win            /* Task# 10251303 */
ON HELP OF FRAME F-Main                                             
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "fi_vend" THEN DO:
      RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE ""                                      AND
         TRIM(FOCUS:SCREEN-VALUE) NE TRIM(ENTRY(1,char-val)) THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-vend.
      END.
    END.  
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  IF AVAILABLE tt-report AND 
     tt-report.ap-inv-rec-key NE "" THEN DO:
      
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                   "(tt-report.ap-inv-rec-key,tt-report.inv-no)"}

     FIND FIRST b-vend WHERE
          b-vend.company EQ cocode AND
          b-vend.vend-no EQ tt-report.vend-no
          NO-LOCK NO-ERROR.

     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.
  ELSE
     RUN pushpin-image-proc(INPUT "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  RUN valid-vend NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_vend
     fi_name
     fi_finv
     fi_tinv
     fi_days
     tb_open.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_print B-table-Win
ON CHOOSE OF btn_print IN FRAME F-Main /* Print */
DO:
  {sys/form/r-topw.f}

  DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.

  DEF VAR lv-vend  LIKE vend.vend-no    NO-UNDO.
  DEF VAR lv-name  LIKE vend.NAME       NO-UNDO.
  DEF VAR lv-finv  LIKE ap-inv.inv-no   NO-UNDO.
  DEF VAR lv-tinv  LIKE lv-finv         NO-UNDO.
  DEF VAR li-days  AS   INT             NO-UNDO.
  DEF VAR ll-open  AS   LOG             NO-UNDO.
  DEF VAR lv-dots  AS   CHAR            NO-UNDO.
  DEF VAR lv-po-no LIKE tt-report.po-no NO-UNDO.

  FORM SKIP(1)
       lv-vend COLON 30 LABEL "Vendor#"
       lv-name          NO-LABEL
       lv-finv COLON 30 LABEL "Beginning Inv#"
       lv-tinv COLON 30 LABEL "Ending Inv#"
       li-days COLON 30 LABEL "Up to How Many Days Old?"
       ll-open COLON 30 LABEL "Open Invoices Only?"
       SKIP(1)

      WITH STREAM-IO WIDTH 80 FRAME ap-balh SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "       V E N D O R   A C C O U N T S       ".

    
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "APINQ"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "APINQ"
     sys-ctrl.descrip  = "AP Inquiry Print Method"
     sys-ctrl.char-fld = "Triad".
    MESSAGE "System control record NOT found. " sys-ctrl.descrip
            UPDATE sys-ctrl.char-fld.
  END.
  v-print-fmt = sys-ctrl.char-fld.
      
  FIND FIRST tt-report NO-ERROR.

  IF AVAIL tt-report THEN DO WITH FRAME ap-balh:
    SESSION:SET-WAIT-STATE ("general").

    lv-dots = FILL(".",80).
        
    {sys/inc/print1.i}
    {sys/inc/outprint.i 56}

    DISPLAY fi_vend @ lv-vend
            fi_name @ lv-name
            fi_finv @ lv-finv
            fi_tinv @ lv-tinv
            fi_days @ li-days
            tb_open @ ll-open.

    FOR EACH tt-report NO-LOCK BREAK BY tt-report.key-01 BY tt-report.trans-date:
      DISPLAY tt-report.key-02     FORMAT "x(12)"          COLUMN-LABEL "Inv#/Check#"
                tt-report.check-no WHEN tt-report.key-02 EQ "" @ tt-report.key-02
              tt-report.trans-date FORMAT "99/99/99"       COLUMN-LABEL "Date"
              tt-report.dscr       FORMAT "x(8)"           COLUMN-LABEL "Descript"
              tt-report.credits    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Credits"
              tt-report.debits     FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Debits"
              tt-report.balance    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Balance"
          WITH FRAME ap-bal NO-BOX STREAM-IO WIDTH 80 DOWN.
      DOWN WITH FRAME ap-bal.

      IF tt-report.po-no NE "" THEN
        lv-po-no = TRIM(lv-po-no) + " " + tt-report.po-no.

      IF LAST-OF(tt-report.key-01) THEN DO:
        IF v-print-fmt EQ "Brick" AND lv-po-no NE "" THEN
          PUT "P.O. NUMBER(S)" FORMAT "x(14)"
              SPACE(1)
              TRIM(lv-po-no)   FORMAT "x(65)"
              SKIP.

        lv-po-no = "".

        DISPLAY lv-dots          @ tt-report.key-02
                lv-dots          @ tt-report.trans-date
                lv-dots          @ tt-report.dscr
                ".............." @ tt-report.credits
                ".............." @ tt-report.debits
                ".............." @ tt-report.balance
            WITH FRAME ap-bal.
        DOWN WITH FRAME ap-bal.
      END.
    END.

    OUTPUT CLOSE.

    SESSION:SET-WAIT-STATE ("").

    run scr-rpt.w (list-name,TRIM(FRAME ap-balh:TITLE),11,"P"). /* open file-name, title */ 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON ENTRY OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN RUN new-vend.

  lv-save-char = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON LEAVE OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN new-vend.

    RUN valid-vend NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON VALUE-CHANGED OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  RUN new-vend.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile B-table-Win 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var balance as dec format "->>>,>>9.99".
def var t-dscr   as CHAR.
def var x-check-no   as char.
def var t-credits    as dec  format ">,>>>,>>>.99"  column-label "Credits" .
def var t-debits     as dec  format ">,>>>,>>>.99"  column-label "Debits"  .
def var t-balance    as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var work-bal     as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var bal-diff     as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var t-vendor     like  vend.vend-no.
def var f-inv        like  ap-inv.inv-no init "            ".
def var t-inv        like  ap-inv.inv-no init "ZZZZZZZZZZZZ".
def var t-o          as log format "Y/N" init "N".
def var num-day-old  as dec format ">>>9" init 9999.
def var show-bal like t-balance.
DEF VAR lv-key LIKE tt-report.key-01 NO-UNDO.
DEF VAR lv-po-no LIKE tt-report.po-no NO-UNDO.
DEF VAR v-refnum AS CHAR NO-UNDO.

ASSIGN
 t-vendor    = fi_vend
 f-inv       = fi_finv
 t-inv       = fi_tinv
 num-day-old = fi_days
 t-o         = tb_open.

FOR EACH tt-report:
  DELETE tt-report.
END.

    for each ap-inv
        where ap-inv.company  eq cocode
          and ap-inv.vend-no  eq t-vendor
          and ap-inv.inv-no   ge f-inv
          and ap-inv.posted   eq yes
          and ap-inv.inv-no   le t-inv
          and ap-inv.inv-date ge (today - num-day-old)
        no-lock
          
        by ap-inv.inv-date by ap-inv.inv-no:

      ASSIGN
         show-bal = ap-inv.net + ap-inv.freight
         lv-key = STRING(YEAR(ap-inv.inv-date),"9999") +
                  STRING(MONTH(ap-inv.inv-date),"99")  +
                  STRING(DAY(ap-inv.inv-date),"99")    +
                  ap-inv.inv-no.

      if ap-inv.due ne 0 or not t-o then do:
        CREATE tt-report.
        ASSIGN
         tt-report.key-01     = lv-key
         tt-report.key-02     = ap-inv.inv-no
         tt-report.trans-date = ap-inv.inv-date
         tt-report.dscr       = "Invoice"
         tt-report.debits     = ap-inv.net + ap-inv.freight
         tt-report.balance    = t-balance + tt-report.debits
         tt-report.cr-db      = NO
         tt-report.add-sub    = YES
         tt-report.ap-inv-rec-key = ap-inv.rec_key
         tt-report.vend-no    = ap-inv.vend-no
         tt-report.inv-no     = ap-inv.inv-no.
                         
        for each ap-payl
            where ap-payl.inv-no   eq ap-inv.inv-no
              and ap-payl.vend-no  eq ap-inv.vend-no
              and ap-payl.posted   eq YES
            no-lock,
               
            first ap-pay
            where ap-pay.company eq cocode 
              and ap-pay.c-no    eq ap-payl.c-no
            NO-LOCK:
              
          t-dscr = "Payment".
            
          if ap-payl.memo then t-dscr = "CR MEMO".
            
          if ap-payl.amt-paid            lt 0  and
             ap-payl.memo                eq no and
             ap-inv.net + ap-inv.freight gt 0  then t-dscr = "Void Chk".
               
          if ap-payl.amt-paid ne 0 then do:
            ASSIGN
            t-credits  = ap-payl.amt-paid
            t-balance  = t-balance - ap-payl.amt-paid
            x-check-no = string(ap-payl.check-no).

            CREATE tt-report.
            ASSIGN
             tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-credits
             tt-report.cr-db      = yes
             tt-report.add-sub    = NO
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

            FIND FIRST b-ap-inv WHERE
                 b-ap-inv.company EQ ap-pay.company AND
                 b-ap-inv.vend-no EQ ap-payl.vend-no AND
                 b-ap-inv.inv-no  EQ ap-payl.inv-no
                 NO-LOCK NO-ERROR.

            IF AVAIL b-ap-inv THEN
               tt-report.ap-inv-rec-key = b-ap-inv.rec_key.

            IF t-dscr NE "Void Chk" THEN
               tt-report.trans-date = ap-pay.check-date.
            ELSE
            DO:
               v-refnum = "VOIDED CHECK"
                        + string(ap-pay.check-no, "zzzzzzz9").

               FIND FIRST ap-ledger WHERE
                    ap-ledger.company EQ cocode AND
                    ap-ledger.vend-no EQ ap-pay.vend-no AND
                    ap-ledger.refnum = v-refnum
                    NO-LOCK NO-ERROR.

               IF AVAIL ap-ledger THEN
               DO:
                  tt-report.trans-date = ap-ledger.tr-date.
                  RELEASE ap-ledger.
               END.
               ELSE
                  tt-report.trans-date = ap-pay.check-date.

            END.
          end. /* if ap-payl.amt-paid ne 0 */

          if ap-payl.amt-disc ne 0 then do:
            if ap-payl.memo then t-debits = ap-payl.amt-disc.
            else t-credits = ap-payl.amt-disc.

            ASSIGN
            t-dscr = "Discount"
            x-check-no = string(ap-payl.check-no).
              
            if ap-payl.memo then do:

              ASSIGN
                 t-dscr = "DB MEMO"
                 t-balance = t-balance + ap-payl.amt-disc.
                
              create tt-report.
              assign
               tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
               tt-report.check-no   = x-check-no
               tt-report.trans-date = ap-pay.check-date
               tt-report.dscr       = t-dscr
               tt-report.credits    = t-debits
               tt-report.cr-db      = no
               tt-report.add-sub    = YES
               tt-report.vend-no    = ap-payl.vend-no
               tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
            end. /* if ap-payl.memo */

            else do:
              t-balance = t-balance - ap-payl.amt-disc.

              create tt-report.
              assign
               tt-report.key-01     = string(lv-key,"x(100)") + "a" +
                                      STRING(ap-payl.c-no,"9999999999") +
                                      STRING(ap-payl.line,"9999999999")
               tt-report.check-no   = x-check-no
               tt-report.trans-date = ap-pay.check-date
               tt-report.dscr       = t-dscr
               tt-report.credits    = t-credits
               tt-report.cr-db      = yes
               tt-report.add-sub    = NO
               tt-report.vend-no    = ap-payl.vend-no
               tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
            end. /* else do: */
          end. /* if ap-payl.amt-disc ne 0 */
        end. /* for each ap-payl */
      end. /* if (t-o and ap-inv.due ne 0) */

      for each ap-payl
          where ap-payl.inv-no  eq "0"
            and ap-payl.posted  eq yes
            and ap-payl.vend-no eq vend.vend-no
          no-lock,
            
          first ap-pay
          where ap-pay.company eq cocode 
            and ap-pay.c-no eq ap-payl.c-no
          NO-LOCK:
            
        t-dscr = "Payment".
        if ap-payl.memo then t-dscr = "CR MEMO".

        if ap-payl.amt-paid ne 0 then do:
          t-credits = ap-payl.amt-paid.
          t-balance = t-balance - ap-payl.amt-paid.
          x-check-no = string(ap-payl.check-no).
            
          create tt-report.
          assign
           tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                  STRING(ap-payl.c-no,"9999999999") +
                                  STRING(ap-payl.line,"9999999999")
           tt-report.check-no   = x-check-no
           tt-report.trans-date = ap-pay.check-date
           tt-report.dscr       = t-dscr
           tt-report.credits    = t-credits
           tt-report.cr-db      = yes
           tt-report.add-sub    = NO
           tt-report.vend-no    = ap-payl.vend-no
           tt-report.inv-no     = ap-payl.inv-no.

          FIND FIRST b-ap-inv WHERE
               b-ap-inv.company EQ ap-pay.company AND
               b-ap-inv.vend-no EQ ap-payl.vend-no AND
               b-ap-inv.inv-no  EQ ap-payl.inv-no
               NO-LOCK NO-ERROR.

          IF AVAIL b-ap-inv THEN
             tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
        end. /* if ap-payl.amt-paid ne 0 */

        if ap-payl.amt-disc ne 0 then do:
          if ap-payl.memo then t-debits  = ap-payl.amt-disc.
          else t-credits = ap-payl.amt-disc.
          t-dscr = "Discount".
          if not ap-payl.memo then t-balance = t-balance - ap-payl.amt-disc.
            x-check-no = string(ap-payl.check-no).

          if ap-payl.memo then do:

            ASSIGN
            t-dscr = "DB MEMO"
            t-balance = t-balance + ap-payl.amt-disc.
              
            create tt-report.
            assign
             tt-report.key-01     = string(lv-key,"x(100)") + "a" +
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.trans-date = ap-pay.check-date
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-debits
             tt-report.cr-db      = no
             tt-report.add-sub    = YES
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
          end. /* if ap-payl.memo then do */
            
          else do:
            create tt-report.
            assign
             tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                  STRING(ap-payl.c-no,"9999999999") +
                                  STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.trans-date = ap-pay.check-date
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-credits
             tt-report.cr-db      = yes
             tt-report.add-sub    = NO
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
          end. /* else do: */
        end. /* if ap-payl.amt-disc ne 0 */
      end. /* for each ap-payl */

      RELEASE tt-report.

      bal-diff = work-bal.
      if ap-inv.due ne 0 or not t-o THEN work-bal = work-bal + ap-inv.net + ap-inv.freight.

      lv-po-no = "".

      FOR EACH tt-report WHERE tt-report.key-01 BEGINS lv-key BREAK BY tt-report.key-01:

        if tt-report.add-sub then work-bal = work-bal + tt-report.credits.
        else work-bal = work-bal - tt-report.credits.

        if tt-report.dscr EQ "Invoice" then
          work-bal = work-bal - tt-report.credits.

        tt-report.balance = work-bal.
  
        if tt-report.dscr ne "Invoice" AND
           NOT tt-report.cr-db THEN
          ASSIGN
           tt-report.debits  = tt-report.credits
           tt-report.credits = 0.
          
        if FIRST(tt-report.key-01) then do:
          if ap-inv.po-no eq 0 then
          for each ap-invl
              where ap-invl.i-no  eq ap-inv.i-no
                and ap-invl.po-no ne 0
              break by ap-invl.po-no:
              
            IF LAST-OF(ap-invl.po-no) THEN
              lv-po-no = TRIM(lv-po-no) + " " +
                         TRIM(STRING(ap-invl.po-no,">>>>>>>>>>")).
          end.
            
          else lv-po-no = TRIM(STRING(ap-inv.po-no,">>>>>>>>>>")).

          tt-report.po-no = TRIM(lv-po-no).
        END.  
      end. /* for each tt-report break by tt-report.key-01 */
       
      if (show-bal ne work-bal and work-bal ne bal-diff) or
         (show-bal eq work-bal and bal-diff eq 0)        then do:
        CREATE tt-report.
        ASSIGN
         tt-report.key-01     = string(lv-key,"x(100)") + "z"
         tt-report.key-02     = "Balance Due"
         tt-report.trans-date = ap-inv.due-date
         tt-report.balance    = work-bal.                                                
      end. /* if (show-bal ne work-bal and */      
    end. /* for each ap-inv */

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
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_vend.
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
  IF fi_vend NE "" THEN RUN create-tempfile.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL tt-report AND tt-report.ap-inv-rec-key NE "" THEN
  DO:
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                   "(tt-report.ap-inv-rec-key,tt-report.inv-no)"}

     FIND FIRST b-vend WHERE
          b-vend.company EQ cocode AND
          b-vend.vend-no EQ tt-report.vend-no
          NO-LOCK NO-ERROR.
    
     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.
  ELSE
     RUN pushpin-image-proc(INPUT "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend B-table-Win 
PROCEDURE new-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND vend NO-LOCK
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ fi_vend:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL vend THEN fi_name:SCREEN-VALUE = vend.name.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR v-inv-no AS CHAR NO-UNDO.

   IF AVAIL tt-report THEN
      ASSIGN
         v-inv-no = STRING(tt-report.inv-no,"X(12)") + "APINV"
         v-att = CAN-FIND(FIRST attach WHERE
                 attach.company = cocode and
                 attach.rec_key = ip-rec_key AND
                 (attach.est-no eq v-inv-no OR ATTACH.est-no EQ "")).
   

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachinv-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
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
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
{methods/setfocus.i {&BROWSE-NAME}}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN apinq/apbl-exp.w ("", "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend B-table-Win 
PROCEDURE valid-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_vend:SCREEN-VALUE = CAPS(fi_vend:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST vend WHERE vend.company EQ cocode
                                 AND vend.vend-no EQ fi_vend:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_vend.
      RETURN ERROR.
    END.
  END.
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

