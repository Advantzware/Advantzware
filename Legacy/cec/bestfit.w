&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE INPUT PARAM ip-rmino AS CHARACTER NO-UNDO .
DEFINE INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.
DEFINE INPUT PARAM ip-show-all LIKE sys-ctrl.char-fld NO-UNDO.

def var ls-item-dscr as cha no-undo.

{cec/bestfitc.i SHARED}

def var k_frac as dec init 6.25 no-undo.
DEF VAR lv-sort-by AS CHAR INIT "key-01" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Wst MSF" NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE lShtcalcWarm-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE lDisplayMsg AS LOGICAL INIT TRUE .
DEFINE BUFFER bf-item FOR ITEM .
DEFINE BUFFER bff-item FOR ITEM .

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

RUN sys/ref/nk1look.p (INPUT cocode, "SHTCALCWarn", "L" /* Logical */, NO /* check by cust */, 
                           INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                           OUTPUT cRtnChar, OUTPUT lRecFound).
  IF lRecFound THEN
      lShtcalcWarm-log = LOGICAL(cRtnChar) NO-ERROR.

{sys/inc/f16to32.i}

&SCOPED-DEFINE sortby-log                                                                                                                                      ~
    IF lv-sort-by EQ "key-02"   THEN tt-report.key-02                                                                                                     ELSE ~
    IF lv-sort-by EQ "i-name"   THEN item.i-name                                                                                                          ELSE ~
    IF lv-sort-by EQ "tt-wid"   THEN tt-wid                                                                                                               ELSE ~
    IF lv-sort-by EQ "tt-len"   THEN tt-len                                                                                                               ELSE ~
    IF lv-sort-by EQ "tt-dep"   THEN tt-dep                                                                                                               ELSE ~
    IF lv-sort-by EQ "tt-reqs"  THEN STRING(tt-reqs,"9999999999.9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "tt-onhs"  THEN STRING(tt-onhs,"9999999999.9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "tt-onhl"  THEN STRING(tt-onhl,"9999999999.9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "tt-avls"  THEN STRING(tt-avls,"9999999999.9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "tt-avll"  THEN STRING(tt-avll,"9999999999.9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "tt-out"   THEN STRING(tt-out,"9999999999")                                                                                          ELSE ~
                                     STRING(tt-msf,"9999999999.9999999999")

&SCOPED-DEFINE sortby BY tt-msf

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-report item

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-report.key-02 item.i-name tt-wid tt-len tt-dep tt-reqs tt-onhs tt-onhl tt-avls tt-avll tt-out tt-msf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-report.key-02 ~
   item.i-name ~
   tt-wid ~
   tt-len ~
   tt-dep ~
   tt-reqs ~
   tt-onhs ~
   tt-onhl ~
   tt-avls ~
   tt-avll ~
   tt-out ~
   tt-msf   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-report item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-report
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-BROWSE-1 item
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-report                             WHERE ((rd_show EQ 2 AND tt-onhs GT tt-reqs) OR                                    (rd_show EQ 3 AND tt-avls GT 0)       OR                                    rd_show EQ 1), ~
                                   FIRST item NO-LOCK WHERE RECID(item) EQ tt-report.rec-id
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-report                             WHERE ((rd_show EQ 2 AND tt-onhs GT tt-reqs) OR                                    (rd_show EQ 3 AND tt-avls GT 0)       OR                                    rd_show EQ 1), ~
                                   FIRST item NO-LOCK WHERE RECID(item) EQ tt-report.rec-id.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-report item
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-report
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 item


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rd_show BROWSE-1 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS rd_show 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim D-Dialog 
FUNCTION display-cw-dim RETURNS CHAR
  ( input ip-dim as dec)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-i-name D-Dialog 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK /*AUTO-GO */
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE rd_show AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show All Items", 1,
"Only Qty OnHand > Required", 2,
"Only Qty Avail > 0", 3
     SIZE 128 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-report, 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-report.key-02                          COLUMN-LABEL "Item"
                                                FORMAT "x(10)"
                                                WIDTH 15
                                                LABEL-BGCOLOR 14
      item.i-name                               COLUMN-LABEL "Name"
                                                FORMAT "x(30)"
                                                WIDTH 40
                                                LABEL-BGCOLOR 14
      tt-wid                                    COLUMN-LABEL "Width"
                                                FORMAT "x(9)"
                                                WIDTH 12.6
                                                LABEL-BGCOLOR 14
      tt-len                                    COLUMN-LABEL "Length"
                                                FORMAT "x(9)"
                                                WIDTH 12.6
                                                LABEL-BGCOLOR 14
      tt-dep                                    COLUMN-LABEL "Depth"
                                                FORMAT "x(9)"
                                                WIDTH 12.6
                                                LABEL-BGCOLOR 14
      tt-reqs                                   COLUMN-LABEL "Required"
                                                FORMAT "->>>,>>>,>>>,>>9"
                                                WIDTH 14
                                                LABEL-BGCOLOR 14
      tt-onhs                                   COLUMN-LABEL "OnHand Sht"
                                                FORMAT "->>>,>>>,>>>,>>9"
                                                WIDTH 14
                                                LABEL-BGCOLOR 14
      tt-onhl                                   COLUMN-LABEL "OnHand LF"
                                                FORMAT "->>>,>>>,>>>,>>9"
                                                WIDTH 18
                                                LABEL-BGCOLOR 14
      tt-avls                                   COLUMN-LABEL "Avail Sht"
                                                FORMAT "->>>,>>>,>>>,>>9"
                                                WIDTH 14
                                                LABEL-BGCOLOR 14
      tt-avll                                   COLUMN-LABEL "Avail LF"
                                                FORMAT "->>>,>>>,>>>,>>9"
                                                WIDTH 18
                                                LABEL-BGCOLOR 14
      tt-out                                    COLUMN-LABEL "Out"
                                                FORMAT ">>>>9"
                                                LABEL-BGCOLOR 14
      tt-msf                                    COLUMN-LABEL "Wst MSF"
                                                FORMAT ">>9.999"
                                                LABEL-BGCOLOR 14

      ENABLE tt-report.key-02
             item.i-name
             tt-wid
             tt-len
             tt-dep
             tt-reqs
             tt-onhs
             tt-onhl
             tt-avls
             tt-avll
             tt-out
             tt-msf
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 152 BY 10.24
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rd_show AT ROW 1.24 COL 23 NO-LABEL
     BROWSE-1 AT ROW 2.67 COL 1
     Btn_OK AT ROW 13.38 COL 44
     Btn_Cancel AT ROW 13.38 COL 92
     SPACE(46.00) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Best Fit Item"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
/* BROWSE-TAB BROWSE-1 rd_show D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME D-Dialog     = 2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report
                            WHERE ((rd_show EQ 2 AND tt-onhs GT tt-reqs) OR
                                   (rd_show EQ 3 AND tt-avls GT 0)       OR
                                   rd_show EQ 1),
                            FIRST item NO-LOCK WHERE RECID(item) EQ tt-report.rec-id.
     _END_FREEFORM
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Best Fit Item */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON START-SEARCH OF BROWSE-1 IN FRAME D-Dialog
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

  APPLY "END-SEARCH" TO {&BROWSE-NAME}.

  RUN open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lcheckflg AS LOGICAL NO-UNDO .

  IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
  DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
    {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

     IF lShtcalcWarm-log THEN DO:
        IF AVAIL tt-report THEN
        FIND FIRST bf-item NO-LOCK
            WHERE bf-item.company EQ cocode AND 
            RECID(bf-item) EQ tt-report.rec-id
            NO-ERROR.

        IF AVAIL bff-item THEN do:
            IF AVAIL bff-item AND (bf-item.cal NE bff-item.cal AND bf-item.procat NE bff-item.procat) THEN do:
                MESSAGE "Selected Board differs from Board in Job Standard. "  SKIP 
                    " New Caliper: " STRING(bf-item.cal)   " differs from standard: "   STRING(bff-item.cal)  SKIP
                    " New Category: "  STRING(bf-item.procat)  " differs from standard: "  STRING(bff-item.procat) 
                             VIEW-AS ALERT-BOX QUESTION 
                             BUTTONS OK-CANCEL UPDATE lcheckflg .
            END. 
            ELSE IF  AVAIL bff-item AND (bf-item.cal NE bff-item.cal) THEN do:
                MESSAGE "Selected Board differs from Board in Job Standard. " SKIP
                    " New Caliper: "  STRING(bf-item.cal)   " differs from standard: "  STRING(bff-item.cal) 
                             VIEW-AS ALERT-BOX QUESTION 
                             BUTTONS OK-CANCEL UPDATE lcheckflg .
            END.
             ELSE IF AVAIL bff-item AND (bf-item.procat NE bff-item.procat) THEN do:
                MESSAGE "Selected Board differs from Board in Job Standard. "  SKIP
                    " New Category: "  STRING(bf-item.procat)  " differs from standard: "  STRING(bff-item.procat) 
                             VIEW-AS ALERT-BOX QUESTION 
                             BUTTONS OK-CANCEL UPDATE lcheckflg .
             END.
             ELSE
                 lcheckflg = TRUE .

            IF NOT lcheckflg THEN do:
               RETURN .
            END.
        END.
     END.


    IF AVAIL tt-report THEN tt-sel = YES.
  END.
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show D-Dialog
ON VALUE-CHANGED OF rd_show IN FRAME D-Dialog
DO:
  ASSIGN {&self-name}.
  RUN open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

rd_show = LOOKUP(ip-show-all,"AllItems,QOH>QEst,QAvail>0").
FIND FIRST bff-item NO-LOCK
    WHERE bff-item.company EQ cocode 
      AND bff-item.i-no EQ ip-rmino NO-ERROR.

FOR EACH tt-report:
  ASSIGN
   tt-wid = display-cw-dim(DEC(key-08))
   tt-len = display-cw-dim(DEC(key-09))
   tt-dep = display-cw-dim(DEC(key-10))
   tt-out = INT(tt-report.key-05) *
            INT(tt-report.key-06) *
            INT(tt-report.key-07)
   tt-msf = DEC(tt-report.key-01).
END.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY rd_show 
      WITH FRAME D-Dialog.
  ENABLE rd_show BROWSE-1 Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   tt-report.key-02:READ-ONLY IN BROWSE {&browse-name} = YES
   item.i-name:READ-ONLY IN BROWSE {&browse-name}      = YES
   tt-wid:READ-ONLY IN BROWSE {&browse-name}           = YES
   tt-len:READ-ONLY IN BROWSE {&browse-name}           = YES
   tt-dep:READ-ONLY IN BROWSE {&browse-name}           = YES
   tt-reqs:READ-ONLY IN BROWSE {&browse-name}          = YES
   tt-onhs:READ-ONLY IN BROWSE {&browse-name}          = YES
   tt-onhl:READ-ONLY IN BROWSE {&browse-name}          = YES
   tt-avls:READ-ONLY IN BROWSE {&browse-name}          = YES
   tt-avll:READ-ONLY IN BROWSE {&browse-name}          = YES
   tt-out:READ-ONLY IN BROWSE {&browse-name}           = YES
   tt-msf:READ-ONLY IN BROWSE {&browse-name}           = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query D-Dialog 
PROCEDURE open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF ll-sort-asc THEN OPEN QUERY BROWSE-1 {&QUERY-STRING-BROWSE-1} {&sortby-phrase-asc}.
                 ELSE OPEN QUERY BROWSE-1 {&QUERY-STRING-BROWSE-1} {&sortby-phrase-desc}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
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
  {src/adm/template/snd-list.i "item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cw-dim D-Dialog 
FUNCTION display-cw-dim RETURNS CHAR
  ( input ip-dim as dec) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as dec DECIMALS 6 no-undo.
  def var k_frac as dec init 6.25 no-undo.

  if ip-est-type ge 5 and ip-dim <> 0 AND v-cecscrn-char NE "Decimal" then 
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.

  RETURN TRIM(IF out-dim EQ 0 THEN ""        /* Function return value. */
              ELSE         
              IF ip-est-type GE 5 AND v-cecscrn-char NE "Decimal" THEN
                 STRING(out-dim,">>,>>9.99")
              ELSE         
              IF ip-est-type GE 5 AND v-cecscrn-char EQ "Decimal" THEN
                 STRING(out-dim,">>,>>9.999999")
              ELSE STRING(out-dim,">>,>>9.9<<<<")).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-i-name D-Dialog 
FUNCTION display-i-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  find item where recid(item) eq report.rec-id no-lock no-error.
  RETURN (if avail item then item.i-name else "").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

