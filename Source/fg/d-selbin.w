&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\d-selbin.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{sys/inc/VAR.i "new shared"}
{custom/globdefs.i}
/* ip-run: from oe-boll.w = 2 */
/*         from oe-rell.w = 1 */
/*         from cre-rell = 4 */
/*         from fg\fg-to-rm.w = 3 */
DEF INPUT  PARAM ip-run AS INT NO-UNDO.
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-all-one AS cha NO-UNDO.
DEF INPUT  PARAM ip-i-no AS cha NO-UNDO.
DEF INPUT  PARAM ip-qty-needed AS DEC NO-UNDO.
DEF INPUT  PARAM ip-lv-linker  AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-rowid-list AS CHAR NO-UNDO.

{fg/d-selbin.i}

def var v-i-no    like oe-ordl.i-no NO-UNDO.
def var v-ord     like oe-ordl.ord-no NO-UNDO.
def var v-line    like oe-ordl.line NO-UNDO.
def var v-job-no  like oe-ordl.job-no NO-UNDO.
def var v-job-no2 like oe-ordl.job-no2 NO-UNDO.
def var v-qty     like oe-rell.qty NO-UNDO.
DEF VAR v-cust    LIKE oe-ordl.cust-no NO-UNDO.
DEF VAR rel-type  LIKE reftable.code NO-UNDO.

DEF VAR nufile AS LOG NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR bolh_id AS RECID NO-UNDO.
DEF VAR boll_id AS RECID NO-UNDO.
DEF VAR ll-change-qty AS LOG NO-UNDO.
DEF VAR lv-select AS CHAR NO-UNDO.
DEF VAR lv-select-desc AS CHAR NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO.
/* 02/06/07 rdb */
DEF VAR lv-sort-by     AS CHAR INIT "job-no"   NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "job-no"   NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR ll-display-recptdt AS CHAR NO-UNDO.  /* display receipt date */
DEF VAR lvl-new-release AS LOG NO-UNDO.
DEF VAR lvcPStack AS CHAR NO-UNDO.
DEF BUFFER xoe-rell FOR oe-rell.
DEF BUFFER xoe-boll FOR oe-boll.
DEF BUFFER b-reftable FOR reftable.
{methods/template/brwcustomdef.i}
ASSIGN
 cocode = g_company
 locode = g_loc.

ll-sort-asc = NO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "job-no"    THEN w-bin.job-no + STRING(w-bin.job-no2, "999") ELSE ~
    IF lv-sort-by EQ "loc"       THEN w-bin.loc                                  ELSE ~
    IF lv-sort-by EQ "last-rct-date" THEN STRING(w-bin.last-rct-date)            ELSE ~
    IF lv-sort-by EQ "loc-bin"   THEN w-bin.loc-bin                              ELSE ~
    IF lv-sort-by EQ "tag"       THEN w-bin.tag                                  ELSE ~
    IF lv-sort-by EQ "rfid"       THEN w-bin.rfid                                ELSE ~
    IF lv-sort-by EQ "stack-code" THEN w-bin.stack-code                          ELSE ~
    IF lv-sort-by EQ "cust-no"   THEN w-bin.cust-no                              ELSE ~
    IF lv-sort-by EQ "to-bol"    THEN STRING(w-bin.to-bol, "9999999999")         ELSE ~
    IF lv-sort-by EQ "to-rel"    THEN STRING(w-bin.to-rel, "9999999999")         ELSE ~
    IF lv-sort-by EQ "qty"       THEN STRING(9999999999.9999999999 + w-bin.qty, "-9999999999.9999999999") ELSE ~
    IF lv-sort-by EQ "units"     THEN STRING(9999999999 + w-bin.units, "-9999999999")                     ELSE ~
    IF lv-sort-by EQ "case-count" THEN STRING(w-bin.case-count, "9999999999")                             ELSE ~
                                       STRING(w-bin.partial-count, "-9999999999") 
    

&SCOPED-DEFINE sortby BY w-bin.job-no BY w-bin.loc

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

/*Pulled from SetCellColumns.i since this dialog is not ADM compatible*/
DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT 200.
DEFINE VARIABLE columnWidth AS DECIMAL NO-UNDO EXTENT 200.
DEFINE VARIABLE cellColumnDat AS CHARACTER NO-UNDO.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF
/*End SetCellColumns.i extract*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br-bin

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-bin

/* Definitions for BROWSE br-bin                                        */
&Scoped-define FIELDS-IN-QUERY-br-bin w-bin.job-no w-bin.job-no2 NO-LABEL w-bin.last-rct-date w-bin.loc w-bin.loc-bin w-bin.tag w-bin.rfid w-bin.cust-no w-bin.to-bol w-bin.qty w-bin.units w-bin.case-count w-bin.partial-count w-bin.stack-code   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-bin w-bin.job-no   w-bin.job-no2   w-bin.last-rct-date   w-bin.loc ~
 w-bin.loc-bin   w-bin.tag ~
 w-bin.rfid   w-bin.cust-no   w-bin.to-bol   w-bin.qty ~
 ~
w-bin.units ~
   w-bin.case-count ~
 w-bin.partial-count   w-bin.stack-code   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-bin w-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-bin w-bin
&Scoped-define SELF-NAME br-bin
&Scoped-define QUERY-STRING-br-bin FOR EACH w-bin
&Scoped-define OPEN-QUERY-br-bin OPEN QUERY {&SELF-NAME} FOR EACH w-bin.
&Scoped-define TABLES-IN-QUERY-br-bin w-bin
&Scoped-define FIRST-TABLE-IN-QUERY-br-bin w-bin


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-br-bin}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-bin Btn_OK Btn_Cancel Btn_move-sort 
&Scoped-Define DISPLAYED-OBJECTS fi_seq fi_qsel v-help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-stack Dialog-Frame 
FUNCTION get-stack RETURNS CHARACTER
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

DEFINE BUTTON Btn_move-sort 
     LABEL "Move Columns" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_qsel AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "Qty Selected" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_seq AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "Qty Needed" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-help AS CHARACTER FORMAT "X(256)":U INITIAL "Click Tag to Select or Cntrl and Click for Random Tags. For Range of Tags, click 1st Record, Press Shift and Click on Last Record." 
      VIEW-AS TEXT 
     SIZE 128 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-bin FOR 
      w-bin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-bin Dialog-Frame _FREEFORM
  QUERY br-bin DISPLAY
    w-bin.job-no        LABEL "Job#"           FORMAT "X(9)"  LABEL-BGCOLOR  14
    w-bin.job-no2       NO-LABEL               FORMAT "999"   LABEL-BGCOLOR  14
    w-bin.last-rct-date LABEL "Receipt Date"   FORMAT 99/99/9999 LABEL-BGCOLOR 14 
    w-bin.loc           LABEL "Whs"            FORMAT "x(7)"  LABEL-BGCOLOR  14
    w-bin.loc-bin       LABEL "Bin"            FORMAT "x(8)"  LABEL-BGCOLOR  14
    w-bin.tag           LABEL "Tag"            FORMAT "x(25)" LABEL-BGCOLOR  14
    w-bin.rfid          LABEL "RFID"           FORMAT "x(25)" LABEL-BGCOLOR  14
    w-bin.cust-no       LABEL "Customer#"                     LABEL-BGCOLOR  14
                        WIDTH 12
    w-bin.to-bol        LABEL "Assigned to Rcpt" FORMAT "->,>>>,>>>,>>9"
                                                              LABEL-BGCOLOR  14
                        WIDTH 25
    w-bin.qty           LABEL "Quantity   "     FORMAT "->>>,>>>,>>9"
                                                              LABEL-BGCOLOR  14
    w-bin.units         LABEL "Units"           FORMAT "->>>,>>9"
                                                              LABEL-BGCOLOR  14
    w-bin.case-count    LABEL "Unit Count"      FORMAT "->>>,>>9"
                                                              LABEL-BGCOLOR  14
    w-bin.partial-count LABEL "Partial Count"   FORMAT "->,>>>,>>9"
                                                              LABEL-BGCOLOR  14
                        WIDTH 20
    w-bin.stack-code    COLUMN-LABEL "FG Lot" FORMAT "x(15)"  LABEL-BGCOLOR 14

    ENABLE
    w-bin.job-no 
    w-bin.job-no2
    w-bin.last-rct-date
    w-bin.loc    
    w-bin.loc-bin
    w-bin.tag    
    w-bin.rfid
    w-bin.cust-no
    w-bin.to-bol
    w-bin.qty           
    w-bin.units         
    w-bin.case-count    
    w-bin.partial-count
    w-bin.stack-code
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 151 BY 15
         BGCOLOR 8 FONT 0 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     br-bin AT ROW 1 COL 1
     fi_seq AT ROW 17.43 COL 21 COLON-ALIGNED
     fi_qsel AT ROW 17.43 COL 55 COLON-ALIGNED WIDGET-ID 6
     Btn_OK AT ROW 17.43 COL 84
     Btn_Cancel AT ROW 17.43 COL 106
     Btn_move-sort AT ROW 17.43 COL 134 WIDGET-ID 4
     v-help AT ROW 16.24 COL 1.6 NO-LABEL
     SPACE(22.59) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bins/Tags for"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-bin 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_qsel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_seq IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-help IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-bin
/* Query rebuild information for BROWSE br-bin
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-bin
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-bin */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bins/Tags for */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-bin
&Scoped-define SELF-NAME br-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-EXTEND-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-MENU-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-MOVE-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON MOUSE-SELECT-CLICK OF br-bin IN FRAME Dialog-Frame
DO:
  RUN renumber-seq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-bin Dialog-Frame
ON START-SEARCH OF br-bin IN FRAME Dialog-Frame
DO:
  {methods/template/sortindicator.i} 
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  lh-column = {&BROWSE-NAME}:CURRENT-COLUMN.
  IF lh-column:LABEL-BGCOLOR NE 14 THEN RETURN NO-APPLY.

  ASSIGN
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  IF ll-sort-asc THEN OPEN QUERY br-bin {&QUERY-STRING-br-bin} {&sortby-phrase-asc}.
                 ELSE OPEN QUERY br-bin {&QUERY-STRING-br-bin} {&sortby-phrase-desc}.
  {methods/template/sortindicatorend.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  {&browse-name}:DESELECT-ROWS () NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_move-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_move-sort Dialog-Frame
ON CHOOSE OF Btn_move-sort IN FRAME Dialog-Frame /* Move Columns */
DO:
  RUN move-columns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li-selected-qty LIKE v-qty NO-UNDO.
  DEF VAR ll-update-qty-no AS LOG NO-UNDO.
  DEF VAR ll-BOLQtyPopup AS LOG NO-UNDO.

  DEF BUFFER bf-oerel FOR oe-rel.

     
  ll-BOLQtyPopup = YES.
  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BOLQtyPopup"
    NO-LOCK NO-ERROR.

  IF avail(sys-ctrl) THEN
      ASSIGN ll-BOLQtyPopup = sys-ctrl.log-fld.
  else do:
      RUN sys/inc/CREATE_nk1.p (INPUT cocode, INPUT "BOLQtyPopup").
      ASSIGN ll-BOLQtyPopup = yes.
  end.

  &SCOPED-DEFINE w-bin-where                                                ~
       WHERE w-bin.selekt-log EQ YES                                        

  RUN set-select.

  IF CAN-FIND(FIRST w-bin {&w-bin-where}) THEN
  MESSAGE "This will select all bins/tags selected from browser." SKIP
          "Do you want to continue?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll-ans AS LOG.
  IF NOT ll-ans THEN RETURN NO-APPLY.

  SESSION:SET-WAIT-STATE("general").   

  FOR EACH w-bin {&w-bin-where}:
    
    ASSIGN
     w-bin.selekt    = "X"
     li-selected-qty = li-selected-qty + w-bin.qty.
    FIND FIRST fg-bin WHERE recid(fg-bin) EQ w-bin.rec-id NO-LOCK NO-ERROR.    
    IF AVAIL fg-bin THEN
      op-rowid-list = op-rowid-list + STRING(ROWID(fg-bin)) + ",".
  END.
  op-rowid-list = TRIM(op-rowid-list, ",").
  IF li-selected-qty LT fi_seq THEN
  DO:
    MESSAGE  "Tag Qty must be greater then Scheduled Release Qty" 
    VIEW-AS ALERT-BOX INFO.
    RETURN NO-APPLY.
  END.
  IF ll-change-qty AND li-selected-qty GT v-qty THEN DO:

    /* value of ll-change-qty affects ll-update-qty-no below */
    IF ll-BOLQtyPopup THEN
        MESSAGE  "Tag Qty Exceeds Scheduled Release Qty" SKIP
                 "YES to Transfer Quantity of Selected Bin/Tags" SKIP
                 "NO to Transfer Scheduled Release Qty of Selected Bin/Tags"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-change-qty AS LOG.
    ELSE
        ll-change-qty = FALSE.

    /* ll-update-qty-no is used in sel-binsrel.i */

    IF ll-change-qty THEN v-qty = li-selected-qty.


  END.


  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    cellColumnDat = 'users/' + USERID('nosweat') + '/{&cellColumnDat}.dat'.
    find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ADDRELSE" no-lock no-error.

    IF AVAIL sys-ctrl THEN 
        ASSIGN ll-display-recptdt = sys-ctrl.char-fld.

  FRAME {&FRAME-NAME}:TITLE = "Bins/Tags for " + ip-i-no.
  /* Indicates that this is run from add-release and the oe-rell is already */
  /* created, therefore the quantity should not appear in the qty release   */
  /* column until the user selects a tag                                    */
  lvcPStack = get-stack().
  lvl-new-release = NO.

  ll-change-qty = ip-run NE 3.

  RUN initialize.

 DO:
  RUN enable_UI.
  /* To display quantity needed, etc */

  RUN setCellColumns. /*get columns from .dat and display in saved order*/

  ASSIGN 
      w-bin.job-no :READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.last-rct-date:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.loc:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.tag:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.rfid:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.to-bol:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.qty:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.units:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.case-count:READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.partial-count :READ-ONLY IN BROWSE {&browse-name} = YES
      w-bin.stack-code :READ-ONLY IN BROWSE {&browse-name} = YES
      .

  IF ll-display-recptdt NE "BIN/TAG" THEN
  ASSIGN
      w-bin.last-rct-date:VISIBLE IN BROWSE {&browse-name} = FALSE.

    IF ip-run EQ 3 THEN DO: 
      /* {&browse-name}:SELECT-ALL (). */
      RUN set-select.
      RUN renumber-seq.
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.              
  END.
END.
RUN SaveColumns. /*save column positions to .dat*/
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-bin Dialog-Frame 
PROCEDURE create-w-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lItemType AS LOG NO-UNDO.
  CREATE w-bin.
  BUFFER-COPY fg-bin TO w-bin
  ASSIGN
   w-bin.rec-id        = RECID(fg-bin)
   w-bin.selekt        = ""
   w-bin.selekt-log    = NO /* ip-run EQ 3 */
   w-bin.units         = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
   w-bin.partial-count = fg-bin.qty - (w-bin.units * fg-bin.case-count).


/*      FOR  EACH oe-boll FIELDS(qty job-no job-no2) NO-LOCK */
/*        WHERE oe-boll.company EQ fg-bin.company            */
/*          /* AND oe-boll.b-no    EQ oe-bolh.b-no */        */
/*          AND oe-boll.i-no    EQ fg-bin.i-no               */
/*          AND oe-boll.loc     EQ fg-bin.loc                */
/*          AND oe-boll.loc-bin EQ fg-bin.loc-bin            */
/*          AND oe-boll.tag     EQ fg-bin.tag                */
/*          AND oe-boll.cust-no EQ fg-bin.cust-no            */
/*        USE-INDEX i-no,                                    */
/*      FIRST oe-bolh FIELDS(company b-no) NO-LOCK           */
/*        WHERE oe-bolh.b-no EQ oe-boll.b-no                 */
/*          AND oe-bolh.posted  EQ NO:                       */
/*                                                           */
/*        IF fg-bin.job-no NE "" AND                         */
/*           NOT(oe-boll.job-no EQ fg-bin.job-no AND         */
/*               oe-boll.job-no2 EQ fg-bin.job-no2) THEN     */
/*           NEXT.                                           */
/*                                                           */
/*        w-bin.to-bol = w-bin.to-bol + oe-boll.qty.         */
/*    END.                                                   */


  DEF VAR dTo-Rel AS DEC NO-UNDO.
  /* Set w-bin.to-rel here */
  dTo-Rel = 0.
  FOR EACH fg-rctd
     WHERE fg-rctd.company EQ fg-bin.company
       AND fg-rctd.i-no EQ fg-bin.i-no
       AND fg-rctd.tag  EQ fg-bin.tag
       AND fg-rctd.loc  EQ fg-bin.loc
       AND fg-rctd.loc-bin EQ fg-bin.loc-bin
       AND fg-rctd.rita-code EQ "R"
       AND fg-rctd.t-qty LT 0
       AND fg-rctd.SetHeaderRno GT 0 /* Set Parts */
     NO-LOCK:

    dTo-Rel = dTo-Rel - fg-rctd.t-qty.
  END.
  w-bin.to-rel = dTo-Rel.
  w-bin.to-bol = dTo-Rel.

  FIND FIRST fg-rdtlh
      WHERE fg-rdtlh.company EQ fg-bin.company
        AND fg-rdtlh.i-no    EQ fg-bin.i-no
        AND fg-rdtlh.job-no  EQ fg-bin.job-no
        AND fg-rdtlh.job-no2 EQ INT(fg-bin.job-no2)
        AND fg-rdtlh.tag     EQ fg-bin.tag NO-LOCK NO-ERROR.

   IF AVAIL fg-rdtlh THEN
       ASSIGN
       w-bin.stack-code =  fg-rdtlh.stack-code.

   FIND FIRST itemfg WHERE itemfg.company = fg-bin.company
        AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
   IF AVAIL itemfg THEN
       lItemType = NO.
   ELSE
       lItemType = YES.

   FIND FIRST rfidtag WHERE rfidtag.company = fg-bin.company
       AND rfidtag.item-type = lItemType
       AND rfidtag.tag-no = fg-bin.tag NO-LOCK NO-ERROR.
   IF AVAIL rfidtag THEN
       ASSIGN
       w-bin.rfid = rfidtag.rfidtag.

   IF w-bin.last-rct-date EQ ? THEN DO:

     IF w-bin.tag GT "" THEN DO:
          FOR EACH fg-rdtlh
                 WHERE fg-rdtlh.company EQ fg-bin.company
                   AND fg-rdtlh.tag     EQ fg-bin.tag NO-LOCK
             USE-INDEX tag,
             EACH fg-rcpth WHERE fg-rcpth.company EQ fg-bin.company
                             AND fg-rcpth.r-no    EQ fg-rdtlh.r-no
                             AND fg-rcpth.i-no    EQ fg-bin.i-no
                             AND fg-rcpth.rita-code EQ "R"
                           NO-LOCK
                           USE-INDEX r-no
             BY fg-rcpth.trans-date DESCENDING.

             LEAVE.
          END.
     END.
      IF AVAIL fg-rcpth THEN
          w-bin.last-rct-date = fg-rcpth.trans-date.
   END.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi_seq fi_qsel v-help 
      WITH FRAME Dialog-Frame.
  ENABLE br-bin Btn_OK Btn_Cancel Btn_move-sort 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize Dialog-Frame 
PROCEDURE initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-linkno AS INT NO-UNDO.

FOR EACH w-bin:
  DELETE w-bin.
END.

v-i-no = ip-i-no.

rel-type = "" .


IF ip-all-one EQ "all" THEN
   FOR EACH fg-bin
       WHERE fg-bin.company EQ cocode
         AND fg-bin.i-no    EQ v-i-no
         AND fg-bin.qty     GT 0
    NO-LOCK:

    IF v-cust EQ fg-bin.cust-no AND rel-type EQ "S" THEN NEXT.          /*Task# 12231304*/ 

    IF NOT((TRIM(fg-bin.job-no) EQ "" OR
              NOT CAN-FIND(FIRST job
                        WHERE job.company EQ fg-bin.company
                          AND job.job-no  EQ fg-bin.job-no
                          AND job.job-no2 EQ fg-bin.job-no2
                          AND job.stat    EQ "H"))) THEN
       NEXT.
  RUN create-w-bin.
END.

ELSE
FOR EACH fg-bin
    WHERE fg-bin.company EQ cocode
      AND fg-bin.i-no    EQ v-i-no
      AND fg-bin.job-no  EQ v-job-no
      AND fg-bin.job-no2 EQ v-job-no2
      AND fg-bin.qty     GT 0
    NO-LOCK:

    IF v-cust EQ fg-bin.cust-no AND rel-type EQ "S" THEN NEXT.      /*Task# 12231304*/ 

    IF NOT((TRIM(fg-bin.job-no) EQ "" OR
           NOT CAN-FIND(FIRST job
                        WHERE job.company EQ fg-bin.company
                          AND job.job-no  EQ fg-bin.job-no
                          AND job.job-no2 EQ fg-bin.job-no2
                          AND job.stat    EQ "H"))) THEN
       NEXT.

  RUN create-w-bin.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns Dialog-Frame 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         br-bin:COLUMN-MOVABLE = NOT v-col-move
         br-bin:COLUMN-RESIZABLE = NOT v-col-move
         v-col-move = NOT v-col-move
         Btn_move-sort:LABEL = IF NOT v-col-move THEN "Move Columns" ELSE "Sort Columns".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumber-seq Dialog-Frame 
PROCEDURE renumber-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-w-bin FOR w-bin.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR dTotQtySel AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-rowid = IF AVAIL w-bin THEN ROWID(w-bin) ELSE ?.

    RUN set-select.

    FOR EACH b-w-bin WHERE b-w-bin.selekt-log EQ NO:
      b-w-bin.seq = 0.
    END.

    li = 0.
    dTotQtySel = 0.
    FOR EACH b-w-bin
        WHERE b-w-bin.selekt-log
          AND b-w-bin.seq GT 0
          AND b-w-bin.seq LT 1000
        BY b-w-bin.seq:
      ASSIGN
       li          = li + 1
       b-w-bin.seq = (li * 1000) + (IF ROWID(b-w-bin) EQ lv-rowid THEN 2 ELSE 1).
    END.
    FOR EACH b-w-bin
        WHERE b-w-bin.selekt-log
          AND b-w-bin.seq EQ 0:
      ASSIGN
       li          = li + 1
       b-w-bin.seq = (li * 1000) + (IF ROWID(b-w-bin) EQ lv-rowid THEN 2 ELSE 1).
    END.
    li = 0.
    FOR EACH b-w-bin
        WHERE b-w-bin.selekt-log
          AND b-w-bin.seq GE 1000
        BY b-w-bin.seq:
      ASSIGN
       li          = li + 1
       b-w-bin.seq = li.
    END.
    FOR EACH b-w-bin
        WHERE b-w-bin.selekt-log
        BY b-w-bin.seq:
      ASSIGN
       dTotQtySel = dTotQtySel + 
          /* (IF b-w-bin.to-bol GT 0 THEN b-w-bin.to-bol ELSE b-w-bin.qty) */
          b-w-bin.qty.
    END.

    fi_seq:SCREEN-VALUE = STRING(ip-qty-needed).
    fi_qsel:SCREEN-VALUE = STRING(dTotQtySel).
    IF ip-qty-needed LT dTotQtySel THEN
       fi_qsel:BGCOLOR = 14.
      ELSE IF ip-qty-needed EQ dTotQtySel THEN
       fi_qsel:BGCOLOR = 2.
      ELSE IF ip-qty-needed GT dTotQtySel THEN
       fi_qsel:BGCOLOR = 12.
 /*     FOR EACH b-w-bin WHERE b-w-bin.selekt-log BY b-w-bin.seq: */
/*       fi_seq:SCREEN-VALUE = STRING(b-w-bin.seq).              */
/*     END.                                                      */

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveColumns Dialog-Frame 
PROCEDURE SaveColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  /* check for any columns changes */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    IF cellColumn[i]:NAME EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME AND
       columnWidth[i] EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS THEN NEXT.    
    DO:
      OUTPUT TO VALUE(cellColumnDat).
      DO j = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        EXPORT {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):NAME {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):WIDTH-PIXELS.
      END. /* do j */
      OUTPUT CLOSE.
    END. /* if savechanges */
    LEAVE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-select Dialog-Frame 
PROCEDURE set-select :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-w-bin FOR w-bin.


  DO WITH FRAME {&frame-name}:
    FOR EACH b-w-bin:
      b-w-bin.selekt-log = NO.
    END.
    IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
      IF AVAIL w-bin THEN w-bin.selekt-log = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCellColumns Dialog-Frame 
PROCEDURE SetCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE userColumn AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-index AS INT NO-UNDO.
  
  IF SEARCH(cellColumnDat) NE ? THEN DO:
     /* get user cell column order */
     INPUT FROM VALUE(cellColumnDat) NO-ECHO.
     REPEAT:
        IMPORT userColumn[j] columnWidth[j].
        j = j + 1.
     END. /* repeat */
     INPUT CLOSE.
     
     /* change default columns to user order */
     DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:       
        cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
     END.
    
     j = j - 1.
     DO i = 1 TO j:
     
        DO k = 1 TO j:
          IF NOT VALID-HANDLE(cellColumn[k]) THEN NEXT.
           IF userColumn[i] EQ cellColumn[k]:NAME THEN
              LEAVE.
        END.

        IF NOT VALID-HANDLE(cellColumn[k]) THEN NEXT.
        IF columnWidth[i] NE cellColumn[k]:WIDTH-PIXELS THEN
           cellColumn[k]:WIDTH-PIXELS = columnWidth[i].

        IF NOT VALID-HANDLE(cellColumn[i]) THEN NEXT.
        IF userColumn[i] NE cellColumn[i]:NAME THEN DO:
    
           {&BROWSE-NAME}:MOVE-COLUMN(k,i) IN FRAME {&FRAME-NAME}.
          
           DO v-index = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
              cellColumn[v-index] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(v-index).
           END.
        END.
     END. /* do i */

  END. /* search */
  
  /* read new order to check for changes when exiting */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    ASSIGN
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      columnWidth[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS.    
  END. /* do i */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-stack Dialog-Frame 
FUNCTION get-stack RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lvpStack AS CHAR NO-UNDO.
lvpStack = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
        + (IF PROGRAM-NAME(8) NE ? THEN "," + PROGRAM-NAME(8) ELSE "")
        + (IF PROGRAM-NAME(9) NE ? THEN "," + PROGRAM-NAME(9) ELSE "")
        + (IF PROGRAM-NAME(10) NE ? THEN "," + PROGRAM-NAME(10) ELSE "")
        + (IF PROGRAM-NAME(11) NE ? THEN "," + PROGRAM-NAME(11) ELSE "").
  RETURN lvpStack.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

