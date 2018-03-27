&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: ap\d-selpos.w
  
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
{methods/prgsecur.i}

/* Parameters Definitions ---                                           */
DEF INPUT param ip-ap-recid AS RECID.

/* Local Variable Definitions ---                                       */
DEF VAR lv-i AS INT NO-UNDO.

DEF SHARED TEMP-TABLE tt-pol FIELD selekt AS LOG LABEL "Selected"
                      FIELD rec-id AS RECID                      
                      FIELD qty-inv AS log
                      FIELD amt-inv AS LOG
                      FIELD qty-to-inv LIKE ap-invl.qty
                      FIELD qty-to-inv-uom AS CHAR.

DEF TEMP-TABLE tt-rec NO-UNDO
    FIELD selekt AS LOG FORMAT "yes/no" LABEL "Selected"
    FIELD rec-id AS RECID
    FIELD po-date LIKE fg-rcpth.trans-date LABEL "P.O. Date"
    FIELD rcpt-date LIKE fg-rcpth.trans-date LABEL "Receipt Date"
    FIELD r-no AS INT
    FIELD qty-rec AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty Received"
    FIELD qty-rec-uom AS CHAR
    FIELD qty-inv AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty To Invoice"
    FIELD qty-inv-uom AS CHAR
    FIELD s-len LIKE po-ordl.s-len
    FIELD row-id AS ROWID.

/* {custom/globdefs.i} */
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-num-rec AS INT NO-UNDO.

DO TRANSACTION:
  {sys/inc/appaper.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pol po-ordl tt-rec

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-rec.selekt tt-rec.qty-inv tt-rec.qty-inv-uom tt-rec.qty-rec tt-rec.qty-rec-uom tt-rec.rcpt-date po-ordl.line po-ordl.job-no po-ordl.job-no2 po-ordl.s-num po-ordl.i-no tt-rec.s-len po-ordl.s-wid tt-rec.po-date po-ordl.cost po-ordl.pr-uom po-ordl.t-cost po-ordl.i-name   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-rec.selekt tt-rec.qty-inv   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-pol , ~
                                   FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK, ~
                                   EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol)                             BY po-ordl.line                             BY tt-rec.rcpt-date                             BY tt-rec.r-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-pol , ~
                                   FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK, ~
                                   EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol)                             BY po-ordl.line                             BY tt-rec.rcpt-date                             BY tt-rec.r-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-pol po-ordl tt-rec
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-pol
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 po-ordl
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-2 tt-rec


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 rd_qty Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS rd_qty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE rd_qty AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purchased Qty", 1,
"Receipt Qty", 2
     SIZE 58 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-pol, 
      po-ordl, 
      tt-rec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-rec.selekt
     tt-rec.qty-inv   LABEL "Qty To Invoice" 
     tt-rec.qty-inv-uom LABEL "U/M" FORMAT "X(4)"
     tt-rec.qty-rec
     tt-rec.qty-rec-uom LABEL "U/M" FORMAT "X(4)"
     tt-rec.rcpt-date
     po-ordl.line
     po-ordl.job-no
     po-ordl.job-no2
     po-ordl.s-num
     po-ordl.i-no WIDTH 19
     tt-rec.s-len
     po-ordl.s-wid
     tt-rec.po-date
     po-ordl.cost
     po-ordl.pr-uom
     po-ordl.t-cost
     po-ordl.i-name
     ENABLE tt-rec.selekt tt-rec.qty-inv
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 135 BY 12.38
         BGCOLOR 8  ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 2
     rd_qty AT ROW 13.86 COL 2 NO-LABEL WIDGET-ID 2
     Btn_OK AT ROW 15.48 COL 41
     Btn_Cancel AT ROW 15.48 COL 84
     SPACE(38.19) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Items for PO:"
         CANCEL-BUTTON Btn_Cancel.


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

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pol ,
                            FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK,
                            EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol)
                            BY po-ordl.line
                            BY tt-rec.rcpt-date
                            BY tt-rec.r-no
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN {&displayed-objects}.
  END. 
  FOR EACH tt-pol,
      FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK:

    IF po-ordl.item-type THEN
    FOR EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol):
      IF tt-rec.rec-id EQ ? THEN
        ASSIGN
         tt-pol.selekt     = tt-rec.selekt
         tt-pol.qty-to-inv = tt-rec.qty-inv
         tt-pol.qty-to-inv-uom = tt-rec.qty-inv-uom.

      ELSE
      FOR FIRST rm-rcpth WHERE RECID(rm-rcpth) EQ tt-rec.rec-id NO-LOCK,
          EACH rm-rdtlh
          WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code:
          
        IF tt-rec.selekt THEN
          ASSIGN
           rm-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                   STRING(tt-rec.qty-inv,"-9999999999.99999"))
           tt-pol.qty-to-inv    = tt-pol.qty-to-inv + tt-rec.qty-inv
           tt-pol.qty-to-inv-uom = tt-rec.qty-inv-uom
           tt-pol.selekt        = YES.

        ELSE rm-rdtlh.receiver-no = "".
      END.
    END.

    ELSE
    FOR EACH tt-rec WHERE tt-rec.row-id EQ ROWID(tt-pol),
        FIRST fg-rcpth WHERE RECID(fg-rcpth) EQ tt-rec.rec-id NO-LOCK,
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
          
      IF tt-rec.selekt THEN
        ASSIGN
         fg-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                 STRING(tt-rec.qty-inv,"-9999999999.99999"))
         tt-pol.qty-to-inv    = tt-pol.qty-to-inv + tt-rec.qty-inv
         tt-pol.selekt        = YES.

      ELSE fg-rdtlh.receiver-no = "".
    END.
  END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty D-Dialog
ON VALUE-CHANGED OF rd_qty IN FRAME D-Dialog
DO:
    ASSIGN {&SELF-NAME}.

    RUN build-table.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


ON 'mouse-select-click':U OF tt-rec.selekt OR
   'return':U OF tt-rec.selekt
DO:
  tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} =
      STRING(NOT (tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} EQ "yes"),"yes/no").
END.

ON 'leave':U OF tt-rec.selekt IN BROWSE {&browse-name}
DO:
    ASSIGN tt-rec.selekt = tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".
    RETURN.
END.

/* ***************************  Main Block  *************************** */
DO WITH FRAME {&FRAME-NAME}:
 {custom/usrprint.i}
 APPLY 'ENTRY' TO rd_qty.  

IF rd_qty:SCREEN-VALUE EQ ? THEN
   rd_qty:SCREEN-VALUE = "1".

ASSIGN rd_qty = INT(rd_qty:SCREEN-VALUE).

FIND ap-inv WHERE RECID(ap-inv) EQ ip-ap-recid NO-LOCK NO-ERROR.
IF NOT AVAIL ap-inv THEN RETURN.

FIND FIRST tt-pol NO-ERROR.

IF NOT AVAIL tt-pol THEN RETURN.

FIND FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK.

FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " " +
                            TRIM(STRING(po-ordl.po-no,">>>>>>>>")).

RUN build-table.

IF lv-num-rec LE 1 THEN DO WITH FRAME {&FRAME-NAME}:
  FOR EACH tt-rec:
    tt-rec.selekt = YES.
  END.

  APPLY "choose" TO Btn_OK.
END.

ELSE DO:
  {src/adm/template/dialogmn.i}
END.

END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ld AS DEC EXTENT 2 NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR v-rec-qty AS DEC NO-UNDO.
DEF VAR lv-uom LIKE po-ordl.pr-qty-uom NO-UNDO.
DEF VAR v-pur-qty AS DEC NO-UNDO.
DEFINE VARIABLE cMatExceptionList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.


DEF BUFFER b-ap-invl FOR ap-invl.

lv-num-rec = 0.

EMPTY TEMP-TABLE tt-rec.
RUN sys/ref/nk1Look.p(INPUT cocode,
                      INPUT "APMatTypeExceptions",
                      INPUT "C",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cMatExceptionList,
                      OUTPUT lFound).
IF NOT lFound OR cMatExceptionList EQ '' THEN
    cMatExceptionList = 'MOXY789@'.

FOR EACH tt-pol,
    FIRST po-ordl WHERE RECID(po-ordl) EQ tt-pol.rec-id NO-LOCK,
    FIRST po-ord WHERE
          po-ord.company EQ po-ordl.company AND
          po-ord.po-no EQ po-ordl.po-no NO-LOCK:

  /* gdm - 05200908 */
  IF rd_qty EQ 1 THEN 
     ASSIGN tt-rec.qty-rec:LABEL IN BROWSE {&BROWSE-NAME} = "Purchased Qty".
  ELSE 
     ASSIGN  tt-rec.qty-rec:LABEL IN BROWSE {&BROWSE-NAME} = "Qty Received".
  /* gdm - 05200908 end */

  IF po-ordl.item-type THEN DO:
    ASSIGN
     v-bwt = 0
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-dep = 0.

    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      v-dep = item.s-dep.          
      {po/pol-dims.i}
    END.

    IF AVAIL item           AND
       item.i-code EQ "R"   AND
       INDEX(cMatExceptionList,ITEM.mat-type) GT 0 AND
       item.stocked EQ NO   THEN DO:       

       v-qty = po-ordl.cons-qty.

       IF po-ordl.cons-uom NE po-ordl.pr-qty-uom THEN
          RUN sys/ref/convquom.p (po-ordl.cons-uom, po-ordl.pr-qty-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  v-qty, OUTPUT v-qty).

       v-qty = v-qty - po-ordl.t-inv-qty.

       IF v-qty GT 0 THEN DO:
          CREATE tt-rec.
          ASSIGN
          tt-rec.rec-id     = ?
          tt-rec.selekt     = NO
          tt-rec.po-date = po-ord.po-date
          tt-rec.r-no       = 0
          tt-rec.qty-rec    = IF rd_qty EQ 1 THEN v-qty
                              ELSE po-ordl.t-rec-qty
          tt-rec.qty-rec-uom = IF rd_qty EQ 1 THEN po-ordl.pr-qty-uom
                               ELSE ITEM.cons-uom
          tt-rec.qty-inv    = IF rd_qty EQ 1 THEN v-qty
                              ELSE po-ordl.t-rec-qty
          tt-rec.qty-inv-uom = po-ordl.pr-qty-uom
          tt-rec.row-id     = ROWID(tt-pol).

          FIND FIRST rm-rcpth WHERE
              rm-rcpth.company   EQ cocode AND
              rm-rcpth.i-no      EQ po-ordl.i-no AND
              rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")) AND
              rm-rcpth.job-no    EQ po-ordl.job-no AND
              rm-rcpth.job-no2   EQ po-ordl.job-no2 AND
              rm-rcpth.rita-code EQ "R"
              NO-LOCK NO-ERROR.
      
          IF AVAIL rm-rcpth THEN
             tt-rec.rcpt-date = rm-rcpth.trans-date.
       END.
    END.  
    ELSE
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ po-ordl.i-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
          AND rm-rcpth.job-no    EQ po-ordl.job-no
          AND rm-rcpth.job-no2   EQ po-ordl.job-no2
          AND rm-rcpth.rita-code EQ "R"
        USE-INDEX item-po NO-LOCK,

        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND (rm-rdtlh.s-num    EQ po-ordl.s-num OR po-ordl.s-num EQ 0)
          AND rm-rdtlh.receiver-no EQ ''  /*24963 - Do not show *any* receipt that has already been linked*/
/*          AND NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no  EQ INT(SUBSTR(rm-rdtlh.receiver-no,1,10))*/
/*                                             AND b-ap-invl.line  EQ (po-ordl.po-no * 1000) + po-ordl.line)*/
        NO-LOCK:        

     lv-uom = po-ordl.pr-qty-uom.

     IF lv-uom EQ "ROLL" THEN
       ASSIGN
        v-len  = 12
        lv-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       lv-uom = appaper-chr.

     ASSIGN
        v-rec-qty = rm-rdtlh.qty
        v-qty = rm-rdtlh.qty.

      IF rd_qty EQ 1 THEN
      DO:
         v-pur-qty = po-ordl.cons-qty.
      
         IF po-ordl.cons-uom NE lv-uom THEN
            RUN sys/ref/convquom.p (po-ordl.cons-uom, lv-uom,
                                    v-bwt, v-len, v-wid, v-dep,
                                    v-pur-qty, OUTPUT v-pur-qty).
      END.

      IF rm-rcpth.pur-uom NE lv-uom THEN
         RUN sys/ref/convquom.p (rm-rcpth.pur-uom, lv-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 v-qty, OUTPUT v-qty).

      /* gdm - 05200908 end */  
      
      /*24963 - Prevent the re-use of the same receipt, multiple times*/
      IF NOT CAN-FIND(FIRST tt-rec WHERE tt-rec.rec-id EQ RECID(rm-rcpth) ) THEN DO:
          CREATE tt-rec.
          ASSIGN
           tt-rec.rec-id     = RECID(rm-rcpth)
           tt-rec.selekt     = SUBSTR(rm-rdtlh.receiver-no,1,10) EQ
                               STRING(ap-inv.i-no,"9999999999")
           tt-rec.po-date   = po-ord.po-date
           tt-rec.rcpt-date = rm-rcpth.trans-date
           tt-rec.r-no       = rm-rcpth.r-no
           /* gdm - 05200908  */ 
           tt-rec.qty-rec    = IF rd_qty EQ 1 THEN v-pur-qty
                                              ELSE v-rec-qty
           tt-rec.qty-rec-uom = IF rd_qty EQ 1 THEN lv-uom
                                ELSE rm-rcpth.pur-uom
           tt-rec.qty-inv    =  IF tt-rec.selekt THEN
                                   DEC(SUBSTR(rm-rdtlh.receiver-no,11,17))
                                 ELSE 
                                    v-qty
           tt-rec.qty-inv-uom = lv-uom
           /* gdm - 05200908 end */ 
           tt-rec.s-len      = v-len
           tt-rec.row-id     = ROWID(tt-pol).
       END.
    END.
  END.

  ELSE
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ cocode
        AND fg-rcpth.i-no      EQ po-ordl.i-no
        AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX item-po NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no  EQ INT(SUBSTR(fg-rdtlh.receiver-no,1,10))
                                           AND b-ap-invl.line  EQ (po-ordl.po-no * 1000) + po-ordl.line)
      NO-LOCK:
    IF NOT CAN-FIND(FIRST tt-rec WHERE tt-rec.rec-id EQ RECID(fg-rcpth)) THEN DO:
        
        CREATE tt-rec.
        ASSIGN
         tt-rec.rec-id     = RECID(fg-rcpth)
         tt-rec.selekt     = SUBSTR(fg-rdtlh.receiver-no,1,10) EQ
                             STRING(ap-inv.i-no,"9999999999")
         tt-rec.po-date   = po-ord.po-date
         tt-rec.rcpt-date = fg-rcpth.trans-date
         tt-rec.r-no       = fg-rcpth.r-no
         tt-rec.qty-rec    = fg-rdtlh.qty
         tt-rec.qty-inv    = IF tt-rec.selekt THEN DEC(SUBSTR(fg-rdtlh.receiver-no,11,17))
                                              ELSE fg-rdtlh.qty
         tt-rec.s-len      = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
         tt-rec.row-id     = ROWID(tt-pol).
    END.
  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id EQ ?:
    lv-num-rec = lv-num-rec + 1.
  END.
  
  FOR EACH tt-rec WHERE tt-rec.rec-id EQ ?:
    lv-num-rec = lv-num-rec + 1.
  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id NE ? BREAK BY tt-rec.rec-id:
      
    ASSIGN
     ld[1] = ld[1] + tt-rec.qty-rec
     ld[2] = ld[2] + tt-rec.qty-inv.

    IF LAST-OF(tt-rec.rec-id) THEN
      ASSIGN
       lv-num-rec     = lv-num-rec + 1
       lv-i             = lv-i + 1
       tt-rec.qty-rec = ld[1]
       tt-rec.qty-inv = ld[2]
       ld             = 0.

    ELSE DELETE tt-rec.
  END.
END.

{&OPEN-QUERY-BROWSE-2}

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
  DISPLAY rd_qty 
      WITH FRAME D-Dialog.
  ENABLE BROWSE-2 rd_qty Btn_OK Btn_Cancel 
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
  /*RUN build-table. built from b-apinvl.w */
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 


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
  {src/adm/template/snd-list.i "tt-pol"}
  {src/adm/template/snd-list.i "po-ordl"}
  {src/adm/template/snd-list.i "tt-rec"}

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

