&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{oe/tt-item-qty-price.i}


/* Local Variable Definitions ---                                       */
DEF VAR ip-recid AS RECID NO-UNDO.
DEF VAR     ip-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR     ip-type AS cha NO-UNDO .   /* add,update,view */
/* DEF TEMP-TABLE FOR tt-item-qty-price. */

  DEF VAR    op-rowid-list AS CHAR NO-UNDO. /* if added records from history */
  DEF VAR    op-cancel AS LOG NO-UNDO.

  DEF VAR ll-new-file AS LOG NO-UNDO.
  DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
  DEF VAR cp-rowid AS ROWID NO-UNDO.
  {custom/globdefs.i}
  {sys/inc/VAR.i NEW SHARED}
  {oe/ordholdstat.i}
  DEF VAR v-use-rel LIKE sys-ctrl.log-fld NO-UNDO.
  DEF VAR v-upd-comm AS LOG INIT YES NO-UNDO.
  DEF VAR v-dup-item AS LOG NO-UNDO.
  DEF VAR v-rtn-code AS INT NO-UNDO.
  DEF VAR v-rtn-char AS CHAR NO-UNDO.
  DEF NEW SHARED BUFFER xest FOR est.
  DEF NEW SHARED BUFFER xeb FOR eb.
  DEF NEW SHARED BUFFER xef FOR ef.
  DEF VAR v-valdcode AS cha INIT "ON,BY,MH" NO-UNDO.
  DEF VAR v-bld-job AS cha NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.  /* for adjust est-no */
  /* for oe/oe-price.p ========*/
  DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */
  /* ==== FOR REPRICE ===*/
  DEF NEW SHARED VAR v-procat LIKE oe-prmtx.procat NO-UNDO. /* ITEM CATEGORY */
  DEF NEW SHARED VAR v-price-lev AS INT NO-UNDO.
  DEF NEW SHARED VAR s-est-no AS cha NO-UNDO.  /* for fgadd2.p */
  DEF NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
  DEF NEW SHARED VAR save_id AS RECID NO-UNDO.  /* RECORD ID FOR ORDER LINE */
  DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
  DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
  DEF NEW SHARED VAR price-ent AS LOG NO-UNDO.
  DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.
  DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
  DEF NEW SHARED VAR nufile AS LOG NO-UNDO.
  DEF NEW SHARED VAR v-qty-mod AS LOG NO-UNDO.
  DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
  DEF NEW SHARED VAR v-create-job AS LOG NO-UNDO.
  DEF VAR lv-ordl-recid AS RECID NO-UNDO.
  DEF VAR lv-change-prom-date AS LOG NO-UNDO.  /* flag for updating oe-ordl.prom-date*/
  DEF VAR lv-change-cst-po AS LOG NO-UNDO.    /* flag for updateing oe-ordl.po-no */
  DEF VAR lv-uom-list AS cha INIT "M,EA,L,CS,C,LB,DRM,ROL,PLT,PKG,SET,DOZ,BDL" NO-UNDO.
  DEF VAR lv-ea-list AS CHAR NO-UNDO.
  DEF VAR lv-valid-uom AS CHAR NO-UNDO.
  DEF VAR v-valtype AS cha INIT "O,R,C" NO-UNDO.
  DEF VAR v-duelist AS cha INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF BUFFER xoe-ordl FOR oe-ordl.
  DEF VAR lv-item-recid AS RECID NO-UNDO.
  DEF VAR first-cust-part-no AS cha NO-UNDO.
  DEF VAR ll-ok-i-no AS LOG NO-UNDO.
  DEF VAR ls-stock AS cha NO-UNDO.  /* for eb.stock-no */
  DEF VAR ll-help-ran AS LOG NO-UNDO.  /* qty help */
  DEF VAR ll-bypass AS LOG NO-UNDO.    /* bypass fields for price */
  {ce/print4.i "new shared"}
  {ce/print42.i "new shared"}
  DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
  DEF NEW SHARED VAR qty AS INT NO-UNDO.
  DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
  DEF BUFFER xoe-rel FOR oe-rel.
  DEF VAR ld-prev-price AS DEC NO-UNDO.
  DEF VAR ll-got-qtprice AS LOG NO-UNDO.
  DEF VAR li-prev-qty AS INT NO-UNDO.
  DEF VAR lv-add-mode AS LOG NO-UNDO.
  DEF VAR lv-help-qty AS INT NO-UNDO.
  DEF VAR ll-qty-leave-done AS LOG NO-UNDO.
  DEF VAR ll-new-fg-created AS LOG NO-UNDO.
  DEF VAR lv-new-tandem AS ROWID NO-UNDO.
  DEF VAR ll-is-tandem AS LOG NO-UNDO.
  DEF VAR ll-do-entry AS LOG NO-UNDO.
  DEF VAR lv-update-job-stdate AS LOG NO-UNDO.
  DEF VAR v-print-head LIKE sys-ctrl.log-fld NO-UNDO.
  DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.
  DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
  DEF VAR v-run-schedule AS LOG NO-UNDO.
  DEF VAR lv-type-codes AS CHAR NO-UNDO.
  DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
  DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
  DEF VAR ll-prev-whs-item AS LOG NO-UNDO.
  DEF VAR ld-prev-t-price LIKE oe-ordl.t-price NO-UNDO.
  DEF VAR li-prev-ord-qty LIKE oe-ordl.qty NO-UNDO.
  DEF VAR v-duplicateFGDayClient AS CHAR NO-UNDO.
  DEF VAR v-rec-found AS LOG NO-UNDO.
  DEF VAR v-orig-ip-type AS CHAR NO-UNDO.
  DEF VAR op-error AS LOG NO-UNDO.
  DEF VARIABLE historyQty AS DECIMAL NO-UNDO.
  DEF VARIABLE historyPrice LIKE oe-ordl.price NO-UNDO.
  DEF VARIABLE historyPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VARIABLE setFromHistory AS LOGICAL NO-UNDO.
  DEF VARIABLE historyButton AS LOGICAL NO-UNDO.
  DEF VAR v-rel AS INT NO-UNDO.
  DEF VAR v-margin AS DEC NO-UNDO.
  DEF VAR llGotLowerPrice AS LOG NO-UNDO.
  DEF VAR v-widhand AS WIDGET-HANDLE  NO-UNDO.
  DEF VAR v-pricehand AS WIDGET-HANDLE  NO-UNDO.
  DEF VAR v-custparthand AS WIDGET-HANDLE  NO-UNDO.
  DEF VAR lv-multi-select        AS LOG NO-UNDO INIT NO.

  DEF VAR r-current-ord AS ROWID NO-UNDO.
  DEF VAR r-current-ordl AS ROWID NO-UNDO.
  DEF VAR h_callproc AS HANDLE NO-UNDO.

  DEF TEMP-TABLE tt-qty-price
  FIELD oeordl-rowid AS ROWID
  FIELD tt-historyQty LIKE oe-ordl.qty
  FIELD tt-historyPrice LIKE oe-ordl.price
  FIELD tt-historyPrUOM AS CHAR
  FIELD tt-setFromHistory AS LOG
  INDEX oe-rowid oeordl-rowid.
  cocode = g_company.
  {oe/oe-sysct1.i NEW}
  {custom/framechk.i NEW}
  DEF BUFFER bf-ef FOR ef.
  DEF BUFFER bf-eb FOR eb.
  DEF BUFFER oe-ord-whs-order FOR reftable.
  DEF BUFFER oe-ordl-whs-item FOR reftable.
  /* gdm - 06220908*/
  DEF VAR v-relflg AS LOG NO-UNDO.
  /* gdm - 11090905*/
  DEF VAR v-ponoUp AS LOG NO-UNDO.
  DEF TEMP-TABLE w-est-no NO-UNDO FIELD w-est-no LIKE itemfg.est-no FIELD w-run AS LOG.
  ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").
  DO TRANSACTION:
    {sys/inc/oeship.i}
    {sys/inc/oereleas.i}
    {sys/inc/oescreen.i}
    {sys/inc/job#.i}
    {sys/inc/oeround.i}
    {sys/inc/fgmaster.i}
    {sys/inc/oeestcom.i}
    {sys/inc/fgsecur.i}
    {sys/inc/runship.i}
    {sys/inc/oepricecheck.i}
    {sys/inc/reltype.i}
  END.
  {sys/ref/fgoecost.i}
  {sys/ref/oecustpart.i}
  {sys/ref/oecount.i}
  {sys/inc/f16to32.i}
  DO TRANSACTION:
    
    RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).
    RUN sys/ref/uom-ea.p (OUTPUT lv-ea-list).
    {sys/inc/schedule.i}
    v-run-schedule = NOT (AVAIL sys-ctrl AND sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld).
    {sys/inc/graphic.i}
  END.
  /* gdm - 06220908 - INSTEAD OF CHANGING sys/inc/oereleas.i */
  FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ cocode
  AND sys-ctrl.NAME    EQ "OERELEAS" NO-ERROR.
  IF AVAIL sys-ctrl   AND
  sys-ctrl.log-fld AND
  sys-ctrl.int-fld = 1
  THEN ASSIGN v-relflg = YES.
DEFINE BUTTON Btn_Cancel AUTO-END-KEY
LABEL "Ca&ncel"
SIZE 15 BY 1.14
BGCOLOR 8 .
DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT
LABEL "&Done"
SIZE 15 BY 1.14
BGCOLOR 8 .
DEFINE BUTTON Btn_hist
LABEL "&History"
SIZE 15 BY 1
BGCOLOR 8 .
DEFINE BUTTON Btn_OK
LABEL "&Save"
SIZE 15 BY 1.14
BGCOLOR 8 .
DEFINE VARIABLE fi_qty-uom AS CHARACTER FORMAT "x(4)" INITIAL "EA"
VIEW-AS FILL-IN
SIZE 10 BY 1 NO-UNDO.
DEFINE VARIABLE fi_s-comm-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Comm.%"
VIEW-AS FILL-IN
SIZE 11 BY .71
FGCOLOR 9  NO-UNDO.
DEFINE VARIABLE fi_s-pct-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "% of Sales"
VIEW-AS FILL-IN
SIZE 14 BY .71
FGCOLOR 9  NO-UNDO.
DEFINE VARIABLE fi_sman-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Sales Rep"
VIEW-AS FILL-IN
SIZE 14 BY .71
FGCOLOR 9  NO-UNDO.
DEFINE VARIABLE fi_sname-1 AS CHARACTER FORMAT "x(8)"
VIEW-AS FILL-IN
SIZE 29 BY 1 NO-UNDO.
DEFINE VARIABLE fi_sname-2 AS CHARACTER FORMAT "x(20)"
VIEW-AS FILL-IN
SIZE 29 BY 1.
DEFINE VARIABLE fi_sname-3 AS CHARACTER FORMAT "x(20)"
VIEW-AS FILL-IN
SIZE 29 BY 1.
DEFINE VARIABLE fi_sname-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Name"
VIEW-AS FILL-IN
SIZE 13 BY .71
FGCOLOR 9  NO-UNDO.
DEFINE VARIABLE fi_type-dscr AS CHARACTER FORMAT "X(15)":U
VIEW-AS FILL-IN
SIZE 20 BY 1 NO-UNDO.
DEFINE VARIABLE spare-dec-1 LIKE itemfg.spare-dec-1
LABEL "Full Cost"
VIEW-AS FILL-IN
SIZE 18 BY 1 NO-UNDO.
DEFINE RECTANGLE RECT-31
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
SIZE 77.2 BY 4.62.
DEFINE RECTANGLE RECT-39
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
SIZE 77.2 BY 10.91.
DEFINE RECTANGLE RECT-40
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
SIZE 64 BY 10.86.
DEFINE RECTANGLE RECT-41
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
SIZE 64 BY 4.62.
DEFINE VARIABLE tb_whs-item AS LOGICAL INITIAL NO
LABEL "Managed Inventory"
VIEW-AS TOGGLE-BOX
SIZE 27 BY .81 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-get-handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-handle Procedure 
FUNCTION get-handle RETURNS HANDLE
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-itemfg-cost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-itemfg-cost Procedure 
FUNCTION get-itemfg-cost RETURNS DECIMAL
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-modified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-modified Procedure 
FUNCTION get-modified RETURNS LOGICAL
  ( ipv-item AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-sv Procedure 
FUNCTION get-sv RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-entry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD set-entry Procedure 
FUNCTION set-entry RETURNS CHARACTER
  ( ipv-item AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD set-sv Procedure 
FUNCTION set-sv RETURNS CHARACTER
  ( ipv-item AS CHAR, ipv-value AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calc-alloc-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-alloc-qty Procedure 
PROCEDURE calc-alloc-qty :
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
  DEF OUTPUT PARAM op-alloc AS INT NO-UNDO.
  DEF VAR v-type AS cha NO-UNDO.
  FIND FIRST itemfg WHERE itemfg.company = cocode AND
  itemfg.i-no = oe-ordl.i-no
  NO-LOCK NO-ERROR.
  op-alloc = 0.
  FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode
    AND oe-ordl.i-no    EQ itemfg.i-no
    USE-INDEX ITEM NO-LOCK,
    FIRST oe-ord  WHERE oe-ord.company EQ cocode
    AND oe-ord.ord-no  EQ oe-ordl.ord-no
    AND INDEX("CZ",oe-ord.stat) EQ 0
    USE-INDEX ord-no NO-LOCK:
    FOR EACH oe-rel WHERE oe-rel.company EQ cocode
      AND oe-rel.ord-no  EQ oe-ordl.ord-no
      AND oe-rel.i-no    EQ oe-ordl.i-no
      AND oe-rel.LINE    EQ oe-ordl.LINE
      USE-INDEX ord-item NO-LOCK:
      {oe/rel-stat.i v-type}
      IF v-type EQ "P" THEN op-alloc = op-alloc + oe-rel.qty.
      ELSE
      IF v-type EQ "Z" AND NOT oe-ctrl.u-inv THEN
      FOR EACH inv-line FIELDS(ship-qty) WHERE inv-line.company EQ cocode
        AND inv-line.ord-no  EQ oe-bolh.ord-no
        AND inv-line.b-no    EQ oe-bolh.b-no
        AND inv-line.i-no    EQ oe-boll.i-no
        AND inv-line.LINE    EQ oe-boll.LINE
        NO-LOCK:
        op-alloc = op-alloc + inv-line.ship-qty.
      END.
    END.
    FOR EACH oe-relh  WHERE oe-relh.company EQ cocode
      AND oe-relh.ord-no  EQ oe-ordl.ord-no
      AND oe-relh.posted  EQ NO
      AND oe-relh.deleted EQ NO
      USE-INDEX order NO-LOCK,
      EACH oe-rell FIELDS(qty)  WHERE oe-rell.company EQ cocode
      AND oe-rell.r-no    EQ oe-relh.r-no
      AND oe-rell.ord-no  EQ oe-ordl.ord-no
      AND oe-rell.i-no    EQ oe-ordl.i-no
      AND oe-rell.LINE    EQ oe-ordl.LINE
      NO-LOCK:
      op-alloc = op-alloc + oe-rell.qty.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calc-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty Procedure 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DO WITH FRAME {&FRAME-NAME}:
  set-sv("oe-ordl.qty",  ).
  STRING(get-sv("(DEC(oe-ordl.cases")) *
  DEC(get-sv("oe-ordl.cas-cnt"))) +
  DEC(get-sv("oe-ordl.partial")),
  oe-ordl.qty:FORMAT).
  END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-duplicateFGDayClient) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-duplicateFGDayClient Procedure 
PROCEDURE check-duplicateFGDayClient :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ord FOR oe-ord.
  DEF VAR v-cnt AS INT NO-UNDO.
  DO:
    FOR EACH b-oe-ordl NO-LOCK WHERE b-oe-ordl.company EQ oe-ordl.company
      AND b-oe-ordl.cust-no EQ oe-ordl.cust-no
      AND b-oe-ordl.i-no    EQ oe-ordl.i-no
      AND RECID(b-oe-ordl) NE RECID(oe-ordl),
      FIRST b-oe-ord WHERE b-oe-ord.company = b-oe-ordl.company
      AND b-oe-ord.ord-no  = b-oe-ordl.ord-no
      AND b-oe-ord.ord-date = oe-ord.ord-date NO-LOCK:
      v-cnt = v-cnt + 1.
    END.
    IF v-cnt > 0 THEN
    RUN oe/d-oeckit.w(INPUT oe-ordl.company,
    INPUT oe-ordl.cust-no,
    INPUT oe-ordl.i-no,
    INPUT oe-ord.ord-date).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-quote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-quote Procedure 
PROCEDURE check-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-part-no LIKE oe-ordl.part-no NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR ldQuotePrice AS DEC NO-UNDO.
  DO:
    IF AVAIL xest AND v-quo-price-log                             AND
    v-quo-price-dec EQ 1 AND get-sv("oe-ordl.est-no") NE "" THEN DO:
      FOR EACH quotehd
        WHERE quotehd.company EQ cocode
        AND quotehd.loc     EQ xest.loc
        AND quotehd.est-no  EQ xest.est-no
        NO-LOCK,
        EACH quoteitm OF quotehd
        WHERE (quoteitm.part-no  EQ get-sv("oe-ordl.part-no") OR
        (quoteitm.part-no EQ ip-part-no AND ip-part-no NE ""))
        NO-LOCK,
        EACH quoteqty OF quoteitm
        WHERE quoteqty.qty LE INT(get-sv("oe-ordl.qty"))
        NO-LOCK
        BY quoteqty.qty DESC:
        ldQuotePrice = quoteqty.price.
        LEAVE.
      END.
      ll = AVAIL quoteqty.
      IF ll THEN DO:
        ll = quoteqty.qty EQ INT(get-sv("oe-ordl.qty")).
        IF NOT ll THEN
        MESSAGE "Quote does not exist for order quantity, import smaller quoted quantity sell price?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
        END.
        ELSE MESSAGE "No quotes exists for estimate/quantity..."
          VIEW-AS ALERT-BOX ERROR.
          IF NOT ll THEN DO:
            /* wfk APPLY "entry" TO oe-ordl.qty. */
            RETURN ERROR.
          END.
          ELSE DO:  /* get lower qty price from quote */
            IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
            tt-item-qty-price.tt-selected = YES AND
            (tt-item-qty-price.part-no EQ get-sv("oe-ordl.part-no") OR
            (tt-item-qty-price.part-no EQ ip-part-no AND ip-part-no EQ "")))
            THEN DO:
              FOR EACH quoteitm WHERE quoteitm.company = oe-ord.company AND
                quoteitm.est-no = get-sv("oe-ordl.est-no")  AND
                (quoteitm.part-no = ip-part-no OR ip-part-no <> ""),
                EACH quoteqty WHERE quoteqty.company = quoteitm.company AND
                quoteqty.loc = quoteitm.loc AND
                quoteqty.q-no = quoteitm.q-no AND
                quoteqty.LINE = quoteitm.LINE AND
                quoteqty.qty < INT(get-sv("oe-ordl.qty"))
                BY quoteitm.q-no DESC
                BY quoteitm.LINE
                BY quoteqty.qty DESC:
                CREATE tt-item-qty-price.
                ASSIGN tt-item-qty-price.q-no = quoteitm.q-no
                tt-item-qty-price.LINE = quoteitm.LINE
                tt-item-qty-price.quote-date = quoteqty.quote-date
                tt-item-qty-price.part-no = quoteitm.part-no
                tt-item-qty-price.qty = quoteqty.qty
                tt-item-qty-price.price = quoteqty.price
                tt-item-qty-price.uom = quoteqty.uom
                tt-item-qty-price.rels = quoteqty.rels
                tt-item-qty-price.quote-user = quoteqty.quote-user
                tt-item-qty-price.tt-selected = YES
                llGotLowerPrice = YES.
                LEAVE.
              END.
            END.
          END.
        END.
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-quote-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-quote-qty Procedure 
PROCEDURE check-quote-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-est-no AS cha NO-UNDO.
  DEF VAR v-tmp-part AS cha NO-UNDO.
  DO:
    lv-est-no = FILL(" ",8 - LENGTH(TRIM(get-sv("oe-ordl.est-no")))) +
    TRIM(get-sv("oe-ordl.est-no")).
    IF lv-est-no NE "" AND NOT AVAIL xest THEN
    FIND FIRST xest WHERE xest.company EQ cocode AND
    xest.est-no EQ lv-est-no NO-LOCK NO-ERROR.
    v-tmp-part = get-sv("oe-ordl.i-no").
    RUN check-quote (v-tmp-part) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CHOOSE_btn_go) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CHOOSE_btn_go Procedure 
PROCEDURE CHOOSE_btn_go :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-ord-row       AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-ordl-row      AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipl-autotrans     AS LOG   NO-UNDO.

DEF VAR ll-price-mod      AS   LOG  NO-UNDO.
DEF VAR lv-price          AS   CHAR NO-UNDO.
DEF VAR ll-pruom-mod      AS   LOG  NO-UNDO.
DEF VAR lv-pruom          AS   CHAR NO-UNDO.
DEF VAR lv-prev-req-date  AS   DATE NO-UNDO.
DEF VAR lv-stat           AS   CHAR NO-UNDO.
DEF VAR ll                AS   LOG  NO-UNDO.
DEF VAR ld                AS   DEC  NO-UNDO.
DEF VAR ll-reopen         AS   LOG  NO-UNDO.
DEF VAR ll-runship        AS   LOG  NO-UNDO.
DEF VAR v-job-rec_key     AS   CHAR NO-UNDO.
DEF VAR v-runsh           AS   INT  NO-UNDO.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-ord FOR oe-ord.

FIND oe-ord WHERE ROWID(oe-ord) EQ r-current-ord EXCLUSIVE-LOCK.
FIND oe-ordl WHERE ROWID(oe-ordl) EQ r-current-ordl EXCLUSIVE-LOCK.


    DISABLE TRIGGERS FOR LOAD OF xoe-ord.
    /* display spec notes for the item */
    RUN windows/d-spnote.w (get-sv("oe-ordl.i-no")).
    
    IF ip-type EQ "view" THEN DO:
      /* wfk   APPLY "go" TO FRAME {&FRAME-NAME}. */
      RETURN.
    END.
    
    /* wfk RUN custom/framechk.p (2, FRAME {&FRAME-NAME}:HANDLE). */
    
    
    ll-reopen = framechk-i-changed AND oe-ordl.stat EQ "C".
    /* gdm - 10220907 */
    IF TRIM(get-sv("oe-ordl.pr-uom")) EQ "" THEN DO:
      /* wfk  MESSAGE "UOM can't be blank. Please enter a valid UOM"
      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      /* wfk APPLY "entry" TO oe-ordl.pr-uom. */
      RETURN.
    END.
    /* gdm - 10220907 end */
    
    DO :
      ASSIGN
      v-qty-mod       = oe-ordl.qty NE li-prev-ord-qty
      li-prev-qty     = oe-ordl.qty
      li-prev-ord-qty = oe-ordl.qty
      ll-price-mod    = YES /* oe-ordl.price:MODIFIED */.
      lv-price        = get-sv("oe-ordl.price").
      ll-pruom-mod    = YES /*oe-ordl.pr-uom:MODIFIED */.
      lv-pruom        = get-sv("oe-ordl.pr-uom").
    END.
    
    lv-prev-req-date = oe-ordl.req-date.
    RUN itemfg-cost.
    IF get-sv("oe-ordl.est-no") <> "" THEN DO:
      RUN check-quote-qty NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        /* wfk APPLY 'entry' TO oe-ordl.qty. */
        MESSAGE "check-qty-qty error"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
      END.
    END.
    
    RUN validate-all (INPUT ipl-autotrans) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Validate all error"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
    
    IF (runship-char EQ "RUN&SHIP Prompt" AND ip-type = "ADD") 
        AND ipl-autotrans = NO THEN DO:
      IF get-sv("oe-ordl.est-no") GT ""  THEN
      set-sv("asi.oe-ordl.whsed", "YES" ).
      ELSE DO:
        ll-runship = LOGICAL(get-sv("asi.oe-ordl.whsed")).
        RUN oe/d-runsh.w (INPUT ll-runship, OUTPUT v-runsh).
        IF v-runsh = 1 THEN
        set-sv("asi.oe-ordl.whsed", "YES" ).
        IF v-runsh = 2 THEN DO:
          set-sv("tb_whs-item", "YES"  ).
          tb_whs-item = YES.
        END.
      END.
      DO TRANSACTION:
        
        IF AVAIL oe-ordl-whs-item THEN DO:
          IF tb_whs-item THEN
          oe-ordl.managed = true.
          ELSE
          oe-ordl.managed = false.
          FIND CURRENT oe-ordl-whs-item NO-LOCK.
        END.
      END.
    END.

    IF oepricecheck-log AND get-sv("oe-ordl.est-no") EQ "" AND
    ll-new-record THEN
    RUN prev-quote-proc(INPUT-OUTPUT lv-price,
    INPUT-OUTPUT lv-pruom).
    IF ipl-autotrans THEN
        lv-price = "0".
    DO:
      set-sv("oe-ordl.price", lv-price ).
      set-sv("oe-ordl.pr-uom", lv-pruom ).
      RUN lib-ordltot (INPUT ROWID(oe-ordl)).
    END.
    IF ll-reopen THEN DO:
      ll-reopen = NO.
      MESSAGE "This line item is closed, REOPEN?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE ll-reopen.
    END.
    SESSION:SET-WAIT-STATE ("general").
    DO TRANSACTION:
      FIND CURRENT oe-ordl EXCLUSIVE.
      
      IF ll-reopen THEN oe-ordl.stat = "".
      IF NOT ll-new-record THEN DO:
        RUN oe/upinvqty.p (RECID(oe-ordl)).
      END.
      /* wfk
      DO WITH FRAME {&FRAME-NAME}:
      /* wfk ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} */
      tb_whs-item.
      IF asi.oe-ordl.whsed:HIDDEN = FALSE THEN
      ASSIGN oe-ordl.whsed.
      END.
      */
      RUN whs-item (1).
      FIND xoe-ord WHERE RECID(xoe-ord) = RECID(oe-ord) EXCLUSIVE.
      FIND FIRST itemfg WHERE itemfg.company EQ cocode
      AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        ASSIGN
        xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
        oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
        xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
        /*IF TRIM(oe-ordl.est-no) NE "" AND
        TRIM(xoe-ord.est-no) EQ "" AND
        ll-new-record              THEN
        RUN fg/makenote.p (BUFFER oe-ordl, ?, itemfg.rec_key).*/
      END.
      FIND CURRENT xoe-ord NO-LOCK.
      IF lv-change-prom-date THEN DO:
        FOR EACH xoe-ordl WHERE xoe-ordl.company EQ g_company
          AND xoe-ordl.ord-no EQ oe-ord.ord-no
          AND RECID(xoe-ordl) NE RECID(oe-ordl):
          ASSIGN xoe-ordl.prom-date = oe-ordl.prom-date.
        END.
      END.
      IF lv-change-cst-po THEN DO:
        FOR EACH xoe-ordl WHERE xoe-ordl.company EQ g_company
          AND xoe-ordl.ord-no EQ oe-ord.ord-no
          AND RECID(xoe-ordl) NE RECID(oe-ordl):
          ASSIGN xoe-ordl.po-no = oe-ordl.po-no.
        END.
      END.
      RELEASE xoe-ordl.
      RUN update-itemfg.
      /* wfk ASSIGN {&list-2} .*/  /* job-no job-no2 */
      FIND CURRENT oe-ordl NO-LOCK.
    END. /* trans */

    IF ip-type NE "update" AND oe-ordl.est-no NE "" THEN
    RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).
    IF oereleas-log THEN
    IF ll-new-record THEN RUN create-release.
    ELSE RUN update-release.
    
    DO TRANSACTION:
      FIND CURRENT oe-ordl EXCLUSIVE.
      FIND CURRENT oe-ord EXCLUSIVE.
      RUN final-steps (INPUT ROWID(oe-ord), INPUT ROWID(oe-ordl)).
      IF ll-new-record THEN DO:
        RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
        xoe-ord.t-freight = xoe-ord.t-freight + oe-ordl.t-freight.
      END.
      RUN oe/ordfrate.p (ROWID(oe-ord)).
      RUN oe/oe-comm.p.
      RUN oe/calcordt.p (ROWID(oe-ord)).
      IF ld-prev-t-price NE oe-ordl.t-price OR ip-type BEGINS "update-" THEN
      RUN oe/creditck.p (ROWID(oe-ord), YES).
      IF oe-ordl.job-no NE "" THEN
      RUN oe/palchk.p(ROWID(oe-ord), oe-ordl.i-no).
      ld-prev-t-price = oe-ordl.t-price.
      /* gdm - 11090905 */
      IF ip-type EQ "Update" AND
      v-ponoUp THEN DO:
        IF lv-change-cst-po THEN
        FOR EACH job-hdr WHERE
          job-hdr.company EQ oe-ordl.company AND
          job-hdr.job-no  EQ oe-ordl.job-no AND
          job-hdr.job-no2 EQ oe-ordl.job-no2 AND
          job-hdr.ord-no  EQ oe-ordl.ord-no:
          ASSIGN job-hdr.po-no = oe-ordl.po-no.
        END.
        ELSE
        FOR EACH job-hdr WHERE
          job-hdr.company EQ oe-ordl.company AND
          job-hdr.job-no  EQ oe-ordl.job-no AND
          job-hdr.job-no2 EQ oe-ordl.job-no2 AND
          job-hdr.ord-no EQ oe-ordl.ord-no AND
          job-hdr.i-no EQ oe-ordl.i-no:
          ASSIGN job-hdr.po-no = oe-ordl.po-no.
        END.
        RELEASE job-hdr.
      END.
      /* gdm - 11090905 end */
      IF ip-type EQ "Update" AND
      TRIM(oe-ordl.job-no) EQ "" AND
      TRIM(oe-ord.est-no) NE "" THEN
      DO:
        FIND FIRST job-hdr WHERE
        job-hdr.company EQ oe-ordl.company AND
        job-hdr.ord-no EQ oe-ordl.ord-no AND
        job-hdr.i-no EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.
        IF AVAIL job-hdr THEN
        DO:
          ASSIGN
          oe-ordl.job-no = job-hdr.job-no
          oe-ordl.job-no2 = job-hdr.job-no2.
          IF TRIM(oe-ord.job-no) EQ "" THEN
          ASSIGN
          oe-ord.job-no = job-hdr.job-no
          oe-ord.job-no2 = job-hdr.job-no2.
          RELEASE job-hdr.
        END.
      END.
      /* end of job update */
      FIND CURRENT oe-ord NO-LOCK.
      FIND CURRENT oe-ordl NO-LOCK.
      IF ll-new-record AND TRIM(v-duplicateFGDayClient) = "DuplicateFGDayClient" THEN DO:
        RUN check-duplicateFGDayClient.
      END.
    END.
    RUN sys/inc/ordlcomp.p (ROWID(oe-ordl)).
    
    RUN final-steps2 (INPUT ROWID(oe-ord), INPUT ROWID(oe-ordl), INPUT ipl-autotrans).
    /* need to assign oe-ordl.est-type = eb.est-type
    job */
    ASSIGN
    v-qty-mod         = NO
    lv-add-mode       = NO
    ll-new-fg-created = NO.
    IF NOT ipl-autotrans THEN DO:
      
      DISPLAY {&DISPLAYED-FIELDS}.
    END.
    DO TRANSACTION:
      FIND CURRENT oe-ordl EXCLUSIVE.
      FIND CURRENT oe-ord EXCLUSIVE.
      /* assign rec_key to oe-ord for notes */
      IF oe-ord.est-no <> "" THEN
      DO:
        /*if notes frozen from jc/jobnotes.p, don't update rec_key*/
        FIND FIRST job-hdr WHERE
        job-hdr.company EQ cocode AND
        job-hdr.job-no  EQ oe-ordl.job-no AND
        job-hdr.job-no2 EQ oe-ordl.job-no2
        NO-LOCK NO-ERROR.
        IF AVAIL job-hdr THEN
        DO:
          FIND FIRST job WHERE
          job.company EQ cocode AND
          job.job EQ job-hdr.job AND
          job.job-no EQ job-hdr.job-no AND
          job.job-no2 EQ job-hdr.job-no2
          NO-LOCK NO-ERROR.
          IF AVAIL job THEN
          DO:
            v-job-rec_key = job.rec_key.
            RELEASE job.
          END.
          RELEASE job-hdr.
        END.
        IF oe-ordl.rec_key EQ "" OR
        (v-job-rec_key NE oe-ordl.rec_key) THEN
        oe-ordl.rec_key = est.rec_key.
      END.
      FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
      AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
      AND RECID(b-oe-ordl) <> RECID(oe-ordl) NO-LOCK NO-ERROR.
      IF NOT AVAIL b-oe-ordl AND oe-ordl.est-no <> "" THEN DO:
        FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
        b-oe-ord.rec_key = oe-ordl.rec_key.
        RELEASE b-oe-ord.
      END.
      ELSE DO:
        FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
        AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
        AND RECID(b-oe-ordl) <> RECID(oe-ordl)
        AND b-oe-ordl.est-no <> oe-ordl.est-no
        NO-LOCK NO-ERROR.
        IF NOT AVAIL b-oe-ordl AND oe-ordl.est-no <> "" THEN DO:
          FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
          b-oe-ord.rec_key = oe-ordl.rec_key.
          RELEASE b-oe-ord.
        END.
      END.
      /* end of job update */
      FIND CURRENT oe-ord NO-LOCK.
      FIND CURRENT oe-ordl NO-LOCK.
    END. /* trans */
    DO TRANSACTION:
      FIND CURRENT oe-ord.
      RUN oe/ordfrate.p (ROWID(oe-ord)). /* strange problem with freight */
      ll = NO.
      IF AVAIL oe-ord AND oe-ord.due-date GT oe-ordl.req-date THEN
      MESSAGE "Change order header due date to " + TRIM(STRING(oe-ordl.req-date)) "?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.
      IF ll THEN oe-ord.due-date = oe-ordl.req-date.
      FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    END.
    IF (oe-ordl.req-date NE lv-prev-req-date OR ip-type EQ "ADD"
    /*OR ip-type = "UPdate-2" doen in v-ord.w order-from-est proc */)
    /* update job's start-date when req-date is changed */
    AND get-sv("oe-ordl.est-no") NE "" /*AND lv-update-job-stdate */
    AND (v-run-schedule OR schedule-log)
    THEN RUN update-start-date.
    IF oe-ordl.job-no NE '' THEN RUN update-due-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CHOOSE_btn_hist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CHOOSE_btn_hist Procedure 
PROCEDURE CHOOSE_btn_hist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                   DEF VAR lv-rowid               AS ROWID NO-UNDO.
                    DEF VAR lv-qty                 AS DEC NO-UNDO.
                    DEF VAR io-rowid-list          AS CHAR NO-UNDO.
                    DEF VAR io-qty-list            AS CHAR NO-UNDO.
                    DEF VAR li                     AS INT NO-UNDO.
                    DEF VAR lr-save-xoeordl-buffer AS ROWID NO-UNDO.
                    DEF VAR lv-on-screen-item      AS CHAR NO-UNDO. /* Others pulled in from history not shown */
                    DEF BUFFER b-oe-ordl      FOR oe-ordl.
                    DEF BUFFER bf-new-oe-ordl FOR oe-ordl.
                    IF AVAIL oe-ordl THEN DO:
                      lv-rowid = ROWID(oe-ordl).
                      io-rowid-list = STRING(ROWID(oe-ordl)).
                      /* if there is an existing order line, pass this to the     */
                      /* procedure so it won't be a multi-select                  */
                      /* add an output parameter to this call with the rowid list */
                      RUN oe/d-oehist.w (INPUT-OUTPUT io-rowid-list,THIS-PROCEDURE, OUTPUT io-qty-list).
                      IF NUM-ENTRIES(io-rowid-list) GT 1 THEN
                      lv-multi-select = TRUE.
                      ELSE
                      lv-multi-select = FALSE.
                      EACH-SELECTED:
                      DO li = 1 TO NUM-ENTRIES(io-rowid-list):
                        IF io-rowid-list = "" THEN
                        LEAVE.
                        lv-rowid = TO-ROWID(ENTRY(li, io-rowid-list)).
                        lv-qty   = DECIMAL(ENTRY(li, io-qty-list)).
                        /* do this for each rowid returned: */
                        FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ lv-rowid NO-LOCK NO-ERROR.
                        IF AVAIL b-oe-ordl THEN
                        RUN getQtyPrice (INPUT ROWID(b-oe-ordl)).
                        IF li EQ 1 THEN DO:
                          /* Process row currently on screen, i.e. first rowid returned */
                          IF AVAIL b-oe-ordl THEN DO:
                            IF ip-type NE 'Update' THEN
                            set-sv("oe-ordl.i-no", b-oe-ordl.i-no ).
                            /* oe-ordl.i-no = b-oe-ordl.i-no */.
                            FIND FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no EQ get-sv("oe-ordl.i-no") NO-ERROR.
                            IF NOT AVAIL itemfg OR get-sv("oe-ordl.i-no") EQ "" THEN DO:
                              /* wfk APPLY 'ENTRY' TO oe-ordl.i-no. */
                              NEXT EACH-SELECTED.
                            END.
                            IF AVAIL itemfg THEN

                            set-sv("oe-ordl.part-no", itemfg.part-no ).
                            set-sv("oe-ordl.i-name", itemfg.i-name ).
                            set-sv("oe-ordl.part-dscr1", itemfg.part-dscr1 ).
                            set-sv("oe-ordl.part-dscr2", itemfg.part-dscr2 ).

                            set-sv("oe-ordl.price", STRING(b-oe-ordl.price) ).
                            set-sv("oe-ordl.pr-uom", b-oe-ordl.pr-uom ).
                            price-ent = YES.
                            IF INTEGER(get-sv("oe-ordl.qty")) EQ 0 THEN
                            set-sv("oe-ordl.qty", STRING(lv-qty /* b-oe-ordl.qty */)  ).
                            lv-on-screen-item = oe-ordl.i-no.
                            IF get-sv("oe-ordl.est-no") NE ""
                            AND oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                          END. /* if avail */
                          ELSE NEXT. /* RETURN NO-APPLY. */
                          IF setFromHistory AND INTEGER(get-sv("oe-ordl.qty")) EQ 0 THEN
                          set-sv("oe-ordl.qty", STRING(historyQty) ).
                          ASSIGN
                          save_id      = RECID(oe-ordl)
                          v-i-item     = get-sv("oe-ordl.i-no")
                          v-i-qty      = INTEGER(get-sv("oe-ordl.qty"))
                          price-ent    = NO
                          matrixExists = NO.
                          FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ ROWID(oe-ordl)
                          EXCLUSIVE-LOCK NO-ERROR.
                          SAVE_id = RECID(oe-ordl).
                          /* Depends on v-i-item */
                          IF v-i-item GT "" THEN
                          RUN oe/oe-price.p.
                          IF matrixExists THEN RUN get-price.
                          ELSE IF setFromHistory THEN DO:
                            MESSAGE 'No price exists in the price matrix,' SKIP
                            'Import Sell Price from History?' VIEW-AS ALERT-BOX
                            QUESTION BUTTONS YES-NO UPDATE setFromHistory.
                            IF setFromHistory THEN
                            DO:

                              set-sv("oe-ordl.price", STRING(historyPrice) ).
                              set-sv("oe-ordl.pr-uom", STRING(historyPrUOM) ).
                              price-ent                   = YES.
                              IF get-sv("oe-ordl.est-no") NE "" AND
                              oeestcom-log = YES THEN
                              RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                            END.
                          END. /* not matrixexits */
                          /* Only do this for the order currently in the viewer */
                          ASSIGN
                          ll-ok-i-no = NO
                          historyButton = YES.
                          /* wfk
                          APPLY 'LEAVE' TO oe-ordl.i-no.
                          APPLY 'ENTRY' TO oe-ordl.price.
                          */
                        END. /* if li = 1 */
                        ELSE DO:
                          /* Process new oe-ordl records if multi-select */
                          IF AVAIL b-oe-ordl THEN DO:
                            RUN create-item.
                            FIND bf-new-oe-ordl WHERE RECID(bf-new-oe-ordl) = lv-item-recid
                            EXCLUSIVE-LOCK NO-ERROR.
                            bf-new-oe-ordl.i-no = b-oe-ordl.i-no.
                            IF lv-on-screen-item NE bf-new-oe-ordl.i-no THEN
                            op-rowid-list = op-rowid-list + STRING(ROWID(bf-new-oe-ordl)) + ",".
                            FIND FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no EQ bf-new-oe-ordl.i-no NO-ERROR.
                            IF AVAIL itemfg THEN
                            ASSIGN
                            bf-new-oe-ordl.part-no = itemfg.part-no
                            bf-new-oe-ordl.i-name = itemfg.i-name
                            bf-new-oe-ordl.part-dscr1 = itemfg.part-dscr1
                            bf-new-oe-ordl.part-dscr2 = itemfg.part-dscr2.
                            ASSIGN
                            bf-new-oe-ordl.price = b-oe-ordl.price
                            bf-new-oe-ordl.pr-uom = b-oe-ordl.pr-uom
                            price-ent = YES.
                            /* IF INTEGER(bf-new-oe-ordl.qty) EQ 0 THEN */
                            bf-new-oe-ordl.qty = lv-qty.
                            IF bf-new-oe-ordl.est-no NE ""
                            AND oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(bf-new-oe-ordl), INPUT NO).
                          END. /* if avail */
                          ELSE NEXT. /* RETURN NO-APPLY. */
                          IF setFromHistory AND bf-new-oe-ordl.qty EQ 0 THEN
                          bf-new-oe-ordl.qty = historyQty.
                          ASSIGN
                          save_id      = RECID(bf-new-oe-ordl)
                          v-i-item     = bf-new-oe-ordl.i-no
                          v-i-qty      = bf-new-oe-ordl.qty
                          price-ent    = NO
                          matrixExists = NO.
                          /* Code modified to operate on bf-new-oe-ordl - bring in multiple from history */
                          lr-save-xoeordl-buffer = ?.
                          IF AVAIL xoe-ordl THEN
                          lr-save-xoeordl-buffer = ROWID(xoe-ordl).
                          FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ ROWID(bf-new-oe-ordl)
                          EXCLUSIVE-LOCK NO-ERROR.
                          /* Depends on v-i-item */
                          RUN oe/oe-price.p.
                          /* Modify from get-price to operate on new buffer */
                          IF matrixExists THEN RUN get-price-hidden (INPUT ROWID(bf-new-oe-ordl)).
                          ELSE IF setFromHistory THEN DO:
                            MESSAGE 'No price exists in the price matrix for item ' + xoe-ordl.i-no + ',' SKIP
                              'Import Sell Price from History?' VIEW-AS ALERT-BOX
                              QUESTION BUTTONS YES-NO UPDATE setFromHistory.
                              IF setFromHistory THEN
                              DO:
                                ASSIGN
                                bf-new-oe-ordl.price  = historyPrice
                                bf-new-oe-ordl.pr-uom = historyPrUOM
                                price-ent                   = YES.
                                /* Modified get-est-comm for new buffer */
                                IF bf-new-oe-ordl.est-no NE ""
                                AND oeestcom-log = YES THEN
                                RUN get-est-comm (INPUT ROWID(bf-new-oe-ordl), INPUT NO).
                              END.
                            END. /* not matrixexits */
                            /* Restore xoe-ordl to original row in case needed somewhere else */
                            IF lr-save-xoeordl-buffer NE ? THEN
                            FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lr-save-xoeordl-buffer
                            EXCLUSIVE-LOCK NO-ERROR.
                          END. /* new records from multi-select */
                        END. /* Each li */
                        op-rowid-list = TRIM(op-rowid-list, ",").
                        /* End loop processing */
                        RETURN NO-APPLY.
                      END. /* if avail oe-ordl */
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clean-s-pct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clean-s-pct Procedure 
PROCEDURE clean-s-pct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-pct LIKE oe-ordl.s-pct EXTENT 4 NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DO:
    ASSIGN
    ld-pct[1] = DEC(get-sv("oe-ordl.s-pct[1]"))
    ld-pct[2] = DEC(get-sv("oe-ordl.s-pct[2]"))
    ld-pct[3] = DEC(get-sv("oe-ordl.s-pct[3]"))
    ld-pct[4] = ld-pct[1] + ld-pct[2] + ld-pct[3].
    IF ld-pct[4] NE 100 THEN DO li = 1 TO 3:
      ld-pct[li] = ld-pct[li] / ld-pct[4] * 100.
    END.
    ld-pct[1] = ld-pct[1] + (100 - (ld-pct[1] + ld-pct[2] + ld-pct[3])).
    
    set-sv("oe-ordl.s-pct[1]", STRING(ld-pct[1]) ).
    set-sv("oe-ordl.s-pct[2]", STRING(ld-pct[2]) ).
    set-sv("oe-ordl.s-pct[3]", STRING(ld-pct[3]) ).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Procedure 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-ord-row   AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipi-qty       AS INT   NO-UNDO.
  DEF OUTPUT PARAMETER opr-ordl-row AS ROWID NO-UNDO.

  DEF VAR ll-ans AS LOG INIT NO NO-UNDO.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  FIND oe-ord WHERE ROWID(oe-ord) EQ ipr-ord-row EXCLUSIVE-LOCK .
/*      oe-ord.company = g_company
  AND oe-ord.ord-no = ip-ord-no NO-LOCK NO-ERROR. */
  IF AVAIL oe-ord THEN DO:
    CREATE bf-oe-ordl.
    ASSIGN lv-item-recid = RECID(bf-oe-ordl)
    ll-new-record = YES.
    FIND FIRST cust {sys/ref/custW.i} AND cust.cust-no = oe-ord.cust-no
    USE-INDEX cust NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DO:
        MESSAGE "Customer not found " oe-ord.cust-no " Order: " oe-ord.ord-no
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    ASSIGN
    bf-oe-ordl.company   = cocode
    bf-oe-ordl.ord-no    = oe-ord.ord-no
    bf-oe-ordl.type-code = oe-ord.TYPE
    bf-oe-ordl.cust-no   = oe-ord.cust-no
    bf-oe-ordl.po-no     = oe-ord.po-no
    bf-oe-ordl.req-code  = oe-ord.due-code
    bf-oe-ordl.req-date  = oe-ord.due-date
    bf-oe-ordl.prom-code = oe-ord.due-code
    bf-oe-ordl.prom-date = oe-ord.due-date
    bf-oe-ordl.disc      = cust.disc
    bf-oe-ordl.tax       = cust.SORT EQ "Y" AND oe-ord.tax-gr NE ""
    bf-oe-ordl.over-pct  = oe-ord.over-pct
    bf-oe-ordl.qty       = ipi-qty
    bf-oe-ordl.under-pct = oe-ord.under-pct.
    opr-ordl-row         = ROWID(bf-oe-ordl).
    /* wfk */ DEF VAR lastship-cha AS CHAR.
    IF lastship-cha = "Stock/Custom" THEN DO:
      /* If order has no estimate. */
      /* wfk */ DEF VAR lastship-int AS INT.
      DEF VAR lastship-dec AS DEC.
      IF bf-oe-ordl.est-no = "" THEN
      ASSIGN bf-oe-ordl.req-date = (TODAY + lastship-int).
      ELSE
      ASSIGN bf-oe-ordl.req-date = (TODAY + INT(lastship-dec)).
      ASSIGN bf-oe-ordl.prom-date = bf-oe-ordl.req-date.
    END.
    {custom/shptotax.i oe-ord.cust-no oe-ord.sold-id bf-oe-ordl.tax}
    FOR LAST b-oe-ordl OF oe-ord
      WHERE ROWID(b-oe-ordl) NE ROWID(bf-oe-ordl)
      NO-LOCK
      BY b-oe-ordl.LINE:
      bf-oe-ordl.LINE = b-oe-ordl.LINE + 1.
    END.
    IF bf-oe-ordl.LINE EQ 0 THEN bf-oe-ordl.LINE = 1. /* wfk - changed this */
    /*
    if oe-ord.est-no ne "" then
    assign
    oe-ordl.job-no  = oe-ord.job-no
    oe-ordl.job-no2 = oe-ord.job-no2
    oe-ordl.est-no   = oe-ord.est-no
    oe-ordl.e-num    = oe-ord.e-num
    oe-ordl.est-type = oe-ord.est-type.
    */
    DO i = 1 TO 3:
      ASSIGN
      bf-oe-ordl.s-man[i]  = oe-ord.sman[i]
      bf-oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
      bf-oe-ordl.s-comm[i] = oe-ord.s-comm[i].
    END.
    ASSIGN
    bf-oe-ordl.q-qty = oe-ord.t-fuel.
    v-margin = oe-ord.t-fuel.
    {oe/defwhsed.i bf-oe-ordl}
    IF bf-oe-ordl.LINE EQ 1 OR NOT AVAIL oe-ordl THEN
    FIND oe-ordl WHERE ROWID(oe-ordl) = ROWID(bf-oe-ordl) NO-ERROR.
  END. /* avail oe-ord */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-job) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job Procedure 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-recid AS RECID NO-UNDO.
  DEF VAR v-job-job LIKE job.job NO-UNDO.
  DEF VAR v-job-no LIKE job.job-no NO-UNDO.
  DEF VAR v-job-no2 LIKE job.job-no2 NO-UNDO.
  DEF VAR li-j-no AS INT NO-UNDO.
  DEF VAR v-i AS INT NO-UNDO.
  DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
  /* === from oe/oe-ord1.p  ============= */
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  FIND LAST job WHERE job.company EQ cocode USE-INDEX job NO-LOCK NO-ERROR.
  v-job-job = IF AVAIL job THEN job.job + 1 ELSE 1.
  DO v-i = 1 TO 10:
    FIND job WHERE job.company EQ cocode
    AND job.job = v-job-job USE-INDEX job
    NO-LOCK NO-ERROR.
    IF  NOT AVAIL job THEN
    LEAVE.
    v-job-job = v-job-job + 1.
  END.
  IF oe-ord.job-no <> "" THEN
  ASSIGN v-job-no = oe-ord.job-no
  v-job-no2 =  oe-ord.job-no2.
  ELSE
  IF oe-ordl.job-no EQ "" THEN DO:
    FIND FIRST est
    WHERE est.company EQ cocode
    AND est.est-no  EQ oe-ordl.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN
    FIND FIRST eb
    WHERE eb.company  EQ oe-ordl.company
    AND eb.est-no   EQ oe-ordl.est-no
    AND eb.cust-no  EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    IF AVAIL eb THEN
    v-prod-cat = eb.procat.
    v-job-no = FILL(" ",6 - LENGTH(TRIM(STRING(oe-ordl.ord-no)))) + STRING(oe-ordl.ord-no).
    
    RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                     INPUT-OUTPUT v-job-no2,
                     INPUT v-prod-cat,
                     INPUT "").
    
    IF v-job-no NE "" THEN DO:
      ASSIGN
      oe-ordl.job-no  = v-job-no
      oe-ordl.job-no2 = v-job-no2.
      DISPLAY oe-ordl.job-no oe-ordl.job-no2 .
    END.
  END.
  ELSE
  IF oe-ordl.job-no NE "" THEN
  ASSIGN v-job-no = oe-ordl.job-no
  v-job-no2 = oe-ordl.job-no2.
  IF v-job-no NE "" THEN
  FOR EACH job
    WHERE job.company EQ cocode
    AND job.job-no  EQ v-job-no
    AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.
  CREATE job.
  ASSIGN job.job        = v-job-job
  job.company    = cocode
  job.loc        = locode
  job.est-no     = oe-ordl.est-no
  job.job-no     = oe-ordl.job-no
  job.job-no2    = oe-ordl.job-no2
  job.stat       = "P"
  op-recid = RECID(job).
  FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
  AND job-hdr.job-no  EQ oe-ordl.job-no
  AND job-hdr.job-no2 EQ oe-ordl.job-no2
  AND job-hdr.ord-no  EQ oe-ordl.ord-no
  AND job-hdr.i-no    EQ oe-ordl.i-no NO-ERROR.
  IF NOT AVAIL job-hdr THEN DO:
    FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
    AND itemfg.i-no    EQ oe-ordl.i-no
    NO-LOCK NO-ERROR.
    CREATE job-hdr.
    ASSIGN job-hdr.company      = cocode
    job-hdr.loc          = locode
    job-hdr.e-num        = oe-ordl.e-num
    job-hdr.est-no       = oe-ordl.est-no
    job-hdr.i-no         = oe-ordl.i-no
    /*     job-hdr.qty          = oe-ordl.qty */
    job-hdr.cust-no      = oe-ordl.cust-no
    job-hdr.ord-no       = oe-ordl.ord-no
    job-hdr.po-no        = oe-ordl.po-no
    job-hdr.job     = job.job
    job-hdr.job-no  = job.job-no
    job-hdr.job-no2 = job.job-no2.
    IF AVAIL itemfg THEN
    ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
    job-hdr.std-lab-cost = itemfg.std-lab-cost
    job-hdr.std-var-cost = itemfg.std-var-cost
    job-hdr.std-fix-cost = itemfg.std-fix-cost.
    ASSIGN job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
    job-hdr.std-var-cost + job-hdr.std-fix-cost).
  END.
  ASSIGN job-hdr.est-no  = oe-ordl.est-no
  job-hdr.job     = job.job
  job-hdr.job-no  = job.job-no
  job-hdr.job-no2 = job.job-no2
  oe-ordl.j-no = job-hdr.j-no.
    ASSIGN 
        v-job-no  = job.job-no
        v-job-no2 = job.job-no2.
  IF oe-ord.stat EQ "H" THEN 
    RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
  RELEASE job.
  RELEASE job-hdr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-release) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-release Procedure 
PROCEDURE create-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty-sum AS INT NO-UNDO.
  DEF VAR v-nxt-r-no AS INT INIT 1 NO-UNDO.
  DEF VAR v-lst-rel AS DATE NO-UNDO.
  DEF VAR v-pct-chg AS DEC NO-UNDO.
  DEF VAR v-ship-id LIKE oe-rel.ship-id NO-UNDO.
  DEF VAR v-num-shipto AS INT NO-UNDO.
  DEF VAR v-relType AS cha NO-UNDO.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  ASSIGN v-qty-sum  = 0.
  /* gdm - 06220908*/
  DEF VAR v-relflg2 AS LOG INIT YES NO-UNDO.
  IF v-relflg THEN
  MESSAGE
  "CREATE RELEASE ? "
  VIEW-AS ALERT-BOX INFO BUTTONS YES-NO
  UPDATE v-relflg2.
  IF v-relflg2 THEN DO:
    {oe/oe-rel.a &fil="oe-ordl"}.
  END.
  /* gdm - 06220908 end */
  ASSIGN v-ship-id = ""
         lv-qty = oe-ordl.qty.
   
  FIND FIRST xoe-rel WHERE xoe-rel.company EQ cocode
        AND xoe-rel.ord-no  EQ oe-ordl.ord-no
        AND RECID(xoe-rel)  NE RECID(oe-rel)
        AND xoe-rel.link-no EQ 0
    NO-LOCK NO-ERROR.

      /* 12061207 For transfer orders, ship-id will be filled in */
      IF AVAIL oe-ord AND oe-ord.ship-id NE "" THEN
          v-ship-id = oe-ord.ship-id. 

  IF NOT AVAIL xoe-rel OR oe-ordl.est-no NE "" THEN DO:

    FOR EACH shipto WHERE shipto.company EQ cocode
      AND shipto.cust-no EQ oe-ordl.cust-no:
      ASSIGN v-num-shipto = v-num-shipto + 1.
    END.

    IF v-num-shipto GT 1 THEN
    DO:
      IF oe-ordl.est-no NE "" THEN
      FOR EACH eb
        WHERE eb.company  EQ oe-ordl.company
        AND eb.est-no   EQ oe-ordl.est-no
        AND eb.cust-no  EQ oe-ord.cust-no
        AND eb.ship-id  NE ""
        NO-LOCK
        BREAK BY eb.stock-no DESC:
        IF LAST(eb.stock-no)           OR
        eb.stock-no EQ oe-ordl.i-no THEN DO:
          v-ship-id = eb.ship-id.
          LEAVE.
        END.
      END.
      ELSE
      FOR EACH shipto
        WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ oe-ord.cust-no
        NO-LOCK
        BREAK BY shipto.ship-no DESC:
        IF shipto.ship-id EQ oe-ord.cust-no THEN DO:
          v-ship-id = shipto.ship-id.
          LEAVE.
        END.
      END.
      /* task# 09160502 */
      DEF BUFFER bf-ordl FOR oe-ordl.
      DEF BUFFER bf-rel FOR oe-rel.

      IF oe-ord.est-no = "" THEN
      FIND FIRST bf-ordl WHERE bf-ordl.company = oe-ordl.company
      AND bf-ordl.ord-no = oe-ordl.ord-no
      AND bf-ordl.i-no <> ""
      AND RECID(bf-ordl) <> RECID(oe-ordl) NO-LOCK NO-ERROR.
      IF AVAIL bf-ordl AND oeship-cha = "OEShipto" AND NOT (v-orig-ip-type EQ "ADD" AND lv-multi-select = YES) THEN DO:
        FIND FIRST bf-rel WHERE bf-rel.company EQ bf-ordl.company
        AND bf-rel.ord-no  EQ bf-ordl.ord-no
        AND bf-rel.i-no    EQ bf-ordl.i-no
        AND bf-rel.LINE    EQ bf-ordl.LINE NO-LOCK NO-ERROR.
        v-ship-id = IF AVAIL bf-rel THEN bf-rel.ship-id ELSE v-ship-id.
      END.
      /* 12061207 For transfer orders, ship-id will be filled in */
      IF AVAIL oe-ord AND oe-ord.ship-id NE "" THEN
          v-ship-id = oe-ord.ship-id. 

      /* gdm - 06220908 */
      /* wfk IF v-relflg2 THEN  RUN oe/d-shipid.w (INPUT oe-ordl.cust-no, INPUT-OUTPUT v-ship-id)  . */
      /* gdm - 06220908*/
      IF v-relflg2 THEN ASSIGN oe-rel.ship-id = TRIM(v-ship-id).
      FIND FIRST shipto WHERE shipto.company = cocode AND
      shipto.cust-no = xoe-ord.cust-no  AND
      shipto.ship-id = v-ship-id
      USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN DO:
        ASSIGN v-ship-id           = shipto.ship-id.
        /* gdm - 06220908 */
        IF v-relflg2 THEN
        ASSIGN oe-rel.ship-no      = shipto.ship-no
        oe-rel.ship-id      = shipto.ship-id
        oe-rel.ship-addr[1] = shipto.ship-addr[1]
        oe-rel.ship-addr[2] = shipto.ship-addr[2]
        oe-rel.ship-city    = shipto.ship-city
        oe-rel.ship-state   = shipto.ship-state
        oe-rel.ship-zip     = shipto.ship-zip
        oe-rel.ship-i[1] = shipto.notes[1]
        oe-rel.ship-i[2] = shipto.notes[2]
        oe-rel.ship-i[3] = shipto.notes[3]
        oe-rel.ship-i[4] = shipto.notes[4].
        /* gdm - 06220908 end */
        /* maybe later
        IF shipto.notes[1] <> "" OR shipto.notes[2] <> "" OR
        shipto.notes[3] <> "" OR shipto.notes[4] <> "" THEN DO:
        FIND FIRST notes WHERE notes.rec_key = oe-rel.rec_key NO-LOCK NO-ERROR.
        IF NOT AVAIL notes THEN DO:
        CREATE notes.
        ASSIGN notes.rec_key = oe-rel.rec_key
        notes.note_date = TODAY
        notes.note_title = shipto.notes[1]
        notes.note_text = shipto.notes[1] + CHR(13) +
        shipto.notes[2] + CHR(13) +
        shipto.notes[3] + CHR(13) +
        ship.notes[4] + CHR(13).
        END.
        END.
        */
        /* if add mode then use default carrier */
        /*   if sel = 3 /* and NOT oe-rel.carrier ENTERED */ then do: */
        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "OECARIER"
        NO-LOCK NO-ERROR.
        IF NOT AVAIL sys-ctrl THEN DO:
          CREATE sys-ctrl.
          ASSIGN
          sys-ctrl.company  = cocode
          sys-ctrl.NAME     = "OECARIER"
          sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
          sys-ctrl.char-fld = "ShipTo".
          DO WHILE TRUE:
            MESSAGE "Default Shipping Carrier from Header or Shipto?"
            UPDATE sys-ctrl.char-fld.
            IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE.
          END.
        END.
        /* gdm - 06220908 */
        IF v-relflg2 THEN
        ASSIGN oe-rel.carrier = IF sys-ctrl.char-fld = "Shipto"
        THEN shipto.carrier
        ELSE xoe-ord.carrier.
      END.
      /* Run Freight calculation  */
      RUN oe/oe-frtcl.p.
    END.  /* multi ship to */
    ELSE DO:
      FIND FIRST shipto WHERE shipto.company EQ cocode AND
      shipto.cust-no EQ xoe-ord.cust-no AND
      shipto.ship-id EQ v-ship-id
      NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN
      FIND FIRST shipto WHERE shipto.company EQ cocode AND
      shipto.cust-no EQ xoe-ord.cust-no
      NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN DO:
        /* gdm - 06220908 */
        IF v-relflg2 THEN
        ASSIGN oe-rel.ship-no      = shipto.ship-no
        oe-rel.ship-id      = shipto.ship-id
        oe-rel.ship-addr[1] = shipto.ship-addr[1]
        oe-rel.ship-addr[2] = shipto.ship-addr[2]
        oe-rel.ship-city    = shipto.ship-city
        oe-rel.ship-state   = shipto.ship-state
        oe-rel.ship-zip     = shipto.ship-zip
        oe-rel.ship-i[1] = shipto.notes[1]
        oe-rel.ship-i[2] = shipto.notes[2]
        oe-rel.ship-i[3] = shipto.notes[3]
        oe-rel.ship-i[4] = shipto.notes[4].
        /* ===== maybe later
        IF shipto.notes[1] <> "" OR shipto.notes[2] <> "" OR
        shipto.notes[3] <> "" OR shipto.notes[4] <> "" THEN DO:
        FIND FIRST notes WHERE notes.rec_key = oe-rel.rec_key NO-LOCK NO-ERROR.
        IF NOT AVAIL notes THEN DO:
        CREATE notes.
        ASSIGN notes.rec_key = oe-rel.rec_key
        notes.note_date = TODAY
        notes.note_title = shipto.notes[1]
        notes.note_text = shipto.notes[1] + CHR(13) +
        shipto.notes[2] + CHR(13) +
        shipto.notes[3] + CHR(13) +
        ship.notes[4] + CHR(13).
        END.
        END.
        ===========*/
        /* if add mode then use default carrier */
        IF ll-new-record /* and NOT oe-rel.carrier ENTERED */ THEN DO:
          /* wfk
          find first sys-ctrl where sys-ctrl.company eq cocode
          and sys-ctrl.name    eq
          no-lock no-error.         */
          IF NOT AVAIL sys-ctrl THEN DO:
            CREATE sys-ctrl.
            ASSIGN sys-ctrl.company  = cocode
            sys-ctrl.NAME     = "OECARIER"
            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
            sys-ctrl.char-fld = "ShipTo".
            DO WHILE TRUE:
              MESSAGE "Default Shipping Carrier from Header or Shipto?"
              UPDATE sys-ctrl.char-fld.
              IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
            END.
          END.
          /* gdm - 06220908 */
          IF v-relflg2 THEN
          oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
          ELSE xoe-ord.carrier.
        END.
      END. /* avail shipto */
    END. /* not multi */
  END. /* if no oe-rel */
  ELSE DO:
    FIND FIRST shipto WHERE shipto.company = cocode AND
    shipto.cust-no = xoe-ord.cust-no  AND
    shipto.ship-id = xoe-rel.ship-id
    USE-INDEX ship-id NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN DO:
      /* gdm - 06220908 */
      IF v-relflg2 THEN
      ASSIGN oe-rel.ship-no      = shipto.ship-no
      oe-rel.ship-id      = shipto.ship-id
      oe-rel.ship-addr[1] = shipto.ship-addr[1]
      oe-rel.ship-addr[2] = shipto.ship-addr[2]
      oe-rel.ship-city    = shipto.ship-city
      oe-rel.ship-state   = shipto.ship-state
      oe-rel.ship-zip     = shipto.ship-zip
      oe-rel.ship-i[1] = shipto.notes[1]
      oe-rel.ship-i[2] = shipto.notes[2]
      oe-rel.ship-i[3] = shipto.notes[3]
      oe-rel.ship-i[4] = shipto.notes[4].
      /*   IF shipto.notes[1] <> "" OR shipto.notes[2] <> "" OR
      shipto.notes[3] <> "" OR shipto.notes[4] <> "" THEN DO:
      FIND FIRST notes WHERE notes.rec_key = oe-rel.rec_key NO-LOCK NO-ERROR.
      IF NOT AVAIL notes THEN DO:
      CREATE notes.
      ASSIGN notes.rec_key = oe-rel.rec_key
      notes.note_date = TODAY
      notes.note_title = shipto.notes[1]
      notes.note_text = shipto.notes[1] + CHR(13) +
      shipto.notes[2] + CHR(13) +
      shipto.notes[3] + CHR(13) +
      ship.notes[4] + CHR(13).
      END.
      END.
      */
      /* if add mode then use default carrier */
      IF ll-new-record THEN DO:
        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "OECARIER"
        NO-LOCK NO-ERROR.
        IF NOT AVAIL sys-ctrl THEN DO:
          CREATE sys-ctrl.
          ASSIGN sys-ctrl.company  = cocode
          sys-ctrl.NAME     = "OECARIER"
          sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
          sys-ctrl.char-fld = "ShipTo".
          DO WHILE TRUE:
            MESSAGE "Default Shipping Carrier from Header or Shipto?"
            UPDATE sys-ctrl.char-fld.
            IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
          END.
        END.
        /* gdm - 06220908 */
        IF v-relflg2 THEN
        oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
        ELSE xoe-ord.carrier.
      END.
    END.
  END.
  IF v-relflg2 THEN DO:
    /* task 04011103*/
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.NAME EQ "RelType" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
    AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl-shipto THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
    AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
    ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
    IF v-relType <> "" THEN DO:
      oe-rel.s-code = IF oe-ordl.is-a-component THEN "S" 
                        ELSE SUBSTRING(v-relType,1,1).
      IF oe-ord.TYPE = "T" THEN
        ASSIGN oe-rel.s-code = "T".
      FIND FIRST reftable
      WHERE reftable.reftable EQ "oe-rel.s-code"
      AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") NO-LOCK NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN reftable.reftable = "oe-rel.s-code"
        reftable.company = STRING(oe-rel.r-no,"9999999999")
        reftable.CODE = IF oe-ordl.is-a-component THEN "S" 
                        ELSE SUBSTRING(v-relType,1,1).
        IF oe-ord.TYPE = "T" THEN
        ASSIGN reftable.CODE = "T".
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-crt-itemfg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg Procedure 
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
  DEF INPUT PARAMETER v-uom LIKE itemfg.prod-uom.
  DEF VAR tmpstore AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ll-one-part AS LOG NO-UNDO.
  DEF BUFFER x-eb FOR eb.
  DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
  DEF BUFFER b-eb2 FOR eb.
  DEF BUFFER bf-itemfg FOR itemfg.
  {sys/inc/setprint.i}
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  FIND FIRST cust  WHERE cust.company EQ cocode
  AND cust.cust-no EQ xeb.cust-no
  NO-LOCK NO-ERROR.
  {oe\fgfreight.i}
  FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ cocode
  AND sys-ctrl.NAME EQ "FGMASTER" NO-ERROR.
  IF AVAIL sys-ctrl THEN
  FIND FIRST bf-itemfg NO-LOCK
  WHERE bf-itemfg.company EQ sys-ctrl.company
  AND bf-itemfg.i-no EQ TRIM(sys-ctrl.char-fld) NO-ERROR.
  CREATE itemfg.
  ASSIGN
  itemfg.company    = cocode
  itemfg.loc        = locode
  itemfg.i-no       = v-item.
  ASSIGN
  itemfg.i-name     = get-sv("oe-ordl.i-name") .
  ASSIGN
  itemfg.part-dscr1 = get-sv("oe-ordl.part-dscr1")
  itemfg.part-dscr2 = get-sv("oe-ordl.part-dscr2")
  itemfg.sell-price = DEC(get-sv("oe-ordl.price"))
  itemfg.part-no    = get-sv("oe-ordl.part-no")
  itemfg.cust-no    = oe-ord.cust-no
  itemfg.cust-name  = oe-ord.cust-name.
  ASSIGN
  itemfg.pur-uom    = get-sv("oe-ordl.pr-uom")
  itemfg.ship-meth  = IF AVAIL bf-itemfg THEN bf-itemfg.ship-meth ELSE YES.
  
  ASSIGN
  itemfg.taxable = IF AVAIL cust
  THEN cust.SORT EQ "Y" AND cust.tax-gr NE ""
  ELSE
  IF AVAIL bf-itemfg THEN bf-itemfg.taxable
  ELSE NO.
  IF fgmaster-cha EQ "FGITEM" THEN
  ASSIGN
  itemfg.sell-uom   = get-sv("oe-ordl.pr-uom")
  itemfg.prod-uom   = v-uom
  itemfg.i-code     = "C"
  itemfg.stocked    = YES
  itemfg.alloc      = IF AVAIL xeb AND xeb.est-type LE 4 THEN v-allocf ELSE v-alloc.
  IF v-graphic-char NE "" THEN
  DO:
    IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
    v-graphic-char = v-graphic-char + "\".
    IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
    itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
  END.
  IF AVAIL xeb THEN DO:
    ASSIGN itemfg.die-no     = xeb.die-no
    itemfg.plate-no   = xeb.plate-no
    itemfg.style      = xeb.style
    itemfg.cad-no     = xeb.cad-no
    itemfg.upc-no     = xeb.upc-no
    itemfg.spc-no     = xeb.spc-no
    itemfg.isaset     = xeb.form-no EQ 0
    itemfg.procat     = xeb.procat
    itemfg.alloc      = xeb.set-is-assembled
    itemfg.pur-man    = xeb.pur-man.
    /*IF xeb.pur-man THEN itemfg.pur-uom = "EA".*/
    /* see task 10241105 */
    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.
    {oe/fgfreighta.i "xeb"}
    {fg/set-inks1.i itemfg xeb}
    {sys/inc/fgcascnt.i itemfg xeb}
    /* {sys/inc/updfgdim.i "xeb"} replaced with below (02211202) */
    RUN oe/updfgdim.p (INPUT ROWID(xeb), INPUT ROWID(itemfg)).
    FIND CURRENT xeb EXCLUSIVE-LOCK.
    FIND CURRENT itemfg EXCLUSIVE-LOCK.
    IF xeb.form-no EQ 0 THEN DO:
      itemfg.pur-man = NOT CAN-FIND(FIRST x-eb
      WHERE x-eb.company EQ xeb.company
      AND x-eb.est-no  EQ xeb.est-no
      AND x-eb.form-no NE 0
      AND x-eb.pur-man EQ NO).
      FOR EACH x-eb
        WHERE x-eb.company EQ xeb.company
        AND x-eb.est-no  EQ xeb.est-no
        AND x-eb.form-no NE 0
        NO-LOCK BREAK BY x-eb.form-no:
        ll-one-part = FIRST(x-eb.form-no) AND LAST(x-eb.form-no).
        LEAVE.
      END.
    /* Wade Kaldawi   3/9/16
            Ticket 13466, ll-on-part should not change itemfg.alloc */
    /*   IF ll-one-part THEN itemfg.alloc = YES. */
    END.
  END.
  ELSE IF fgmaster-cha EQ "FGITEM" THEN DO:
    FIND FIRST cust WHERE cust.company = cocode AND
    cust.active  = "X"    NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN DO:
        ASSIGN itemfg.def-loc     = shipto.loc
        itemfg.def-loc-bin = shipto.loc-bin.
      END.
    END.
  END.
  IF fgmaster-cha EQ "FGITEM" THEN DO:
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    itemfg.i-code = IF oe-ordl.est-no NE "" THEN "C"
    ELSE IF AVAIL oe-ctrl THEN
    IF oe-ctrl.i-code THEN "S"
    ELSE "C"
    ELSE "S".
  END.
  {est/fgupdtax.i oe-ord}
  ll-new-fg-created = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-default-type) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE default-type Procedure 
PROCEDURE default-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-itemfg AS ROWID NO-UNDO.
  DO : 
      /* wfk */ DEFINE BUFFER io-itemfg FOR itemfg.
      /* wfk */ DEF BUFFER DEF-oe-ordl FOR oe-ordl.
    FIND io-itemfg WHERE ROWID(io-itemfg) EQ ipr-itemfg NO-ERROR.
    IF NOT AVAIL io-itemfg THEN
        RETURN.
    IF get-sv("oe-ordl.type-code") NE "T" AND lv-add-mode THEN DO:
      set-sv("oe-ordl.type-code", "O" ).
      IF AVAIL io-itemfg THEN DO:
        IF get-sv("oe-ordl.type-code") EQ "O" AND
        CAN-FIND(FIRST def-oe-ordl
        WHERE def-oe-ordl.company EQ io-itemfg.company
        AND def-oe-ordl.i-no    EQ io-itemfg.i-no
        AND def-oe-ordl.ord-no  LT oe-ordl.ord-no
        AND ROWID(def-oe-ordl)  NE ROWID(oe-ordl)) THEN
        set-sv("oe-ordl.type-code", "R" ).
        ELSE
        IF TRIM(io-itemfg.type-code) NE ""  AND
        io-itemfg.type-code       NE "T" THEN
        set-sv("oe-ordl.type-code", io-itemfg.type-code ).
      END.
    END.
    RUN new-type.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-display-fgitem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgitem Procedure 
PROCEDURE display-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.

  DEF VAR li-cnt LIKE oe-ordl.cas-cnt NO-UNDO.
  DEF VAR li-unit LIKE oe-ordl.cases-unit NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR X AS INT NO-UNDO.
  DEF VAR li-alloc AS INT NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-price AS DEC NO-UNDO.
  DEF VAR lv-pr-uom AS CHAR NO-UNDO.
  DEF VAR v-tmp-part AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR ll-tax LIKE oe-ordl.tax NO-UNDO.
  DEF VAR lv-new-i-no LIKE oe-ordl.i-no NO-UNDO.
  DEF VAR lv-calc-qty AS DEC NO-UNDO.
  DEF VAR lv-case-qty AS INT NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  IF NOT AVAIL oe-ord THEN
    FIND oe-ord WHERE ROWID(oe-ord) EQ ipr-ord-row EXCLUSIVE-LOCK.
  IF NOT AVAIL oe-ordl THEN
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ipr-ordl-row EXCLUSIVE-LOCK.
  
  DO :
    IF NOT AVAIL xoe-ord THEN FIND xoe-ord WHERE RECID(xoe-ord) = RECID(oe-ord) NO-LOCK NO-ERROR.
    RELEASE itemfg.
    DO WHILE NOT AVAIL itemfg:
      FIND FIRST itemfg
      WHERE itemfg.company EQ g_company
      AND itemfg.i-no    EQ get-sv("oe-ordl.i-no")
      NO-LOCK NO-ERROR.
      IF NOT AVAIL itemfg THEN DO:
        IF ll-new-file THEN
        RUN custom/getobitm.p (cocode, get-sv("oe-ordl.i-no"),
        INDEX(ip-type,"Update-") EQ 0,
        OUTPUT lv-new-i-no).
        IF lv-new-i-no EQ "" OR lv-new-i-no EQ FILL("?",30) THEN RETURN ERROR.
        set-sv("oe-ordl.i-no", lv-new-i-no ).
      END.
    END.
    
    IF get-sv("oe-ordl.est-no") <> "" THEN DO:
      IF oe-ord.est-no EQ "" THEN DO:
        IF NOT AVAIL xest THEN
        FIND FIRST xest WHERE xest.company EQ cocode AND
        xest.est-no EQ  oe-ordl.est-no NO-LOCK NO-ERROR.
        IF AVAIL xest AND v-quo-price-log AND
        (ld-prev-price = 0 OR ld-prev-price = DEC(get-sv("oe-ordl.price")) )
        /* to allow user's overriding price */
        THEN DO:
          IF ll-got-qtprice THEN DO:
            /*ll-got-qtprice = no.*/
          END.
          ELSE DO:
            ASSIGN lv-price = DEC(get-sv("oe-ordl.price"))
            lv-pr-uom = get-sv("oe-ordl.pr-uom")
            v-tmp-part = get-sv("oe-ordl.i-no")
            lv-qty     = DEC(get-sv("oe-ordl.qty"))
            ll-got-qtprice = YES.
            IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
            tt-item-qty-price.tt-selected = YES AND
            (tt-item-qty-price.part-no EQ get-sv("oe-ordl.part-no") OR
            (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))) THEN
            DO:
              RUN oe/getqpric.p (RECID(xest),
              get-sv("oe-ordl.part-no"),
              v-tmp-part,
              INPUT-OUTPUT lv-price,
              INPUT-OUTPUT lv-pr-uom,
              OUTPUT lv-q-no,
              INPUT-OUTPUT lv-qty).
              set-sv("oe-ordl.qty", STRING(lv-qty) ).
            END.
            ELSE
            DO:
              FIND FIRST tt-item-qty-price WHERE
              tt-item-qty-price.tt-selected = YES AND
              (tt-item-qty-price.part-no EQ get-sv("oe-ordl.part-no") OR
              (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))
              NO-ERROR.
              IF AVAIL tt-item-qty-price THEN
              ASSIGN
              lv-price = tt-item-qty-price.price
              lv-pr-uom = tt-item-qty-price.uom
              lv-q-no = tt-item-qty-price.q-no.
            END.
            
            set-sv("oe-ordl.price", STRING(lv-price) ).
            set-sv("oe-ordl.pr-uom", lv-pr-uom ).
            
            IF get-sv("oe-ordl.est-no") NE "" AND oeestcom-log = YES THEN
            RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
            /* wfk {oe/ordltot.i oe-ordl qty oe-ordl} */
            /* end.            */
          END.
        END.     /* oe-ordl.est-no <> "" */
        RUN validate-fgitem NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
        /*find first itemfg where itemfg.company = g_company and
        itemfg.i-no = get-sv("oe-ordl.i-no")
        no-lock no-error.
        if not avail itemfg then return error.*/
        RETURN.
      END.  /* update and est-no <> "" */
      RUN validate-fgitem NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
      FIND FIRST cust
      {sys/ref/custW.i}
      AND cust.cust-no EQ oe-ord.cust-no
      USE-INDEX cust
      NO-LOCK NO-ERROR.
      ll-tax = AVAIL cust AND cust.SORT EQ "Y" AND oe-ord.tax-gr NE "" AND itemfg.taxable.
      IF NOT ll-tax THEN DO:
        {custom/shptotax.i oe-ord.cust-no oe-ord.sold-id ll-tax}
      END.
      set-sv("oe-ordl.tax", STRING(ll-tax,"Y/N") ).
      RUN default-type (BUFFER itemfg).
      
 /* {custom/fgexempt.i itemfg oe-ordl.disc:SCREEN-VALUE} */

  IF get-sv("oe-ordl.type-code") EQ "O" AND oe-ordl.est-no NE "" THEN DO:
            set-sv("oe-ordl.i-name"     , IF itemfg.i-name <> "" THEN itemfg.i-name ELSE get-sv("oe-ordl.i-name") ).
            set-sv("oe-ordl.i-no"       , IF itemfg.i-no <> "" THEN itemfg.i-no ELSE get-sv("oe-ordl.i-no") ) .
            set-sv("oe-ordl.part-dscr2" , IF itemfg.part-dscr2 <> "" THEN itemfg.part-dscr2 ELSE get-sv("oe-ordl.part-dscr2")).
  END.
  ELSE
  DO:
            set-sv("oe-ordl.i-name" , IF itemfg.i-name <> "" THEN itemfg.i-name ELSE get-sv("oe-ordl.i-name")).
            set-sv("oe-ordl.i-no"   , IF itemfg.i-no <> "" AND get-sv("oe-ordl.i-no") = "" THEN itemfg.i-no ELSE get-sv("oe-ordl.i-no")).
            set-sv("oe-ordl.price"  , IF setFromHistory THEN STRING(historyPrice) ELSE
                                          IF itemfg.sell-price <> 0 THEN STRING(itemfg.sell-price) ELSE get-sv("oe-ordl.price")).
            set-sv("oe-ordl.pr-uom" , IF setFromHistory THEN STRING(historyPrUOM) ELSE
                                          IF itemfg.sell-uom <> "" AND get-sv("oe-ordl.pr-uom") = "" THEN itemfg.sell-uom ELSE get-sv("oe-ordl.pr-uom")).
            set-sv("oe-ordl.cas-cnt" , IF itemfg.case-count <> 0 THEN STRING(itemfg.case-count) ELSE get-sv("oe-ordl.cas-cnt")).
            set-sv("oe-ordl.cases-unit" , IF itemfg.case-pall <> 0 THEN STRING(itemfg.case-pall) ELSE get-sv("oe-ordl.cases-unit")).
            set-sv("oe-ordl.part-dscr2" , IF itemfg.part-dscr2 <> "" THEN itemfg.part-dscr2 ELSE get-sv("oe-ordl.part-dscr2")).

     IF get-sv("oe-ordl.est-no") NE "" AND
        oeestcom-log = YES THEN
        RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
  END.

    END.
    IF get-sv("oe-ordl.est-no") EQ "" THEN DO:
      set-sv("oe-ordl.part-no", IF itemfg.part-no <> "" THEN itemfg.part-no ELSE get-sv("oe-ordl.part-no") ).
      set-sv("oe-ordl.part-dscr1", IF itemfg.part-dscr1 <> "" THEN itemfg.part-dscr1 ELSE get-sv("oe-ordl.part-dscr1") ).
      set-sv("oe-ordl.cas-cnt", IF itemfg.case-count <> 0 THEN STRING(itemfg.case-count) ELSE get-sv("oe-ordl.cas-cnt") ).
      set-sv("oe-ordl.cases-unit", IF itemfg.case-pall <> 0 THEN STRING(itemfg.case-pall) ELSE get-sv("oe-ordl.cases-unit") ).
      IF ll-new-file THEN DO:
        ASSIGN
        cp-part-no = ""
        cp-rowid   = ROWID(itemfg).
        RUN custom/getcpart.p (cocode, oe-ord.cust-no,
        INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
        IF cp-part-no NE "" THEN
          set-sv("oe-ordl.part-no", cp-part-no ).
      END.
    END.
    ELSE DO:
      RUN oe/oe-cnt.p(RECID(oe-ordl), OUTPUT li-cnt, OUTPUT li-unit).
      
      set-sv("oe-ordl.cas-cnt", STRING(li-cnt) ).
      set-sv("oe-ordl.cases-unit", STRING(li-unit) ).
    END.
    IF INT(get-sv("oe-ordl.qty")) GT 0 THEN DO:
      ASSIGN
      lv-calc-qty = DEC(get-sv("oe-ordl.qty"))
      lv-uom = get-sv("fi_qty-uom")
      lv-case-qty = (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE
      IF lv-uom EQ "EA" THEN 1 ELSE 1000)
      lv-calc-qty = DEC(get-sv("oe-ordl.qty")).
      IF get-sv("oe-ordl.pr-uom") NE "EA" THEN
      ASSIGN
      lv-calc-qty = lv-calc-qty * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE
      IF lv-uom EQ "EA" THEN 1 ELSE 1000).
      IF lv-calc-qty LT INT(get-sv("oe-ordl.cas-cnt")) THEN
      set-sv("oe-ordl.cas-cnt", STRING(lv-calc-qty) ).
      IF INT(get-sv("oe-ordl.cas-cnt")) EQ 0 AND get-sv("oe-ordl.i-no") NE "0" THEN DO:
        IF lv-calc-qty LE lv-case-qty THEN
        set-sv("oe-ordl.cas-cnt", STRING(lv-calc-qty) ).
        ELSE
        set-sv("oe-ordl.cas-cnt", STRING(lv-case-qty) ).
      END.
    END.
    IF INT(get-sv("oe-ordl.cases-unit")) EQ 0 THEN
    set-sv("oe-ordl.cases-unit", "1" ).
    /*ASSIGN
    set-sv("oe-ordl.cases", STRING(TRUNC(INT(oe-ordl.qty:SCREEN-VALUE) / INT(oe-ordl.cas-cnt:SCREEN-VALUE),0)) ).
    set-sv("oe-ordl.partial", STRING(INT(oe-ordl.qty:SCREEN-VALUE) MOD INT(oe-ordl.cas-cnt:SCREEN-VALUE)). */ .
    IF v-foamdate-log                                             AND
    itemfg.style NE ""                                         AND
    CAN-FIND(FIRST style WHERE style.company EQ itemfg.company
    AND style.style   EQ itemfg.style
    AND style.TYPE    EQ "F")           THEN DO:
      set-sv("oe-ordl.req-date", STRING(oe-ord.ord-date + v-foamdate-int) ).
      IF DATE(get-sv("oe-ordl.req-date"))  GT
      DATE(get-sv("oe-ordl.prom-date")) THEN
      set-sv("oe-ordl.prom-date", get-sv("oe-ordl.req-date") ).
    END.
    RUN itemfg-cost.
    /* ======= end of oe/ordlfg.i ========*/
    IF itemfg.isaset AND itemfg.t-sqft EQ 0 AND
    CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
    AND fg-set.set-no  EQ itemfg.i-no
    AND fg-set.part-no NE fg-set.set-no) THEN
    RUN fg/updsetdm.p (RECID(itemfg)).
    IF get-sv("oe-ordl.est-no") EQ "" THEN DO:
      IF v-upd-comm THEN DO:
        /* wfk
        {oe/oescomm.i get-sv("oe-ordl.s-man[1]") 1}
        {oe/oescomm.i get-sv("oe-ordl.s-man[2]") 2}
        {oe/oescomm.i get-sv("oe-ordl.s-man[3]") 3}    */
        /* Populate 2nd sales rep per NK1 value */
        FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.NAME    EQ "SALESREP" NO-ERROR.
        IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
          FIND FIRST sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company EQ cocode
          AND sys-ctrl-shipto.NAME    EQ "SALESREP"
          AND sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no
          NO-ERROR.
          IF AVAIL sys-ctrl-shipto AND get-sv("oe-ordl.s-man[1]") NE sys-ctrl-shipto.char-fld THEN DO:
            set-sv("oe-ordl.s-man[2]", sys-ctrl-shipto.char-fld ).
            set-sv("oe-ordl.s-comm[2]", STRING(sys-ctrl-shipto.dec-fld) ).
            set-sv("oe-ordl.s-pct[2]", "100.00" ).
            /*
            find sman where sman.company = oe-ordl.company
            and sman.sman = get-sv("oe-ordl.s-man[2]")
            no-lock no-error.
            set-sv("oe-ordl.s-name[2]", sman.sname ).
            */
          END.
        END.
      END.
    END.
    IF lv-add-mode THEN DO:
      /*wfk ASSIGN oe-ordl.pr-uom oe-ordl.price. */
      RUN get-price.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-display-fgpart) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgpart Procedure 
PROCEDURE display-fgpart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF INPUT PARAM ip-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-ordl-row AS ROWID NO-UNDO.

  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR li-cnt LIKE oe-ordl.cas-cnt NO-UNDO.
  DEF VAR li-unit LIKE oe-ordl.cases-unit NO-UNDO.
  DEF VAR X AS INT NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.

  FIND itemfg WHERE RECID(itemfg) = ip-recid NO-LOCK.
  IF NOT AVAIL oe-ord THEN
    FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) = ip-ord-row
                NO-ERROR.
  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.


  IF get-sv("oe-ordl.est-no") <> "" THEN RETURN.
  IF get-sv("oe-ordl.i-no") EQ "" OR get-sv("oe-ordl.i-no") EQ "0" THEN DO:

      set-sv("oe-ordl.i-no", itemfg.i-no).
      oe-ordl.i-no = itemfg.i-no.
      RELEASE oe-ordl.
  END.
  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.


  RUN validate-fgitem NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  DO :
    FIND itemfg WHERE RECID(itemfg) = ip-recid NO-LOCK.
    /* wfk {custom/fgexempt.i itemfg get-sv("oe-ordl.disc")} */
    RUN default-type (/* wfk * BUFFER itemfg */ ROWID(itemfg)).

    IF get-sv("oe-ordl.type-code") EQ "O" AND oe-ordl.est-no NE "" THEN DO:
      set-sv("oe-ordl.i-name", IF get-sv("oe-ordl.i-no") = "" THEN itemfg.i-name ELSE get-sv("oe-ordl.i-name") ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.i-no", IF get-sv("oe-ordl.i-no") = "" THEN itemfg.i-no ELSE get-sv("oe-ordl.i-no")).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.part-dscr2", itemfg.part-dscr2 ).
    END.
    ELSE
    DO:
      set-sv("oe-ordl.i-name", IF get-sv("oe-ordl.i-name") = "" THEN itemfg.i-name ELSE get-sv("oe-ordl.i-name") ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.i-no", IF get-sv("oe-ordl.i-no") = "" THEN itemfg.i-no ELSE get-sv("oe-ordl.i-no") ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.price", IF setFromHistory THEN STRING(historyPrice) ELSE STRING(itemfg.sell-price) ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.pr-uom", IF setFromHistory THEN STRING(historyPrUOM) ELSE itemfg.sell-uom ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.cas-cnt", STRING(itemfg.case-count) ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.cases-unit", STRING(itemfg.case-pall) ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.part-dscr2", itemfg.part-dscr2 ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      IF get-sv("oe-ordl.est-no") NE "" AND
      oeestcom-log = YES THEN
      RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
    END.
    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
    IF get-sv("oe-ordl.est-no") EQ "" THEN DO:
      set-sv("oe-ordl.part-no", itemfg.part-no ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      
      set-sv("oe-ordl.part-dscr1", itemfg.part-dscr1 ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
      set-sv("oe-ordl.cas-cnt", STRING(itemfg.case-count) ).
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.

      IF ll-new-file THEN DO:
        ASSIGN
        cp-part-no = ""
        cp-rowid   = ROWID(itemfg).
        
        RUN custom/getcpart.p (itemfg.company, oe-ord.cust-no,
        INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
        IF cp-part-no NE "" THEN
          set-sv("oe-ordl.part-no", cp-part-no ).
          
        FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.
        
      END.
    END.
    ELSE DO:
      RUN oe/oe-cnt.p(RECID(oe-ordl), OUTPUT li-cnt, OUTPUT li-unit).
      
      set-sv("oe-ordl.cas-cnt", STRING(li-cnt) ).
      set-sv("oe-ordl.cases-unit", STRING(li-unit) ).
    END.

    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-ordl-row EXCLUSIVE-LOCK.

    IF INT(get-sv("oe-ordl.qty")) GT 0 THEN
    IF INT(get-sv("oe-ordl.qty")) LT INT(get-sv("oe-ordl.cas-cnt")) THEN
    set-sv("oe-ordl.cas-cnt", get-sv("oe-ordl.qty") ).
    ELSE
    IF INT(get-sv("oe-ordl.cas-cnt")) EQ 0 AND get-sv("oe-ordl.i-no") NE "0" THEN
    set-sv("oe-ordl.cas-cnt", "1" ).
    IF INT(get-sv("oe-ordl.cases-unit")) EQ 0 THEN
    set-sv("oe-ordl.cases-unit", "1" ).
    /*ASSIGN
    set-sv("oe-ordl.cases", STRING(TRUNC(INT(oe-ordl.qty:SCREEN-VALUE) / INT(oe-ordl.cas-cnt:SCREEN-VALUE),0)) ).
    set-sv("oe-ordl.partial", STRING(INT(oe-ordl.qty:SCREEN-VALUE) MOD INT(oe-ordl.cas-cnt:SCREEN-VALUE)).*/ .
    set-sv("oe-ordl.job-no", "" ).
    FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
    AND po-ordl.i-no      EQ get-sv("oe-ordl.i-no")
    AND po-ordl.po-no     EQ INT(get-sv("oe-ordl.po-no-po"))
    AND po-ordl.item-type EQ NO
    USE-INDEX item-ordno NO-LOCK NO-ERROR.
    IF AVAIL po-ordl THEN DO:
        set-sv("oe-ordl.pr-uom", po-ordl.cons-uom ).
        set-sv("oe-ordl.cost", STRING(po-ordl.cons-cost) ).
        v-cost = po-ordl.cons-cost.
        set-sv("oe-ordl.pr-uom", itemfg.prod-uom ).
        set-sv("oe-ordl.cost", STRING(get-itemfg-cost(itemfg.i-no)) ).
        v-cost = get-itemfg-cost(itemfg.i-no).
    END.

    IF oe-ordl.pr-uom NE "M" THEN
    RUN sys/ref/convcuom.p(get-sv("oe-ordl.pr-uom"), "M", 0, 0, 0, 0,
    v-cost, OUTPUT lv-out-cost).
    ELSE DO:
       lv-out-cost = v-cost.
       set-sv("oe-ordl.cost", STRING(lv-out-cost) ).
       set-sv("oe-ordl.pr-uom", itemfg.sell-uom ).
    END.

    IF AVAIL po-ordl THEN
    DO:
      FIND FIRST po-ord WHERE
      po-ord.company EQ po-ordl.company AND
      po-ord.po-no EQ po-ordl.po-no
      NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
      DO:
        FIND FIRST reftable WHERE
        reftable.reftable EQ 'e-itemfg-vend.markup' AND
        reftable.company EQ po-ordl.company AND
        reftable.loc EQ po-ordl.i-no AND
        reftable.CODE EQ po-ord.vend-no
        NO-LOCK NO-ERROR.
        IF AVAIL reftable THEN
        DO:
          set-sv("oe-ordl.cost", STRING(DEC(get-sv("oe-ordl.cost")) * (1 + (reftable.val[1] / 100.0 ))) ).
          RELEASE reftable.
        END.
        RELEASE po-ord.
      END.
    END.
  END.
  /* wfk end. */  /* frame {&frame-name} */
  IF itemfg.isaset AND itemfg.t-sqft EQ 0 AND
  CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
  AND fg-set.set-no  EQ itemfg.i-no
  AND fg-set.part-no NE fg-set.set-no) THEN
  RUN fg/updsetdm.p (RECID(itemfg)).
  IF get-sv("oe-ordl.est-no") EQ "" THEN DO:
    IF v-upd-comm THEN DO:
      /* wfk
      {oe/oescomm.i get-sv("oe-ordl.s-man[1]") 1}
      {oe/oescomm.i get-sv("oe-ordl.s-man[2]") 2}
      {oe/oescomm.i get-sv("oe-ordl.s-man[3]") 3}     */
      /* Populate 2nd sales rep per NK1 value */
      FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "SALESREP" NO-ERROR.
      IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
        WHERE sys-ctrl-shipto.company EQ cocode
        AND sys-ctrl-shipto.NAME    EQ "SALESREP"
        AND sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no
        NO-ERROR.
        IF AVAIL sys-ctrl-shipto AND get-sv("oe-ordl.s-man[1]") NE sys-ctrl-shipto.char-fld THEN DO:
          set-sv("oe-ordl.s-man[2]", sys-ctrl-shipto.char-fld ).
          set-sv("oe-ordl.s-comm[2]", STRING(sys-ctrl-shipto.dec-fld) ).
          set-sv("oe-ordl.s-pct[2]", "100.00" ).
          /*
          find sman where sman.company = oe-ordl.company
          and sman.sman = get-sv("oe-ordl.s-man[2]")
          no-lock no-error.
          set-sv("oe-ordl.s-name[2]", sman.sname ).
          */
          /* END. */
          /* END. */
          /* end. */
          /* wfk end. */
          RUN get-price.
        END.
      END.
    END.
  END.
  /* wfk set-sv("oe-ordl.qty", "0" or get-sv("oe-ordl.qty") = ""). */
  /* wfk apply "entry" to oe-ordl.qty. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-display-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Procedure 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ipr-ord-row AS ROWID NO-UNDO.
DEF INPUT PARAM ipr-ordl-row AS ROWID NO-UNDO.

FIND oe-ord WHERE ROWID(oe-ord) EQ ipr-ord-row .
FIND oe-ordl WHERE ROWID(oe-ordl) EQ ipr-ordl-row .

IF AVAIL oe-ordl THEN DO:

    /* wfk
    DISPLAY oe-ordl.est-no
    oe-ordl.type-code
    oe-ordl.job-no
    oe-ordl.job-no2
    oe-ordl.qty
    fi_qty-uom
    oe-ordl.i-no
    oe-ordl.price
    oe-ordl.pr-uom
    oe-ordl.tax
    oe-ordl.part-no
    oe-ordl.disc
    oe-ordl.cas-cnt
    oe-ordl.partial
    oe-ordl.cases-unit
    oe-ordl.i-name
    oe-ordl.t-price
    oe-ordl.part-dscr1
    oe-ordl.cost
    oe-ordl.part-dscr2
    oe-ordl.req-code
    oe-ordl.req-date
    oe-ordl.po-no
    oe-ordl.e-num
    oe-ordl.po-no-po
    oe-ordl.vend-no
    oe-ordl.prom-code
    oe-ordl.prom-date
    oe-ordl.s-man[1]
    fi_sname-1
    oe-ordl.s-pct[1]
    oe-ordl.s-comm[1]
    oe-ordl.s-man[2]
    fi_sname-2
    oe-ordl.s-pct[2]
    oe-ordl.s-comm[2]
    oe-ordl.s-man[3]
    fi_sname-3
    oe-ordl.s-pct[3]
    oe-ordl.s-comm[3]
    oe-ordl.over-pct oe-ordl.under-pct
    oe-ordl.spare-char-1
    . */
    /*wfk
    IF oe-ordl.whse:HIDDEN = NO THEN
    DISPLAY oe-ordl.whse . */
    FIND itemfg WHERE itemfg.company = oe-ordl.company 
        AND itemfg.i-no = oe-ordl.i-no
        NO-LOCK.
    /* wfk - check this statement */
    IF AVAIL itemfg THEN
       set-sv("spare-dec-1", STRING(itemfg.spare-dec-1) ).

    /* wfk ASSIGN oe-ordl.spare-char-1:TOOLTIP = getOrdStatDescr(oe-ordl.spare-char-1). */
    RUN new-type.
    RUN new-s-man (0).
    RUN whs-item (0).
  END.
  /* wfk ENABLE btn_ok btn_cancel . */
  /*  wfk
  VIEW FRAME {&FRAME-NAME}.
  APPLY "entry" TO FRAME {&FRAME-NAME}.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exit-delete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-delete Procedure 
PROCEDURE exit-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER exit-oe-ordl FOR oe-ordl.
  DEF BUFFER temp-itemfg FOR itemfg.
  IF lv-item-recid NE ? THEN DO WHILE TRUE:
    FIND exit-oe-ordl WHERE RECID(exit-oe-ordl) EQ lv-item-recid
    EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL exit-oe-ordl THEN DO:
      DELETE exit-oe-ordl.
      LEAVE.
    END.
  END.
  op-cancel = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-final-steps) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-steps Procedure 
PROCEDURE final-steps :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
  
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF VAR v-pallet-cnt AS DEC NO-UNDO.
  DEF BUFFER temp-itemfg FOR itemfg.
  DEF VAR lv-job-recid AS RECID NO-UNDO.

  FIND oe-ordl WHERE ROWID(oe-ordl) EQ r-current-ordl NO-ERROR.
  IF NOT AVAIL oe-ordl THEN DO:
      MESSAGE "Error - order line not available during final steps"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  fil_id = RECID(oe-ordl).
  IF oe-ordl.est-no NE "" AND NOT AVAIL xest THEN
  FIND FIRST xest
  WHERE xest.company EQ cocode
  AND xest.est-no  EQ oe-ordl.est-no
  NO-LOCK NO-ERROR.
  IF AVAIL xest THEN DO:
    IF lv-new-tandem NE ? THEN RUN upd-new-tandem.
    ELSE
    IF ll-is-tandem THEN RUN upd-tandem.
    IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN DO:
      s-est-no = oe-ordl.est-no.
      RUN oe/fgadd2.p.   /** 2pc box fg create/update routine **/
    END.
    /*ELSE
    IF v-qty-mod AND xest.est-type GE 3 AND xest.est-type LE 4 THEN RUN oe/tancomup.p.*/
  END.
  IF AVAIL oe-ordl AND (oe-ordl.est-no NE "" AND oe-ordl.job-no EQ "") THEN
  /*message "Since job number is blank, a job will not be created "
  view-as alert-box*/.
  ELSE DO:
    IF AVAIL oe-ordl AND oe-ordl.est-no NE "" AND v-create-job THEN DO:
      FIND FIRST job WHERE job.company EQ cocode
      AND job.job-no  EQ oe-ordl.job-no
      AND job.job-no2 EQ oe-ordl.job-no2 NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN DO:
        RUN create-job (OUTPUT lv-job-recid).
        FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
      END.
    END.
    RUN oe/ordlup.p.         /* Update Inventory and Job Costing */
  END.

  IF CAN-FIND(FIRST b-oe-ordl
  WHERE b-oe-ordl.company EQ oe-ordl.company
  AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
  AND b-oe-ordl.LINE    GE 1
  AND b-oe-ordl.LINE    LT 99999999) THEN
  FOR EACH b-oe-ordl
    WHERE b-oe-ordl.company EQ oe-ordl.company
    AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
    AND (b-oe-ordl.LINE   LT 1 OR
    b-oe-ordl.LINE   GE 99999999):
    DELETE b-oe-ordl.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-final-steps2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-steps2 Procedure 
PROCEDURE final-steps2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipl-autotrans AS LOG NO-UNDO.

  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-itemfg FOR itemfg.
  DEF BUFFER temp-itemfg FOR itemfg.
  FIND oe-ordl WHERE ROWID(oe-ordl) EQ r-current-ordl NO-ERROR.
  IF NOT AVAIL oe-ordl THEN DO:
      MESSAGE "Error - order line not available during final steps"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  DEF VAR v-q-back AS INT NO-UNDO.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.

  IF oe-ordl.est-no NE "" THEN DO TRANSACTION:
    fil_id = RECID(oe-ordl).
    IF NOT v-qty-mod THEN RUN oe/job-qty.p (ROWID(oe-ordl), OUTPUT v-qty-mod).
    IF oe-ord.est-no EQ ""                                       OR
    (v-qty-mod AND (NOT ll-new-record OR lv-new-tandem NE ?)) THEN DO:
      /*03300902 need sensitive = no to display job ticket print*/
      /* wfk FRAME {&FRAME-NAME}:SENSITIVE = NO. */
      RUN oe/estupl.p.
      /* wfk FRAME {&FRAME-NAME}:SENSITIVE = YES. */
      fil_id = RECID(oe-ordl).
    END.
    IF lv-q-no NE 0 THEN DO:
     FIND CURRENT oe-ordl.
      ASSIGN oe-ordl.q-no = lv-q-no.
    END.
  END.


  DO TRANSACTION:

    IF (oe-ord.TYPE NE "T" OR ipl-autotrans)                  AND
    (lv-add-mode                                              OR
    (NOT ip-type BEGINS "update-"                             AND
    (v-qty-mod OR oe-ordl.po-no-po EQ 0 OR lv-new-tandem NE ? OR
    NOT CAN-FIND(FIRST po-ord
    WHERE po-ord.company EQ oe-ordl.company
    AND po-ord.po-no   EQ oe-ordl.po-no-po))))       THEN DO:
    fil_id = RECID(oe-ordl).

    RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
    END.
    FIND CURRENT oe-ordl.
    /* freight calc */
    RUN oe/oe-frtcl.p.  /* Calculate Freight  */
    ASSIGN
    oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000
    oe-ordl.q-qty  = v-margin.
    FIND CURRENT oe-ordl NO-LOCK.

  END.


  /* This section is needed because previous calculations of component
  quantities were based on the quantity before it was updated */
  FIND FIRST fg-set WHERE fg-set.company = cocode
  AND fg-set.SET-no  = oe-ordl.i-no NO-LOCK NO-ERROR.
  IF AVAIL fg-set THEN DO:
    FOR EACH fg-set WHERE fg-set.company = cocode
      AND fg-set.SET-no  = oe-ordl.i-no NO-LOCK:
      FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-ordl.company
      AND bf-oe-ordl.ord-no  EQ oe-ordl.ord-no
      AND bf-oe-ordl.i-no    EQ fg-set.part-no
      NO-LOCK NO-ERROR.
      IF AVAIL bf-oe-ordl THEN
      FIND FIRST bf-itemfg WHERE bf-itemfg.company EQ cocode
      AND bf-itemfg.i-no    EQ fg-set.part-no
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL bf-oe-ordl AND avail(bf-itemfg) AND xoe-ord.TYPE NE "T" THEN
      RUN fg/calcqa&b.p (ROWID(bf-itemfg), OUTPUT bf-itemfg.q-alloc, OUTPUT v-q-back).
      IF AVAIL(bf-itemfg) THEN DO:          
          RUN fg/chkfgloc.p (INPUT bf-itemfg.i-no, INPUT xoe-ord.loc).
          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ bf-itemfg.company
                AND itemfg-loc.i-no    EQ bf-itemfg.i-no
                AND itemfg-loc.loc     EQ xoe-ord.loc
              EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF AVAIL(itemfg-loc) AND AVAIL bf-oe-ordl AND avail(bf-itemfg) AND xoe-ord.TYPE NE "T" THEN
        RUN fg/calcqabl.p (ROWID(bf-itemfg), xoe-ord.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).

      IF AVAIL(bf-itemfg) AND bf-itemfg.q-alloc LT 0 THEN DO:
          bf-itemfg.q-alloc = 0.
          IF AVAIL itemfg-loc THEN
              itemfg-loc.q-alloc = 0.
      END.
      IF AVAIL itemfg-loc THEN
        itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.
  END.
  fil_id = RECID(oe-ordl).
  RUN oe/ordlup.p.         /* Update Inventory and Job Costing */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-eb-info) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb-info Procedure 
PROCEDURE get-eb-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF get-sv("oe-ordl.est-no")  <> "" THEN DO:
    FIND FIRST eb WHERE eb.company = cocode AND
    eb.est-no = get-sv("oe-ordl.est-no")
    AND eb.cust-no = oe-ord.cust-no
    AND ((eb.est-type = 1 AND eb.form-no <> 0) OR
    (eb.est-type = 2 AND eb.form-no = 0) OR
    (eb.est-type = 5 AND eb.form-no <> 0) OR
    (eb.est-type = 6 AND eb.form-no = 0) )
    NO-LOCK NO-ERROR.
    IF AVAIL eb THEN ls-stock = eb.stock-no.
  END.
  ELSE ls-stock = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-est-comm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-est-comm Procedure 
PROCEDURE get-est-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
  DEF VAR v-tmp-price-2 AS DEC NO-UNDO.
  DEF VAR v-price-per-1000 AS DEC NO-UNDO.
  DEF VAR v-qty-value      AS INT NO-UNDO.
  DEF VAR v-est-no-value   AS CHAR NO-UNDO.
  DEF VAR v-pr-uom-value   AS CHAR NO-UNDO.
  DEF VAR v-i-no-value     AS CHAR NO-UNDO.
  DEF VAR v-cas-cnt-value  LIKE oe-ordl.cas-cnt NO-UNDO.
  DEF VAR v-price-value    AS DEC NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) = ip-ordl-rowid NO-ERROR.
  IF NOT AVAIL bf-oe-ordl THEN
  RETURN NO-APPLY.
  DO :
    IF ip-on-screen THEN
    ASSIGN v-qty-value      = INT(get-sv("oe-ordl.qty"))
    v-est-no-value   = get-sv("oe-ordl.est-no")
    v-pr-uom-value   = get-sv("oe-ordl.pr-uom")
    v-i-no-value     = get-sv("oe-ordl.i-no")
    v-cas-cnt-value  = DEC(get-sv("oe-ordl.cas-cnt"))
    v-price-value    = DEC(get-sv("oe-ordl.price")).
    ELSE
    ASSIGN v-qty-value      = bf-oe-ordl.qty
    v-est-no-value   = bf-oe-ordl.est-no
    v-pr-uom-value   = bf-oe-ordl.pr-uom
    v-i-no-value     = bf-oe-ordl.i-no
    v-cas-cnt-value  = bf-oe-ordl.cas-cnt
    v-price-value    = bf-oe-ordl.price.
    IF oeestcom-log = YES AND v-qty-value NE 0 THEN
    DO:
      FIND FIRST eb WHERE
      eb.company EQ cocode AND
      eb.est-no EQ v-est-no-value AND
      ((eb.est-type = 1 AND eb.form-no <> 0) OR
      (eb.est-type = 2 AND eb.form-no = 0) OR
      (eb.est-type = 4 AND eb.form-no <> 0) OR
      (eb.est-type = 5 AND eb.form-no <> 0) OR
      (eb.est-type = 6 AND eb.form-no = 0) OR
      (eb.est-type = 8 AND eb.form-no <> 0))
      NO-LOCK NO-ERROR.
      IF AVAIL eb THEN
      DO:
        FIND FIRST sman WHERE
        sman.company EQ eb.company AND
        sman.sman EQ eb.sman
        NO-LOCK NO-ERROR.
        IF AVAIL sman AND sman.commbasis EQ "M" THEN
        DO:
          IF v-pr-uom-value NE "M" THEN
          DO:
            FIND FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no EQ v-i-no-value
            NO-LOCK NO-ERROR.
            ASSIGN
            v-tmp-price-2 = IF v-pr-uom-value BEGINS "L" AND
            v-pr-uom-value NE "LB" THEN
            IF DEC(v-qty-value) LT 0 THEN -1
            ELSE 1
            ELSE
            IF v-pr-uom-value EQ "CS" OR v-pr-uom-value EQ "PLT" THEN
            DEC(v-qty-value) / (IF v-cas-cnt-value NE 0 THEN v-cas-cnt-value ELSE
            IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
            itemfg.case-count ELSE 1)
            ELSE
            IF v-pr-uom-value EQ "C" THEN
            DEC(v-qty-value) / 100
            ELSE
            DEC(v-qty-value)
            v-tmp-price-2 = v-tmp-price-2 * v-price-value
            v-price-per-1000 = v-tmp-price-2 / ( DEC(v-qty-value) / 1000).
          END.
          ELSE
          v-price-per-1000 = v-price-value.
          RELEASE itemfg.
          /*not combo est*/
          IF NOT (eb.est-type EQ 4 OR eb.est-type EQ 8) THEN
          DO:
            FOR EACH probe WHERE
              probe.company = eb.company AND
              probe.est-no = eb.est-no AND
              probe.probe-date NE ? AND
              probe.est-qty EQ INT(v-qty-value)
              NO-LOCK
              BY probe.probe-date DESC
              BY probe.probe-time DESC:
              IF probe.sell-price EQ v-price-per-1000 OR
              ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN
              LEAVE.
            END.
            IF NOT AVAIL probe THEN
            FOR EACH probe WHERE
              probe.company = eb.company AND
              probe.est-no = eb.est-no AND
              probe.probe-date NE ? AND
              probe.est-qty EQ INT(v-qty-value)
              NO-LOCK
              BY probe.probe-date DESC
              BY probe.probe-time DESC:
              LEAVE.
            END.
          END.
          ELSE
          DO:
            FOR EACH probe WHERE
              probe.company = eb.company AND
              probe.est-no = eb.est-no AND
              probe.probe-date NE ?
              NO-LOCK
              BY probe.probe-date DESC
              BY probe.probe-time DESC:
              IF probe.sell-price EQ v-price-per-1000 OR
              ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN
              LEAVE.
            END.
            IF NOT AVAIL probe THEN
            FOR EACH probe WHERE
              probe.company = eb.company AND
              probe.est-no = eb.est-no AND
              probe.probe-date NE ?
              NO-LOCK
              BY probe.probe-date DESC
              BY probe.probe-time DESC:
              LEAVE.
            END.
          END.
          IF AVAIL probe THEN
          DO:
            FIND oe-ord OF oe-ordl.
            IF ip-on-screen THEN
            set-sv("oe-ordl.s-comm[1]", STRING(probe.comm) ).
            ELSE
            ASSIGN bf-oe-ordl.s-comm[1] = probe.comm.
            ASSIGN
            oe-ord.s-comm[1] = probe.comm
            oe-ord.t-fuel = probe.market-price
            v-margin = probe.market-price.
            FIND oe-ord OF oe-ordl NO-LOCK.
          END.
          RELEASE probe.
          RELEASE sman.
        END.
        RELEASE eb.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-est-cost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-est-cost Procedure 
PROCEDURE get-est-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-est-no AS CHAR NO-UNDO.
  DEF VAR v-run-list AS CHAR NO-UNDO.
  IF ip-est-no NE "" AND NOT AVAIL xest THEN
  FIND FIRST xest WHERE xest.company EQ cocode AND
  xest.est-no EQ ip-est-no NO-LOCK NO-ERROR.
  IF AVAIL xest THEN DO :
    FIND FIRST xeb
    WHERE xeb.company = g_company
    AND xeb.est-no = ip-est-no
    AND xeb.part-no = get-sv("oe-ordl.part-no")
    NO-LOCK NO-ERROR.
    IF AVAIL xeb THEN
    FIND FIRST xef
    WHERE xef.company = g_company
    AND xef.est-no = ip-est-no
    AND (xef.form-no = xeb.form-no OR xeb.form-no = 0)
    NO-LOCK NO-ERROR.
    ASSIGN
    v-run-list = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
    "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
    "cec/tan/print4.p,cec/com/print4.p".
    
    qty = INT(get-sv("oe-ordl.qty")).
    v-shared-rel = v-rel.
    IF AVAIL xeb AND AVAIL xef                                     AND
    xest.est-type NE 3                                          AND
    xest.est-type NE 4                                          AND
    xest.est-type NE 8                                          AND
    (oe-ordl.qty NE qty OR DEC(get-sv("oe-ordl.cost")) EQ 0) THEN DO:
      RUN VALUE(ENTRY(xest.est-type,v-run-list)).
      set-sv("oe-ordl.cost", STRING((IF v-full-cost THEN tt-tot ELSE ord-cost) /
      INT(get-sv("oe-ordl.qty")) / 1000)).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-next-workday) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-workday Procedure 
PROCEDURE get-next-workday :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM ip-date AS DATE.
  DEF INPUT PARAM ip-next-prev AS cha . /* next or prev */
  DEF VAR is-workday AS LOG NO-UNDO.
  IF WEEKDAY(ip-date) = 1 OR WEEKDAY(ip-date) = 7 THEN DO:
    is-workday = NO.
    DO WHILE NOT is-workday:
      IF ip-next-prev = "Next" THEN ip-date = ip-date + 1.
      ELSE IF ip-next-prev = "PREV" THEN ip-date = ip-date - 1.
      IF WEEKDAY(ip-date) = 1 OR WEEKDAY(ip-date) = 7 THEN NEXT.
      ELSE is-workday = YES.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-num-of-hol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-of-hol Procedure 
PROCEDURE get-num-of-hol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-date-from AS DATE  NO-UNDO.
  DEF INPUT PARAM ip-date-to AS DATE NO-UNDO.
  DEF OUTPUT PARAM op-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  lv-chk-date = ip-date-from.
  op-num-of-wkend = 0.
  DO i = 1 TO ip-date-to - ip-date-from + 1:
    IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7
    THEN op-num-of-wkend = op-num-of-wkend + 1.
    lv-chk-date = lv-chk-date + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-price) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-price Procedure 
PROCEDURE get-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-price-ent LIKE price-ent NO-UNDO.
  DO :
    IF NOT price-ent                           AND
    AVAIL oe-ordl                           AND
    TRIM(get-sv("oe-ordl.est-no")) EQ "" THEN DO:
      lv-price-ent = price-ent.
      IF NOT lv-add-mode THEN price-ent = YES.
      IF NOT AVAIL xoe-ord THEN
      FIND FIRST xoe-ord
      WHERE xoe-ord.company EQ g_company
      AND xoe-ord.ord-no  EQ oe-ordl.ord-no
      NO-LOCK NO-ERROR.
      ASSIGN
      save_id   = RECID(oe-ordl)
      lv-rowid  = ROWID(oe-ordl)
      v-i-item  = get-sv("oe-ordl.i-no")
      v-i-qty   = INT(get-sv("oe-ordl.qty"))
      v-qty-mod = NO /* oe-ordl.qty*/ /* wfk :MODIFIED */.

      FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ v-i-item
      NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        RUN oe/oe-price.p.
        FIND oe-ordl WHERE ROWID(oe-ordl) EQ lv-rowid NO-ERROR.
        /*wfk  DISPLAY oe-ordl.price oe-ordl.pr-uom oe-ordl.t-price. */
        /* wfk {oe/ordltot.i oe-ordl qty oe-ordl} */
      END.
      price-ent = lv-price-ent.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-price-hidden) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-price-hidden Procedure 
PROCEDURE get-price-hidden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-price-ent LIKE price-ent NO-UNDO.
  DEF VAR lv-save-xoe-ordl AS ROWID NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ip-ordl-rowid NO-ERROR.
  IF NOT AVAIL bf-oe-ordl THEN
  RETURN NO-APPLY.
  DO :
    IF NOT price-ent                           AND
    AVAIL bf-oe-ordl                           AND
    TRIM(bf-oe-ordl.est-no) EQ "" THEN DO:
      lv-price-ent = price-ent.
      IF NOT lv-add-mode THEN price-ent = YES.
      IF NOT AVAIL xoe-ord THEN
      FIND FIRST xoe-ord
      WHERE xoe-ord.company EQ g_company
      AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
      NO-LOCK NO-ERROR.
      ASSIGN
      save_id   = RECID(bf-oe-ordl)
      lv-rowid  = ROWID(bf-oe-ordl)
      v-i-item  = bf-oe-ordl.i-no
      v-i-qty   = INT(bf-oe-ordl.qty)
      v-qty-mod = YES. /* new record, so will have been modified */
      FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ v-i-item
      NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        IF AVAIL xoe-ordl THEN
        lv-save-xoe-ordl= ROWID(xoe-ordl).
        /* Depends on xoeitem */
        RUN oe/oe-price.p.
        FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lv-rowid NO-ERROR.
        {oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl}
        IF lv-save-xoe-ordl NE ? THEN
        FIND xoe-ordl WHERE ROWID(xoe-ordl) = lv-save-xoe-ordl
        EXCLUSIVE-LOCK NO-ERROR.
      END.
      price-ent = lv-price-ent.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-valid-uom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-valid-uom Procedure 
PROCEDURE get-valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS CHAR NO-UNDO.
  DEF INPUT PARAM db-field AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  lv-valid-uom = "".
  IF db-field EQ "fi_qty-uom" THEN DO:
    DO li = 1 TO NUM-ENTRIES(lv-uom-list):
      IF ENTRY(li,lv-uom-list) NE "L" THEN
      lv-valid-uom = lv-valid-uom + TRIM(ENTRY(li,lv-uom-list)) + ",".
    END.
    IF lv-valid-uom NE ""                                 AND
    SUBSTR(lv-valid-uom,LENGTH(lv-valid-uom),1) EQ "," THEN
    SUBSTR(lv-valid-uom,LENGTH(lv-valid-uom),1) = "".
  END.
  IF db-field EQ "pr-uom" THEN DO:
    DO li = 1 TO NUM-ENTRIES(lv-uom-list):
      IF ENTRY(li,lv-uom-list) NE "PLT" THEN
      lv-valid-uom = lv-valid-uom + TRIM(ENTRY(li,lv-uom-list)) + ",".
    END.
    IF lv-valid-uom NE ""                                 AND
    SUBSTR(lv-valid-uom,LENGTH(lv-valid-uom),1) EQ "," THEN
    SUBSTR(lv-valid-uom,LENGTH(lv-valid-uom),1) = "".
  END.
  IF lv-valid-uom EQ "" THEN lv-valid-uom = lv-uom-list.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQtyPrice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQtyPrice Procedure 
PROCEDURE getQtyPrice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipOeordlRowid AS ROWID NO-UNDO.
  FIND tt-qty-price WHERE tt-qty-price.oeordl-ROWID EQ ipOeordlRowid
  NO-ERROR.
  IF AVAIL tt-qty-price THEN
  ASSIGN historyQty = tt-historyQty
  historyPrice = tt-historyPrice
  historyPrUOM = tt-historyPrUOM
  setFromHistory = tt-setFromHistory.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-itemfg-cost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemfg-cost Procedure 
PROCEDURE itemfg-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
  /* wfk - refactor
  DO :
  set-sv("oe-ordl.job-no", "").
  v-cost = DEC(get-sv("oe-ordl.cost")).
  find first itemfg
  where itemfg.company = g_company
  and itemfg.i-no = get-sv("oe-ordl.i-no")
  NO-LOCK NO-ERROR.
  find first po-ordl where po-ordl.company   eq cocode
  and po-ordl.i-no      eq get-sv("oe-ordl.i-no")
  and po-ordl.po-no     eq int(get-sv("oe-ordl.po-no-po"))
  and po-ordl.item-type eq no
  use-index item-ordno no-lock no-error.
  if AVAIL po-ordl AND int(get-sv("oe-ordl.po-no-po")) NE 0 then
  set-sv("oe-ordl.pr-uom", IF oe-ordl.pr-uom:SCREEN-VALUE = "" THEN po-ordl.cons-uom ELSE oe-ordl.pr-uom:SCREEN-VALUE ).
  ASSIGN
  lv-uom                      = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
  v-cost   = po-ordl.cons-cost.
  set-sv("oe-ordl.cost", STRING(po-ordl.cons-cost) ).
  else
  IF AVAIL itemfg THEN
  set-sv("oe-ordl.pr-uom", if itemfg.prod-uom NE "" AND oe-ordl.pr-uom:SCREEN-VALUE = "" then itemfg.prod-uom else oe-ordl.pr-uom:screen-value ).
  lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
  v-cost   = get-itemfg-cost(itemfg.i-no)
  set-sv("oe-ordl.cost", STRING(get-itemfg-cost(itemfg.i-no)) ).
  if lv-uom ne "M" THEN do:
  run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
  v-cost, output lv-cost).
  set-sv("oe-ordl.cost", string(lv-cost) ).
  set-sv("/*oe-ordl.pr-uom", itemfg.sell-uom ?? */  ).
  end.
  if AVAIL po-ordl AND int(get-sv("oe-ordl.po-no-po")) NE 0 then
  DO:
  FIND FIRST po-ord WHERE
  po-ord.company EQ po-ordl.company AND
  po-ord.po-no EQ po-ordl.po-no
  NO-LOCK NO-ERROR.
  IF AVAIL po-ord THEN
  DO:
  FIND FIRST reftable WHERE
  reftable.reftable EQ 'e-itemfg-vend.markup' AND
  reftable.company EQ po-ordl.company AND
  reftable.loc EQ po-ordl.i-no AND
  reftable.code EQ po-ord.vend-no
  NO-LOCK NO-ERROR.
  IF AVAIL reftable THEN
  DO:
  set-sv("oe-ordl.cost", STRING(DEC(oe-ordl.cost:SCREEN-VALUE) * (1 + (reftable.val[1]/ 100.0 ))) ).
  RELEASE reftable.
  END.
  RELEASE po-ord.
  END.
  END.
  end.
  END.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-leave-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-qty Procedure 
PROCEDURE leave-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-est-no AS CHAR.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-price AS DEC NO-UNDO.
  DEF VAR lv-pr-uom AS cha NO-UNDO.
  DEF VAR v-tmp-part AS cha NO-UNDO.
  DEF VAR v-set AS cha NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-checkset AS LOG NO-UNDO.
  /* wfk
  DO :
  if (ll-help-ran AND lv-help-qty = int(get-sv("oe-ordl.qty"))) then do:
  if ll-help-ran THEN ASSIGN ll-help-ran = no
  lv-help-qty = 0
  ll-qty-leave-done = YES.
  return.
  end.
  lv-est-no = FILL(" ",8 - LENGTH(get-sv("TRIM(oe-ordl.est-no")))) +
  TRIM(get-sv("oe-ordl.est-no")).
  set-sv("oe-ordl.qty", NO ).
  IF li-prev-qty <> INT(get-sv("oe-ordl.qty")) OR
  ip-type EQ "update-2" OR ip-type EQ "add"    THEN DO:
  set-sv("oe-ordl.qty", FILL(" ",8 - LENGTH(TRIM(oe-ordl.qty:SCREEN-VALUE))) + TRIM(oe-ordl.qty:SCREEN-VALUE) ).
  li-prev-qty = oe-ordl.qty.
  IF INT(get-sv("oe-ordl.qty")) GT 0 THEN
  IF INT(get-sv("oe-ordl.qty")) LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
  set-sv("oe-ordl.cas-cnt", oe-ordl.qty:SCREEN-VALUE ).
  ELSE
  IF INT(get-sv("oe-ordl.cas-cnt")) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN
  set-sv("oe-ordl.cas-cnt", "1" ).
  set-sv("oe-ordl.i-no", "" or oe-ordl.i-no:screen-value = "0" then return. ).
  RUN get-price.
  if lv-est-no ne "" and not avail xest then
  find first xest where xest.company eq cocode and
  xest.est-no eq lv-est-no no-lock no-error.
  RUN get-est-cost (lv-est-no).
  if avail xest and v-quo-price-log AND NOT ll-got-qtprice then do:
  assign lv-price = dec(get-sv("oe-ordl.price"))
  lv-pr-uom = get-sv("oe-ordl.pr-uom")
  lv-qty    = DEC(get-sv("oe-ordl.qty"))
  v-tmp-part = get-sv("oe-ordl.i-no").
  RUN check-quote (v-tmp-part) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR .
  ll-got-qtprice = yes.
  IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
  tt-item-qty-price.tt-selected = YES AND
  (tt-item-qty-price.part-no EQ get-sv("oe-ordl.part-no") OR
  (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))) THEN
  DO:
  run oe/getqpric.p (recid(xest), get-sv("oe-ordl.part-no"),
  v-tmp-part,
  input-output lv-price,
  input-output lv-pr-uom,
  OUTPUT lv-q-no,
  INPUT-OUTPUT lv-qty).
  set-sv("oe-ordl.qty", STRING(lv-qty) ).
  END.
  ELSE
  DO:
  FIND FIRST tt-item-qty-price WHERE
  tt-item-qty-price.tt-selected = YES AND
  (tt-item-qty-price.part-no EQ get-sv("oe-ordl.part-no") OR
  (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ "")).
  ASSIGN
  lv-price = tt-item-qty-price.price
  lv-pr-uom = tt-item-qty-price.uom
  lv-q-no = tt-item-qty-price.q-no.
  IF llGotLowerPrice THEN DO:
  llGotLowerPrice = NO.
  DELETE tt-item-qty-price.
  END.
  END.
  assign
  set-sv("oe-ordl.price", string(lv-price) ).
  set-sv("oe-ordl.pr-uom", string(lv-pr-uom) ).
  IF get-sv("oe-ordl.est-no") NE "" AND
  oeestcom-log = YES THEN
  RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
  end.
  /* Begin Calculate Weight for Order */
  /* take old qty - new qty find weight and add to order */
  find first itemfg where itemfg.company eq cocode
  and itemfg.i-no eq get-sv("oe-ordl.i-no") no-lock no-error.
  if avail itemfg then do:
  if itemfg.isaset and itemfg.alloc NE YES and v-checkset then do:
  assign v-set = get-sv("oe-ordl.i-no")
  v-qty = int(get-sv("oe-ordl.qty")).
  run fg/checkset.p (recid(itemfg), ?, input-output v-qty).
  if v-qty lt int(get-sv("oe-ordl.qty")) then do:
  pause.
  hide frame checkset no-pause.
  end.
  end.
  end.
  run oe/oe-frtcl.p.  /* Calculate Freight  */
  {oe/ordltot.i oe-ordl qty oe-ordl}
  END.
  END.
  ASSIGN ll-help-ran = NO
  lv-help-qty = 0
  ll-qty-leave-done = YES .
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LEAVE_est_no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LEAVE_est_no Procedure 
PROCEDURE LEAVE_est_no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                  IF get-sv("oe-ordl.est-no") <> "" THEN DO:
                    v-est-no = get-sv("oe-ordl.est-no").
                    RUN util/rjust.p (INPUT-OUTPUT v-est-no,8).
                    set-sv("oe-ordl.est-no", v-est-no ).
                    FIND FIRST est WHERE est.company = oe-ordl.company
                    AND est.est-no = get-sv("oe-ordl.est-no")
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL est THEN DO:
                      MESSAGE "Invalid Estimate#. Try help." VIEW-AS ALERT-BOX ERROR.
                      RETURN NO-APPLY.
                    END.
                    ELSE DO:
                      /* check eb.stock# if FGITEM = "HOLD" */
                      IF v-est-fg1 = "Hold" THEN DO:
                        FIND FIRST eb WHERE eb.company = cocode AND
                        eb.est-no = get-sv("oe-ordl.est-no") AND
                        eb.stock-no = ""
                        NO-LOCK NO-ERROR.
                        IF AVAIL eb THEN DO:
                          MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                          VIEW-AS ALERT-BOX ERROR.
                          /* wfk  APPLY "ENTRY" TO OE-ORDL.EST-NO. */
                          RETURN NO-APPLY.
                        END.
                      END.
                      FIND FIRST eb WHERE eb.company = cocode AND
                      eb.est-no = get-sv("oe-ordl.est-no")
                      AND eb.cust-no = oe-ord.cust-no
                      AND ((eb.est-type = 1 AND eb.form-no <> 0) OR
                      (eb.est-type = 2 AND eb.form-no = 0) OR
                      (eb.est-type = 5 AND eb.form-no <> 0) OR
                      (eb.est-type = 6 AND eb.form-no = 0) )
                      NO-LOCK NO-ERROR.
                      IF AVAIL eb AND (
                      (eb.est-type = oe-ord.est-type) OR
                      ((eb.est-type <= 2 OR eb.est-type >= 5 ) AND
                      oe-ord.est-type = 0) )
                      THEN DO:
                        RUN display-est-detail (RECID(eb)).
                        DISABLE oe-ordl.part-no.
                      END.
                      ELSE DO:
                        MESSAGE "Estimate number: " SELF:SCREEN-VALUE "is either a Combo or Tandem"
                        "Estimate or Does not exist for this customer."
                          "Try Help please.  "
                          VIEW-AS ALERT-BOX ERROR .
                          RETURN NO-APPLY.
                        END.
                      END.
                      IF get-sv("oe-ordl.est-no") GT "" AND runship-char EQ "RUN&SHIP Prompt" THEN
                      set-sv("asi.oe-ordl.whsed", "YES" ).
                    END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LEAVE_i_no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LEAVE_i_no Procedure 
PROCEDURE LEAVE_i_no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
          DEF VAR ls-i-no AS cha NO-UNDO.
          DEF VAR ls-part-no AS cha NO-UNDO.
          DEF VAR ls-est-no AS cha NO-UNDO.
          DEF VAR ls-uom AS cha NO-UNDO.
          DEF VAR ll-secure AS LOG NO-UNDO.
          IF /*self:modified and*/ SELF:SCREEN-VALUE <> "0" AND NOT ll-ok-i-no /* done in leave trigger */
          THEN DO:
            RUN display-fgitem NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            IF CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ g_company
            AND itemfg.i-no    EQ get-sv("oe-ordl.i-no")) THEN RETURN NO-APPLY.
            ELSE DO:
              /*  message "This item does not exist, would you like to add it?" view-as alert-box question
              button yes-no update ll-ans as log.
              if ll-ans then do:
              */
              ASSIGN ls-i-no = get-sv("oe-ordl.i-no")
              ls-part-no = get-sv("oe-ordl.part-no")
              ls-est-no = get-sv("oe-ordl.est-no")
              ls-uom = get-sv("oe-ordl.pr-uom").
              RUN default-type (BUFFER itemfg).
              /* need to check security */
              IF oe-ord.est-no = "" AND get-sv("oe-ordl.est-no") = "" THEN DO:
                RUN sys/ref/d-passwd.w (4, OUTPUT ll-secure).
                IF NOT ll-secure THEN RETURN NO-APPLY.
              END.
              RUN oe/d-citmfg.w (ls-est-no, INPUT-OUTPUT ls-i-no,
              INPUT-OUTPUT ls-part-no,INPUT-OUTPUT ls-uom) NO-ERROR.
              IF ls-i-no = "" THEN DO:
                /* wfk apply "entry" to oe-ordl.i-no. */
                RETURN NO-APPLY.  /* cancel */
              END.
              ELSE DO:
                set-sv("oe-ordl.i-no", ls-i-no ).
                set-sv("oe-ordl.part-no", ls-part-no ).
                set-sv("oe-ordl.pr-uom", ls-uom ).
                FIND FIRST xest WHERE xest.company = g_company
                AND xest.est-no = FILL(" ",8 - LENGTH(TRIM(get-sv("oe-ordl.est-no")))) +
                TRIM(get-sv("oe-ordl.est-no"))
                NO-LOCK NO-ERROR.
                IF AVAIL xest THEN DO:
                  FIND xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                  AND xeb.form-no = 0 NO-LOCK NO-ERROR.
                  IF NOT AVAIL xeb THEN FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                  AND xeb.form-no = oe-ordl.form-no
                  AND xeb.blank-no = oe-ordl.blank-no
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL xeb THEN
                  FIND FIRST xeb
                  WHERE xeb.company EQ g_company
                  AND xeb.est-no  EQ xest.est-no
                  AND xeb.part-no EQ ls-part-no
                  NO-LOCK NO-ERROR.
                  IF AVAIL xeb THEN DO:
                    FIND xef WHERE xef.company = g_company AND xef.est-no = xeb.est-no
                    AND xef.form-no = xeb.form-no
                    NO-LOCK NO-ERROR.
                    RUN crt-itemfg (get-sv(SELF:NAME),get-sv("oe-ordl.pr-uom")). /*(self:screen-value,"M")*/
                  END.
                END.
                ELSE .  /* no xest or oe-ordl.est-no = "" */
                RUN crt-itemfg (SELF:SCREEN-VALUE,get-sv("oe-ordl.pr-uom")).  /*(self:screen-value,"M")*/
              END.
              RUN display-fgitem NO-ERROR.
              IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END. /* error and no itemfg */
            ll-ok-i-no = YES.
            IF oescreen-cha NE "item-qty" THEN DO:
              /* wfk apply "entry" to oe-ordl.price. */
              RETURN NO-APPLY.
            END.
          END. /* modified */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LEAVE_part_no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LEAVE_part_no Procedure 
PROCEDURE LEAVE_part_no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        IF ll-new-file THEN DO:
            ASSIGN
            cp-part-no = get-sv("oe-ordl.part-no")
            cp-rowid   = ?.
            RUN custom/getcpart.p (cocode, oe-ord.cust-no,
            INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
            FIND itemfg WHERE ROWID(itemfg) EQ cp-rowid NO-LOCK NO-ERROR.
          END.
          IF NOT AVAIL itemfg THEN
          FIND FIRST itemfg WHERE itemfg.company = g_company
          AND itemfg.part-no = get-sv("oe-ordl.part-no")
          AND itemfg.cust-no = oe-ord.cust-no
          NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN DO:
            FIND FIRST itemfg WHERE itemfg.company = g_company
            AND itemfg.part-no = get-sv("oe-ordl.part-no")
            NO-LOCK NO-ERROR.
            IF NOT AVAIL itemfg THEN DO:
              MESSAGE "Invalid Cust Part#. Try help. " VIEW-AS ALERT-BOX.
              RETURN NO-APPLY.
            END.
            ELSE DO:
              FIND FIRST cust WHERE cust.company = oe-ord.company
              AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
              IF itemfg.cust-no NE oe-ord.cust-no AND itemfg.cust-no NE "" AND
              AVAIL cust AND cust.active NE "X"                         THEN DO:
                FIND FIRST cust WHERE cust.company = g_company AND
                cust.cust-no = itemfg.cust-no
                NO-LOCK NO-ERROR.
                IF AVAIL cust AND cust.active NE "X" THEN DO:
                  MESSAGE "This item exists for a different customer!. Do you want to continue?"
                      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                      IF NOT ll-ans THEN  RETURN NO-APPLY.
                    END.
                  END.
                END.
              END.
              /* wfk
              set-sv("oe-ordl.i-no") =  "" OR oe-ordl.i-no:SCREEN-VALUE = "0" ).
              set-sv("oe-ordl.i-no", itemfg.i-no ). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LEAVE_po_no_po) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LEAVE_po_no_po Procedure 
PROCEDURE LEAVE_po_no_po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
             FIND FIRST po-ord WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ  oe-ordl.po-no-po
              NO-LOCK NO-ERROR.
              IF NOT AVAIL po-ord THEN DO:
                MESSAGE "You have entered an invalid Purchase Order."
                VIEW-AS ALERT-BOX ERROR .
                RETURN NO-APPLY.
              END.
              IF oe-ordl.job-no NE "" THEN DO:
                FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ get-sv("oe-ordl.job-no")
                AND job-hdr.job-no2 EQ INT(get-sv("oe-ordl.job-no2"))
                NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN
                FOR EACH job-mat WHERE job-mat.company EQ cocode
                  AND job-mat.job-no  EQ job-hdr.job-no
                  AND job-mat.job-no2 EQ job-hdr.job-no2
                  AND job-mat.job EQ job-hdr.job  NO-LOCK,
                  FIRST ITEM WHERE ITEM.company  EQ cocode
                  AND ITEM.i-no     EQ job-mat.rm-i-no
                  AND ITEM.mat-type EQ "B" NO-LOCK:
                  FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                  AND po-ordl.i-no      EQ job-mat.rm-i-no
                  AND po-ordl.po-no     EQ po-ord.po-no
                  AND po-ordl.item-type EQ YES
                  USE-INDEX item-ordno NO-LOCK NO-ERROR.
                  IF NOT AVAIL po-ordl THEN DO:
                    MESSAGE "You have entered an invalid Purchase Order for this Item."
                      VIEW-AS ALERT-BOX ERROR.
                      RETURN NO-APPLY.
                    END.
                  END. /* for each job-mat */
                END. /* job-no ne "" */
                ELSE DO:
                  FIND FIRST po-ordl  WHERE po-ordl.company   EQ cocode
                  AND po-ordl.i-no      EQ get-sv("oe-ordl.i-no")
                  AND po-ordl.po-no     EQ po-ord.po-no
                  AND po-ordl.item-type EQ NO
                  USE-INDEX item-ordno NO-LOCK NO-ERROR.
                  IF NOT AVAIL po-ordl THEN DO:
                    MESSAGE "You have entered an invalid Purchase Order for this Item."
                      VIEW-AS ALERT-BOX ERROR.
                      RETURN NO-APPLY.
                    END.
                    IF po-ordl.cons-uom EQ "M" THEN
                    set-sv("oe-ordl.cost", STRING(po-ordl.cons-cost) ).
                    ELSE DO:
                      /* wfk */ DEF VAR ld-cost AS DEC.
                      RUN sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                      po-ordl.cons-cost, OUTPUT ld-cost).
                      set-sv("oe-ordl.cost", STRING(ld-cost) ).
                    END.
                    FIND FIRST reftable WHERE
                    reftable.reftable EQ 'e-itemfg-vend.markup' AND
                    reftable.company EQ po-ordl.company AND
                    reftable.loc EQ po-ordl.i-no AND
                    reftable.CODE EQ po-ord.vend-no
                    NO-LOCK NO-ERROR.
                    IF AVAIL reftable THEN
                    DO:
                      set-sv("oe-ordl.cost", STRING(DEC(get-sv("oe-ordl.cost")) * (1 + (reftable.val[1]/ 100.0 ))) ).
                      RELEASE reftable.
                    END.
                  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lib-ordltot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lib-ordltot Procedure 
PROCEDURE lib-ordltot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* o/e module - Calculate order line ext. price                              */
/* -------------------------------------------------------------------------- */
  DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ipr-ordl-row EXCLUSIVE-LOCK.
  DEF VAR v-tmp-price AS DEC FORMAT ">,>>>,>>9.9999" NO-UNDO.
  DEF VAR lv-t-price AS DEC NO-UNDO.


  FIND FIRST itemfg
      {sys/look/itemfgrlW.i}
        AND itemfg.i-no EQ /* wfk input */ oe-ordl.i-no
      NO-LOCK NO-ERROR.
      
  ASSIGN
   v-tmp-price = IF  oe-ordl.pr-uom BEGINS "L" AND  oe-ordl.pr-uom NE "LB" THEN
                   IF  oe-ordl.qty LT 0 THEN -1 ELSE 1
                 ELSE
                 IF  oe-ordl.pr-uom EQ "CS" THEN
                    oe-ordl.qty / (IF  oe-ordl.cas-cnt NE 0 THEN  oe-ordl.cas-cnt ELSE
                                    IF AVAIL itemfg AND itemfg.case-count NE 0
                                                   THEN itemfg.case-count ELSE
                                                        1)
                 ELSE
                 IF  oe-ordl.pr-uom EQ "C" THEN
                    oe-ordl.qty / 100
                 ELSE
                 IF  oe-ordl.pr-uom EQ "M" THEN
                    oe-ordl.qty / 1000
                 ELSE
                    oe-ordl.qty
                            
    lv-t-price = v-tmp-price *  oe-ordl.price
    /*oe-ordl.t-price:screen-value = string(round(lv-t-price - (lv-t-price *  oe-ordl.disc / 100),2)).*/
    oe-ordl.t-price = DECIMAL(
          IF v-print-fmt EQ "Dayton" THEN 
            (lv-t-price - ROUND(lv-t-price *  oe-ordl.disc / 100,2))
          ELSE
            ROUND(lv-t-price * (1 - ( oe-ordl.disc / 100)),2)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty Procedure 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO :
    RUN get-price.
    /*ASSIGN
    set-sv("oe-ordl.cases",  ).
    STRING(get-sv("TRUNC((DEC(oe-ordl.qty")) -
    DEC(get-sv("oe-ordl.partial"))) /
    DEC(get-sv("oe-ordl.cas-cnt")),0),
    oe-ordl.cases:FORMAT).
    set-sv("oe-ordl.partial",  ).
    STRING(get-sv("DEC(oe-ordl.qty")) -
    (get-sv("DEC(oe-ordl.cases")) *
    DEC(get-sv("oe-ordl.cas-cnt"))),
    oe-ordl.partial:FORMAT).*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-s-man) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-s-man Procedure 
PROCEDURE new-s-man :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.
  DEF VAR ll-all AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  IF ip-int EQ 0 THEN
  ASSIGN
  li     = 3
  ip-int = 1
  ll-all = YES.
  ELSE
  li = ip-int.


  DO ip-int = ip-int TO li :
    lv-sman = IF ip-int EQ 3 THEN get-sv("oe-ordl.s-man[3]")
    ELSE
    IF ip-int EQ 2 THEN get-sv("oe-ordl.s-man[2]")
    ELSE get-sv("oe-ordl.s-man[1]").
    IF lv-sman NE "" THEN DO:
      FIND FIRST sman
      WHERE sman.company EQ cocode
      AND sman.sman    EQ lv-sman
      NO-LOCK NO-ERROR.
      IF AVAIL sman THEN DO:
        IF ip-int EQ 3 THEN DO:
          set-sv("fi_sname-3", sman.sname ).
          IF NOT ll-all THEN DO:
            IF DEC(get-sv("oe-ordl.s-pct[3]")) EQ 0 THEN
            set-sv("oe-ordl.s-pct[3]", "100" ).
            IF DEC(get-sv("oe-ordl.s-comm[3]")) EQ 0 THEN
            set-sv("oe-ordl.s-comm[3]", STRING(sman.scomm) ).
          END.
        END.
        ELSE
        IF ip-int EQ 2 THEN DO:
          set-sv("fi_sname-2", sman.sname ).
          IF NOT ll-all THEN DO:
            IF DEC(get-sv("oe-ordl.s-pct[2]")) EQ 0 THEN
            set-sv("oe-ordl.s-pct[2]", "100" ).
            IF DEC(get-sv("oe-ordl.s-comm[2]")) EQ 0 THEN
            set-sv("oe-ordl.s-comm[2]", STRING(sman.scomm) ).
          END.
        END.
        ELSE DO:
          set-sv("fi_sname-1", sman.sname ).
          IF NOT ll-all THEN DO:
            IF DEC(get-sv("oe-ordl.s-pct[1]")) EQ 0 THEN
            set-sv("oe-ordl.s-pct[1]", "100" ).
            IF DEC(get-sv("oe-ordl.s-comm[1]")) EQ 0 THEN
            
            set-sv("oe-ordl.s-comm[1]", STRING(sman.scomm) ).
            v-margin = 0.
          END.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-tandem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tandem Procedure 
PROCEDURE new-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER b-eb FOR eb.
  DEF VAR v-rowid AS ROWID NO-UNDO.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  RUN est/NewEstimateForm.p ('F', ROWID(est), OUTPUT lv-new-tandem).
  FIND eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.
  IF AVAIL eb THEN DO :
    
    set-sv("oe-ordl.est-no", oe-ord.est-no ).
    set-sv("oe-ordl.job-no", oe-ord.job-no ).
    set-sv("oe-ordl.job-no2", STRING(oe-ord.job-no2) ).
    RUN est/d-selest.w (lv-new-tandem, YES, oe-ord.cust-no,
    OUTPUT v-qty-mod, OUTPUT v-rowid).
    IF v-qty-mod THEN DO:
      FIND FIRST b-eb
      WHERE b-eb.company EQ eb.company
      AND b-eb.est-no  EQ eb.est-no
      AND b-eb.eqty    EQ eb.eqty
      NO-LOCK NO-ERROR.
      IF AVAIL eb THEN
      ASSIGN
      eb.cust-no = b-eb.cust-no
      eb.ship-id = b-eb.ship-id.
      IF CAN-FIND(FIRST b-eb
      WHERE b-eb.company EQ eb.company
      AND b-eb.est-no  EQ eb.est-no
      AND b-eb.eqty    EQ eb.eqty
      AND b-eb.part-no EQ eb.part-no
      AND ROWID(b-eb)  NE ROWID(eb)) THEN
      ASSIGN
      eb.part-no    = ""
      eb.stock-no   = ""
      eb.part-dscr1 = ""
      eb.part-dscr2 = "".
      RUN display-est-detail (RECID(eb)).
      DISABLE oe-ordl.est-no.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-type) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type Procedure 
PROCEDURE new-type :
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
  DEF VAR li AS INT NO-UNDO.
  DO :
    set-sv("oe-ordl.type-code", CAPS(get-sv("oe-ordl.type-code")) ).
    li = LOOKUP(get-sv("oe-ordl.type-code"),lv-type-codes) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li = 0.
    IF li GT 0 AND li LE NUM-ENTRIES(lv-type-dscrs) THEN DO:
      set-sv("fi_type-dscr", ENTRY(li,lv-type-dscrs) ).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oescreen-values) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oescreen-values Procedure 
PROCEDURE oescreen-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL oe-ordl THEN DO:
    RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "I" /* Integer */, YES /* check by cust */,
    INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
    oescreen-int = INTEGER(v-rtn-char) NO-ERROR.
    IF AVAIL oe-ordl THEN
    RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "C" /* Char */, YES /* check by cust */,
    INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
    oescreen-cha = v-rtn-char NO-ERROR.
    IF AVAIL oe-ordl THEN
    RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "L" /* Logical */, YES /* check by cust */,
    INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
    oescreen-log = LOGICAL(v-rtn-char) NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ordltot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ordltot Procedure 
PROCEDURE ordltot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* wfk {oe/ordltot.i oe-ordl qty oe-ordl}  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-prev-quote-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prev-quote-proc Procedure 
PROCEDURE prev-quote-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER lv-price AS CHAR NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER lv-pruom AS CHAR NO-UNDO.
  DEF VAR lv-choice AS CHAR NO-UNDO.
  DEF VAR lv-price-dec LIKE oe-ordl.price NO-UNDO.
  DO :
    FOR EACH quotehd FIELDS(q-no) WHERE
      quotehd.company EQ cocode AND
      quotehd.cust-no EQ oe-ord.cust-no
      NO-LOCK,
      EACH quoteitm WHERE
      quoteitm.company EQ cocode AND
      quoteitm.loc     EQ locode AND
      quoteitm.q-no    EQ quotehd.q-no
      NO-LOCK
      BY quotehd.q-no DESC:
      IF NOT(quoteitm.part-no EQ get-sv("oe-ordl.part-no") OR
      (get-sv("oe-ordl.i-no") NE "" AND
      quoteitm.part-no EQ get-sv("oe-ordl.i-no"))) THEN
      NEXT.
      FIND FIRST quoteqty WHERE
      quoteqty.company = quoteitm.company AND
      quoteqty.loc = quoteitm.loc AND
      quoteqty.q-no = quoteitm.q-no AND
      quoteqty.LINE = quoteitm.LINE AND
      quoteqty.qty = INT(get-sv("oe-ordl.qty"))
      NO-LOCK NO-ERROR.
      IF AVAIL quoteqty AND
      NOT(quoteqty.price EQ DEC(get-sv("oe-ordl.price")) AND
      quoteqty.uom EQ get-sv("oe-ordl.pr-uom")) THEN
      DO:
        RUN oe\d-lastquote.w(INPUT cocode,
        INPUT locode,
        INPUT quoteitm.q-no,
        INPUT quoteitm.LINE,
        INPUT-OUTPUT lv-pruom,
        OUTPUT lv-price-dec,
        OUTPUT lv-choice).
        IF lv-choice = "OK" THEN
        DO:
          ASSIGN
          lv-price = STRING(lv-price-dec).
          set-sv("oe-ordl.price", lv-price ).
          set-sv("oe-ordl.pr-uom", lv-pruom ).
          /* wfk {oe/ordltot.i oe-ordl qty oe-ordl}  */
        END.
      END.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-current-oe-ord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-current-oe-ord Procedure 
PROCEDURE set-current-oe-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-oe-ord AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-oe-ordl AS ROWID NO-UNDO.

r-current-ord = ipr-oe-ord.
FIND oe-ord WHERE ROWID(oe-ord) = r-current-ord EXCLUSIVE-LOCK NO-ERROR.
r-current-ordl = ipr-oe-ordl.
FIND oe-ordl WHERE ROWID(oe-ordl) EQ r-current-ordl EXCLUSIVE-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-hcaller) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-hcaller Procedure 
PROCEDURE set-hcaller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER hCaller AS HANDLE.
h_callproc = hCaller.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-params) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-params Procedure 
PROCEDURE set-params :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-ip-recid AS RECID NO-UNDO.
DEF INPUT PARAMETER ip-ip-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF INPUT PARAMETER ip-ip-type AS cha NO-UNDO .   /* add,update,view */
DEF INPUT PARAMETER ip-add-mode AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-cocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-locode AS CHAR NO-UNDO.

   
ip-recid = ip-ip-recid.
ip-ord-no = ip-ip-ord-no.
ip-type = ip-ip-type.
lv-add-mode = ip-add-mode.
cocode = ip-cocode.
locode = ip-locode.
g_company = ip-cocode.
g_loc     = ip-locode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQtyPrice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQtyPrice Procedure 
PROCEDURE setQtyPrice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipOeordlRowid AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ipQty AS DECIMAL NO-UNDO.
  DEF INPUT PARAMETER ipPrice LIKE oe-ordl.price NO-UNDO.
  DEF INPUT PARAMETER ipPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
  DEF INPUT PARAMETER ipSetFromHistory AS LOGICAL NO-UNDO.
  ASSIGN
  historyQty     = ipQty
  historyPrice   = ipPrice
  historyPrUOM   = ipPrUOM
  setFromHistory = ipSetFromHistory.
  FIND tt-qty-price WHERE tt-qty-price.oeordl-ROWID EQ ipOeordlRowid
  NO-ERROR.
  IF NOT AVAIL tt-qty-price THEN DO:
    CREATE tt-qty-price.
    ASSIGN tt-qty-price.oeordl-rowid = ipOeordlRowid.
  END.
  ASSIGN
  tt-historyQty    = ipQty
  tt-historyPrice  = ipPrice
  tt-historyPrUOM  = ipPrUom
  tt-setFromHistory = ipSetFromHistory.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-upd-new-tandem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-new-tandem Procedure 
PROCEDURE upd-new-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-est-qty FOR est-qty.
  DEF BUFFER b-Unit# FOR reftable.
  DEF VAR lv-master AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  FIND FIRST eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.
  IF AVAIL eb THEN DO:
    ASSIGN
    eb.stock-no      = oe-ordl.i-no
    eb.part-no       = oe-ordl.part-no
    eb.part-dscr1    = oe-ordl.i-name
    eb.part-dscr2    = oe-ordl.part-dscr1
    eb.bl-qty        = oe-ordl.qty
    eb.yld-qty       = oe-ordl.qty
    oe-ordl.est-type = eb.est-type
    oe-ordl.form-no  = eb.form-no
    oe-ordl.blank-no = eb.blank-no
    v-qty-mod        = YES.
    /* Add to master estimate */
    IF INT(eb.master-est-no) NE 0 THEN DO:
      FIND FIRST b-eb
      WHERE b-eb.company EQ eb.company
      AND b-eb.est-no  EQ eb.master-est-no
      AND b-eb.part-no EQ eb.part-no
      NO-LOCK NO-ERROR.
      IF NOT AVAIL b-eb THEN DO:
        ASSIGN
        eb.i-col     = 0
        eb.i-pass    = 0
        eb.i-coat    = 0
        eb.i-coat-p  = 0
        eb.i-coldscr = ""
        eb.i-code    = ""
        eb.i-ps      = 0
        eb.i-dscr    = ""
        eb.i-%       = 0
        eb.i-code2   = ""
        eb.i-ps2     = 0
        eb.i-dscr2   = ""
        eb.i-%2      = 0.
        FOR EACH itemfg-ink
          WHERE itemfg-ink.company EQ eb.company
          AND itemfg-ink.i-no    EQ eb.stock-no:
          DELETE itemfg-ink.
        END.
        DO li = 1 TO 2:
          FOR EACH b-Unit#
            WHERE b-Unit#.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(li - 1,">"))
            AND b-Unit#.company  EQ eb.company
            AND b-Unit#.loc      EQ eb.est-no
            AND b-Unit#.CODE     EQ STRING(eb.form-no,"9999999999")
            AND b-Unit#.code2    EQ STRING(eb.blank-no,"9999999999"):
            DELETE b-Unit#.
          END.
        END.
        FIND FIRST xest
        WHERE xest.company EQ eb.company
        AND xest.est-no  EQ eb.master-est-no
        NO-LOCK NO-ERROR.
        IF AVAIL xest THEN DO:
          FIND FIRST ef OF eb NO-LOCK NO-ERROR.
          RUN est/NewEstimateForm.p ('F', ROWID(xest), OUTPUT lv-master).
          FIND b-eb WHERE ROWID(b-eb) EQ lv-master NO-ERROR.
          IF AVAIL b-eb THEN DO:
            FIND FIRST b-est-qty
            WHERE b-est-qty.company EQ b-eb.company
            AND b-est-qty.est-no  EQ b-eb.est-no
            NO-LOCK NO-ERROR.
            BUFFER-COPY eb EXCEPT e-num form-no blank-no est-no rec_key TO b-eb
            ASSIGN
            b-eb.master-est-no = ""
            b-eb.eqty          = b-est-qty.eqty.
            FIND FIRST b-ef OF b-eb NO-ERROR.
            IF AVAIL b-ef AND AVAIL ef THEN
            DO:
              BUFFER-COPY ef EXCEPT e-num form-no est-no rec_key TO b-ef
              ASSIGN
              b-ef.eqty = b-est-qty.eqty.
              FIND CURRENT b-ef NO-LOCK.
              RELEASE b-ef.
            END.
            /*RUN est/oeselest.p.*/
          END.
        END.
      END.
    END.
    FIND xeb WHERE ROWID(xeb) EQ ROWID(eb) NO-LOCK NO-ERROR.
    RUN est/oeselest.p.
    RELEASE xeb.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-upd-tandem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-tandem Procedure 
PROCEDURE upd-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL oe-ordl THEN
  FOR EACH eb
    WHERE eb.company  EQ oe-ordl.company
    AND eb.est-no   EQ oe-ordl.est-no
    AND eb.part-no  EQ oe-ordl.part-no
    AND eb.stock-no EQ oe-ordl.i-no
    BREAK BY eb.form-no  DESC
    BY eb.blank-no DESC:
    IF LAST(eb.form-no)                     OR
    (eb.form-no EQ oe-ordl.form-no AND
    eb.blank-no EQ oe-ordl.blank-no)    THEN DO:
      ASSIGN
      eb.part-dscr1    = oe-ordl.i-name
      eb.part-dscr2    = oe-ordl.part-dscr1
      eb.bl-qty        = oe-ordl.qty
      eb.yld-qty       = oe-ordl.qty.
      LEAVE.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-due-date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-due-date Procedure 
PROCEDURE update-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bJobHdr FOR job-hdr.
  DEF BUFFER bJob FOR job.
  DEF VAR li AS INT NO-UNDO.
  IF CAN-FIND(FIRST bJobHdr
  WHERE bJobHdr.company EQ oe-ordl.company
  AND bJobHdr.job-no  EQ oe-ordl.job-no
  AND bJobHdr.job-no2 EQ oe-ordl.job-no2
  AND bJobHdr.i-no    EQ oe-ordl.i-no) THEN
  DO WHILE li LT 1000:
    li = li + 1000.
    FIND FIRST bJobHdr EXCLUSIVE-LOCK
    WHERE bJobHdr.company EQ oe-ordl.company
    AND bJobHdr.job-no EQ oe-ordl.job-no
    AND bJobHdr.job-no2 EQ oe-ordl.job-no2
    AND bJobHdr.i-no EQ oe-ordl.i-no NO-WAIT NO-ERROR.
    IF AVAIL bJobHdr THEN DO:
      bJobHdr.due-date = oe-ordl.req-date.
      FIND CURRENT bJobHdr NO-LOCK.
      FIND FIRST bJob OF bJobHdr EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL bJob THEN RETURN.
      bJob.due-date = bJobHdr.due-date.
      FIND CURRENT bJob NO-LOCK.
      RELEASE bJobHdr.
      li = 1000.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-itemfg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-itemfg Procedure 
PROCEDURE update-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-flag AS LOG EXTENT 10 NO-UNDO.
  DEF VAR v-prompt AS LOG NO-UNDO.
  DEF VAR ls-flag AS cha NO-UNDO.
  DEF VAR lv-est-no LIKE itemfg.est-no NO-UNDO.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-upd-oe-ordl FOR oe-ordl.
  DEF BUFFER b-eb2 FOR eb.
  DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
  DEF BUFFER b-reftable-1 FOR reftable.
  DEF VAR v-cost-updated AS LOG NO-UNDO.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  DISABLE TRIGGERS FOR LOAD OF eb.
      FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "OEFGUPDT"
      NO-LOCK NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
        sys-ctrl.NAME    = "OEFGUPDT"
        sys-ctrl.descrip =
        "Update FG? Sell Price,UOM,Count,Name,Desc1,Desc2,Job,Vendor,Est.,Desc3"
        sys-ctrl.log-fld = NO.
        
        RUN oe/d-asgnfg.w  (RECID(sys-ctrl), oe-ordl.est-no, "New", OUTPUT ls-flag).
      END. /* not avail sys-ctrl */
      FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ oe-ordl.i-no
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        FIND oe-ord OF oe-ordl NO-LOCK.
        IF sys-ctrl.log-fld THEN
        RUN oe/d-asgnfg.w (RECID(sys-ctrl), oe-ordl.est-no,"exist",OUTPUT ls-flag).
        ELSE DO:
          FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.NAME    EQ "OEFGUPDT"
          NO-LOCK NO-ERROR.
          ls-flag = sys-ctrl.char-fld.
          SUBSTRING(ls-flag,9,1) = STRING(sys-ctrl.int-fld EQ 1 AND oe-ordl.est-no NE "","Y/N").
        END.
        ASSIGN v-flag[1] = SUBSTRING(ls-flag,1,1) = "Y"
        v-flag[2] = SUBSTRING(ls-flag,2,1) = "Y"
        v-flag[3] = SUBSTRING(ls-flag,3,1) = "Y"
        v-flag[4] = SUBSTRING(ls-flag,4,1) = "Y"
        v-flag[5] = SUBSTRING(ls-flag,5,1) = "Y"
        v-flag[6] = SUBSTRING(ls-flag,6,1) = "Y"
        v-flag[7] = SUBSTRING(ls-flag,7,1) = "Y"
        v-flag[8] = SUBSTRING(ls-flag,8,1) = "Y"
        v-flag[9] = SUBSTRING(ls-flag,9,1) = "Y"
        v-flag[10] = SUBSTRING(ls-flag,10,1) = "Y".
        IF v-flag[1] OR ll-new-fg-created THEN
        itemfg.sell-price  = oe-ordl.price.
        IF v-flag[2] THEN itemfg.sell-uom    = oe-ordl.pr-uom.
        IF v-flag[3] THEN
        ASSIGN itemfg.case-count  = oe-ordl.cas-cnt
        itemfg.case-pall   = oe-ordl.cases-unit.
        IF v-flag[4] THEN itemfg.i-name      = oe-ordl.i-name.
        IF v-flag[5] THEN itemfg.part-dscr1  = oe-ordl.part-dscr1.
        IF v-flag[6] THEN itemfg.part-dscr2  = oe-ordl.part-dscr2.
        IF v-flag[10] THEN itemfg.part-dscr3  = oe-ordl.part-dscr3.
        IF v-flag[7] THEN itemfg.cust-job-no = oe-ordl.job-no + "-" +
        STRING(oe-ordl.job-no2).
        IF v-flag[8] THEN itemfg.vend-no     = oe-ordl.vend-no.
        IF v-flag[9] THEN DO:
          itemfg.est-no = oe-ordl.est-no.
          FOR EACH w-est-no:
            DELETE w-est-no.
          END.
          CREATE w-est-no.
          w-est-no = itemfg.est-no.
          DO WHILE AVAIL w-est-no:
            ASSIGN
            w-run     = YES
            lv-est-no = w-est-no.
            FOR EACH eb
              WHERE eb.company   EQ oe-ordl.company
              AND eb.est-no    EQ lv-est-no
              AND eb.cust-no   EQ oe-ord.cust-no
              AND ((eb.part-no EQ first-cust-part-no AND eb.stock-no EQ "") OR
              eb.stock-no EQ oe-ordl.i-no)
              AND TRIM(eb.master-est-no) NE ""
              AND NOT CAN-FIND(FIRST w-est-no WHERE w-est-no EQ eb.master-est-no)
              NO-LOCK:
              CREATE w-est-no.
              w-est-no = eb.master-est-no.
            END.
            FIND FIRST w-est-no WHERE w-run EQ NO NO-ERROR.
          END.
          FOR EACH w-est-no BREAK BY w-est-no:
            IF NOT FIRST-OF(w-est-no) THEN DELETE w-est-no.
          END.
          FOR EACH w-est-no,
            EACH eb
            WHERE eb.company   EQ oe-ordl.company
            AND eb.est-no    EQ w-est-no
            AND eb.cust-no   EQ oe-ord.cust-no
            AND ((eb.part-no EQ first-cust-part-no AND eb.stock-no EQ "") OR
            eb.stock-no EQ oe-ordl.i-no):
            ASSIGN
            eb.stock-no   = oe-ordl.i-no
            eb.part-no    = oe-ordl.part-no
            eb.part-dscr1 = oe-ordl.i-name
            eb.part-dscr2 = oe-ordl.part-dscr1.
            /*IF oe-ordl.cas-cnt    NE 0 THEN eb.cas-cnt = oe-ordl.cas-cnt.
            IF oe-ordl.cases-unit NE 0 THEN eb.cas-pal = oe-ordl.cases-unit.
            eb.tr-cnt = eb.cas-cnt * eb.cas-pal.*/
            IF v-oecount-int EQ 1 THEN DO:
              IF v-oecount THEN
              ASSIGN
              eb.cas-cnt = oe-ordl.cas-cnt
              eb.tr-cnt  = eb.cas-cnt * eb.cas-pal.
              ELSE
              ASSIGN
              eb.tr-cnt  = oe-ordl.cas-cnt
              eb.cas-cnt = TRUNC(eb.tr-cnt / (IF eb.cas-pal GT 0 THEN eb.cas-pal
              ELSE 1),0).
              eb.cas-wt = 0.
            END.
          END.
        END.
        IF ll-new-fg-created THEN itemfg.taxable = oe-ordl.tax.
        IF ll-new-record THEN itemfg.type-code = oe-ordl.type-code.
        FIND FIRST cust-part WHERE
        cust-part.company EQ oe-ordl.company AND
        cust-part.cust-no EQ oe-ordl.cust-no AND
        cust-part.i-no EQ itemfg.i-no
        NO-LOCK NO-ERROR.
        IF AVAIL cust-part THEN
        DO:
          IF v-oecustpart THEN
          DO:
            FOR EACH b-oe-ordl
              WHERE b-oe-ordl.company EQ itemfg.company
              AND b-oe-ordl.i-no    EQ itemfg.i-no
              AND b-oe-ordl.opened  EQ YES
              AND b-oe-ordl.cust-no EQ oe-ordl.cust-no
              AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)
              USE-INDEX ITEM
              NO-LOCK:
              FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
              IF AVAIL b-upd-oe-ordl THEN DO:
                b-upd-oe-ordl.part-no                      = oe-ordl.part-no.
                /*
                IF v-flag[3] THEN b-upd-oe-ordl.cas-cnt    = oe-ordl.cas-cnt.
                */
                IF v-flag[4] THEN b-upd-oe-ordl.i-name     = oe-ordl.i-name.
                IF v-flag[5] THEN b-upd-oe-ordl.part-dscr1 = oe-ordl.part-dscr1.
                IF v-flag[6] THEN b-upd-oe-ordl.part-dscr2 = oe-ordl.part-dscr2.
                IF v-flag[10] THEN b-upd-oe-ordl.part-dscr3 = oe-ordl.part-dscr3.
              END.
              FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
              NO-LOCK NO-ERROR.
            END.
            FIND CURRENT cust-part EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAIL cust-part THEN
            DO:
              cust-part.part-no = oe-ordl.part-no.
              FIND CURRENT cust-part NO-LOCK.
            END.
          END.
        END. /*avai cust-part*/
        /*farm out/purchased items*/
        IF oe-ordl.est-no NE "" AND itemfg.pur-man THEN
        DO:
          IF NOT CAN-FIND(FIRST e-itemfg WHERE
          e-itemfg.company = itemfg.company AND
          e-itemfg.i-no = itemfg.i-no) THEN
          DO:
            CREATE e-itemfg.
            ASSIGN e-itemfg.company = itemfg.company
            e-itemfg.i-no = itemfg.i-no.
            RELEASE e-itemfg.
          END.
          FOR EACH b-eb2 FIELDS(company est-no form-no blank-no stock-no)
            WHERE b-eb2.company EQ itemfg.company
            AND b-eb2.est-no  EQ oe-ordl.est-no
            AND b-eb2.part-no EQ oe-ordl.part-no
            NO-LOCK,
            EACH e-itemfg-vend WHERE
            e-itemfg-vend.company  EQ b-eb2.company AND
            e-itemfg-vend.est-no   EQ b-eb2.est-no AND
            e-itemfg-vend.form-no  EQ b-eb2.form-no AND
            e-itemfg-vend.blank-no EQ b-eb2.blank-no
            NO-LOCK:
            v-cost-updated = NO.
            FIND FIRST b-e-itemfg-vend WHERE
            b-e-itemfg-vend.company EQ b-eb2.company AND
            b-e-itemfg-vend.i-no EQ itemfg.i-no AND
            b-e-itemfg-vend.est-no EQ "" AND
            b-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no AND
            b-e-itemfg-vend.cust-no EQ e-itemfg-vend.cust-no
            NO-ERROR.
            IF NOT AVAIL b-e-itemfg-vend THEN
            DO:
              CREATE b-e-itemfg-vend.
              BUFFER-COPY e-itemfg-vend EXCEPT i-no rec_key est-no eqty form-no blank-no
              TO b-e-itemfg-vend
              ASSIGN b-e-itemfg-vend.est-no = ""
              b-e-itemfg-vend.eqty = 0
              b-e-itemfg-vend.form-no = 0
              b-e-itemfg-vend.blank-no = 0
              b-e-itemfg-vend.i-no = itemfg.i-no
              v-cost-updated = YES.
            END.
            ELSE IF b-eb2.stock-no NE "" THEN /*update costs*/
            BUFFER-COPY e-itemfg-vend EXCEPT i-no rec_key est-no eqty form-no blank-no
            TO b-e-itemfg-vend
            ASSIGN
            v-cost-updated = YES.
            IF v-cost-updated THEN
            DO:
              FIND FIRST b-reftable-1 WHERE
              b-reftable-1.reftable EQ "e-itemfg-vend.std-uom" AND
              b-reftable-1.company  EQ e-itemfg-vend.company AND
              b-reftable-1.loc      EQ "" AND
              b-reftable-1.CODE     EQ e-itemfg-vend.est-no AND
              b-reftable-1.val[1]   EQ e-itemfg-vend.form-no AND
              b-reftable-1.val[2]   EQ e-itemfg-vend.blank-no
              NO-LOCK NO-ERROR.
              IF AVAIL b-reftable-1 THEN
              DO:
                FIND FIRST e-itemfg WHERE
                e-itemfg.company EQ e-itemfg-vend.company AND
                e-itemfg.i-no EQ itemfg.i-no
                NO-ERROR.
                IF AVAIL e-itemfg THEN
                DO:
                  e-itemfg.std-uom = b-reftable-1.code2.
                  RELEASE e-itemfg.
                END.
                RELEASE b-reftable-1.
              END.
            END.
            RELEASE b-e-itemfg-vend.
          END.
        END.
      END.
      RELEASE itemfg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-release) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-release Procedure 
PROCEDURE update-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR li AS INT.
  DEF VAR li-ship AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li-tries AS INT NO-UNDO.
  FOR EACH oe-rel
    WHERE oe-rel.company EQ oe-ordl.company
    AND oe-rel.ord-no  EQ oe-ordl.ord-no
    AND oe-rel.i-no    EQ oe-ordl.i-no
    AND oe-rel.LINE    EQ oe-ordl.LINE
    NO-LOCK:
    li = li + oe-rel.qty.
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
    IF INDEX("PCZ",lv-stat) GT 0 THEN li-ship = li-ship + oe-rel.qty.
  END.
  ASSIGN
  lv-stat  = ""
  li-tries = 0.
  DO WHILE TRUE:
    li-tries = li-tries + 1.
    IF li-tries GE 1000 THEN LEAVE.
    FIND oe-rel EXCLUSIVE
    WHERE oe-rel.company EQ oe-ordl.company
    AND oe-rel.ord-no  EQ oe-ordl.ord-no
    AND oe-rel.i-no    EQ oe-ordl.i-no
    AND oe-rel.LINE    EQ oe-ordl.LINE
    AND oe-rel.link-no EQ 0
    NO-WAIT NO-ERROR.
    IF AVAIL oe-rel THEN DO:
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
      IF INDEX("SIL",lv-stat) GT 0 THEN DO:
        ll = YES.
        FIND FIRST shipto WHERE shipto.company = oe-rel.company AND
        shipto.cust-no = oe-rel.cust-no  AND
        shipto.ship-id = oe-rel.ship-id
        USE-INDEX ship-id NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN DO:
          ASSIGN oe-rel.ship-no      = shipto.ship-no
          oe-rel.ship-id      = shipto.ship-id
          oe-rel.ship-addr[1] = shipto.ship-addr[1]
          oe-rel.ship-addr[2] = shipto.ship-addr[2]
          oe-rel.ship-city    = shipto.ship-city
          oe-rel.ship-state   = shipto.ship-state
          oe-rel.ship-zip     = shipto.ship-zip
          oe-rel.ship-i[1] = shipto.notes[1]
          oe-rel.ship-i[2] = shipto.notes[2]
          oe-rel.ship-i[3] = shipto.notes[3]
          oe-rel.ship-i[4] = shipto.notes[4].
          /* if add mode then use default carrier */
          IF shipto.notes[1] <> "" OR shipto.notes[2] <> "" OR
          shipto.notes[3] <> "" OR shipto.notes[4] <> "" THEN DO:
            FIND FIRST notes WHERE notes.rec_key = oe-rel.rec_key NO-LOCK NO-ERROR.
            IF NOT AVAIL notes THEN DO:
              CREATE notes.
              ASSIGN
              notes.rec_key = oe-rel.rec_key
              notes.note_date = TODAY
              notes.note_title = shipto.notes[1]
              notes.note_text = shipto.notes[1] + CHR(13) +
              shipto.notes[2] + CHR(13) +
              shipto.notes[3] + CHR(13) +
              ship.notes[4] + CHR(13).
            END.
          END.
        END.
      END.
      IF li NE oe-ordl.qty                                  AND
      ((oe-ordl.qty GT 0 AND li-ship LT oe-ordl.qty) OR
      (oe-ordl.qty LT 0 AND li-ship GT oe-ordl.qty))    THEN DO:
        IF ll THEN DO:
          ll = NO.
          MESSAGE "Update Release Quantity?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
          IF ll THEN DO:
              ASSIGN
              oe-rel.qty     = oe-ordl.qty - li-ship
              oe-rel.tot-qty = oe-rel.qty.
              RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
          END.

        END.
        ELSE
        MESSAGE "Order Qty differs from Release Quantites,"
        "click Release Tab to Update..."
        VIEW-AS ALERT-BOX.
      END.
      LEAVE.
    END.
  END. /* do while true */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-start-date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-start-date Procedure 
PROCEDURE update-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-prom-date AS DATE NO-UNDO.
  DEF VAR lv-day-time AS INT NO-UNDO.
  DEF VAR lv-job2-time AS INT NO-UNDO.
  DEF VAR lv-prev-end-time AS INT NO-UNDO.
  DEF VAR lv-lap-time AS INT NO-UNDO.
  IF oe-ordl.job-no = ""  THEN RETURN.
  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  DEF BUFFER bx-ordl FOR oe-ordl.
  DEF VAR lv-first-due-date AS DATE NO-UNDO.
  lv-first-due-date = oe-ordl.req-date.
  FOR EACH bx-ordl FIELDS(req-date) WHERE
    bx-ordl.company = oe-ordl.company AND
    bx-ordl.job-no = oe-ordl.job-no AND
    bx-ordl.job-no2 = oe-ordl.job-no2 AND
    RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
    lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
    ELSE lv-first-due-date.
  END.
  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf2-hdr FOR job-hdr.
  DEF BUFFER bf2-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
  lv-run-time = 0
  lv-job-time = 0
  lv-maccum-time = 0.
  IF oe-ord.job-no GT "" THEN
  FIND FIRST job WHERE job.job-no = oe-ord.job-no
  AND job.job-no2 = oe-ordl.job-no2
  NO-LOCK NO-ERROR.
  FOR EACH bf-hdr FIELDS(company job-no job-no2) WHERE bf-hdr.company = oe-ord.company
    AND bf-hdr.job-no = oe-ordl.job-no
    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
    EACH bf-mch FIELDS(mr-hr run-hr) WHERE bf-mch.company = bf-hdr.company
    AND bf-mch.job-no = bf-hdr.job-no
    AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
    ASSIGN
    lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
    TRUNCATE(bf-mch.mr-hr,0) * 3600 +
    ((bf-mch.mr-hr - TRUNCATE(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
    lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
    TRUNCATE(bf-mch.run-hr,0) * 3600 +
    ((bf-mch.run-hr - TRUNCATE(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
    lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END.
  ASSIGN
  lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
  ELSE TRUNCATE(lv-job-time / 3600,0)
  lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN TRUNCATE(lv-job-hr / 8,0) + 1
  ELSE TRUNCATE(lv-job-hr / 8,0)
  lv-start-date = lv-first-due-date - lv-job-day /*- 1. */
  lv-update-job-stdate = NO.
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY /*AND (ip-type = "add")*/  /* ip-type = "Update-2" is from v-ord.w*/
  AND v-run-schedule AND schedule-log
  THEN DO:
    MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
    /*Due Date is before Calculates Promised Date. Update Due Date? */
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    lv-start-date = TODAY.
  END.
  ELSE IF lv-start-date < TODAY THEN lv-start-date = TODAY.
  IF v-run-schedule THEN DO:
    /* === reset start-date === */
    ASSIGN lv-mr-time = 0
    lv-run-time = 0
    lv-job-time = 0
    lv-maccum-time = 0
    li-num-of-wkend = 0
    lv-day-time = 0
    lv-start-time = 0.
    FOR EACH bf2-hdr NO-LOCK WHERE bf2-hdr.company = oe-ord.company
      AND bf2-hdr.job-no = oe-ordl.job-no
      AND bf2-hdr.job-no2 = oe-ordl.job-no2,
      EACH bf2-mch NO-LOCK WHERE bf2-mch.company = bf2-hdr.company
      AND bf2-mch.job-no = bf2-hdr.job-no
      AND bf2-mch.job-no2 = bf2-hdr.job-no2
      AND NOT bf2-mch.anchored
      BREAK BY bf2-mch.frm BY bf2-mch.blank-no BY bf2-mch.pass BY bf2-mch.m-code:
      REPEAT:
        FIND bf-hdr WHERE ROWID(bf-hdr) EQ ROWID(bf2-hdr) EXCLUSIVE NO-ERROR NO-WAIT.
        FIND bf-mch WHERE ROWID(bf-mch) EQ ROWID(bf2-mch) EXCLUSIVE NO-ERROR NO-WAIT.
        IF AVAIL bf-hdr AND AVAIL bf-mch THEN
        LEAVE.
      END.
      FIND FIRST mach-calendar WHERE
      mach-calendar.company = job.company AND
      mach-calendar.m-code = bf-mch.m-code AND
      mach-calendar.m-date = lv-start-date
      NO-LOCK NO-ERROR.
      lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
      ELSE 28800. /* 8 HRs*/
      IF lv-m-time LT 0 THEN lv-m-time = 28800.
      lv-maccum-time = lv-maccum-time + lv-m-time.
      IF FIRST(bf2-mch.frm) THEN DO:
        REPEAT:
          FIND FIRST bf-job OF bf-hdr EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAIL bf-job THEN
          LEAVE.
        END.
        ASSIGN
        bf-job.start-date = lv-start-date
        lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
      END.
      IF FIRST-OF(bf2-mch.frm) THEN
      bf-hdr.start-date = job.start-date.
      ASSIGN
      lv-start-time = lv-wrk-st-time
      lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
      TRUNCATE(bf-mch.mr-hr,0) * 3600 +
      ((bf-mch.mr-hr - TRUNCATE(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
      lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
      TRUNCATE(bf-mch.run-hr,0) * 3600 +
      ((bf-mch.run-hr - TRUNCATE(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
      bf-mch.seq-no = 0
      bf-mch.start-time-su = lv-wrk-st-time + lv-job-time + lv-day-time
      bf-mch.start-date-su = lv-start-date
      lv-start-date-fr = lv-start-date
      lv-job-time = lv-job-time + lv-mr-time + lv-run-time
      lv-start-date = lv-start-date +
      IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0)
      ELSE 0.
      IF lv-mr-time > lv-m-time THEN DO:
        ASSIGN
        lv-job2-time = lv-mr-time - lv-m-time
        lv-lap-time = bf-mch.start-time-su - lv-start-time.
        FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
        AND mach-calendar.m-code = bf-mch.m-code
        AND mach-calendar.m-date = lv-start-date
        NO-LOCK NO-ERROR.
        lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
        ELSE 28800. /* 8 HRs*/.
        IF lv-m-time LT 0 THEN
        lv-m-time = 28800.
        ASSIGN
        lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0
        bf-mch.end-time-su = lv-start-time + lv-job2-time + lv-lap-time
        bf-mch.start-time = lv-start-time + lv-job2-time  + lv-lap-time
        bf-mch.end-date-su = lv-start-date
        bf-mch.start-date = lv-start-date
        lv-day-time = lv-start-time - lv-prev-end-time + 86400.
      END.
      ELSE
      ASSIGN bf-mch.end-time-su = lv-start-time + lv-job-time - lv-run-time + lv-day-time
      bf-mch.start-time = lv-start-time + lv-job-time - lv-run-time + lv-day-time
      bf-mch.end-date-su = lv-start-date
      bf-mch.start-date = lv-start-date
      lv-lap-time = 0.
      ASSIGN
      lv-start-date = lv-start-date +
      IF (lv-run-time) > lv-m-time THEN TRUNCATE((lv-run-time + lv-mr-time) / lv-m-time,0)
      ELSE 0
      lv-start-date-fr = lv-start-date.
      IF (lv-run-time) > lv-m-time THEN DO:
        ASSIGN
        lv-job2-time = lv-run-time - lv-m-time
        lv-lap-time = bf-mch.start-time - lv-start-time.
        FIND FIRST mach-calendar WHERE
        mach-calendar.company = job.company AND
        mach-calendar.m-code = bf-mch.m-code AND
        mach-calendar.m-date = lv-start-date
        NO-LOCK NO-ERROR.
        lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
        ELSE 28800. /* 8 HRs*/
        IF lv-m-time LT 0 THEN
        lv-m-time = 28800.
        ASSIGN
        lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0
        bf-mch.end-time = lv-start-time + lv-job2-time
        bf-mch.end-date = lv-start-date
        lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400.
      END.
      ELSE
      ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
      bf-mch.end-date = lv-start-date
      lv-lap-time = 0.
      lv-prev-end-time = IF AVAIL mach-calendar THEN mach-calendar.end-time ELSE 86400. /* 24 HRs*/
      IF STRING(bf-mch.end-time,"hh:mm:ss") > STRING(lv-prev-end-time,"hh:mm:ss") THEN DO:
        ASSIGN
        lv-start-date = lv-start-date + 1
        lv-lap-time = bf-mch.end-time - lv-prev-end-time.
        FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
        AND mach-calendar.m-code = bf-mch.m-code
        AND mach-calendar.m-date = lv-start-date
        NO-LOCK NO-ERROR.
        lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
        ELSE 28800. /* 8 HRs*/.
        IF lv-m-time LT 0 THEN lv-m-time = 28800.
        ASSIGN
        lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0
        bf-mch.end-time = lv-start-time + lv-lap-time
        bf-mch.end-date = lv-start-date
        lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400.
      END.
      FIND CURRENT bf-hdr NO-LOCK NO-ERROR.
      FIND CURRENT bf-mch NO-LOCK NO-ERROR.
    END. /*end bf-hdr*/
  END. /* if v-run-schedule*/
  IF schedule-log THEN
  ASSIGN
  bx-ordl.prom-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.prom-date
  bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-cas-cnt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cas-cnt Procedure 
PROCEDURE valid-cas-cnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO :
    DEF VAR lv-calc-qty AS DEC NO-UNDO.
    DEF VAR lv-case-qty AS INT NO-UNDO.
    DEF VAR lv-uom AS CHAR NO-UNDO.
    IF INT(get-sv("oe-ordl.qty")) GT 0 AND get-sv("fi_qty-uom") > "" THEN DO:

      FIND FIRST itemfg WHERE itemfg.company = cocode 
                          AND itemfg.i-no EQ /* wfk INPUT */ oe-ordl.i-no
      NO-LOCK NO-ERROR.
      lv-calc-qty = DEC(get-sv("oe-ordl.qty")).
      IF AVAIL itemfg THEN
      ASSIGN
      lv-uom = get-sv("fi_qty-uom")
      lv-case-qty = (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE
      IF lv-uom EQ "EA" THEN 1 ELSE 1000)
      lv-calc-qty = DEC(get-sv("oe-ordl.qty")).
      IF get-sv("oe-ordl.pr-uom") NE "EA" AND avail(itemfg) THEN
      ASSIGN
      lv-calc-qty = lv-calc-qty * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE
      IF lv-uom EQ "EA" THEN 1 ELSE 1000).
      /*
      IF lv-calc-qty LT INT(get-sv("oe-ordl.cas-cnt")) THEN
      set-sv("oe-ordl.cas-cnt", STRING(lv-calc-qty) ).
      */
      IF INT(get-sv("oe-ordl.cas-cnt")) > lv-calc-qty THEN DO:
        /*wfk
        message "Unit count may not be greater than quantity." skip
        Setting unit count to equal quantity.
        view-as alert-box information. */
        set-sv("oe-ordl.cas-cnt", get-sv("oe-ordl.qty") ).
        /* wfk APPLY "entry" TO oe-ordl.cas-cnt. */
        RETURN.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-i-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Procedure 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF BUFFER bf-ordl FOR oe-ordl.
  DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
  DO:

    v-msg = "".
    IF get-sv("oe-ordl.i-no") EQ "0" THEN
     v-msg =  "may not be 0".
    IF get-sv("oe-ordl.i-no") = "" THEN
    v-msg = "may not be blank" .

    IF v-msg =  "" THEN
    IF CAN-FIND(FIRST bf-ordl WHERE bf-ordl.company EQ oe-ordl.company
    AND bf-ordl.ord-no  EQ oe-ordl.ord-no
    AND bf-ordl.i-no    EQ get-sv("oe-ordl.i-no")
    AND NOT bf-ordl.is-a-component
    AND ROWID(bf-ordl)  NE ROWID(oe-ordl))
    THEN v-msg = "has already been entered on this order".


    IF v-msg EQ "" THEN
    IF get-sv("oe-ordl.est-no") NE "" THEN
    FOR EACH eb
      WHERE eb.company   EQ oe-ordl.company
      AND eb.est-no    EQ get-sv("oe-ordl.est-no")
      AND (eb.est-type EQ 2 OR eb.est-type EQ 6)
      AND eb.form-no   NE 0
      NO-LOCK BREAK BY eb.est-no:
      IF (NOT FIRST(eb.est-no) OR NOT LAST(eb.est-no)) AND
      (eb.stock-no EQ get-sv("oe-ordl.i-no") OR
      eb.part-no  EQ get-sv("oe-ordl.i-no"))    THEN
      v-msg = "is a component on this estimate".
    END.
    /* task: 05150314 */
    IF v-msg EQ "" THEN
    IF get-sv("oe-ordl.i-no") NE "" THEN DO:
/*       FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" */
/*       AND reftable.company  EQ g_company                        */
/*       AND reftable.loc      EQ ""                               */
/*       AND reftable.CODE     EQ get-sv("oe-ordl.i-no")           */
/*       NO-LOCK NO-ERROR.                                         */
/*       IF AVAIL reftable AND reftable.code2 = "I" THEN           */
      RUN fg/GetItemfgActInact.p (INPUT g_company,
                                  INPUT  get-sv("oe-ordl.i-no"),
                                  OUTPUT lActive).
      IF NOT lActive  THEN
          v-msg = get-sv("oe-ordl.i-no") + " has InActive Status. Order cannot be placed for the Inactive Item.".
    END.
    IF v-msg NE "" THEN DO:
    /* wfk MESSAGE TRIM(oe-ordl.i-no:LABEL)
    VIEW-AS ALERT-BOX ERROR. */
    /* wfk APPLY "entry" TO oe-ordl.i-no. */
        RETURN ERROR.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-part-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no Procedure 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO :
    RUN sys/inc/valpart#.p (get-sv("oe-ordl.part-no"),
    get-sv("oe-ordl.i-no")) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
     /* wfk  APPLY "entry" TO oe-ordl.part-no. */
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-po-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no Procedure 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER cust-po-mand FOR reftable.
  DO :
    FIND FIRST cust NO-LOCK
    WHERE cust.company EQ oe-ord.company
    AND cust.cust-no EQ oe-ord.cust-no
    AND CAN-FIND(FIRST cust-po-mand
    WHERE cust-po-mand.reftable EQ "cust.po-mand"
    AND cust-po-mand.company  EQ cust.company
    AND cust-po-mand.loc      EQ ""
    AND cust-po-mand.CODE     EQ cust.cust-no
    AND cust-po-mand.val[1]   EQ 1)
    NO-ERROR.
    /* If qty is zero at this point, assume it's a transfer */
    IF AVAIL cust AND TRIM(get-sv("oe-ordl.po-no")) EQ "" 
        AND DECIMAL(get-sv("oe-ordl.price")) GT 0 THEN DO:
      MESSAGE "PO# is mandatory for this Customer..."
        VIEW-AS ALERT-BOX ERROR.
        /* wfk APPLY "entry" TO oe-ordl.po-no. */
        RETURN ERROR.
      END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty Procedure 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DO :
    IF DEC(get-sv("ip-focus")) EQ 0 THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " may not be 0, please try again..."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-qty-db) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty-db Procedure 
PROCEDURE valid-qty-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-qty AS INT NO-UNDO.
  DO :
    IF DEC(ip-qty) EQ 0 THEN DO:
      MESSAGE "Qty may not be 0, please try again..."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-s-man) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-man Procedure 
PROCEDURE valid-s-man :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.
  li = ip-int.
  IF li EQ 0 THEN
  ASSIGN
  ip-int = 1
  li     = 3.
  DO ip-int = ip-int TO li :
    lv-sman = IF ip-int EQ 3 THEN get-sv("oe-ordl.s-man[3]")
    ELSE
    IF ip-int EQ 2 THEN get-sv("oe-ordl.s-man[2]")
    ELSE get-sv("oe-ordl.s-man[1]").
    IF lv-sman NE "" THEN DO:
      IF NOT CAN-FIND(FIRST sman
      WHERE sman.company EQ cocode
      AND sman.sman    EQ lv-sman) THEN DO:
        MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
        /* wfk 
        IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordl.s-man[3].
        ELSE
        IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordl.s-man[2].
        ELSE APPLY "entry" TO oe-ordl.s-man[1]. */
        RETURN ERROR.
      END.
    END.
    ELSE DO:
      IF ip-int EQ 3 THEN DO:
        
        set-sv("oe-ordl.s-pct[3]", "0" ).
        set-sv("oe-ordl.s-comm[3]", "0" ).
      END.
      
      ELSE
      IF ip-int EQ 2 THEN DO:
        
        
        set-sv("oe-ordl.s-pct[2]", "0" ).
        set-sv("oe-ordl.s-comm[2]", "0" ).
      END.
      ELSE DO:
        
        set-sv("oe-ordl.s-pct[1]", "0" ).
        set-sv("oe-ordl.s-comm[1]", "0" ).
      END.
      v-margin = 0.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-s-pct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-pct Procedure 
PROCEDURE valid-s-pct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  DEF VAR ld-pct AS DEC NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  /* wfk
  DO :
  /* wfk
  ld-pct = IF ip-int EQ 1 THEN DEC(get-sv("oe-ordl.s-pct[1]"))
  ELSE
  IF ip-int EQ 2 THEN DEC(get-sv("oe-ordl.s-pct[2]"))
  ELSE
  IF ip-int EQ 3 THEN DEC(get-sv("oe-ordl.s-pct[3]"))
  ELSE (get-sv("DEC(oe-ordl.s-pct[1]")) +
  DEC(get-sv("oe-ordl.s-pct[2]")) +
  DEC(get-sv("oe-ordl.s-pct[3]"))). */
  IF (get-sv("oe-ordl.s-man[1]") NE "" OR
  get-sv("oe-ordl.s-man[2]") NE "" OR
  get-sv("oe-ordl.s-man[3]") NE "")   AND
  ( /* (ip-int EQ 0 AND ld-pct NE 100) OR */
  (ip-int NE 0 AND ld-pct GT 100)) THEN DO:
  IF ip-int EQ 0 THEN .
  ELSE
  MESSAGE "Sales Rep Commission % of Sales is over 100%..."
  VIEW-AS ALERT-BOX ERROR.
  IF NOT ll THEN DO:
  IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordl.s-pct[3].
  ELSE
  IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordl.s-pct[2].
  ELSE APPLY "entry" TO oe-ordl.s-pct[1].
  RETURN ERROR.
  END.
  END.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-start-date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-start-date Procedure 
PROCEDURE valid-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-prom-date AS DATE NO-UNDO.
  set-sv("oe-ordl.job-no", "").
  DEF BUFFER bx-ordl FOR oe-ordl.
  DEF VAR lv-first-due-date AS DATE NO-UNDO.
  lv-first-due-date = DATE(get-sv("oe-ordl.req-date")).
  FOR EACH bx-ordl FIELDS(req-date) WHERE
    bx-ordl.company = oe-ordl.company AND
    bx-ordl.job-no = oe-ordl.job-no AND
    bx-ordl.job-no2 = oe-ordl.job-no2 AND
    RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
    lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
    ELSE lv-first-due-date.
  END.
  IF lv-first-due-date = ? THEN
  lv-first-due-date = DATE(get-sv("oe-ordl.req-date")).
  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-wkend AS INT NO-UNDO.
  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
  lv-run-time = 0
  lv-job-time = 0
  lv-maccum-time = 0.
  FOR EACH bf-hdr FIELDS(company job-no job-no2) WHERE
    bf-hdr.company = oe-ord.company AND
    bf-hdr.job-no = oe-ordl.job-no AND
    bf-hdr.job-no2 = oe-ordl.job-no2
    NO-LOCK,
    EACH bf-mch FIELDS(mr-hr run-hr) WHERE
    bf-mch.company = bf-hdr.company AND
    bf-mch.job-no = bf-hdr.job-no AND
    bf-mch.job-no2 = bf-hdr.job-no2
    NO-LOCK:
    ASSIGN
    lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
    TRUNCATE(bf-mch.mr-hr,0) * 3600 +
    ((bf-mch.mr-hr - TRUNCATE(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
    lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
    TRUNCATE(bf-mch.run-hr,0) * 3600 +
    ((bf-mch.run-hr - TRUNCATE(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
    lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END.
  ASSIGN
  lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
  ELSE TRUNCATE(lv-job-time / 3600,0)
  lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN TRUNCATE(lv-job-hr / 8,0) + 1
  ELSE TRUNCATE(lv-job-hr / 8,0)
  lv-start-date = lv-first-due-date - lv-job-day
  lv-prom-date = TODAY + lv-job-day
  lv-update-job-stdate = NO.
  IF lv-start-date < TODAY AND schedule-log THEN DO:
    MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
    "Due Date is before Calculates Promised Date. Update Due Date?"
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-type) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type Procedure 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO :
    IF LOOKUP(get-sv("oe-ordl.type-code"),lv-type-codes) LE 0 OR
    (get-sv("oe-ordl.type-code") EQ "T" AND
    NOT CAN-FIND(FIRST cust WHERE cust.company EQ cocode
    AND cust.cust-no EQ oe-ord.cust-no
    AND cust.active  EQ "X")) THEN DO:
      MESSAGE "Invalid Type, try help..." VIEW-AS ALERT-BOX ERROR.
      /* wfk  APPLY "entry" TO oe-ordl.type-code. */
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-uom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Procedure 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  IF ip-focus:SENSITIVE THEN
  DO :
    RUN get-valid-uom (ip-focus).
    IF get-sv("ip-focus") EQ "" THEN
    set-sv("ip-focus", IF ip-focus:NAME EQ "fi_qty-uom" THEN "EA"
    ELSE "M").
    set-sv("ip-focus", CAPS(TRIM(ip-focus:SCREEN-VALUE)) ).
    lv-uom = get-sv("ip-focus").
    IF NOT CAN-FIND(FIRST uom
    WHERE uom.uom EQ lv-uom
    AND CAN-DO(lv-valid-uom,uom.uom)) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
    IF ip-focus:NAME EQ "fi_qty-uom"             AND
    NOT CAN-DO("," + TRIM(lv-ea-list),lv-uom) AND
    ((lv-uom NE "CS" AND lv-uom NE "PLT") OR
    DEC(get-sv("oe-ordl.cas-cnt")) NE 0)  THEN DO:
      ASSIGN
      ld = DEC(get-sv("oe-ordl.qty"))
      ld = ld * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN DEC(get-sv("oe-ordl.cas-cnt")) ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE 1000) .
      set-sv("oe-ordl.qty", STRING(ld) ).
      set-sv("ip-focus", "EA" ).
      RUN leave-qty.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-uom-db) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-db Procedure 
PROCEDURE valid-uom-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-focus AS CHAR NO-UNDO.
  DEF INPUT PARAM db-field AS CHAR NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  
  DO :

    RUN get-valid-uom (ip-focus, db-field).
    IF ip-focus EQ "" THEN
    ip-focus = (IF db-field = "fi_qty-uom" THEN "EA"
    ELSE "M").
    ip-focus = CAPS(TRIM(ip-focus) ).
    lv-uom = ip-focus.

    IF NOT CAN-FIND(FIRST uom
    WHERE uom.uom EQ lv-uom
    AND CAN-DO(lv-valid-uom,uom.uom)) THEN DO:
      MESSAGE TRIM(db-field) + " is invalid, try help..."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    IF db-field EQ "fi_qty-uom"             AND
    NOT CAN-DO("," + TRIM(lv-ea-list),lv-uom) AND
    ((lv-uom NE "CS" AND lv-uom NE "PLT") OR
    DEC(get-sv("oe-ordl.cas-cnt")) NE 0)  THEN DO:
      ASSIGN
      ld = DEC(get-sv("oe-ordl.qty"))
      ld = ld * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN DEC(get-sv("oe-ordl.cas-cnt")) ELSE
      IF lv-uom EQ "C"  THEN 100 ELSE 1000) .
      set-sv("oe-ordl.qty", STRING(ld) ).
      ip-focus = "EA".
      RUN leave-qty.
    END.

  END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-vend-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no Procedure 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO :
    IF get-sv("oe-ordl.vend-no") NE "" THEN DO:
      FIND FIRST vend
      WHERE vend.company EQ oe-ordl.company
      AND vend.vend-no EQ get-sv("oe-ordl.vend-no")
      NO-LOCK NO-ERROR.
      IF NOT AVAIL vend                                                       OR
      (vend.active NE "A" AND
      (oe-ordl.vend-no NE get-sv("oe-ordl.vend-no") OR ll-new-record)) THEN DO:
          /* wfk 
        IF AVAIL vend THEN
        MESSAGE TRIM(oe-ordl.vend-no:LABEL) + " not active, try help..."
        VIEW-AS ALERT-BOX ERROR.
        ELSE
        MESSAGE "Invalid " + TRIM(oe-ordl.vend-no:LABEL) + ", try help..."
        VIEW-AS ALERT-BOX ERROR. */
        RETURN ERROR.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validate-all) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-all Procedure 
PROCEDURE validate-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipl-autotrans AS LOG NO-UNDO.

  DEF VAR ls-i-no AS cha NO-UNDO.
  DEF VAR ls-part-no AS cha NO-UNDO.
  DEF VAR ls-est-no AS cha NO-UNDO.
  DEF VAR ls-uom AS cha NO-UNDO.
  DEF VAR ll-secure AS LOG NO-UNDO.
  /*DEF VAR v-run-schedule AS LOG NO-UNDO.
  find first sys-ctrl where sys-ctrl.company eq cocode
  and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
  v-run-schedule = NOT (AVAIL sys-ctrl AND sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld).
  */

  IF NOT AVAIL oe-ord THEN
  FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
  AND oe-ord.ord-no  EQ oe-ordl.ord-no
  NO-ERROR.
  DO :
    IF v-est-fg1 = "Hold" AND get-sv("oe-ordl.est-no") <> "" THEN DO:
      FIND FIRST eb WHERE eb.company = cocode AND
      INT(eb.est-no) = INT(get-sv("oe-ordl.est-no")) AND
      eb.stock-no = ""
      NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
        MESSAGE "Sorry, FG item does not exist. Order has not been approved."
        VIEW-AS ALERT-BOX ERROR.
        /* wfk APPLY "ENTRY" TO OE-ORDL.EST-NO. */
        RETURN ERROR.
      END.
    END.
     
    RUN valid-qty-db (oe-ordl.qty) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    
    RUN valid-uom-db (get-sv("fi_qty-uom"), "fi_qty-uom") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
    RUN valid-cas-cnt NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        
      RETURN ERROR. 
    END.
    
    ls-est-no = get-sv("oe-ordl.est-no").
    IF NOT ll-qty-leave-done THEN RUN get-est-cost (ls-est-no).

    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "invalid part"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    /*RUN check-quote (get-sv("oe-ordl.part-no")) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Invalid Item number"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    /* wfk
    set-sv("oe-ordl.i-no", ""   ).
    AND NOT ll-ok-i-no
    then do:
    run display-fgitem no-error.  */
    IF ERROR-STATUS:ERROR THEN
    IF CAN-FIND(FIRST itemfg
    WHERE itemfg.company EQ g_company
    AND itemfg.i-no    EQ get-sv("oe-ordl.i-no")) THEN RETURN ERROR.
    ELSE DO:
      ASSIGN ls-i-no = get-sv("oe-ordl.i-no")
      ls-part-no = get-sv("oe-ordl.part-no")
      ls-uom = get-sv("oe-ordl.pr-uom").
      /* need to check security */
      IF oe-ord.est-no = "" AND get-sv("oe-ordl.est-no") = "" THEN DO:
        RUN sys/ref/d-passwd.w (4, OUTPUT ll-secure).
        IF NOT ll-secure THEN RETURN ERROR.
      END.
      RUN oe/d-citmfg.w (ls-est-no, INPUT-OUTPUT ls-i-no,
      INPUT-OUTPUT ls-part-no,INPUT-OUTPUT ls-uom) NO-ERROR.
      IF ls-i-no = "" THEN DO:
        /* wfk APPLY "entry" TO oe-ordl.i-no. */
        RETURN ERROR.  /* cancel */
      END.
      ELSE DO:
        set-sv("oe-ordl.i-no", ls-i-no ).
        set-sv("oe-ordl.part-no", ls-part-no ).
        FIND FIRST xest WHERE xest.company = g_company
        AND xest.est-no = FILL(" ",8 - LENGTH(TRIM(get-sv("oe-ordl.est-no")))) +
        TRIM(get-sv("oe-ordl.est-no"))
        NO-LOCK NO-ERROR.
        IF AVAIL xest THEN DO:
          FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
          AND xeb.form-no = 0 NO-LOCK NO-ERROR.
          IF NOT AVAIL xeb THEN FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
          AND xeb.form-no = oe-ordl.form-no
          AND xeb.blank-no = oe-ordl.blank-no
          NO-LOCK NO-ERROR.
          IF NOT AVAIL xeb THEN
          FIND FIRST xeb
          WHERE xeb.company EQ g_company
          AND xeb.est-no  EQ xest.est-no
          AND xeb.part-no EQ ls-part-no
          NO-LOCK NO-ERROR.
          IF AVAIL xeb THEN DO:
            FIND xef WHERE xef.company = g_company AND xef.est-no = xeb.est-no
            AND xef.form-no = xeb.form-no
            NO-LOCK NO-ERROR.
            RUN crt-itemfg (ls-i-no, get-sv("oe-ordl.pr-uom")). /*(ls-i-no, "M")*/
          END.
        END.
        ELSE /* no xest or oe-ordl.est-no = "" */
        RUN crt-itemfg (ls-i-no, get-sv("oe-ordl.pr-uom")).
      END.  /* ls-i-no */
      RUN display-fgitem NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    END.
  END.   /* else */
  RUN valid-uom-db (get-sv("oe-ordl.pr-uom"), "oe-ordl.pr-uom") NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.



  IF get-modified("oe-ordl.req-date") AND DATE(get-sv("oe-ordl.req-date")) < oe-ord.ord-date THEN DO:
    MESSAGE "Due Date cannot be earlier than order date..." VIEW-AS ALERT-BOX ERROR.
    set-entry("oe-ordl.req-date").
    RETURN ERROR.
  END.
  IF get-modified("oe-ordl.prom-date") AND DATE(get-sv("oe-ordl.prom-date")) < oe-ord.ord-date THEN DO:
    MESSAGE "Scheduled Date cannot be earlier than order date..." VIEW-AS ALERT-BOX ERROR.
    set-entry("oe-ordl.prom-date").
    RETURN ERROR.
  END.
  IF INDEX(v-duelist,get-sv("oe-ordl.req-code")) <= 0 THEN DO:
    MESSAGE "Invalid Priority Code. " VIEW-AS ALERT-BOX ERROR.
    set-entry("oe-ordl.req-code").
    RETURN ERROR.
  END.
  IF INDEX(v-duelist,get-sv("oe-ordl.prom-code")) <= 0 THEN DO:
    MESSAGE "Invalid Priority Code. " VIEW-AS ALERT-BOX ERROR.
    set-entry("oe-ordl.prom-code").
    RETURN ERROR.
  END.
  IF NOT ipl-autotrans THEN DO:
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.
  RUN valid-vend-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-man (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  RUN valid-s-pct (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  RUN valid-type NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
 

  /*
  IF (oe-ordl.req-date <> date(get-sv("oe-ordl.req-date"))
  AND ip-type = "update")
  AND get-sv("oe-ordl.est-no") <> ""
  AND (v-run-schedule OR schedule-log)
  THEN  DO:
  /*
  RUN valid-start-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  */
  END.*/
  /* end.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validate-due-date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-due-date Procedure 
PROCEDURE validate-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* need more work
  DEF BUFFER bf-job-mch FOR job-mch.
  DEF BUFFER bf-tmp-jmch FOR job-mch.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF var lv-seq-anchored AS LOG NO-UNDO.
  DEF VAR lv-date-wkst AS DATE NO-UNDO.
  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mcode AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  /*===== reschedule job-mch for previous date =======*/
  FOR EACH bf-job-mch WHERE bf-job-mch.company = oe-ordl.company
  AND bf-job-mch.job-no = oe-ordl.job-no
  AND NOT bf-job-mch.job-no2 = oe-ordl.job-no2
  BREAK BY bf-job-mch.start-date
  BY bf-job-mch.seq-no:
  ASSIGN lv-mr-time = lv-mr-time + bf-job-mch.mr-hr
  lv-run-time = lv-run-time + bf-job-mch.run-hr.
  /*
  IF FIRST-OF(bf-job-mch.start-date) THEN DO:
  FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
  AND mach-calendar.m-code = mach.m-code
  AND mach-calendar.m-date = lv-prev-st-date
  NO-LOCK NO-ERROR.
  lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
  /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
  AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
  AND bf-tmp-jmch.m-code = bf-job-mch.m-code
  AND bf-tmp-jmch.start-date = bf-job-mch.start-date
  AND bf-tmp-jmch.seq-no < lv-old-seq
  NO-LOCK BY bf-tmp-jmch.start-time DESC:
  lv-start-time = bf-tmp-jmch.end-time.
  LEAVE.
  END.
  END.
  lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
  truncate(bf-job-mch.mr-hr,0) * 3600 +
  ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
  lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
  truncate(bf-job-mch.run-hr,0) * 3600 +
  ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.
  lv-seq-anchored = YES.
  DO WHILE NOT lv-seq-anchored:
  FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
  /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
  AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
  AND bf-tmp-jmch.m-code = bf-job-mch.m-code
  AND bf-tmp-jmch.start-date = bf-job-mch.start-date
  AND bf-tmp-jmch.seq-no = lv-seq
  AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR.
  IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
  ELSE lv-seq-anchored = NO.
  END.
  ASSIGN bf-job-mch.seq-no = lv-seq
  bf-job-mch.start-time-su = lv-start-time
  bf-job-mch.start-time = lv-start-time + lv-mr-time.
  ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
  ASSIGN  bf-job-mch.start-date-su = bf-job-mch.start-date
  bf-job-mch.end-time = lv-start-time
  bf-job-mch.end-time-su = lv-start-time - lv-run-time
  bf-job-mch.end-date = bf-job-mch.start-date +
  IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
  bf-job-mch.end-date-su = bf-job-mch.start-date-su +
  IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0.
  lv-seq = lv-seq + 10.
  /*      MESSAGE bf-job-mch.seq-no
  string(bf-job-mch.start-time-su,"hh:mm")
  string(bf-job-mch.end-time-su,"hh:mm")
  string(lv-mr-time,"hh:mm")
  string(lv-run-time,"hh:mm")
  string(bf-job-mch.start-time,"hh:mm")
  string(bf-job-mch.end-time,"hh:mm")
  VIEW-AS ALERT-BOX.
  */
  END.
  /* reset seq, time for the job */
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
  AND bf-tmp-jmch.job = job-mch.job
  AND bf-tmp-jmch.job-no =  job-mch.job-no
  AND bf-tmp-jmch.job-no2 =  job-mch.job-no2
  /* AND bf-tmp-jmch.m-code = bf-job-mch.m-code
  AND bf-tmp-jmch.start-date = bf-job-mch.start-date
  AND bf-tmp-jmch.seq-no = lv-seq
  AND bf-tmp-jmch.anchored*/
  AND bf-tmp-jmch.frm >= job-mch.frm
  AND bf-tmp-jmch.blank-no >= job-mch.blank-no
  AND bf-tmp-jmch.m-code > job-mch.m-code
  :
  ASSIGN bf-tmp-jmch.start-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time-su
  bf-tmp-jmch.end-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time-su
  bf-tmp-jmch.start-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time
  bf-tmp-jmch.end-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time
  bf-tmp-jmch.seq-no = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.seq-no
  bf-tmp-jmch.start-date = tt-sch.start-date.
  FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
  AND job-hdr.job = bf-tmp-jmch.job
  AND job-hdr.job-no = bf-tmp-jmch.job-no
  AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
  job-hdr.start-date = tt-sch.start-date.
  FIND FIRST job OF bf-tmp-jmch.
  job.start-date = tt-sch.start-date.
  END.
  */
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validate-fgitem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-fgitem Procedure 
PROCEDURE validate-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-est-no AS cha NO-UNDO.
  DEF BUFFER xeb FOR eb.
  {oe/oe-sysct2.i}
  IF NOT AVAIL oe-ord THEN DO:
    FIND oe-ord WHERE oe-ord.company = cocode AND
    oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.
  END.
  DO  :
    IF get-sv("oe-ordl.est-no") NE "" THEN DO:
      v-est-no = /* wfk INPUT */ oe-ordl.est-no.
      RUN util/rjust.p (INPUT-OUTPUT v-est-no,INPUT 8).
      FIND FIRST xeb WHERE xeb.company   EQ cocode
      AND xeb.est-no    EQ v-est-no
      AND xeb.cust-no   EQ oe-ord.cust-no
      AND xeb.form-no   NE 0
      AND (xeb.est-type EQ 1 OR xeb.est-type EQ 5)
      USE-INDEX est-no NO-LOCK NO-ERROR.
      IF AVAIL xeb                          AND
      xeb.stock-no NE ""                 AND
      xeb.stock-no NE /* wfk INPUT */ oe-ordl.i-no  THEN DO:
        MESSAGE "Item # must match Estimate's Item #" VIEW-AS ALERT-BOX ERROR.
        DISPLAY xeb.stock-no @ oe-ordl.i-no.
        APPLY "entry" TO oe-ordl.i-no.
        RETURN ERROR.
      END.
    END.
    FIND FIRST itemfg WHERE itemfg.company = cocode AND itemfg.i-no EQ /* wfk INPUT */ oe-ordl.i-no
    NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      IF NOT v-est-fg                  AND
      AVAIL xeb                     AND
      itemfg.part-no NE xeb.part-no THEN DO:
        MESSAGE " FG customer part number does not match"
        "estimate's, continue?"
        VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE choice AS LOG.
        IF NOT choice THEN RETURN ERROR.
      END.
      cp-part-no = "".
      IF ll-new-file THEN DO:
        cp-rowid = ROWID(itemfg).
        RUN custom/getcpart.p (cocode, oe-ord.cust-no,
        INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
      END.
      FIND FIRST cust WHERE cust.company = oe-ord.company
      AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
      IF cp-part-no EQ "" AND
      itemfg.cust-no NE oe-ord.cust-no AND itemfg.cust-no NE "" AND
      AVAIL cust AND cust.active NE "X"                         THEN DO:
        FIND FIRST cust WHERE cust.company = oe-ord.company
        AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust AND cust.active NE "X" THEN DO:
          choice = NO.
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = oe-ord.company AND
          sys-ctrl.NAME = "OEITEM" NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND NOT sys-ctrl.log-fld THEN
          MESSAGE "This item exists for a different customer!"
            VIEW-AS ALERT-BOX ERROR.
            ELSE MESSAGE "This item exists for a different customer!  Do you want to continue?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
                IF NOT choice THEN RETURN ERROR.
              END.
            END.
          END.
          IF NOT AVAIL itemfg AND
          (get-sv("oe-ordl.i-no") NE "0") THEN RETURN ERROR.
          IF NOT AVAIL itemfg AND
          get-sv("oe-ordl.i-no") = "0" THEN DO:
            MESSAGE "Invalid FG Item#. Try help. " VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
          END. /* not avail */
        END. /* frame {&frame-name} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-whs-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whs-item Procedure 
PROCEDURE whs-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  RELEASE oe-ordl-whs-item.
  
  IF ip-int EQ 0 THEN DO :
    ASSIGN
    ll-prev-whs-item = AVAIL oe-ordl AND oe-ordl.managed = true     .
    set-sv("tb_whs-item", STRING(ll-prev-whs-item,"yes/no") ).
  END.
  ELSE
  IF AVAIL oe-ordl-whs-item THEN DO TRANSACTION:
    IF ll-prev-whs-item NE tb_whs-item THEN v-qty-mod = YES.
    ll-prev-whs-item = tb_whs-item.
    FIND CURRENT oe-ordl-whs-item.
    oe-ordl-whs-item.val[1] = INT(tb_whs-item).
    FIND CURRENT oe-ordl-whs-item NO-LOCK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-get-handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-handle Procedure 
FUNCTION get-handle RETURNS HANDLE
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
  /*------------------------------------------------------------------------------
  Purpose:
  Notes:
  ------------------------------------------------------------------------------*/
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-itemfg-cost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-itemfg-cost Procedure 
FUNCTION get-itemfg-cost RETURNS DECIMAL
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
  /*------------------------------------------------------------------------------
  Purpose:
  Notes:
  ------------------------------------------------------------------------------*/
  DEF BUFFER bfItemfg FOR itemfg.
  DEF VAR v-cost AS DEC NO-UNDO.
  v-cost = 0.
  FIND FIRST bfItemfg WHERE bfItemfg.company = cocode
  AND bfItemfg.i-no    = ipv-item
  NO-LOCK NO-ERROR.
  IF AVAIL(bfItemfg) THEN
  v-cost = bfItemfg.total-std-cost.
  FIND FIRST fg-ctrl WHERE fg-ctrl.company = cocode NO-LOCK NO-ERROR.
  IF AVAIL fg-ctrl THEN DO:
    IF fg-ctrl.inv-meth = "A" AND bfItemfg.avg-cost GT 0 THEN
    v-cost = bfItemfg.avg-cost.
    ELSE
    IF fg-ctrl.inv-meth = "L" AND bfItemfg.last-cost GT 0 THEN
    v-cost = bfItemfg.last-cost.
  END.
  RETURN v-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-modified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-modified Procedure 
FUNCTION get-modified RETURNS LOGICAL
  ( ipv-item AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-sv Procedure 
FUNCTION get-sv RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
DEF VAR cValue AS CHAR NO-UNDO.
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(h_callproc) THEN
    RUN get-sv IN h_callproc (INPUT ipv-item, OUTPUT cValue).
RETURN cValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-entry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION set-entry Procedure 
FUNCTION set-entry RETURNS CHARACTER
  ( ipv-item AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION set-sv Procedure 
FUNCTION set-sv RETURNS CHARACTER
  ( ipv-item AS CHAR, ipv-value AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(h_callproc) THEN
      RUN set-sv IN h_callproc (INPUT ipv-item, INPUT ipv-value).
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

