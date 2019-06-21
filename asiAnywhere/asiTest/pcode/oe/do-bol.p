/* ---------------------------------------------------- oe/do-bol.p 01/98 JLF */
/* order entry - Create actual release and BOL from planned release line      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i SHARED}

DEFINE INPUT PARAMETER ip-check-back-release AS LOG NO-UNDO.

DEF SHARED BUFFER xoe-ord FOR oe-ord.

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER bf-rell FOR oe-rell.
DEFINE SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEF SHARED VAR a-user AS CHAR NO-UNDO.

DEF SHARED VAR out-recid AS RECID NO-UNDO.

DEF VAR v-first-release AS log NO-UNDO.
DEF VAR v-r-no LIKE inv-head.r-no NO-UNDO.
DEF VAR v-tax-rate AS DEC FORMAT ">,>>9.99<<<" NO-UNDO.
DEF VAR v-ext-price LIKE inv-line.t-price NO-UNDO.
DEF VAR v-nxt-r-no AS INT NO-UNDO.
DEF VAR v-royal AS LOG NO-UNDO.
DEF VAR v-po-no LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-n-bol LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR v-bol-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR v-hold-list AS CHAR NO-UNDO.
DEF VAR choice AS log NO-UNDO.
DEF VAR ll-invoice AS LOG NO-UNDO.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR vfrt-pay AS CHAR NO-UNDO.
DEF VAR vfob-code AS CHAR NO-UNDO.
DEF VAR rell-ctr AS INTE NO-UNDO.
DEF VAR vfrt-list AS CHAR NO-UNDO.
DEF VAR vfob-list AS CHAR NO-UNDO.

DEF VAR ll AS LOG NO-UNDO.

v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

{sys/ref/relpost.i}

DO TRANSACTION:
  {sys/inc/boldate.i}
END.


{sa/sa-sls01.i}

ASSIGN
   choice = NO
   lv-cust-x = "".

FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BOLFMT"
    NO-ERROR.
IF AVAIL sys-ctrl THEN
  ASSIGN
   v-royal = CAN-DO(v-hold-list,sys-ctrl.char-fld)
   choice  = sys-ctrl.char-fld EQ "P&P".

FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-ERROR.

IF AVAIL oe-rell THEN headblok:
FOR EACH oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no,
    FIRST cust NO-LOCK
    WHERE cust.company EQ oe-relh.company
      AND cust.cust-no EQ oe-relh.cust-no:
  /*IF choice THEN DO ON ENDKEY UNDO, RETRY:
    MESSAGE "BOL for Release Date-" + TRIM(STRING(oe-relh.rel-date)) +
            " and ShipID-" + TRIM(oe-relh.ship-id).
    MESSAGE "Create Bill of Lading with Todays Date?"
        UPDATE choice.
  END.*/
    
    choice = YES.

  ll-invoice = oe-rell.s-code EQ "I".
 
  DO TRANSACTION:
    oe-relh.printed = YES.
 
    {oe/oe-bolno.i}
  END.

  {oe/do-bol.i}

 
  DO TRANSACTION:
    RUN oe/palcal2.p(ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets).

    RUN oe/bolhfrat.p (ROWID(oe-bolh),
                       oe-bolh.cust-no,
                       oe-bolh.ship-id,
                       oe-bolh.carrier,
                       OUTPUT oe-bolh.freight).
     
    IF oe-bolh.freight EQ 0 AND AVAIL xoe-ord THEN
      oe-bolh.freight = xoe-ord.t-freight.

    IF choice THEN oe-bolh.bol-date = TODAY.
  END.
      
  IF ll-invoice THEN
  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:
    {oe/seq-bolh.i}
  END.
END.

RUN oe/oe-bolp7.p (v-term, ip-check-back-release).



/* end ---------------------------------- copr. 1998  advanced software, inc. */
