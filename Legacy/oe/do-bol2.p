/* create bol from oe-rel instead of oe-rell for GUI
   ---------------------------------------------------- oe/do-bol.p 01/98 JLF */
/* order entry - Create actual release and BOL from planned release line      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xoe-ord for oe-ord.

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER bf-rell FOR oe-rell.

def var v-first-release as log no-undo.
def var v-r-no like inv-head.r-no no-undo.
def var v-tax-rate as dec format ">>9.99" no-undo.
def var v-ext-price like inv-line.t-price no-undo.
def var v-nxt-r-no as int no-undo.
def var v-royal as log no-undo.
DEF VAR v-return-freight AS DEC NO-UNDO.
def var v-po-no like oe-rel.po-no no-undo.
def var v-n-bol like oe-bolh.bol-no no-undo.
def var v-bol-qty like oe-boll.qty no-undo.
def var v-hold-list as char no-undo.
DEF VAR vfrt-pay AS CHAR NO-UNDO.
DEF VAR vfob-code AS CHAR NO-UNDO.
DEF VAR rell-ctr AS INTE NO-UNDO.
DEF VAR vfrt-list AS CHAR NO-UNDO.
DEF VAR vfob-list AS CHAR NO-UNDO.
  
def shared var out-recid as recid no-undo.
def var choice as log no-undo.

v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

{sys/ref/relpost.i}
DO TRANSACTION:
  {sys/inc/boldate.i}
END.

choice = no.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOLFMT"
    no-lock no-error.
if avail sys-ctrl then
  assign
   v-royal = lookup(sys-ctrl.char-fld,v-hold-list) ne 0
   choice  = sys-ctrl.char-fld eq "P&P".

find oe-rell where recid(oe-rell) eq out-recid no-error.

if avail oe-rell then headblok:
for each oe-relh where oe-relh.r-no eq oe-rell.r-no:
  if choice then do on endkey undo, retry:
    message "BOL for Release Date-" + trim(string(oe-relh.rel-date)) +
            " and ShipID-" + trim(oe-relh.ship-id).
    message "Create Bill of Lading with Todays Date?" update choice.
  end.

  do transaction:
    oe-relh.printed = yes.
    oe-relh.spare-char-3 = USERID("NOSWEAT").
    {oe/oe-bolno.i}
  end.
  
  {oe/do-bol.i}

  do transaction:
    run oe/palcal2.p(recid(oe-bolh), OUTPUT oe-bolh.tot-pallets).
    if choice then oe-bolh.bol-date = today.
/*     RUN oe/getBolFrt.p (ROWID(oe-bolh),          */
/*                        oe-bolh.cust-no,          */
/*                        oe-bolh.ship-id,          */
/*                        oe-bolh.carrier,          */
/*                        OUTPUT v-return-freight). */
    RUN oe/calcBolFrt.p (INPUT ROWID(oe-bolh), OUTPUT v-return-freight).     
    IF v-return-freight GT 0 THEN 
      oe-bolh.freight = v-return-freight.

  end.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

