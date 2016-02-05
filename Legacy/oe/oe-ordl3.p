/* -------------------------------------------------- oe/oe-ordl3.p 02/96 FWK */
/* Order entry lines - o/e module                                             */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.
DEF INPUT PARAMETER ip-nufile AS LOG NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var save_id as recid.  /* RECORD ID FOR ORDER LINE */
def shared var vmatch as ch.
def shared var v-procat like oe-prmtx.procat.
def shared var uom-list  as char.
def shared var v-i-item like oe-ordl.i-no. /* INPUT ITEM */
def shared var v-i-qty like oe-ordl.qty. /* INPUT QUANTITY */
def shared var head as ch format "x(80)" extent 2.
def shared var v-unline as char format "x(78)".
def shared var opt       as char format "x" extent 14.
def shared var opt-a     as char extent 14.
def shared var opt-b     as char extent 14.
def shared var vmatchid as char.
def shared var v-qty as int.
def shared var v-misc-tot as dec format "->>>,>>>.99".
def shared var v-misc as logical initial no no-undo.
def shared var v-custype like oe-prmtx.custype.
def shared var v-ord-limit like cust.ord-lim.
def shared var v-crd-limit like cust.cr-lim.
def shared var v-over-limit as logical.
def shared var v-tot-ord as dec format "->>>,>>>,>>9.99".
def shared var v-dup-item as logical no-undo.
def shared var v-abortord as logical format "Yes/No" no-undo.
def shared var v-neword as logical no-undo.
def shared var v-stat-chg as logical no-undo.

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
def shared buffer xoe-ordl for oe-ordl.
def shared buffer xoe-ord for oe-ord.
def shared workfile work-ordl like oe-ordl.

def shared var tmp-t-price like oe-ordl.t-price no-undo.
def shared var tmp-tax like oe-ord.tax no-undo initial 0.
def shared var v-tax-rate AS DEC FORMAT ">,>>9.99<<<".
def shared var v-frt-tax-rate LIKE v-tax-rate.  /* 9508 CAH */
def buffer tmp-oe-ordl for oe-ordl.

def shared var first-cust-part-no like oe-ordl.part-no.
def var v-qty# as integer no-undo.
def var recid# as recid no-undo.
DEF VAR v-tax-amt AS DEC NO-UNDO INIT 0. /* Tax amount */

{ce/print4a.i shared}


{sys/inc/oecredit.i}

find first oe-ordl where recid(oe-ordl) eq ip-recid no-error.
if avail oe-ordl then
do:
  run oe/creditck.p.   /***  CUSTOMER CREDIT CHECK  ***/
  
  find oe-ord where recid(oe-ord) = recid(xoe-ord) no-error.
    assign oe-ord.t-weight  = oe-ord.t-weight  + oe-ordl.t-weight
           oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight.
  find first itemfg {sys/look/itemfgrl.w}
             and itemfg.i-no = oe-ordl.i-no no-lock no-error.
  if available itemfg and oe-ordl.qty ne 0 then
    assign xxx = oe-ordl.cost * (oe-ordl.qty / 1000)
           oe-ordl.t-cost = xxx
           oe-ord.t-cost  = oe-ord.t-cost  + xxx.

  assign tmp-t-price = oe-ordl.t-price - tmp-t-price.

  /******* FIND TAX RATES FOR THIS CUSTOMER ***************/
  run ar/cctaxrt.p (input cocode, oe-ord.tax-gr,
                    output v-tax-rate, output v-frt-tax-rate).

  find first cust
      where cust.company eq oe-ordl.company
        and cust.cust-no eq oe-ordl.cust-no
      no-error.
  if avail cust and (tmp-t-price ne 0) and not cust.cr-hold and
     index("HA",xoe-ord.stat) eq 0                          and
     (oecredit-log or ip-nufile)                            then do:
    if (((if oe-ord.f-bill then oe-ord.t-freight else 0) + oe-ord.tax +
        v-tot-ord + tmp-t-price) > cust.ord-lim) then do:
      MESSAGE "   Customer:" cust.name SKIP
              "   Has Exceeded Their Order Limit.        "
              "   The Order Status Will Be Set To HOLD.   "
              VIEW-AS ALERT-BOX.

      assign cust.cr-hold = YES
             oe-ord.stat = "H".
    end.
  end.

  assign cust.ord-bal = cust.ord-bal + tmp-t-price.

  /******* CALCULATE TAX FOR THIS ORDER ***************/
  ASSIGN oe-ord.tax = 0.

  FOR EACH tmp-oe-ordl NO-LOCK OF oe-ord:
    IF tmp-oe-ordl.tax AND v-tax-rate > 0 THEN DO:

        RUN ar/calctax2.p (oe-ord.tax-gr,
                           NO,
                           tmp-oe-ordl.t-price,
                           oe-ord.company,
                           tmp-oe-ordl.i-no,
                           OUTPUT v-tax-amt).

        ASSIGN oe-ord.tax = oe-ord.tax + v-tax-amt.
    END.
    
  END.

  assign tmp-tax = oe-ord.tax - tmp-tax.

  if (cust.ord-bal + cust.acc-bal + tmp-tax) > cust.cr-lim and
     cust.cr-hold ne yes and index("HA",xoe-ord.stat) eq 0 and
     (oecredit-log or ip-nufile)                           then do:
    MESSAGE "   Customer:" cust.name SKIP
            "   Has Exceeded Their Credit Limit.        "
            "   The Order Status Will Be Set To HOLD.   "
            VIEW-AS ALERT-BOX.
    assign cust.cr-hold = YES
           oe-ord.stat = "H".
  end.

  assign cust.ord-bal = cust.ord-bal + tmp-tax.

  run oe/assignfg.p(recid(oe-ordl)).
end.
/* end ---------------------------------- copr. 1992  advanced software, inc. */

