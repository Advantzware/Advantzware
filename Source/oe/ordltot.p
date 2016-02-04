/* -------------------------------------------------- oe/ordltot.p 6/93 rd  */
/* o/e module - Calculate order line ext. price                              */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
def shared var save_id as recid.  /* RECORD ID FOR ORDER LINE */
def shared var vmatch as ch.
def shared var v-procat like oe-prmtx.procat.
def shared var uom-list  as char.
def shared var v-i-item like oe-ordl.i-no. /* INPUT ITEM */
def shared var v-i-qty like oe-ordl.qty. /* INPUT QUANTITY */
def shared var head as ch format "x(80)" extent 2.
def shared var v-unline as char format "x(78)".
def shared var opt       as char format "x" extent 13.
def shared var opt-a     as char extent 13.
def shared var opt-b     as char extent 13.
def shared var vmatchid as char.
def shared var v-qty as int.
def shared var v-misc-tot as dec format "->>>,>>>.99".
def shared var v-misc as log init no no-undo.
def shared var v-custype like oe-prmtx.custype.
def shared var v-ord-limit like cust.ord-lim.
def shared var v-crd-limit like cust.cr-lim.
def shared var v-over-limit as log.
def shared var v-tot-ord as dec format "->>>,>>>,>>9.99".
def shared var v-dup-item as log no-undo.
def shared var v-abortord as log format "Yes/No" no-undo.
def shared var v-neword as log no-undo.
def shared var v-stat-chg as log no-undo.
def shared var hld_id as recid.
def shared var xx as int.
def shared var v-gonext as log init no.
def shared var v-bld-job like oe-ordl.job-no.
def shared var v-bld-job2 like oe-ordl.job-no2.
def shared var v-valdcode as char init "ON,BY,MH".
def shared var v-pos as int init 1.
def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
def shared buffer xoe-ordl for oe-ordl.
def shared buffer xoe-ord for oe-ord.
def shared workfile work-ordl like oe-ordl.
def shared var v-est-no like est.est-no no-undo.
def shared var v-job-no like job-hdr.job-no no-undo.
def shared var oe-ordl-recid as recid.

def shared var tmp-t-price like oe-ordl.t-price no-undo.
def shared var tmp-tax like oe-ord.tax no-undo init 0.
def shared var v-tax-rate AS dec format ">,>>9.99<<<".
def shared var v-frt-tax-rate like v-tax-rate.

def shared var first-cust-part-no like oe-ordl.part-no.

def shared var v-job-job like job-hdr.job.
def shared var v-job-no2 like job-hdr.job-no2.

DEF VAR v-print-head LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.

{ce/print4a.i shared}

def shared frame oe-ordlh.
def shared frame oe-ordl.
def shared frame oe-ordlf.

{sys/form/s-top.f}

find first oe-ordl where recid(oe-ordl) = fil_id no-error.
if not avail oe-ordl then return.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPRINT"
    no-lock no-error.
if avail sys-ctrl then
  ASSIGN
   v-print-head = sys-ctrl.log-fld
   v-print-fmt  = sys-ctrl.char-fld.

/*{oe/ordltot.i oe-ordl qty oe-ordl}*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */
