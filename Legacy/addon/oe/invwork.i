
def {1} shared var v-post-total         like inv-head.t-inv-rev.
def {1} shared var v-post-freight       like inv-head.t-inv-freight.
def {1} shared var v-post-cash          like inv-head.t-inv-freight.
def {1} shared var v-post-disc          as   dec format "->>>,>>9.99".
def {1} shared var v-cgs-test           as   dec no-undo.

def {1} shared workfile work-job
  field actnum   like account.actnum
  field amt      like inv-line.t-price
  field fg       as log.

def {1} shared workfile tmp-work-job
  field actnum   like account.actnum
  field amt      like inv-line.t-price
  field inv-no   like inv-line.inv-no
  field i-no     like inv-line.i-no
  field fg       as log.

def {1} shared workfile w-inv-line
  field ord-no like inv-line.ord-no
  field i-no like inv-line.i-no
  field i-name like inv-line.i-name
  field qty like inv-line.qty
  field inv-qty like inv-line.inv-qty
  field ship-qty like inv-line.ship-qty
  field price like inv-line.price
  field uom like inv-line.pr-uom
  field t-price like inv-line.t-price.

def {1} shared workfile w-ord-misc
  field ord-no like inv-misc.ord-no
  field charge like inv-misc.charge
  field dscr like inv-misc.dscr
  field amt like inv-misc.amt
  field tax like inv-misc.tax
  field bill like inv-misc.bill.
