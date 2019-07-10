
def {1} shared var v-post-total         like inv-head.t-inv-rev.
def {1} shared var v-post-freight       like inv-head.t-inv-freight.
def {1} shared var v-post-cash          like inv-head.t-inv-freight.
def {1} shared var v-post-disc          as   dec format "->>>,>>9.99".
def {1} shared var v-post-total-w       AS   DEC NO-UNDO.
def {1} shared var v-post-freight-w     AS   DEC NO-UNDO.
def {1} shared var v-post-cash-w        AS   DEC NO-UNDO.
def {1} shared var v-post-disc-w        AS   DEC NO-UNDO.
def {1} shared var v-cgs-test           as   dec no-undo.

{oe/invwork2.i {1}}

def {1} shared TEMP-TABLE work-job NO-UNDO
  field actnum   like account.actnum
  field amt      like inv-line.t-price
  field weight   as dec
  field fg       as log.

def {1} shared TEMP-TABLE tmp-work-job NO-UNDO
  field actnum   like account.actnum
  field amt      like inv-line.t-price
  field inv-no   like inv-line.inv-no
  field i-no     like inv-line.i-no
  field weight   as dec
  field fg       as log.

def {1} shared TEMP-TABLE w-inv-line NO-UNDO
  field ord-no like inv-line.ord-no
  field i-no like inv-line.i-no
  field i-name like inv-line.i-name
  field qty like inv-line.qty
  field inv-qty like inv-line.inv-qty
  field ship-qty like inv-line.ship-qty
  field price like inv-line.price
  field uom like inv-line.pr-uom
  field t-price like inv-line.t-price
  field weight as dec
  FIELD cost LIKE inv-line.cost
  FIELD t-cost LIKE inv-line.t-cost.

def {1} shared TEMP-TABLE w-ord-misc NO-UNDO
  field ord-no like inv-misc.ord-no
  field charge like inv-misc.charge
  field dscr like inv-misc.dscr
  field amt like inv-misc.amt
  field tax like inv-misc.tax
  field bill like inv-misc.bill.
