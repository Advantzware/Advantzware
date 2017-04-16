/* -------------------------------------------------- oe/invmpost.i 9/94 RM  */
/* O/E Invoicing Misc. Charges History Assigment of Fields                   */
/* -------------------------------------------------------------------------- */

ASSIGN
  ar-invl.x-no       = v-XNO
  ar-invl.company    = inv-misc.company
  ar-invl.INV-NO     = inv-head.inv-no
  ar-invl.ord-no     = inv-misc.ord-no
  ar-invl.cust-no    = inv-head.cust-no
  v-XLINE            = v-XLINE + 1
  ar-invl.line       = v-XLINE
  ar-invl.est-no     = inv-misc.est-no
/*
  ar-invl.job-no     = inv-misc.job-no
  ar-invl.job-no2    = inv-misc.job-no2
*/
  ar-invl.tax        = inv-misc.tax
  ar-invl.actnum     = inv-misc.actnum
  ar-invl.prep-amt   = inv-misc.amt
  ar-invl.qty        = 1
  ar-invl.unit-pr    = inv-misc.amt
  ar-invl.amt        = inv-misc.amt
  ar-invl.t-cost     = inv-misc.cost
  ar-invl.cost       = ar-invl.t-cost / 1000
  ar-invl.dscr[1]    = "M"
  ar-invl.prep-charge = inv-misc.charge
  ar-invl.prep-cost  = inv-misc.cost
  ar-invl.prep-dscr  = inv-misc.dscr
  ar-invl.i-name     = inv-misc.charge
  ar-invl.i-dscr     = inv-misc.dscr
  ar-invl.po-no      = inv-misc.po-no
  ar-invl.po-no-po   = inv-misc.po-no-po
  ar-invl.sman[1]    = inv-misc.s-man[1]
  ar-invl.sman[2]    = inv-misc.s-man[2]
  ar-invl.sman[3]    = inv-misc.s-man[3]
  ar-invl.s-pct[1]   = inv-misc.s-pct[1]
  ar-invl.s-pct[2]   = inv-misc.s-pct[2]
  ar-invl.s-pct[3]   = inv-misc.s-pct[3]
  ar-invl.s-comm[1]  = inv-misc.s-comm[1]
  ar-invl.s-comm[2]  = inv-misc.s-comm[2]
  ar-invl.s-comm[3]  = inv-misc.s-comm[3]
  ar-invl.s-commbasis[1] = inv-misc.s-commbasis[1]
  ar-invl.s-commbasis[2] = inv-misc.s-commbasis[2]
  ar-invl.s-commbasis[3] = inv-misc.s-commbasis[3]
  ar-invl.inv-i-no   = inv-misc.inv-i-no
  ar-invl.inv-line   = inv-misc.inv-line
  ar-invl.misc       = YES
  ar-invl.billable   = inv-misc.bill EQ "Y"
  ar-invl.spare-char-1 = inv-misc.spare-char-1
  ar-invl.posted     = YES.

IF NOT ar-invl.billable THEN ar-invl.amt = 0.
