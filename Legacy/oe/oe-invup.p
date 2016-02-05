/* -------------------------------------------------- oe/oe-invup.p 08/96 JLF */
/* update invoice from current invoice lines                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-misc-charge AS LOG NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var hold_invoice_id      as   recid no-undo.
def var v-cred-lim           as   log no-undo.


run oe/surchrg.p (ip-rowid).

find inv-head WHERE ROWID(inv-head) EQ ip-rowid EXCLUSIVE-LOCK.

v-cred-lim = inv-head.bol-no eq 0 and inv-head.terms ne "CASH" and
             not program-name(2) begins "oe/oe-bolp3.".

find first cust no-lock
    where cust.company = inv-head.company
      and cust.cust-no = inv-head.cust-no.
      
if (inv-head.bol-no ne 0                       and
    not program-name(2) begins "oe/oe-invde."  and
    not program-name(2) begins "oe/oe-bolp3.")      or
   v-cred-lim                                       then do:
  FIND CURRENT cust.
  cust.ord-bal = cust.ord-bal - inv-head.t-inv-rev.
  FIND CURRENT cust NO-LOCK.
end.
  
RUN oe/oeinvup2.p (ROWID(inv-head), INPUT ip-misc-charge).

/*FIND CURRENT inv-head EXCLUSIVE NO-ERROR.

if v-cred-lim and inv-head.stat ne "H" then do:
  if cust.cr-hold-invdays gt 0 then do:
    run oe/creditid.p (input recid(cust), output hold_invoice_id).
    if hold_invoice_id ne ? then do:
      bell.
      message "WARNING: Customer has exceeded invoice age limit. "
              "Invoice will be put on HOLD.".
      pause 10.
      inv-head.stat = "H".
    end.
  end.

  else
  if cust.acc-bal + cust.ord-bal + inv-head.t-inv-rev gt cust.cr-lim then do:
    bell.
    message "WARNING: Customer has exceeded credit limit. "
            "Invoice will be put on HOLD.".
    pause 10.
    inv-head.stat = "H".
  end.

  else
  if cust.ord-bal + inv-head.t-inv-rev gt cust.ord-lim then do:
    bell.
    message "WARNING: Customer has exceeded order limit. "
            "Invoice will be put on HOLD.".
    pause 10.
    inv-head.stat = "H".
  end.
end.
    
if (inv-head.bol-no ne 0                       and
    not program-name(2) begins "oe/oe-invde."  and
    not program-name(2) begins "oe/oe-bolp3.")      or
   v-cred-lim                                       then do:
  FIND CURRENT cust.
  cust.ord-bal = cust.ord-bal + inv-head.t-inv-rev.
  FIND CURRENT cust NO-LOCK.
end.*/

IF v-cred-lim AND inv-head.stat NE "H" THEN
  RUN oe/creditck.p (ROWID(inv-head), YES).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
