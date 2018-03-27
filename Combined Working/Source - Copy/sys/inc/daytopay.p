/* --------------------------------------------- sys/inc/daytopay.p 12/02 JLF */
/* Calculate avg number of days to pay on Customer File                       */
/* -------------------------------------------------------------------------- */

def input parameter ip-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

/* def var v-days as int no-undo. */
/* def var v-invs as int no-undo. */


/* find first sys-ctrl                                                             */
/*     where sys-ctrl.company eq cocode                                            */
/*       and sys-ctrl.name    eq "AGEDAYS"                                         */
/*     no-lock no-error.                                                           */
/* if not avail sys-ctrl then do:                                                  */
/*   create sys-ctrl.                                                              */
/*   assign                                                                        */
/*    sys-ctrl.company  = cocode                                                   */
/*    sys-ctrl.name     = "AGEDAYS"                                                */
/*    sys-ctrl.descrip  =                                                          */
/*        "Calc Avg Days to Pay only with invoices paid within (Leave 0 for all)". */
/*                                                                                 */
/*   message sys-ctrl.descrip update sys-ctrl.int-fld.                             */
/* end.                                                                            */

find first ar-cashl where recid(ar-cashl) eq ip-recid no-lock no-error.

if avail ar-cashl then
find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock no-error.

if avail ar-cash and ar-cashl.inv-no ne 0 then
find first ar-inv
    where ar-inv.company eq cocode
      and ar-inv.inv-no  eq ar-cashl.inv-no
    no-lock no-error.

if avail ar-inv then
find first cust
    where cust.company eq ar-inv.company
      and cust.cust-no eq ar-inv.cust-no
    NO-LOCK no-error.

if avail cust then do:
    RUN ar/calcAvgDays.p (ROWID(cust), YES, (ar-cash.check-date - ar-inv.inv-date)).
/*   if cust.avg-pay lt 1 or cust.avg-pay eq ? then cust.avg-pay = 1. */
/*                                                                    */
/*   if sys-ctrl.int-fld eq 0 then                                    */
/*     cust.avg-pay = ((cust.num-inv * cust.avg-pay) +                */
/*                     (ar-cash.check-date - ar-inv.inv-date)) /      */
/*                    (cust.num-inv + 1).                             */
/*                                                                    */
/*   else do:                                                         */
/*     assign                                                         */
/*      v-days = 0                                                    */
/*      v-invs = 0.                                                   */
/*                                                                    */
/*     for each ar-inv                                                */
/*         where ar-inv.company  eq cocode                            */
/*           and ar-inv.posted   eq yes                               */
/*           and ar-inv.cust-no  eq cust.cust-no                      */
/*           and ar-inv.due      le 0                                 */
/*           and ar-inv.pay-date ge (today - sys-ctrl.int-fld)        */
/*         use-index posted-due no-lock:                              */
/*                                                                    */
/*       assign                                                       */
/*        v-days = v-days + (ar-inv.pay-date - ar-inv.inv-date)       */
/*        v-invs = v-invs + 1.                                        */
/*     end.                                                           */
/*                                                                    */
/*     cust.avg-pay = v-days / v-invs.                                */
/*   end.                                                             */
/*                                                                    */
/*   if cust.avg-pay lt 1 or cust.avg-pay eq ? then cust.avg-pay = 1. */
end.

/* end ---------------------------------- copr. 2002  advanced software, inc. */
