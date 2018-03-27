/* ------------------------------------------------- cec/est-summ.p 05/97 JLF */
/* Update mclean totals                                                       */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var qty as int no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}
{ce/print42.i shared}

DEF BUFFER b-probemk FOR reftable.
DEF BUFFER probe-ref FOR reftable.

def var qm as de.
def var v as int.
def var v-comm AS DEC.
DEF VAR v-royl AS DEC NO-UNDO.
DEF VAR v-ware AS DEC NO-UNDO.
DEF VAR v-cust AS DEC NO-UNDO.
DEF VAR v-nman AS DEC NO-UNDO.
def var v-yld as dec.
DEF VAR li AS INT NO-UNDO.
def var v-prf-s as dec NO-UNDO.
def var v-pct-s as dec NO-UNDO.


{ce/msfcalc.i}

{cec/combasis.i}

find probe where rowid(probe) eq v-rowid no-lock no-error.

if vmclean and avail probe then do:
   assign
    v-com = probe.comm
    vmcl  = probe.line
    v-yld = IF xest.est-type GE 3 THEN 1 ELSE
            if xest.form-qty eq 1 then 1 else
            (if xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%)
    qty   = probe.est-qty
    qm    = qty / 1000.

   if vmclean2 then
   for each probeit
       where probeit.company eq probe.company
         and probeit.est-no  eq probe.est-no
         and probeit.line    eq probe.line
       no-lock,
       first eb where eb.company eq xest.company 
                  and eb.est-no  eq xest.est-no
                  and eb.part-no eq probeit.part-no
                  and eb.form-no ne 0
                NO-LOCK
       BREAK BY eb.form-no:

      /* this is replaced...
      find first blk where blk.id eq probeit.part-no no-error.
      find first eb where eb.company  eq xest.company 
                      and eb.est-no   eq xest.est-no
                      and eb.form-no  eq blk.snum
                      and eb.blank-no eq blk.bnum no-lock no-error.

      v-form-no = blk.snum.*/

      /* ...with... */

      ASSIGN
       v-form-no = eb.form-no
      /* ..this */

       v-yld = IF xest.est-type GE 3 THEN 1 ELSE
               if eb.cust-% lt 0 then -1 / eb.cust-% else eb.cust-%
       v-prf-s = probeit.sell-price - probeit.fact-cost
       v-pct-s = ROUND(v-prf-s / probeit.fact-cost * 100,2)
       v-comm  = 0
       v-royl  = 0
       v-ware  = 0
       v-cust  = 0.

      FIND FIRST b-probemk NO-LOCK
          WHERE b-probemk.reftable EQ "ce/com/probemk.p"
            AND b-probemk.company  EQ probeit.company
            AND b-probemk.loc      EQ probeit.est-no
            AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
            AND b-probemk.code2    EQ probeit.part-no
          NO-ERROR.
      IF AVAIL b-probemk THEN
        ASSIGN
         v-comm = b-probemk.val[6]
         v-royl = b-probemk.val[7]
         v-ware = b-probemk.val[8]
         v-cust = b-probemk.val[9].

      {ce/pr4-mcl1.i "probeit" "first-of(eb.form-no)"}

      if (not vsuthrlnd) then do:
        vmcl-desc = "OVERALL".
        {cec/est-summ.i vmcl-desc vmcl-cost}
        IF AVAIL est-summ THEN DO:
          IF FIRST-OF(eb.form-no) THEN est-summ.per-m = 0.
          est-summ.per-m = est-summ.per-m +
                           (probeit.sell-price /
                            if v-corr then (eb.t-sqin * .007)
                                      else (eb.t-sqin / 144)).
        END.
      end.
   end.

   else do:
      ASSIGN
       v-form-no = 1 /*IF xest.est-type EQ 1 THEN 0 ELSE 1*/
       v-yld     = 1
       v-prf-s   = probe.sell-price - probe.fact-cost
       v-pct-s   = ROUND(v-prf-s / probe.fact-cost * 100,2).

      FIND FIRST probe-ref NO-LOCK
          WHERE probe-ref.reftable EQ "probe-ref"
            AND probe-ref.company  EQ probe.company
            AND probe-ref.loc      EQ ""
            AND probe-ref.code     EQ probe.est-no
            AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
          NO-ERROR.
      IF AVAIL probe-ref THEN
        ASSIGN
         v-comm = probe-ref.val[6] / qm
         v-royl = probe-ref.val[7] / qm
         v-ware = probe-ref.val[8] / qm
         v-cust = probe-ref.val[9] / qm.

      {ce/pr4-mcl1.i "probe" "yes"}
   end.
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
