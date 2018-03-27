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

{cec/print4.i shared shared}
{cec/print42.i shared}

def var qm as de.
def var v as int.
def var v-comm like tt-tot.
def var v-yld as dec.
DEF VAR li AS INT NO-UNDO.
def var v-prf-s as dec NO-UNDO.
def var v-pct-s as dec NO-UNDO.

{sys/inc/cerun.i C}


{cec/msfcalc.i}

{cec/rollfac.i}

{cec/combasis.i}

find probe where rowid(probe) eq v-rowid no-lock no-error.

if vmclean and avail probe then do:
   

   assign
    /*IF probe.comm NE 0 THEN*/ v-com = probe.comm
    vmcl  = probe.line
    v-yld = IF xest.est-type GE 7 THEN 1 ELSE
            if xest.form-qty eq 1 then 1 else
            (if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty)
    qty   = probe.est-qty
    qm    = qty / 1000 * v-sqft-fac.

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

      v-yld = IF xest.est-type GE 7 THEN 1 ELSE
              if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty

      v-prf-s = probeit.sell-price - probeit.fact-cost
      v-pct-s = ROUND(v-prf-s / probeit.fact-cost * 100,2).

      {cec/pr4-mcl1.i "probeit" "first-of(eb.form-no)"}

      if (not vsuthrlnd) AND /*cerunc NE "Protagon"*/ LOOKUP(cerunc,"Protagon,CERunC 3") = 0 then do:
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
       v-form-no = IF xest.est-type EQ 5 THEN 0 ELSE 1
       v-yld     = 1
       v-prf-s   = probe.sell-price - probe.fact-cost
       v-pct-s   = ROUND(v-prf-s / probe.fact-cost * 100,2).

      {cec/pr4-mcl1.i "probe" "yes"}
   end.
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
