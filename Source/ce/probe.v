/* ------------------------------------------------------ ce/probe.v 5/92 cd  */
/*                                                                            */
/* form statement - state file                                                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

FIND FIRST probe-ref NO-LOCK
    WHERE probe-ref.reftable EQ "probe-ref"
      AND probe-ref.company  EQ probe.company
      AND probe-ref.loc      EQ ""
      AND probe-ref.code     EQ probe.est-no
      AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.
                           
if cerunf eq "Dee" and avail probe-ref then
  v-contrib-fac = probe-ref.val[10] / (probe.est-qty / 1000).

else
  v-contrib-fac = probe.sell-price -
                  (probe.fact-cost + probe.freight +
	           (probe.sell-price * (xeb.comm / 100))).

v-contrib-fac =	v-contrib-fac /	probe.sell-price * 100.

if v-contrib-fac lt 0 then v-contrib-fac = 0.

display
     probe.est-qty
     probe.freight
     probe.fact-cost
     probe.full-cost
     probe.net-profit
     probe.gross-profit
     probe.sell-price
     v-contrib-fac
     probe.gsh-qty
     probe.tot-lbs.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
