/* ----------------------------------------------- sys/ref/ce-ctrl.v 1/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

if ce-ctr.avg-cscost = 0 then choice = no.
else choice = yes.

if index("SB",ce-ctrl.sell-by) gt 0 then
  assign
   head[2] = " ====== MSF SELL PRICE MATRIX ======="
   head[5] = "    MSF Markup%     MSF Markup%".

else
  assign
   head[2] = " ===== GS&A MARK UP PERCENTAGES ====="
   head[5] = "   Cost  Mat'l%    Cost  Labor%".

find last quote use-index qno no-lock no-error.
  
display
  ce-ctrl.e-num
  ce-ctrl.e-range[1]
  ce-ctrl.e-range[2]
  ce-ctrl.q-num
  quote.q-no when avail quote @ ce-ctrl.q-num
  ce-ctrl.q-range[1]
  ce-ctrl.q-range[2]
  ce-ctrl.sho-labor
/*  ce-ctrl.trunc-99 */
  ce-ctrl.ls-width
  ce-ctrl.ls-length
  ce-ctrl.ls-trimw
  ce-ctrl.ls-triml
  head[1]
  ce-ctrl.def-ink
  ce-ctrl.def-case
  ce-ctrl.def-pal
  ce-ctrl.def-inkcov
  ce-ctrl.def-coat
  "" @ choice
  "Sheet Fed" when ce-ctrl.avg-cscost = 0  @ choice
  "Roll  Fed" when ce-ctrl.avg-cscost ne 0 @ choice
  ce-ctrl.fg-rate
  ce-ctrl.rm-rate
  ce-ctrl.spec-l[1 for 3]
  ce-ctrl.spec-%[1 for 3]
  ce-ctrl.hand-pct
  ce-ctrl.whse-mrkup
  ce-ctrl.comm-mrkup
  ce-ctrl.r-cost
  head[2]
  head[3]
  head[4]
  head[5]
  ce-ctrl.mat-cost[1]
  ce-ctrl.mat-cost[2]
  ce-ctrl.mat-cost[3]
  ce-ctrl.mat-cost[4]
  ce-ctrl.mat-cost[5]
  ce-ctrl.mat-cost[6]
  ce-ctrl.mat-pct[1]
  ce-ctrl.mat-pct[2]
  ce-ctrl.mat-pct[3]
  ce-ctrl.mat-pct[4]
  ce-ctrl.mat-pct[5]
  ce-ctrl.mat-pct[6]
  ce-ctrl.lab-cost[1]
  ce-ctrl.lab-cost[2]
  ce-ctrl.lab-cost[3]
  ce-ctrl.lab-cost[4]
  ce-ctrl.lab-cost[5]
  ce-ctrl.lab-cost[6]
  ce-ctrl.lab-pct[1]
  ce-ctrl.lab-pct[2]
  ce-ctrl.lab-pct[3]
  ce-ctrl.lab-pct[4]
  ce-ctrl.lab-pct[5]
  ce-ctrl.lab-pct[6]
  ce-ctrl.hd-net
  ce-ctrl.hd-gross
  ce-ctrl.sell-by
  ce-ctrl.prof-mrkup
  ce-ctrl.comm-add
  ce-ctrl.shp-add
  ce-ctrl.spec-add[1 for 3]
  ce-ctrl.spec-add[6 for 3].
  
do i = 1 to 3:
  v-spec[i] = if ce-ctrl.spec-%[i] eq 0 then ""  else
              if ce-ctrl.spec-%[i] ge 1 then "$" else "%".
              
  display ce-ctrl.spec-%[i] * 100 when v-spec[i] eq "%" @
            ce-ctrl.spec-%[i]
          v-spec[i].
end.
  

/* end ---------------------------------- copr. 1992  advanced software, inc. */
