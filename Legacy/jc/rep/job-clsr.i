
EMPTY TEMP-TABLE work-mch.

{jc/rep/job-sumr.i}

assign
 v-est-lab-cost = 0
 v-est-foh-cost = 0
 v-est-voh-cost = 0
 v-act-lab-cost = 0
 v-act-foh-cost = 0
 v-act-voh-cost = 0.
       
for each work-mch:
  assign
   v-est-lab-cost = v-est-lab-cost          +
                    work-mch.est-mr-cost1   +
                    work-mch.est-run-cost1
   v-est-foh-cost = v-est-foh-cost          +
                    work-mch.est-mr-cost2   +
                    work-mch.est-run-cost2
   v-est-voh-cost = v-est-voh-cost          +
                    work-mch.est-mr-cost3   +
                    work-mch.est-run-cost3
   v-act-lab-cost = v-act-lab-cost          +
                    work-mch.mr-cost1       +
                    work-mch.run-cost1
   v-act-foh-cost = v-act-foh-cost          +
                    work-mch.mr-cost2       +
                    work-mch.run-cost2
   v-act-voh-cost = v-act-voh-cost          +
                    work-mch.mr-cost3       +
                    work-mch.run-cost3.

  delete work-mch.
end.
