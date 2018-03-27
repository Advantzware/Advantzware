for each job-prep FIELDS(ml std-cost qty)
    where job-prep.company eq cocode
      and job-prep.job     eq job.job
    no-lock:

    if job-prep.ml then
    DO:
       v-est-mat-cost = v-est-mat-cost + (job-prep.std-cost * job-prep.qty).
       IF tb_sep_board THEN
          v-est-other-mat-cost = v-est-other-mat-cost + (job-prep.std-cost * job-prep.qty).
    END.
    else
       v-est-lab-cost = v-est-lab-cost + (job-prep.std-cost * job-prep.qty).
end.

for each misc-act FIELDS(ml cost)
    where misc-act.company eq cocode
      and misc-act.job     eq job.job
    no-lock:

    if misc-act.ml then
    DO:
       v-act-mat-cost = v-act-mat-cost + misc-act.cost.
       IF tb_sep_board THEN
          v-act-other-mat-cost = v-act-other-mat-cost + misc-act.cost.
    END.
    else
       v-act-lab-cost = v-act-lab-cost + misc-act.cost.
end.
