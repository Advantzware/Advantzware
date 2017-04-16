/* ---------------------------------------------- pc/rep/mchprdhr.i 07/97 JLF */
/* Productivity by Department/Machine Report                                  */
/* -------------------------------------------------------------------------- */

mr-eff  = ({1}-mr-std  / {1}-mr-act)  * 100.00.
if mr-eff = ? then mr-eff = 0.
run-eff = ({1}-run-std / {1}-run-act) * 100.00.
if run-eff = ? then run-eff = 0.
tot-std-hrs = {1}-mr-std + {1}-run-std.
tot-act-hrs = {1}-mr-act + {1}-run-act.
tot-eff = (tot-std-hrs / tot-act-hrs) * 100.00.
if tot-eff = ? then tot-eff = 0.
dt-eff = ({1}-dt-act / tot-act-hrs) * 100.00.
if dt-eff = ? then dt-eff = 0.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
