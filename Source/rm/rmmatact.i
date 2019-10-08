/* -------------------------------------------------- rm/rmmatact.i 07/97 JLF */
/* raw materials -                                                            */
/* -------------------------------------------------------------------------- */

create mat-act.
assign
 mat-act.company   = cocode
 mat-act.mat-date  = v-post-date
 mat-act.job       = job.job
 mat-act.job-no    = job-mat.job-no
 mat-act.job-no2   = job-mat.job-no2
 mat-act.s-num     = job-mat.frm
 mat-act.b-num     = job-mat.blank-no
 mat-act.i-no      = job-mat.i-no
 mat-act.i-name    = rm-rctd.i-name
 mat-act.rm-i-no   = job-mat.i-no
 mat-act.rm-i-name = rm-rctd.i-name
 mat-act.pass      = rm-rctd.pass
 mat-act.tag       = rm-rctd.tag
 mat-act.loc       = rm-rctd.loc
 mat-act.loc-bin   = rm-rctd.loc-bin
 mat-act.opn       = yes
 mat-act.mat-time  = time.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
