/* ---------------------------------------------------- ce/kspoil2.i 3/94 cd  */
/* Find Spoil values in Matrices  PASS-2                                      */
/* -------------------------------------------------------------------------- */

if avail mstd and mstd.sp-x ne 0 and mstd.sp-y ne 0 then do:

   /*if mach.p-type eq "B" then v-num-up = xeb.num-up.

   else run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-num-up).
*/
   assign
    zzz     = qty
    /*qty     = {&fil}.num-sh * v-num-up * v-n-out.*/
    call_id = recid(mstd).

   run ce/kxy-spo.p(INPUT {&fil}.n-out, INPUT {&fil}.dept).

   find first mmtx where recid(mmtx) eq call_id no-lock no-error.

   if avail mmtx then yyy = mmtx.vals[(10 * y) + x].

   if yyy ne {&fil}.{&fld} then {&fil}.{&fld} = yyy.

   assign
    qty     = zzz
    call_id = recid(xef).
end.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
