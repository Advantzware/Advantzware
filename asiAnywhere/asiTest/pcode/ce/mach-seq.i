/* --------------------------------------------------- ce/mach-seq.i 9/94 gb  */
/* Determine the Run Quantity Type                                            */
/* -------------------------------------------------------------------------- */

if (mach.p-type = "R" OR mach.dept[1] EQ "LM") and mach.therm then
   v-chk-qty = xef.gsh-qty * (sh-len / 12).
else if mach.p-type = "R" or mach.p-type = "S" then
   v-chk-qty = xef.gsh-qty.
else if mach.p-type = "B" then
   if xef.est-type = 1 then
      v-chk-qty = xest.est-qty[1].
   else
      v-chk-qty = xeb.yld-qty.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
