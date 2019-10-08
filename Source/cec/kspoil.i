/* ---------------------------------------------------- cec/kspoil.i 10/94 gb */
/* Find Spoil values in Matrices                                              */
/* -------------------------------------------------------------------------- */

/* find mstd of machine */
find first mstd
    where mstd.company eq cocode
      and mstd.loc     eq locode
      and mstd.m-code  eq mach.m-code
      and mstd.style   eq xeb.style
    no-lock no-error.

/* maybe there's only a blank style mstd */
if not avail mstd then
find first mstd
    where mstd.company eq cocode
      and mstd.loc     eq locode
      and mstd.m-code  eq mach.m-code
    no-lock no-error.

/* we should now have a mstd.
   kxy sets global vars x & y, */
if available mstd then do:
   call_id = recid(xef).
   find first xef where recid(xef) = call_id no-error.
   xcal = xef.cal.
   if (xef.medium ne "" or xef.flute ne "" ) and xef.trim-pen ne 0
   then do:
      find first dept where dept.code = "LM" no-lock no-error.
      if available dept then i = dept.fc.
      find first dept where dept.code = mach.dept[1] no-lock no-error.
      if available dept and dept.fc > i
      then assign xcal = xef.trim-pen / 1000    /* trim-pen stored as integer */
                  xcal = xcal - .014 + xef.cal. /*Adjust for deviation */
   end.
   assign
    xxx     = xef.cal
    call_id = recid(mstd)
    x = 0
    y = 0.
   if mstd.sp-x ne 0 and mstd.sp-y ne 0 then do:
      zzz = qty.
      run cec/kxy-spo.p (recid({&fil})).
      find first mmtx where recid(mmtx) eq call_id no-lock no-error.
      if avail mmtx then {&fil}.{&fld} = mmtx.vals[(10 * y) + x].
      qty = zzz.
   end.
   else {&fil}.{&fld} = 0.
end.
else do on endkey undo:
/*   bell.
   display "            N O   S T A N D A R D S   F I L E   A V A I L A B L E        "
   with frame oops{&fra} row 21 no-box overlay color value(col-error).
   hide frame oops{&fra}.
   */
   message  "            N O   S T A N D A R D S   F I L E   A V A I L A B L E        "
       view-as alert-box error.
end.

assign
 tmpstore = ""
 call_id  = recid(xef).

find first xef where recid(xef) eq call_id no-error.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
