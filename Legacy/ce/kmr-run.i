/* ---------------------------------------------------- ce/kmr-run.i 9/93 cd  */
/* Find Run & MR values in Matrices                                           */
/* -------------------------------------------------------------------------- */

def var prev-board-cal like ef.cal      no-undo.
def var v-speed-pct    as   int         no-undo.
def var v-up           as   int         no-undo.
DEF VAR kmr-len        LIKE xef.gsh-len NO-UNDO.


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

call_id = recid(xef).

/* we should now have a mstd.
   kxy sets global vars x & y, */
if avail mstd then do:
  find first xef where recid(xef) = call_id no-error.
  xcal = xef.cal.

  run sys/inc/numon.p (rowid({&fil}), output v-n-out).

  if (xef.medium ne "" or xef.flute ne "" ) and xef.trim-pen ne 0 then do:
    find first dept where dept.code eq "LM" no-lock no-error.
    if avail dept then i = dept.fc.
    find first dept where dept.code eq mach.dept[1] no-lock no-error.
    if avail dept and dept.fc gt i then
      assign
       xcal = xef.trim-pen / 1000    /* trim-pen stored as integer */
       xcal = xcal - .014 + xef.cal. /*Adjust for deviation */
  end.

  assign
   xxx      = xef.cal
   tmpstore = "". /* kxy-run.p checks this variable! */

    IF xest.recalc-mr = YES THEN
    if mstd.mr-x ne 0 and mstd.mr-y ne 0 then do:
      call_id  = recid(mstd).
      run ce/kxy-mr.p(INPUT {&fil2}.n-out, INPUT {&fil2}.dept).
      find first mmty where recid(mmty) eq call_id no-lock no-error.
      if avail mmty then {&fil}.{&fld} = mmty.vals[(10 * y) + x].
      if mstd.mr-x eq 99 or mstd.mr-y eq 99 then
        run ce/kxy-form.p (recid(mstd), input-output {&fil}.{&fld}).
    end.
    else {&fil}.{&fld} = 0.


    IF xest.recalc = YES THEN DO:
    if mstd.rs-x ne 0 and mstd.rs-y ne 0 then do:
      call_id  = recid(mstd).

      run ce/kxy-run.p(INPUT {&fil2}.n-out, INPUT {&fil2}.dept).
      find first mmtx where recid(mmtx) eq call_id no-lock no-error.
      if avail mmtx then {&fil2}.{&fld2} = mmtx.vals[(10 * y) + x].
    end.
    else {&fil2}.{&fld2} = 0.

    if mach.p-type eq "B" then v-up = xeb.num-up.
    else
    run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-up).

    assign
     v-speed-pct = 0
     kmr-len     = IF mach.dept[1] EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    /* Extra Sheets Calc. (9 = last extent in mstd) */
    do k = 1 to 9:
      if ( (mach.therm eq true and (mach.p-type eq "R" OR mach.dept[1] EQ "LM")) and
            (((mstd.run-qty[k] / (kmr-len / 12)) *
              (v-n-out * v-up)) gt qty ))
          or ( (mach.p-type eq "S" or
                (mach.therm eq false and mach.p-type eq "R")) and
               (mstd.run-qty[k] * (v-n-out * v-up) gt qty))
          or  (mach.p-type eq "B" and mstd.run-qty[k] gt qty) then leave.
      else if mstd.x-sheets[k] ne 0 then v-speed-pct = mstd.x-sheets[k].
    end.

    /* Speed Reduction for item, by Dept. */
    tmpstore = "NO".
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xef.board
        no-lock no-error.
    if avail item then do k = 1 to 10:
      if item.dept-name[k] eq {&fil2}.dept and
         item.speed%[k] ne 0               then do:
        v-speed-pct = v-speed-pct - item.speed%[k].
        tmpstore = "YES".
        leave.
      end.
    end.

    /* If no item reduction, do caliper reduction if applicable */
    if tmpstore eq "NO" then do k = 1 to 9:
      if mstd.board-cal[k] ge xef.cal and
         mstd.board-cal[k] ne 0       then do:
        v-speed-pct = v-speed-pct - mstd.spd-reduc[k].
        leave.
      end.
    end.

    /* If no item reduction, do depth reduction if applicable */
    if tmpstore eq "NO" then do k = 1 to 15:
      if mstd.board-depth[k] ge xeb.dep and
         mstd.board-depth[k] ne 0       then do:
        v-speed-pct = v-speed-pct - mstd.depth-reduc[k].
        leave.
      end.
    end.

    {&fil2}.{&fld2} = {&fil2}.{&fld2} * ((100 + v-speed-pct) / 100).

    {sys/inc/roundup.i {&fil2}.{&fld2}}
  END.
      
  assign
   tmpstore = ""
   call_id = recid(xef).
end.

else
do on endkey undo:
  bell.
  message "ERROR:  N O   S T A N D A R D S   F I L E   A V A I L A B L E".
  hide message.
  leave.
end.

find xef where recid(xef) = call_id no-error.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
