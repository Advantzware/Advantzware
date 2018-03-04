/* --------------------------------------------------- cec/kmr-run.i 9/93 cd  */
/* Find Run & MR values in Matrices                                           */
/* -------------------------------------------------------------------------- */

def var v-speed-pct as int no-undo.
def var hld-cal like ef.cal no-undo.
DEF VAR kmr-len LIKE xef.gsh-len NO-UNDO.
DEF VAR mstd-rowid AS ROWID NO-UNDO.


/* find mstd of machine & style, use first if none for this style */
RELEASE mstd.
mstd-rowid = ?.
FOR EACH mstd
    WHERE mstd.company EQ mach.company
      AND mstd.loc     EQ mach.loc
      AND mstd.m-code  EQ mach.m-code
    NO-LOCK
    BREAK BY mstd.style DESC:
  IF LAST(mstd.style)        OR
     mstd.style EQ xeb.style THEN DO:
    mstd-rowid = ROWID(mstd).
    LEAVE.
  END.
END.

FIND mstd WHERE ROWID(mstd) EQ mstd-rowid NO-LOCK NO-ERROR.

call_id = recid(xef).

/* we should now have a mstd.
   kxy sets global vars x & y, */
if avail mstd then do:
  find first xef where recid(xef) = call_id no-error.
  xcal = xef.cal.

  if (xef.medium ne "" or xef.flute ne "") and xef.trim-pen ne 0 then do:
    find first dept where dept.code = "LM" no-lock no-error.
    if avail dept then i = dept.fc.
    find first dept where dept.code = mach.dept[1] no-lock no-error.
    if avail dept and dept.fc gt i
    then assign xcal = xef.trim-pen / 1000    /* trim-pen stored as integer */
                xcal = xcal - .014 + xef.cal. /*Adjust for deviation */
  end.

  assign
   hld-cal  = xef.cal
   xef.cal  = xcal
   tmpstore = "". /* kxy-run.p checks this variable! */

  
  if op-lock.val[2] EQ 1 THEN
    IF mstd.mr-x ne 0 and mstd.mr-y ne 0 then do:
      call_id  = recid(mstd).

      run cec/kxy-mr.p (recid({&fil})).

      find first mmty where recid(mmty) eq call_id no-lock no-error.

      if avail mmty then {&fil}.{&fld} = mmty.vals[(10 * y) + x].
    end.
    else {&fil}.{&fld} = 0.

  
  IF op-lock.val[1] EQ 1 THEN DO:
    if mstd.rs-x ne 0 and mstd.rs-y ne 0 then do:
      call_id = recid(mstd).
      run cec/kxy-run.p (recid({&fil2})).
      find first mmtx where recid(mmtx) eq call_id no-lock no-error.
      if avail mmtx then 
        if mstd.mr-x eq 98 or mstd.mr-y eq 98 then {&fil}.{&fld} = mmtx.vals[1].
        else {&fil2}.{&fld2} = mmtx.vals[(10 * y) + x].
    end.
    else {&fil2}.{&fld2} = 0.

    assign
     v-speed-pct = 0
     kmr-len     = IF mach.dept[1] EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    /* Extra Sheets Calc. (9 = last extent in mstd) */
    do k = 1 to 9:
      if ( (mach.therm eq true and (mach.p-type eq "R" OR mach.dept[1] EQ "LM")) and
            (((mstd.run-qty[k]) *
              xeb.num-up * {&out}) gt qty ))
          or ( (mach.p-type = "S" or
                (mach.therm = false and mach.p-type = "R")) and
               (mstd.run-qty[k] * xeb.num-up * {&out} gt qty))
          or  (index("APB",mach.p-type) gt 0 and mstd.run-qty[k] gt qty) then leave.
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
      if mstd.board-depth[k] ge v-dep and
         mstd.board-depth[k] ne 0       then do:
        v-speed-pct = v-speed-pct - mstd.depth-reduc[k].
        leave.
      end.
    end.

    /* Machine Adder/Attachment Speed Reduction */
    DO k = 1 TO 3:
      RELEASE mach-attach.
      IF est-op.att-type[k] NE "" THEN
      FOR EACH mach-attach NO-LOCK
          WHERE mach-attach.company  EQ {&fil2}.company
            AND mach-attach.m-code   EQ {&fil2}.m-code
            AND mach-attach.att-type EQ {&fil2}.att-type[k]
          BREAK BY mach-attach.style DESC:
        IF mach-attach.style EQ xeb.style OR
           LAST(mach-attach.style)        THEN DO:
          v-speed-pct = v-speed-pct - mach-attach.run-speed.
          LEAVE.
        END.
      END.
    END.

    {&fil2}.{&fld2} = {&fil2}.{&fld2} * ((100 + v-speed-pct) / 100).

    {sys/inc/roundup.i {&fil2}.{&fld2}}

    
  END.

  assign tmpstore = ""
         xef.cal = hld-cal
         call_id = recid(xef).
end.

else do on endkey undo:
  /*   bell.
  display "            N O   S T A N D A R D S   F I L E   A V A I L A B L E        "
  with frame oops row 21 no-box overlay color value(col-error).
  hide frame oops.
  */
  message "            N O   S T A N D A R D S   F I L E   A V A I L A B L E        "
     view-as alert-box error.   
end.

find xef where recid(xef) = call_id no-error.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
