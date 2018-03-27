/* --------------------------------------------------- ce/prokalk.i 07/96 JLF */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */
   
   {est/op-lock.i xest}

   IF NOT xef.op-lock THEN

     ASSIGN
      op-lock.val[1] = 1
      op-lock.val[2] = 1.


   find first mach {sys/look/machW.i} and
              mach.m-code  eq est-op.m-code no-lock no-error.
     
   if est-op.op-pass eq 0 then est-op.op-pass = 1.
   if est-op.dept eq "PR" or est-op.dept eq "CT" then do:
     maxco = 0.
     find first w-ink
         where w-ink.form-no eq est-op.s-num
           and w-ink.pass    eq est-op.op-pass
         no-lock no-error.

     IF AVAIL w-ink THEN DO:
       IF est-op.num-col + est-op.num-coat EQ 0 THEN
         ASSIGN
          est-op.num-col  = w-ink.inks
          est-op.num-coat = w-ink.varn.
       ELSE
         ASSIGN
          w-ink.inks = est-op.num-col
          w-ink.varn = est-op.num-coat.

       maxco = MIN(w-ink.inks + w-ink.varn,mach.max-color).
     END.
   END.

   {sys/inc/roundup.i cumul}

   est-op.num-sh = cumul.
   
   qty = est-op.num-sh *
         v-num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
                    (if xef.n-out-l eq 0 then 1 else xef.n-out-l).


   IF op-lock.val[1] EQ 1                          AND 
      (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN DO:
      est-op.op-waste = mach.mr-waste.

      if est-op.dept eq "PR" or est-op.dept eq "CT" then 
        est-op.op-waste = est-op.op-waste +
                          (mach.col-wastesh *
                           if est-op.plates ne 0 then est-op.plates
                                                 else maxco).

      RUN est/diewaste.p (BUFFER est-op).

      est-op.op-spoil = 0.
      {ce/kspoil.i &fil=est-op &fld=op-spoil}
      est-op.op-spoil = est-op.op-spoil + mach.run-spoil.

      if not xef.lsh-lock and (mstd.rs-x eq 20 or mstd.rs-y eq 20) then do:

         est-op.op-spoil = 0.
         {ce/kspoil2.i &fil=est-op &fld=op-spoil}
         est-op.op-spoil = est-op.op-spoil + mach.run-spoil.

      end.
   END.

   v-spo = (cumul / (1 - (est-op.op-spoil / 100))) - cumul.
   {sys/inc/roundup.i v-spo}

   assign
    est-op.num-sh      = est-op.num-sh      + v-spo
    r-spo[xef.form-no] = r-spo[xef.form-no] + v-spo.

   run sys/inc/numout.p (recid(est-op), output v-n-out).

   v-spo = est-op.op-waste /
           if est-op.op-sb then v-n-out else
             (v-num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
                         (if xef.n-out-l eq 0 then 1 else xef.n-out-l)).

   {sys/inc/roundup.i v-spo}

   assign
    est-op.num-sh = est-op.num-sh + v-spo
    spo           = spo           + v-spo.

   qty = est-op.num-sh *
         v-num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
                    (if xef.n-out-l eq 0 then 1 else xef.n-out-l).


   IF (op-lock.val[1] EQ 1 OR op-lock.val[2] EQ 1) AND 
      (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN DO:
      /* flip dimensions if corr. xgrain */
      if est-op.dept eq "LM" and ((xef.n-out-l ne 0 and
         (xef.lam-dscr eq  "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")))
         or
         (xef.n-out-l eq 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S")))
      then do:
         fil_id = recid(xef).
         find xef where recid(xef) eq fil_id.
         zzz = xef.gsh-wid. xef.gsh-wid = xef.gsh-len. xef.gsh-len = zzz.
         zzz = xef.lsh-wid. xef.lsh-wid = xef.lsh-len. xef.lsh-len = zzz.
         zzz = xef.nsh-wid. xef.nsh-wid = xef.nsh-len. xef.nsh-len = zzz.
         zzz = xef.trim-w. xef.trim-w = xef.trim-l. xef.trim-l = zzz.
      end.
      {ce/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed}
      /* reset dimensions if corr.xgrain */
      if est-op.dept eq "LM" and ((xef.n-out-l ne 0 and
         (xef.lam-dscr eq  "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")))
         or
         (xef.n-out-l eq 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S")))
      then do:
         zzz = xef.gsh-wid. xef.gsh-wid = xef.gsh-len. xef.gsh-len = zzz.
         zzz = xef.lsh-wid. xef.lsh-wid = xef.lsh-len. xef.lsh-len = zzz.
         zzz = xef.nsh-wid. xef.nsh-wid = xef.nsh-len. xef.nsh-len = zzz.
         zzz = xef.trim-w. xef.trim-w = xef.trim-l. xef.trim-l = zzz.
         fil_id = recid(xef).
         find xef where recid(xef) eq fil_id no-lock.
      end.


      IF op-lock.val[2] EQ 1 THEN
        IF LOOKUP(est-op.dept,"PR,CT") gt 0 THEN DO:
          IF est-op.plates NE 0 OR est-op.fountains ne 0 THEN 
            est-op.op-mr = (mach.tan-mrp * est-op.plates) +
                           (mach.tan-mrf * est-op.fountains).

          ELSE
          IF mach.washup GT 0 AND mach.col-pass EQ "P" THEN
            est-op.op-mr = est-op.op-mr + mach.washup.
         
          ELSE
          IF mach.washup GT 0 AND mach.col-pass EQ "C" THEN
            est-op.op-mr = est-op.op-mr + (mach.washup * maxco).
        END.

        ELSE RUN est/gluer-mr.p (BUFFER est-op) NO-ERROR.

      RUN est/diewaste.p (BUFFER est-op).
   END. /* lock */
   
   assign
    est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) +
                         mach.mr-varoh + mach.mr-fixoh
    est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) +
                         mach.run-varoh + mach.run-fixoh.

   cumul = est-op.num-sh.

   IF NEW op-lock THEN DELETE op-lock.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

