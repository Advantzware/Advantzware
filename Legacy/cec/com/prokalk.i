/* ---------------------------------------------- cec/com/prokalk.i 09/05 JLF */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */
 

   {est/op-lock.i xest}
   
   IF NEW op-lock AND NOT xef.op-lock THEN
     ASSIGN
      op-lock.val[1] = 1
      op-lock.val[2] = 1.

   find first mach {sys/look/machW.i} and
              mach.m-code  eq est-op.m-code no-lock no-error.
       
   est-op.m-dscr = mach.m-dscr.
   if mach.p-type = "B" then est-op.op-sb = false.
   if est-op.op-pass = 0 then est-op.op-pass = 1.

   maxco = 0.
   IF est-op.dept EQ "PR" THEN
   DO i = 1 TO 10:
     IF xeb.i-ps[i] NE est-op.op-pass THEN NEXT.
     FIND FIRST item NO-LOCK
         {sys/look/itemW.i} 
           AND item.i-no EQ xeb.i-code[i]
           AND INDEX("IV",item.mat-type) GT 0
           AND item.ink-type NE "A" 
         NO-ERROR.
     IF AVAIL item THEN maxco = maxco + 1. /* maxco now = # colors/coating this machine/this pass */
   END.

   if ll-foam and lookup(est-op.dept,"CR,RC") gt 0 then do:
     assign
      est-op.op-spoil = 0
      est-op.op-waste = 0
      cumul           = cumul / (if xef.n-out-d gt 0 then xef.n-out-d else 1)
      v-cumul         = cumul.
      
     {sys/inc/roundup.i cumul}
     
     assign
      est-op.num-sh = cumul
      qty           = est-op.num-sh * (v-num-up * vn-out).
   end.
   
   else do:

     IF op-lock.val[1] EQ 1
        AND NOT est-op.isLocked                          
        AND (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN DO:
        est-op.op-waste = mach.mr-waste.

        if est-op.dept eq "PR" or est-op.dept eq "CT" then 
          est-op.op-waste = est-op.op-waste +
                            (mach.col-wastesh *
                             if est-op.plates ne 0 then est-op.plates
                                                   else maxco).

       RUN est/diewaste.p (BUFFER est-op).

       est-op.op-spoil = 0.
       {cec/kspoil.i &fil=est-op &fld=op-spoil}
       est-op.op-spoil = est-op.op-spoil + mach.run-spoil.
     end.

     if lookup(est-op.dept,"CR,RC") gt 0 then do:
       if v-widp then
         assign
          sh-tmp = sh-wid
          sh-wid = sh-len
          sh-len = sh-tmp
          sh-tmp = v-outl
          v-outl = v-outw
          v-outw = sh-tmp
          sh-tmp = sh-len
          v-on-l = v-on-l / v-outf
          v-outf = 0
          v-widp = no.

       assign
        v-outl = v-outl - est-op.n-out
        v-outf = v-outf + est-op.n-out
        sh-len = sh-tmp * v-outf
        v-on-f = v-on-l / v-outf.

       if v-outl lt 1 and not v-widp then v-widp = yes.
     end.

     run sys/inc/numout.p (recid(est-op), output v-on-f).

     IF FIRST(est-op.d-seq) THEN v-blk = cumul * (v-num-up * vn-out).

     ASSIGN
      v-blk    = v-blk / (1 - (est-op.op-spoil / 100))
      v-blk    = v-blk + (est-op.op-waste * IF est-op.op-sb THEN ((v-num-up * vn-out) / v-on-f) ELSE 1)
      v-spo[2] = est-op.op-waste / IF est-op.op-sb THEN v-on-f ELSE (v-num-up * vn-out)
      v-dec    = (v-spo[2] - TRUNC(v-spo[2],0)) * (v-num-up * vn-out)
      v-spo[2] = TRUNC(v-spo[2],0).

     {sys/inc/roundup.i v-dec}
     v-sav[2] = v-sav[2] + v-dec.

     ASSIGN
      v-sav[2] = v-sav[2] / (v-num-up * vn-out)
      v-dec    = (v-sav[2] - TRUNC(v-sav[2],0)) * (v-num-up * vn-out)
      v-spo[2] = v-spo[2] + TRUNC(v-sav[2],0).

     {sys/inc/roundup.i v-dec}
     v-sav[2] = v-dec.

     cumul = v-blk / (v-num-up * vn-out).

     IF LAST(est-op.d-seq) THEN DO:
       v-sav[2] = v-sav[2] / (v-num-up * vn-out).
       {sys/inc/roundup.i v-sav[2]}

       ASSIGN
        v-spo[2] = v-spo[2] + v-sav[2]
        v-sav[2] = 0.

       {sys/inc/roundup.i cumul}
     END.

     ASSIGN
      est-op.num-sh = cumul
      spo           = spo + v-spo[2].

     qty = est-op.num-sh * v-num-up * vn-out.
   END.


   IF (op-lock.val[1] EQ 1 OR op-lock.val[2] EQ 1) 
    AND NOT est-op.isLocked
    AND (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN DO:
   /* flip dimensions if corr. xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
        (xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then do:
      fil_id = recid(xef).
      find xef where recid(xef) eq fil_id.
      ASSIGN
      zzz = sh-wid
      sh-wid = sh-len
      sh-len = zzz
      zzz = xef.lsh-wid
      xef.lsh-wid = xef.lsh-len
      xef.lsh-len = zzz
      zzz = xef.gsh-wid
      xef.gsh-wid = xef.gsh-len
      xef.gsh-len = zzz
      zzz = xef.nsh-wid
      xef.nsh-wid = xef.nsh-len
      xef.nsh-len = zzz.
   end.

   v-dep = xeb.dep.

   {cec/kmr-run.i
                 &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed &out=vn-out}
   /* reset dimensions if corr.xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
        (xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then do:
      ASSIGN
      zzz = sh-wid
      sh-wid = sh-len
      sh-len = zzz
      zzz = xef.lsh-wid
      xef.lsh-wid = xef.lsh-len
      xef.lsh-len = zzz
      zzz = xef.gsh-wid
      xef.gsh-wid = xef.gsh-len
      xef.gsh-len = zzz
      zzz = xef.nsh-wid
      xef.nsh-wid = xef.nsh-len
      xef.nsh-len = zzz
      fil_id = recid(xef).
      find xef where recid(xef) eq fil_id no-lock.
   end.
       

     IF op-lock.val[1] EQ 1 AND NOT est-op.isLocked THEN
       est-op.op-waste = mach.mr-waste + (mach.col-wastesh * maxco).


     IF op-lock.val[2] EQ 1 AND NOT est-op.isLocked THEN DO:
       assign
        j             = 0
        v-pass-colors = 0.

       for each xop
          where xop.company = est-op.company 
            and xop.est-no eq est-op.est-no
            and xop.s-num eq est-op.s-num
            and xop.qty   eq est-op.qty
            and xop.line  ge (if est-op.line ge 500 then 500 else 0)
            and xop.dept  eq "PR"
          no-lock
          by xop.line:
         j = j + 1.

         if recid(est-op) eq recid(xop) then do:
           do i = 1 to 10:
             if xeb.i-ps[i] eq j    and
                xeb.i-code[i] ne "" then v-pass-colors = v-pass-colors + 1.
           end.

           leave.
         end.
       end.

       if v-pass-colors gt 0 then
         est-op.op-mr = est-op.op-mr +
                        (mach.washup *
                         if mach.col-pass eq "P" then 1             else
                         if mach.col-pass eq "C" then v-pass-colors else 0).

       if est-op.dept eq "CT" then do:
         find last xop
             where xop.company = est-op.company
               and xop.est-no  eq est-op.est-no
               and xop.s-num  eq est-op.s-num
               and xop.qty   eq est-op.qty
               and xop.m-code eq est-op.m-code
               and xop.line   ge (if est-op.line ge 500 then 500 else 0)
               and xop.line   lt est-op.line
             no-lock no-error.
         if avail xop then coater-loop:
         for each eb
             where eb.company = est-op.company 
               and eb.est-no   eq est-op.est-no
               and eb.form-no eq est-op.s-num
             no-lock break by eb.est-no.
           do i = 1 to 10:
             if eb.i-ps[i] eq est-op.op-pass then do:
               find first item
                   where item.company eq cocode
                     and item.i-no    eq eb.i-code[i]
                   no-lock no-error.
               if not avail item or item.mat-type ne "V" then next.
               do j = 1 to 10:
                 if j            ne i            and
                    eb.i-ps[j]   eq xop.op-pass  and
                    eb.i-code[j] eq eb.i-code[i] and
                    eb.i-%[j]    eq eb.i-%[i]    then leave.
                 if j eq 10 then leave coater-loop.
               end.
             end.
           end.
           if last(eb.est-no) then est-op.op-mr = 0.
         end.
       end.

       IF LOOKUP(est-op.dept,"PR,CT") gt 0 then do:
         IF est-op.plates NE 0 OR est-op.fountains ne 0 THEN
           est-op.op-mr = (mach.tan-mrp * est-op.plates) +
                          (mach.tan-mrf * est-op.fountains).
       END.

       ELSE RUN est/gluer-mr.p (BUFFER est-op) NO-ERROR.

       DO k = 1 TO 3:
         RELEASE mach-attach.
         IF est-op.att-type[k] NE "" THEN
         FOR EACH mach-attach NO-LOCK
             WHERE mach-attach.company  EQ est-op.company
               AND mach-attach.m-code   EQ est-op.m-code
               AND mach-attach.att-type EQ est-op.att-type[k]
             BREAK BY mach-attach.style DESC:
           IF mach-attach.style EQ xeb.style OR
              LAST(mach-attach.style)        THEN DO:
             est-op.op-mr = est-op.op-mr +
                            (mach-attach.setup /
                             (IF mach-attach.qty LE 0 THEN 1 ELSE mach-attach.qty) *
                             est-op.att-qty[k]).
             LEAVE.
           END.
         END.
       END.
     END.

     RUN est/diewaste.p (BUFFER est-op).
   end. /* lock */

   assign
    est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) +
                         mach.mr-varoh + mach.mr-fixoh
    est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) +
                         mach.run-varoh + mach.run-fixoh.

   /*if avail style and style.type eq "F" then
     cumul = v-cumul * (if xef.n-out-d gt 0 then xef.n-out-d else 1).*/

   IF NEW op-lock THEN DELETE op-lock.
     
/* end ---------------------------------- copr. 1996  advanced software, inc. */
