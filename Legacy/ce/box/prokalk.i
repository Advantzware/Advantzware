/* ----------------------------------------------- ce/box/prokalk.i 07/96 JLF */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */
{est/op-lock.i xest}

IF NEW op-lock AND NOT xef.op-lock THEN
    ASSIGN
        op-lock.val[1] = 1
        op-lock.val[2] = 1.

FIND FIRST mach {sys/look/machW.i} AND
              mach.m-code  EQ est-op.m-code NO-LOCK NO-ERROR.
       
IF est-op.op-pass EQ 0 THEN est-op.op-pass = 1.
IF est-op.dept EQ "PR" OR est-op.dept EQ "CT" THEN 
DO:
    maxco = 0.
    FIND FIRST w-ink
        WHERE w-ink.form-no EQ est-op.s-num
        AND w-ink.pass    EQ est-op.op-pass
        NO-LOCK NO-ERROR.

    IF AVAILABLE w-ink THEN 
    DO:
        IF est-op.num-col + est-op.num-coat EQ 0 THEN
        DO:
            ASSIGN
                est-op.num-col  = w-ink.inks
                est-op.num-coat = w-ink.varn
                v-program-6     = PROGRAM-NAME(6).

            IF est-op.s-num GT 1 AND
            index(PROGRAM-NAME(5),"est/oeselest") GT 0 AND

                (  INDEX(v-program-6,"est-from-tandem oe/v-ord") GT 0 OR 
               INDEX(v-program-6,"order-from-est oe/v-ord") GT 0 OR
               INDEX(v-program-6,"upd-new-tandem oe/d-oeitem") GT 0 OR
                CAN-FIND(FIRST b-eb3 WHERE /*adding new part to an
                existing estimate that came from a master estimate*/
                b-eb3.company EQ xeb.company AND
                b-eb3.est-no  EQ xeb.est-no AND
                b-eb3.master-est-no NE "")) THEN
            DO:
                ASSIGN
                    v-num-inks    = xeb.i-col
                    est-op.plates = v-num-inks
                    /*est-op.fountains = v-num-inks*/ .
            END.
        END.
        ELSE
            ASSIGN
                w-ink.inks = est-op.num-col
                w-ink.varn = est-op.num-coat.

        maxco = MIN(w-ink.inks + w-ink.varn,mach.max-color).
    END.
END.
ELSE 
DO:
    /*14152 - Calculate the maxco using all ink passes for non printers/coaters*/
    maxco = 0. 
    FOR EACH w-ink:
        maxco = maxco + w-ink.inks + w-ink.varn.
    END.
END.
   
   
FIND FIRST style
    WHERE style.company EQ cocode
    AND style.style   EQ xeb.style
    NO-LOCK NO-ERROR.
       
   /*if avail style and style.type eq "F" then do:
     assign
      est-op.op-spoil = 0
      est-op.op-waste = 0
      cumul           = cumul / (if xef.n-out-d gt 0 then xef.n-out-d else 1)
      v-cumul         = cumul.
      
     {sys/inc/roundup.i cumul}
     
     assign
      est-op.num-sh = cumul
      qty           = est-op.num-sh * (v-num-up * vn-out * xef.n-out-d).
   end.
   
   else do:*/

    {sys/inc/roundup.i cumul}

est-op.num-sh = cumul.

qty = est-op.num-sh *
    v-num-up * (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
    (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).


IF op-lock.val[1] EQ 1                          AND 
    (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN 
DO:
    est-op.op-waste = mach.mr-waste.

    IF est-op.dept EQ "PR" OR est-op.dept EQ "CT" THEN 
        est-op.op-waste = est-op.op-waste +
            (mach.col-wastesh *
            IF est-op.plates NE 0 THEN est-op.plates
            ELSE maxco).

    RUN est/diewaste.p (BUFFER est-op).

    est-op.op-spoil = 0.
    {ce/kspoil.i &fil=est-op &fld=op-spoil}
    est-op.op-spoil = est-op.op-spoil + mach.run-spoil.

    IF NOT xef.lsh-lock AND (mstd.rs-x EQ 20 OR mstd.rs-y EQ 20) THEN 
    DO:
        est-op.op-spoil = 0.
        {ce/kspoil2.i &fil=est-op &fld=op-spoil}
        est-op.op-spoil = est-op.op-spoil + mach.run-spoil.
    END.
END.

IF LOOKUP(est-op.dept,"CR,RC") GT 0 THEN 
DO:
    IF v-widp THEN
        ASSIGN
            sh-tmp = sh-wid
            sh-wid = sh-len
            sh-len = sh-tmp
            sh-tmp = v-outl
            v-outl = v-outw
            v-outw = sh-tmp
            sh-tmp = sh-len
            v-on-l = v-on-l / v-outf
            v-outf = 0
            v-widp = NO.

    ASSIGN
        v-outl = v-outl - est-op.n-out
        v-outf = v-outf + est-op.n-out
        sh-len = sh-tmp * v-outf
        v-on-f = v-on-l / v-outf.

    IF v-outl LT 1 AND NOT v-widp THEN v-widp = YES.
END.

RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

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

IF LAST(est-op.d-seq) THEN 
DO:
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

qty = est-op.num-sh *
    v-num-up * (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
    (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).
/*END.*/


IF (op-lock.val[1] EQ 1 OR op-lock.val[2] EQ 1) AND 
    (ip-rowid EQ ? OR ip-rowid EQ ROWID(est-op)) THEN 
DO:
    /* flip dimensions if corr. xgrain */
    IF est-op.dept EQ "LM" AND ((xef.n-out-l NE 0 AND
        (xef.lam-dscr EQ  "R" OR (xef.lam-dscr NE "R" AND xef.xgrain EQ "S")))
        OR
        (xef.n-out-l EQ 0 AND (xef.lam-dscr NE "R" AND xef.xgrain NE "S")))
        THEN 
    DO:
        fil_id = RECID(xef).
        FIND xef WHERE RECID(xef) EQ fil_id.
        ASSIGN
            zzz         = xef.gsh-wid
            xef.gsh-wid = xef.gsh-len
            xef.gsh-len = zzz
            zzz         = xef.lsh-wid
            xef.lsh-wid = xef.lsh-len
            xef.lsh-len = zzz
            zzz         = xef.nsh-wid
            xef.nsh-wid = xef.nsh-len
            xef.nsh-len = zzz
            zzz         = xef.trim-w
            xef.trim-w  = xef.trim-l
            xef.trim-l  = zzz.
    END.
    {ce/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed}
    /* reset dimensions if corr.xgrain */
    IF est-op.dept EQ "LM" AND ((xef.n-out-l NE 0 AND
        (xef.lam-dscr EQ  "R" OR (xef.lam-dscr NE "R" AND xef.xgrain EQ "S")))
        OR
        (xef.n-out-l EQ 0 AND (xef.lam-dscr NE "R" AND xef.xgrain NE "S")))
        THEN 
    DO:
        ASSIGN
            zzz         = xef.gsh-wid
            xef.gsh-wid = xef.gsh-len
            xef.gsh-len = zzz
            zzz         = xef.lsh-wid
            xef.lsh-wid = xef.lsh-len
            xef.lsh-len = zzz
            zzz         = xef.nsh-wid
            xef.nsh-wid = xef.nsh-len
            xef.nsh-len = zzz
            zzz         = xef.trim-w
            xef.trim-w  = xef.trim-l
            xef.trim-l  = zzz
            fil_id      = RECID(xef).
        FIND xef WHERE RECID(xef) EQ fil_id NO-LOCK.
    END.


    IF op-lock.val[2] EQ 1 THEN
        IF LOOKUP(est-op.dept,"PR,CT") GT 0 THEN 
        DO:
            IF est-op.plates NE 0 OR est-op.fountains NE 0 THEN
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

ASSIGN
    est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) +
                         mach.mr-varoh + mach.mr-fixoh
    est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) +
                         mach.run-varoh + mach.run-fixoh.

/*if avail style and style.type eq "F" then
  cumul = v-cumul * (if xef.n-out-d gt 0 then xef.n-out-d else 1).*/

IF NEW op-lock THEN DELETE op-lock.
     
/* end ---------------------------------- copr. 1996  advanced software, inc. */
