DEF INPUT PARAMETER ip-est-no LIKE est.est-no NO-UNDO.
DEF INPUT PARAMETER ip-assembly-type AS CHAR NO-UNDO /* A or N */.
DEF OUTPUT PARAMETER op-mach-hours AS DEC NO-UNDO.
DEF OUTPUT PARAMETER op-mach-run-hours AS DEC NO-UNDO.

DEF BUFFER xef FOR ef.
DEF SHARED TEMP-TABLE tt-est-op LIKE est-op.

DEF VAR sh-len AS DEC.
def var v-num-up as int NO-UNDO.
def var v-num-in like est-op.num-sh NO-UNDO.
def var vn-out like xef.n-out-l init 1 NO-UNDO.
def var v-outw like xef.n-out NO-UNDO.
def var v-outl like xef.n-out-l NO-UNDO.
def var v-outf as dec NO-UNDO.
def var v-on-f as dec NO-UNDO.
def var v-on-l as dec NO-UNDO.
def var sh-tmp like sh-len NO-UNDO.
def var v-widp as log NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-yld as dec NO-UNDO.
def var v-msf as dec NO-UNDO.
DEF VAR v-rm$ AS DEC NO-UNDO.
def var v-on-s as DEC NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR v-num-sh LIKE est-op.num-sh NO-UNDO.
DEF VAR ll-unitize AS LOG NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR li-die AS INT NO-UNDO.
DEF VAR v-parts AS DEC NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.
DEF VAR opmr LIKE est-op.op-mr NO-UNDO.
DEF VAR opsp AS DEC NO-UNDO.
DEF VAR oprun AS DEC NO-UNDO.
DEF VAR cocode AS CHAR INIT '001'.
DEF VAR locode AS CHAR INIT 'main'.

DEF BUFFER xest FOR est.
FIND FIRST xest WHERE xest.est-no = ip-est-no NO-LOCK NO-ERROR.
IF NOT AVAIL xest THEN
    RETURN.

for each xef
    where xef.company = xest.company
      and xef.est-no eq xest.est-no
    NO-LOCK:

  for each est-op
      where est-op.company eq xest.company
        and est-op.est-no  eq xest.est-no
        and est-op.s-num   eq xef.form-no
        AND est-op.LINE    LT 500
      by est-op.b-num by est-op.d-seq by est-op.op-pass
      with frame ae down no-labels no-box  stream-io:
 
    find first mach
        {sys/look/machW.i}
          and mach.m-code eq est-op.m-code
        no-lock no-error.
    if not avail mach then next.

    find first eb
        where eb.company   eq est-op.company
          and eb.est-no    eq est-op.est-no
          and eb.form-no   eq est-op.s-num
          and (eb.blank-no eq est-op.b-num or est-op.b-num eq 0)
        no-lock no-error.


    IF INDEX("AP",mach.p-type) GT 0 THEN
      ASSIGN
       v-num-up = 1
       v-n-out  = 1
       v-on-s   = 1
       v-on-f   = 1.

    ELSE DO:
      RUN sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, OUTPUT v-num-up).
      RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
      RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
    END.

    IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
      FIND FIRST ef-nsh OF xef
          WHERE ef-nsh.pass-no EQ est-op.op-pass
            AND ef-nsh.dept    EQ est-op.dept
          NO-LOCK NO-ERROR.
      IF AVAIL ef-nsh THEN DO:
        RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die).
        v-on-f = v-on-f * (est-op.n-out + INT(li-die GT 0)).
      END.
    END.

    ASSIGN
     v-on-s = v-num-up * v-n-out
     v-len  = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.
                                                           /*
    MESSAGE "xef.form-no" xef.form-no "v-on-f" v-on-f "v-on-s " v-on-s
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */
    ll-unitize = NO.
    FOR EACH mstd
        WHERE mstd.company eq mach.company
          AND mstd.loc     eq mach.loc
          AND mstd.m-code  eq mach.m-code
        NO-LOCK
        BREAK BY mstd.style DESC:
      IF LAST(mstd.style)        OR
         mstd.style EQ eb.style THEN DO:
        ll-unitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
        LEAVE.
      END.
    END.
    FIND FIRST tt-est-op WHERE tt-est-op.company = est-op.company
                           AND tt-est-op.est-no  = est-op.est-no
                           AND tt-est-op.eqty    = est-op.eqty
                           AND tt-est-op.qty     = est-op.qty
                           AND tt-est-op.s-num   = est-op.s-num
                         NO-LOCK NO-ERROR.
    IF AVAIL tt-est-op THEN
      v-num-sh = tt-est-op.qty.
    ELSE
      v-num-sh = est-op.qty.

    ASSIGN
     opsp = est-op.op-speed
     opmr = est-op.op-mr.
    IF opsp NE 0 THEN
      IF ll-unitize THEN DO:
          /*
        FOR EACH cas
            WHERE cas.snum EQ eb.form-no
              AND cas.bnum EQ eb.blank-no
              AND cas.typ  EQ 3
            NO-LOCK:
          ACCUMULATE cas.qty (TOTAL).
        END.
        oprun = (ACCUM TOTAL cas.qty) / opsp.
        */
      END.
      ELSE
      IF mach.p-type EQ "P" THEN
        oprun = (v-num-sh - est-op.op-waste) * v-parts / opsp.
      ELSE
      IF mach.therm                                  AND
         mach.p-type NE "A"                          AND
         (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
        oprun = (((v-num-sh * v-on-f) - est-op.op-waste) *
                 (v-len / 12)) / opsp.
      ELSE
      IF est-op.op-sb THEN
        oprun = ((v-num-sh * v-on-f) - est-op.op-waste) / opsp.
      ELSE
        oprun = ((v-num-sh * v-on-s) - est-op.op-waste) / opsp.
    ELSE oprun = 0.
    DO:
        /*
        MESSAGE "rates" est-op.op-rate[1] est-op.op-rate[2]
            "mrk-rate" mach.mrk-rate
            VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    END.
/*     MESSAGE "calc-mr.p"                                               */
/*              "oprun" oprun "xef.qty" xef.gsh-qty "op.num-sh" v-num-sh */
/*       "opmr" opmr "speed" opsp                                        */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                            */
    IF (ip-assembly-type = "A" AND mach.p-type = "A") 
        OR (ip-assembly-type NE "A" AND mach.p-type NE "A") THEN
      ASSIGN
      op-mach-run-hours = op-mach-run-hours + oprun /* (oprun * xef.gsh-qty / v-num-sh)*/
      op-mach-hours = op-mach-hours + opmr.
    /*
    MESSAGE "opmr" opmr "oprun" oprun "opsp" opsp "mch.p-type" mach.p-type
        "unitize" ll-unitize "op-sb" est-op.op-sb
"numsh" v-num-sh "v-on-f" v-on-f "op-waste" est-op.op-waste "v-on-s" v-on-s
        "est-op.dept" est-op.dept "est-op.op-speec" est-op.op-speed
"v-num-sh" v-num-sh "est-op.num-out" est-op.n-out
    VIEW-AS ALERT-BOX INFO BUTTONS OK. */

  END.
END.

