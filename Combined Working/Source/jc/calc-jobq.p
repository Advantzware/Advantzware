/* -----------------------------COPIED FROM------ ce/box/pr42-mch.p           */
/* Machine Operations for setup boxes                                         */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ip-est-row   AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-i-no      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER ip-mach-dept AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER op-run-qty  AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER op-last-qty AS INT NO-UNDO.

{sys/inc/VAR.i SHARED}

DEF  BUFFER xest FOR est.
DEF  BUFFER xef  FOR ef.
DEF  BUFFER xeb  FOR eb.
DEF  BUFFER xop FOR est-op.
DEF VAR oprun AS DEC.

DEF NEW SHARED VAR qty AS INT NO-UNDO.

DEFINE VARIABLE v-on-f        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v-run-qty     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v-factor      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE v-lab-fac     AS DECIMAL     EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-fix-fac     AS DECIMAL     EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-var-fac     AS DECIMAL     EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-num-up      AS INTEGER     NO-UNDO.
DEFINE VARIABLE v-len         LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE li-die        AS INTEGER     NO-UNDO.
DEFINE VARIABLE v-gu-out      AS INTEGER     INIT 1 NO-UNDO.
DEFINE VARIABLE v-printed-lit AS LOGICAL     NO-UNDO.
DEFINE VARIABLE opmr          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE optot         AS DECIMAL     NO-UNDO.


{sys/inc/ceprice.i}

FIND xest WHERE ROWID(xest) EQ ip-est-row NO-LOCK NO-ERROR.
IF NOT AVAIL xest THEN
    RETURN.
FIND FIRST xef WHERE xef.company = xest.company
                 AND xef.est-no = xest.est-no NO-LOCK NO-ERROR.
FIND FIRST xeb WHERE xeb.company = xest.company
                 AND xeb.est-no = xest.est-no NO-LOCK NO-ERROR.

ASSIGN
v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode 
                           AND prodl.prolin EQ 'Printed' 
                           AND prodl.procat EQ xeb.procat).
                           
FOR EACH est-op WHERE est-op.company EQ xest.company
                  AND est-op.est-no  EQ xest.est-no
                  AND est-op.LINE LT 501
                NO-LOCK,
                FIRST mach {sys/look/machW.i}
                  AND mach.m-code EQ est-op.m-code
                NO-LOCK,
                FIRST xef WHERE xef.company EQ est-op.company
                  AND xef.est-no  EQ est-op.est-no
                  AND xef.form-no EQ est-op.s-num
                NO-LOCK
                BREAK BY est-op.s-num 
                      BY est-op.b-num 
                      BY est-op.d-seq 
                      BY est-op.op-pass
          WITH FRAME ae DOWN NO-LABELS NO-BOX:
  
  FIND FIRST xeb WHERE xeb.company  EQ est-op.company
                   AND xeb.est-no   EQ est-op.est-no
                   AND xeb.form-no  EQ est-op.s-num
                   AND xeb.blank-no EQ est-op.b-num
                 NO-LOCK NO-ERROR.
     IF NOT avail xeb THEN
        FIND FIRST xeb WHERE xeb.company  EQ est-op.company
                         AND xeb.est-no   EQ est-op.est-no
                         AND xeb.form-no  EQ est-op.s-num
                       NO-LOCK.
  
  RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).
  
  RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
  
  IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
    FIND FIRST ef-nsh OF xef WHERE ef-nsh.pass-no EQ est-op.op-pass
                               AND ef-nsh.dept    EQ est-op.dept
                              NO-LOCK NO-ERROR.
    IF AVAIL ef-nsh THEN DO:
      RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die).
      v-on-f = v-on-f * (est-op.n-out + INT(li-die GT 0)).
    END.
  END.
  
  v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.
  
  IF est-op.op-speed NE 0 THEN
  DO:
    IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
      oprun = (((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
              (v-len / 12)) / est-op.op-speed.
    ELSE
    IF est-op.op-sb = TRUE THEN
      oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) /
                est-op.op-speed.
    ELSE
    IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
      oprun = ((est-op.num-sh *
              v-num-up *
              (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
              (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l)) -
              est-op.op-waste) / est-op.op-speed.
    ELSE
      oprun = ((est-op.num-sh * v-gu-out) - est-op.op-waste) / est-op.op-speed.
  END.
  ELSE oprun = 0.
  
  /*Run Qty Divisor 24462 (also undoes 19774)*/
    IF est-op.n_out_div GT 0 THEN 
        oprun = oprun / est-op.n_out_div.
        
  IF v-printed-lit AND est-op.n-out GT 1 THEN
    v-gu-out = v-gu-out * est-op.n-out.
  
  
  ASSIGN
  opmr        = est-op.op-mr * est-op.op-rate[1]
  oprun       = oprun        * est-op.op-rate[2]
  optot       = opmr + oprun.
  
  IF v-min-mchg THEN DO:
    IF ceprice-chr EQ "MR+Run" AND mach.mrk-rate GT optot THEN
    ASSIGN    v-factor = (mach.mrk-rate / optot)
              opmr    = opmr  * v-factor
              oprun   = oprun * v-factor.
    
    ELSE
    IF ceprice-chr EQ "RunOnly" AND mach.mrk-rate GT oprun THEN
      oprun = mach.mrk-rate.
    
    ASSIGN    v-factor     = optot
              optot        = oprun + opmr
              v-factor     = optot / v-factor.
    
    IF v-factor EQ ? THEN v-factor = 0.
    
    ASSIGN    v-lab-fac[1] = v-lab-fac[1] * v-factor
              v-lab-fac[2] = v-lab-fac[2] * v-factor
              v-var-fac[1] = v-var-fac[1] * v-factor
              v-var-fac[2] = v-var-fac[2] * v-factor
              v-fix-fac[1] = v-fix-fac[1] * v-factor
              v-fix-fac[2] = v-fix-fac[2] * v-factor.
  END.
  
  DO:
    
    IF est-op.m-code NE "" THEN DO:
      FIND itemfg WHERE itemfg.company = cocode AND
      itemfg.i-no    = xeb.stock-no
      NO-LOCK NO-ERROR.
     
/*       create OP.                        */
/*       assign OP.blank-no = est-op.b-num */
/*       OP.dept     = est-op.dept         */
/*       OP.form-no   = est-op.s-num       */
/*       OP.i-no     = xeb.stock-no        */
/*       OP.line     = est-op.line         */
/*       OP.m-code   = est-op.m-code       */
/*       OP.mr-fixoh = mach.mr-fixoh       */
/*       OP.mr-hr    = est-op.op-mr        */
/*       OP.mr-rate  = mach.mr-rate        */
/*       OP.mr-varoh = mach.mr-varoh       */
/*       OP.mr-waste = est-op.op-waste     */
/*       OP.pass     = est-op.op-pass      */
/*       OP.run-hr   = oprun               */
/*       OP.wst-prct = est-op.op-spoil     */
/*       OP.speed    = est-op.op-speed.    */
            
      IF est-op.op-sb THEN
        v-run-qty = est-op.num-sh * v-on-f.
      ELSE
        v-run-qty = est-op.num-sh * v-num-up *
          (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
          (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).
        
      /*Run Qty Divisor 24462 (also undoes 19774)*/
      IF est-op.n_out_div GT 0 THEN 
        v-run-qty = v-run-qty / est-op.n_out_div.

      /* Found the given item and machine, so exit program  */
      IF /* est-op.m-code EQ ip-m-code AND */ 
          (xef.flute EQ ip-i-no  OR xef.medium EQ ip-i-no)
          AND est-op.dept EQ ip-mach-dept
          AND op-run-qty EQ 0 THEN
          op-run-qty = v-run-qty.
      op-last-qty = v-run-qty.

    END. /* if est-op.b-num ne 0 */
    /*  {util/tmsg.i ""TEST"" v-run-qty est-op.dept est-op.qty est-op.LINE } */
  END. /* if v-min-mchg */
  
END. /* each est-op */

/* end ---------------------------------- copr. 1996  advanced software, inc. */
