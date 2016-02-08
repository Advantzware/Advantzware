/* ----------------------------------------------- ce/com/pr4-mch.p 10/98 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop for est-op.

{ce/print4.i shared shared}

def var v-on-f as DEC NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-num-up as int NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR li-die AS INT NO-UNDO.
DEF VAR v-gu-out AS INT INIT 1 NO-UNDO.
DEF VAR v-printed-lit AS LOG NO-UNDO.

def shared var v-summ as log NO-UNDO.

def TEMP-TABLE w-op NO-UNDO
    field m-code like op.m-code
    field mr-hr  like op.mr-hr
    field run-hr like op.run-hr
    field units  like op.run-qty
    field speed  as   dec
    field rec-id as   recid.

DEF BUFFER bf-eb FOR eb.
DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.

{sys/inc/ceprice.i}

ASSIGN
     opsplit$ = 0
     v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                     prodl.prolin EQ 'Printed' AND
                     prodl.procat EQ xeb.procat).

FOR EACH op:
  DELETE op.
END.

for each est-op where est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      and est-op.line gt 500,
      
    first mach
    {sys/look/machW.i}
      and mach.m-code eq est-op.m-code
    no-lock,

    first xef
    where xef.company = xest.company
      AND xef.est-no   eq est-op.est-no
      and xef.form-no eq est-op.s-num
    no-lock
    
    break by est-op.s-num
          by est-op.b-num
          by est-op.d-seq
          by est-op.op-pass:

  if est-op.b-num gt 0 then
  find first bf-eb
      where bf-eb.company = est-op.company
        AND bf-eb.est-no   eq est-op.est-no
        and bf-eb.form-no  eq est-op.s-num
        and bf-eb.blank-no eq est-op.b-num
      no-lock no-error.
      
  run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-num-up).
  
  run sys/inc/numout.p (recid(est-op), output v-on-f).

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

  v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.
  
  if est-op.op-speed gt 0 then do:
    if avail mach then do:
        if mach.therm and (mach.p-type eq "R" OR est-op.dept EQ "LM") then
           oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
                    (v-len / 12).
        else
        if est-op.op-sb then
           oprun = (est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste.
        else
          IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
             oprun = (est-op.num-sh * v-num-up * v-on-f) - est-op.op-waste.
        ELSE
           oprun = (est-op.num-sh * v-gu-out * v-on-f) - est-op.op-waste.

        oprun = oprun / est-op.op-speed.
    end.       
  end.

  else oprun = 0.

  IF v-printed-lit AND est-op.n-out GT 1 THEN
     v-gu-out = est-op.n-out.

  assign
   opmr$        = est-op.op-mr * est-op.op-rate[1]
   oprun$       = oprun        * est-op.op-rate[2]
   optot$       = opmr$ + oprun$

   v-lab-fac[1] = est-op.op-mr * est-op.op-crew[1] * mach.lab-rate[lab-drate]
   v-lab-fac[2] = oprun        * est-op.op-crew[2] * mach.lab-rate[lab-drate]

   v-var-fac[1] = est-op.op-mr * mach.mr-varoh
   v-var-fac[2] = oprun        * mach.run-varoh

   v-fix-fac[1] = est-op.op-mr * mach.mr-fixoh
   v-fix-fac[2] = oprun        * mach.run-fixoh.

  IF v-min-mchg THEN DO:
    IF ceprice-chr EQ "MR+Run" AND mach.mrk-rate GT optot$ THEN
      ASSIGN
       v-factor = (mach.mrk-rate / optot$)
       opmr$    = opmr$  * v-factor
       oprun$   = oprun$ * v-factor.

    ELSE
    IF ceprice-chr EQ "RunOnly" AND mach.mrk-rate GT oprun$ THEN
      oprun$ = mach.mrk-rate.

    ASSIGN
     v-factor     = optot$
     optot$       = oprun$ + opmr$
     v-factor     = optot$ / v-factor.

    IF v-factor EQ ? THEN v-factor = 0.

    ASSIGN
     v-lab-fac[1] = v-lab-fac[1] * v-factor
     v-lab-fac[2] = v-lab-fac[2] * v-factor
     v-var-fac[1] = v-var-fac[1] * v-factor
     v-var-fac[2] = v-var-fac[2] * v-factor
     v-fix-fac[1] = v-fix-fac[1] * v-factor
     v-fix-fac[2] = v-fix-fac[2] * v-factor.
  END.

  assign
   opsplit$[1] = opsplit$[1] + v-lab-fac[1] + v-lab-fac[2]
   opsplit$[2] = opsplit$[2] + v-var-fac[1] + v-var-fac[2]
   opsplit$[3] = opsplit$[3] + v-fix-fac[1] + v-fix-fac[2].
  
  if est-op.b-num ne 0 then do:
    find first blk
        where blk.snum eq est-op.s-num
          and blk.bnum eq est-op.b-num
        no-error.
    if avail blk then do:
      find first xjob
          where xjob.i-no     eq blk.id
            and xjob.form-no  eq blk.snum
            and xjob.blank-no eq blk.bnum
          no-error.
      blk.lab  = blk.lab  + optot$. blk.cost = blk.cost + optot$.
      assign
        xjob.lab = xjob.lab +
                  (est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
                   (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate]))
        xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
        xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
    end.
  end.
  else
  for each blk where blk.snum eq est-op.s-num:
    assign
     blk.lab  = blk.lab  + (optot$ * blk.pct)
     blk.cost = blk.cost + (optot$ * blk.pct).
    find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
        no-error.
    assign
     xjob.lab = xjob.lab +
      (((est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
      (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate])))
        * blk.pct)
     xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
     xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
  end.

  find first bf-eb
      where bf-eb.company = xest.company
        AND bf-eb.est-no    eq xest.est-no
        and bf-eb.form-no  eq est-op.s-num
        and bf-eb.blank-no eq est-op.b-num
      no-lock no-error.

  if est-op.m-code ne "" then do:
    create op.
    assign
     op.blank-no = est-op.b-num
     op.dept     = est-op.dept
     op.form-no  = est-op.s-num
     op.i-no     = if est-op.b-num ne 0 then bf-eb.stock-no else ""
     op.line     = est-op.line
     op.m-code   = est-op.m-code
     op.mr-fixoh = mach.mr-fixoh
     op.mr-hr    = est-op.op-mr
     op.mr-rate  = mach.mr-rate
     op.mr-varoh = mach.mr-varoh
     op.mr-waste = est-op.op-waste
     op.pass     = est-op.op-pass
     op.run-hr   = oprun
     op.wst-prct = est-op.op-spoil
     op.speed    = est-op.op-speed.

    if est-op.op-sb then
      op.run-qty = est-op.num-sh * v-on-f.
    else
      op.run-qty = est-op.num-sh * v-num-up *
                   (if xef.n-out   eq 0 then 1 else xef.n-out) *
                   (if xef.n-out-l eq 0 then 1 else xef.n-out-l).
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq op.i-no
        no-lock no-error.
    if avail itemfg and est-op.b-num ne 0 then op.i-name = itemfg.i-name.
    if op.line >= 500 then op.line = op.line - 500.
  end.

  assign
   op-tot[1] = op-tot[1] + est-op.op-mr
   op-tot[2] = op-tot[2] + oprun
   op-tot[3] = op-tot[3] + opmr$
   op-tot[4] = op-tot[4] + oprun$
   op-tot[5] = op-tot[5] + optot$
   op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
   op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2]
   r-spoil   = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100)).
end.

for each op:
  release w-op.
  if v-summ then
  find first w-op where w-op.m-code eq op.m-code no-lock no-error.

  if not avail w-op then do:
    create w-op.
    assign
     w-op.m-code = op.m-code
     w-op.rec-id = recid(op).
  end.

  assign
   w-op.mr-hr  = w-op.mr-hr  + op.mr-hr
   w-op.run-hr = w-op.run-hr + op.run-hr
   w-op.units  = w-op.units  + op.run-qty
   w-op.speed  = w-op.speed  + (op.speed * op.run-qty).
end.

for each op,
    first est-op
    where est-op.company = xest.company
      AND est-op.est-no  eq xest.est-no
      and est-op.m-code eq op.m-code
      and est-op.s-num  eq op.form-no
      and est-op.b-num  eq op.blank-no
      and est-op.line   gt 500
    no-lock,

    first mach
    {sys/look/machW.i}
      and mach.m-code eq est-op.m-code
    no-lock,
    
    first w-op where w-op.rec-id eq recid(op)

    break by op.form-no by op.blank-no by est-op.d-seq by op.pass

    with frame ae down no-labels no-box:

  assign
   opmr$  = w-op.mr-hr  * est-op.op-rate[1]
   oprun$ = w-op.run-hr * est-op.op-rate[2].

  if opmr$  = ? then opmr$  = 0.
  if oprun$ = ? then oprun$ = 0.

  optot$ = opmr$ + oprun$.

  if v-min-mchg and mach.mrk-rate gt optot$ then
    assign
     v-factor = (mach.mrk-rate / optot$)

     opmr$  = opmr$  * v-factor
     oprun$ = oprun$ * v-factor
     optot$ = mach.mrk-rate.
  ASSIGN op.opmr = opmr$
         op.oprun = oprun$
         op.optot = optot$. 

  IF NOT gEstSummaryOnly THEN
    display string(est-op.s-num,">9") + "-" +
          string(est-op.b-num,"99")          format "x(5)"
            when not v-summ
          est-op.m-dscr                      format "x(16)"
          w-op.mr-hr                         format ">>9.99"
          w-op.run-hr                to 35   format ">>9.99"
          op.speed                   to 42   format ">>>>>9"
            when not v-summ
          w-op.speed / w-op.units
            when v-summ              @ op.speed
          est-op.op-rate[2]          to 50   format ">>9.99"
          opmr$                      to 59   format ">>>>9.99"
          oprun$                     to 69   format ">>>>>9.99"
          optot$                     to 80   format ">>>>,>>9.99"
          SKIP
      WITH STREAM-IO.  
end.

IF gEstSummaryOnly THEN 
    FOR EACH op,
        first est-op where est-op.company = xest.company
                  AND est-op.est-no  eq xest.est-no
                  and est-op.m-code eq op.m-code
                  and est-op.s-num  eq op.form-no
                  and est-op.b-num  eq op.blank-no
                  and est-op.line   gt 500 NO-LOCK,
        first w-op where w-op.rec-id eq recid(op)
        BREAK BY op.m-code :

       
        ACCUM w-op.mr-hr (TOTAL BY op.m-code).
        ACCUM w-op.run-hr (TOTAL BY op.m-code).
        ACCUM op.speed (TOTAL BY op.m-code).
        ACCUM est-op.op-rate[2] (TOTAL BY op.m-code).
        ACCUM op.opmr (TOTAL BY op.m-code).
        ACCUM op.oprun (TOTAL BY op.m-code).
        ACCUM op.optot (TOTAL BY op.m-code).

        IF LAST-OF(op.m-code) THEN DO:
           DISP est-op.m-dscr format "x(21)"
                ACCUM TOTAL BY op.m-code w-op.mr-hr @ w-op.mr-hr format ">>9.99"
                ACCUM TOTAL BY op.m-code w-op.run-hr @ w-op.run-hr format ">>9.99"
                /*ACCUM TOTAL BY op.m-code op.speed @*/ op.speed format ">>>>9"
                /*ACCUM TOTAL BY op.m-code est-op.op-rate[2] @*/ est-op.op-rate[2] format ">>>9.99"
                ACCUM TOTAL BY op.m-code op.opmr @ op.opmr format ">>>>9.99"
                ACCUM TOTAL BY op.m-code op.oprun @ op.oprun format ">>>>>9.99"
                ACCUM TOTAL BY op.m-code op.optot @ op.optot FORMAT ">>>>,>>9.99"
               WITH FRAME opsum NO-LABEL DOWN STREAM-IO NO-BOX.
           DOWN WITH FRAME opsum.
        END.
       /* IF LAST(op.m-code) THEN DO:
           DISP "Total" @ est-op.m-dscr
                ACCUM TOTAL w-op.mr-hr @ w-op.mr-hr
                ACCUM TOTAL w-op.run-hr @ w-op.run-hr
                ACCUM TOTAL op.speed @ op.speed
                ACCUM TOTAL est-op.op-rate[2] @ est-op.op-rate[2]
                ACCUM TOTAL op.opmr @ op.opmr
                ACCUM TOTAL op.oprun @ op.oprun
                ACCUM TOTAL op.optot @ op.optot
               WITH FRAME opsum NO-LABEL DOWN STREAM-IO.
           DOWN WITH FRAME opsum.
        END.
        */
    END.
/* end ---------------------------------- copr. 1998  advanced software, inc. */

