
DEF BUFFER b-mach FOR mach.

RELEASE mach.

IF begin_mach NE "" THEN
FIND FIRST mach
    WHERE mach.company EQ cocode
      AND mach.m-code  EQ begin_mach
    NO-LOCK NO-ERROR.

for each xest
    where xest.company eq cocode
      and xest.est-no  ge fest
      and xest.est-no  le test
      AND CAN-FIND(FIRST xeb
                   where xeb.company eq xest.company
                     and xeb.est-no  eq xest.est-no
                     and xeb.cust-no ge fcus
                     and xeb.cust-no le tcus)
    NO-LOCK:

  v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Updating Estimate " + TRIM(xest.est-no).

  FOR each xeb
      where xeb.company eq xest.company
        and xeb.est-no  eq xest.est-no
        and xeb.cust-no ge fcus
        and xeb.cust-no le tcus
      USE-INDEX est-no,
      
      first xef
      WHERE xef.company EQ xeb.company
        AND xef.est-no  EQ xeb.est-no
        AND xef.eqty    EQ xeb.eqty
        AND xef.form-no EQ xeb.form-no
       no-lock,

      first cust
      where cust.company eq xeb.company
        and cust.cust-no eq xeb.cust-no
      no-lock:

    IF tb_slsmn OR tb_comm THEN DO:
      IF tb_slsmn THEN xeb.sman = "".

      RUN ce/markup.p (xeb.company, ROWID(xeb), OUTPUT ld).

      RUN sys/inc/getsmncm.p (xeb.cust-no,
                              INPUT-OUTPUT xeb.sman,
                              xeb.procat,
                              ld,
                              OUTPUT ld).

      IF tb_comm THEN xeb.comm = ld.
    END.
  
    if tb_pallet                                    and
       cust.pallet ne "" and cust.case-bundle ne "" then do:
     
      find first item
          where item.company eq cocode
            and item.i-no    eq cust.case-bundle
          no-lock no-error.
      if avail item then do:
        xeb.cas-no = cust.case-bundle.
       
        find first e-item
            where e-item.company eq cocode
              and e-item.i-no    eq item.i-no
            no-lock no-error.
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq xeb.stock-no
            no-lock no-error.
        if avail e-item then
          assign
           xeb.cas-len = e-item.case-l
           xeb.cas-wid = e-item.case-w
           xeb.cas-dep = e-item.case-d
           xeb.cas-wt  = e-item.avg-w
           xeb.cas-pal = e-item.case-pall
           xeb.cas-cnt = if avail itemfg and xest.est-type le 4 then
                          itemfg.case-count else e-item.box-case.

        if xeb.cas-len eq 0 then xeb.cas-len = item.case-l.
        if xeb.cas-wid eq 0 then xeb.cas-wid = item.case-w.
        if xeb.cas-dep eq 0 then xeb.cas-dep = item.case-d.
        if xeb.cas-wt  eq 0 then xeb.cas-wt  = item.avg-w.
        if xeb.cas-pal eq 0 then xeb.cas-pal = item.case-pall.
        if xeb.cas-cnt eq 0 then xeb.cas-cnt =
                                    if avail itemfg and xest.est-type le 4 then
                                      itemfg.case-count else item.box-case.
      end.
 
      find first item
          where item.company eq cocode
            and item.i-no    eq cust.pallet
          no-lock no-error.
      if avail item then
        assign
         xeb.tr-no  = item.i-no
         xeb.tr-len = item.case-l
         xeb.tr-wid = item.case-w
         xeb.tr-dep = item.case-d.
    
      v-error = yes.
    
      if xest.est-type ge 5 then
        run cec/kpallet.p(input recid(xeb),
                          output v-cas-pal,
                          output v-tr-cnt,
                          output v-numstacks,
                          output v-stackcode,
                          output v-error).
                        
      if not v-error then
        assign
         xeb.cas-pal = v-cas-pal
         xeb.tr-cnt  = v-tr-cnt.
        
      else
      if xeb.cas-cnt ne 0 and xeb.cas-pal ne 0 then
        xeb.tr-cnt = xeb.cas-cnt * xeb.cas-pal.
    end.
  END.

  IF tb_slsmn THEN
  FOR EACH quotehd WHERE quotehd.company EQ xest.company
                     AND quotehd.loc EQ xest.loc
                     AND quotehd.est-no EQ xest.est-no
                     AND quotehd.cust-no GE fcus
                     AND quotehd.cust-no LE tcus,
      first cust where cust.company eq quotehd.company
                   and cust.cust-no eq quotehd.cust-no NO-LOCK:
    IF quotehd.sman <> cust.sman THEN quotehd.sman = cust.sman.
  END.

  IF AVAIL mach THEN
  FOR EACH est-qty
      WHERE est-qty.company EQ xest.company
        AND est-qty.est-no  EQ xest.est-no
      NO-LOCK,
      EACH ef
      WHERE ef.company EQ est-qty.company
        AND ef.est-no  EQ est-qty.est-no
        AND ef.eqty    EQ est-qty.eqty
      NO-LOCK,
      EACH eb
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.eqty    EQ ef.eqty
        AND eb.form-no EQ ef.form-no
        AND eb.cust-no GE fcus
        AND eb.cust-no LE tcus
      NO-LOCK
      BREAK BY (IF xest.est-type EQ 1 THEN est-qty.eqty ELSE 1)
            BY eb.form-no
            BY eb.blank-no:

    fil_id = ?.

    IF (mach.p-type EQ "B" AND FIRST-OF(eb.blank-no)) OR
       (mach.p-type NE "B" AND FIRST-OF(eb.form-no))  THEN DO:

      lv-eqty = 0.

      FOR EACH est-op
          WHERE est-op.company EQ est-qty.company
            AND est-op.est-no  EQ est-qty.est-no
            AND est-op.line    LT 500
          BY est-op.qty:
        lv-eqty = est-op.qty.
        LEAVE.
      END.

      li = 1.
      FOR EACH est-op
          WHERE est-op.company EQ xest.company
            AND est-op.est-no  EQ xest.est-no
            AND est-op.line    LT 500
          BY est-op.line DESC:
        li = est-op.line + 1.
        LEAVE.
      END.

      CREATE est-op.
      ASSIGN
       est-op.company    = xest.company
       est-op.est-no     = xest.est-no
       est-op.m-code     = mach.m-code
       est-op.auto       = NO
       est-op.line       = li
       est-op.s-num      = eb.form-no
       est-op.b-num      = IF mach.p-type EQ "B" THEN eb.blank-no
                           ELSE
                           IF xest.est-type EQ 5 THEN 1 ELSE 0
       est-op.op-pass    = 1
       est-op.qty        = IF xest.est-type LE 1 OR
                              xest.est-type EQ 5 OR
                              xest.est-type EQ 6 THEN est-qty.eqty ELSE lv-eqty
       est-op.d-seq      = mach.d-seq
       est-op.dept       = mach.dept[1]
       est-op.op-sb      = mach.p-type ne "B"
       est-op.m-code     = mach.m-code
       est-op.m-dscr     = mach.m-dscr
       est-op.op-spoil   = mach.run-spoil
       est-op.op-crew[1] = mach.mr-crusiz
       est-op.op-crew[2] = mach.run-crusiz.

      RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "M R",
                          INPUT-OUTPUT est-op.op-crew[1]).

      RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "RUN",
                          INPUT-OUTPUT est-op.op-crew[2]).

      ASSIGN
       est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                           mach.mr-varoh  + mach.mr-fixoh
       est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                           mach.run-varoh + mach.run-fixoh.
 
      li = 0.
      FOR EACH xop
          WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.qty     EQ est-op.qty
            AND xop.s-num   EQ est-op.s-num
            AND xop.b-num   EQ est-op.b-num
            AND xop.dept    EQ est-op.dept
            AND xop.line    LT 500
          NO-LOCK:
        IF RECID(xop) EQ RECID(est-op) THEN LEAVE.
        li = li + 1.
      END.
      est-op.op-pass = li + 1.

      li = 0.
      FOR EACH xop
          WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.line    LT 500
          BY xop.qty BY xop.s-num BY xop.b-num BY xop.d-seq BY xop.op-pass:

        {sys/inc/outstrPL.i xop SHARE}
        ASSIGN
         li       = li + 1
         xop.line = li.
     
        IF AVAIL reftable THEN reftable.loc = STRING(xop.line,"9999999999"). 
      END.

      fil_id  = RECID(est-op).
    END.

    IF LAST((IF xest.est-type EQ 1 THEN est-qty.eqty ELSE 1)) AND
       fil_id NE ?                                            THEN DO:

      v-recid = fil_id.

      FOR EACH xef 
          WHERE xef.company EQ est-qty.company
            AND xef.est-no  EQ est-qty.est-no:
        xef.op-lock = NO.
      END.    

      IF xest.est-type LE 4 THEN
        RUN ce/mach-rek.p (?).
      ELSE
        RUN cec/mach-rek.p (?).
  
      fil_id = v-recid.

      FOR EACH xef 
          WHERE xef.company EQ est-qty.company
            AND xef.est-no  EQ est-qty.est-no:
        xef.op-lock = YES.
      END.
    END.
  END.

  IF tb_m-r-crew OR tb_m-r-rate OR
     tb_run-crew OR tb_run-rate THEN
  FOR EACH est-op
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    LT 500
        AND est-op.m-code  GE begin_mach
        AND est-op.m-code  LE end_mach
        AND TRIM(est-op.m-code) NE "",
      FIRST b-mach NO-LOCK
      WHERE b-mach.company EQ est-op.company
        AND b-mach.m-code  EQ est-op.m-code,
      FIRST eb NO-LOCK
      WHERE eb.company   EQ est-op.company
        AND eb.est-no    EQ est-op.est-no
        AND eb.cust-no   GE fcus
        AND eb.cust-no   LE tcus
        AND eb.form-no   EQ est-op.s-num
        AND (eb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0):

    IF tb_m-r-crew THEN DO:
      est-op.op-crew[1] = b-mach.mr-crusiz.

      RUN est/getcrusz.p (ROWID(b-mach), ROWID(eb), est-op.dept, "M R",
                          INPUT-OUTPUT est-op.op-crew[1]).
    END.

    IF tb_run-crew THEN DO:
      est-op.op-crew[2] = b-mach.run-crusiz.

      RUN est/getcrusz.p (ROWID(b-mach), ROWID(eb), est-op.dept, "RUN",
                          INPUT-OUTPUT est-op.op-crew[2]).
    END.

    IF tb_m-r-rate THEN
      est-op.op-rate[1] = (b-mach.lab-rate[b-mach.lab-drate] * est-op.op-crew[1]) + 
                           b-mach.mr-varoh  + b-mach.mr-fixoh.

    IF tb_run-rate THEN
      est-op.op-rate[2] = (b-mach.lab-rate[b-mach.lab-drate] * est-op.op-crew[2]) + 
                          b-mach.run-varoh + b-mach.run-fixoh.
  END.
end.
