/* ---------------------------------------------- ce/quote/quohop.i 03/99 JLF */
/* print quote items in HOP format                                            */
/* -------------------------------------------------------------------------- */

  FOR EACH xqitm OF xquo NO-LOCK BREAK BY xqitm.part-no:
    numfit = 0.

    FOR EACH xqqty OF xqitm NO-LOCK:
      numfit = numfit + 1.
    END.
    IF numfit LT 2 THEN numfit = 2.

    for each ef
        where ef.company eq xquo.company
          and ef.est-no  eq xquo.est-no
        no-lock,
        each eb of ef
        where eb.part-no eq xqitm.part-no
        no-lock:
      v-blank = string(eb.form-no,"99") + "-" + string(eb.blank-no,"99").
      leave.
    end.

    FIND FIRST xqqty OF xqitm NO-LOCK NO-ERROR.
    FIND FIRST style WHERE style.company = xqitm.company AND
                           style.style = xqitm.style NO-LOCK NO-ERROR.
    style-name = IF AVAIL style THEN style.dscr ELSE xqitm.style.

    DO i = 1 TO numfit WITH FRAME item-hop:
      

      if i eq 1 then do:
        trim-size = "".
        release fgcat.
        if avail eb then
        find first fgcat
            where fgcat.company eq cocode
              and fgcat.procat  eq eb.procat
            no-lock no-error.
        if avail fgcat and fgcat.comm gt 0 then
        do j = 1 to 3:
          run sys/inc/dec-frac.p (if j eq 1 then eb.len else
                                  if j eq 2 then eb.wid else eb.dep,
                                  64, output v-str[j]).
        end.
        else
          assign
           v-str[1] = string(eb.len,">>>9.9<<<")
           v-str[2] = string(eb.wid,">>>9.9<<<")
           v-str[3] = string(eb.dep,">>>9.9<<<").

        trim-size = trim(v-str[1]) + " x " +
                    trim(v-str[2]) + " x " +
                    trim(v-str[3]).

        display {1}
                xqitm.part-no
                xqitm.part-dscr1
                xqitm.i-coldscr.
      end.

      else
      if i eq 2 then
        display {1}
                style-name   @ xqitm.part-no
                trim-size    @ xqitm.part-dscr1
                xqitm.i-dscr @ xqitm.i-coldscr.
                
      if avail xqqty then do:
        assign
         v-line = xqqty.line
         xxx    = if xqqty.uom eq "L" or xqqty.uom eq "/L" then
                    xqqty.price
                  else
                  if xqqty.uom eq "C" or xqqty.uom eq "/C" then
                    ((xqqty.qty / 100) * xqqty.price)
                  else
                  if xqqty.uom eq "M" or xqqty.uom eq "/M" then
                    ((xqqty.qty / 1000) * xqqty.price)
                  else
                    (xqqty.qty * xqqty.price).
        display {1}
                xqqty.qty
                xqqty.price
                xqqty.uom
                xxx.

      end.
      else down {1} 1.
      down {1}.

      FIND NEXT xqqty OF xqitm NO-LOCK NO-ERROR.
    end.

    FOR EACH xqqty OF xqitm NO-LOCK,
        /* EACH xqchg OF xqqty NO-LOCK */
        EACH xqchg NO-LOCK
        WHERE xqchg.company EQ xqqty.company
          AND xqchg.loc EQ xqqty.loc
          AND xqchg.q-no EQ xqqty.q-no
          AND xqchg.line EQ xqqty.line
          AND xqchg.qty EQ xqqty.qty
        BREAK BY xqchg.qty
              BY xqchg.charge:

      IF FIRST-OF(xqchg.qty) THEN DO:
        IF FIRST(xqchg.qty) THEN PUT {1} SKIP(1).

        PUT {1}
            "For QTY: "
            TRIM(STRING(xqchg.qty,">>>>>>>")).
      END.

      IF xqchg.bill EQ "N" THEN
        PUT {1}
            xqchg.charge AT 18
            "N/C"        TO 126 SKIP.
      
      ELSE
      IF INDEX("TML",xqchg.bill) GT 0 THEN
        PUT {1}
            xqchg.charge       AT 18
            "Time & Materials" AT 38
            xqchg.amt          TO 126 SKIP.

      ELSE
      IF xqchg.bill EQ "W" THEN
        PUT {1}
            xqchg.charge    AT 18
            "Will Advise "  AT 46 skip.
    END.
  END.

  FOR EACH xqchg OF xquo
      WHERE xqchg.qty  EQ 0
        AND xqchg.line EQ 0
      BREAK BY xqchg.qty
            BY xqchg.charge:

    IF FIRST(xqchg.charge) THEN PUT {1} SKIP(1).

    IF xqchg.bill EQ "N" THEN
      PUT {1}
          xqchg.charge AT 18
          "N/C"        TO 126 SKIP.
      
    ELSE
    IF INDEX("TML",xqchg.bill) GT 0 THEN
      PUT {1}
          xqchg.charge       AT 18
          "Time & Materials" AT 38
          xqchg.amt          TO 126 SKIP.

    ELSE
    IF xqchg.bill EQ "W" THEN
      PUT {1}
          xqchg.charge    AT 18
          "Will Advise "  AT 46 skip.
  END.

  do i = 1 to 5:
    if xquo.comment[i] ne "" then do:
      put {1} skip(1).
      leave.
    end.
  end.

  do i = 1 to 5:
    if xquo.comment[i] ne "" then put {1} xquo.comment[i] at 32 skip.
  end.

  if ch-inst then
  for each est-inst
      where est-inst.company eq xquo.company
        and est-inst.est-no  eq xquo.est-no
        and est-inst.dept    ge fdept
        and est-inst.dept    le tdept
      no-lock break by est-inst.est-no by est-inst.dept:
    if first-of(est-inst.est-no) then 
      put {1}
          skip(1)
          "Dept" at 27 "Department Manufacturing Instructions" at 32 skip
          "----" at 27 "-------------------------------------" at 32 skip.

    put {1}
        est-inst.dept    at 27
        est-inst.inst[1] at 32 skip
        est-inst.inst[2] at 32 skip
        est-inst.inst[3] at 32 skip.
  end.

  put {1} skip(2).

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
