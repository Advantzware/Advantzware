 ASSIGN
 tot-ytd-sales = 0
 tot-ptd-sales = 0
 ptd-sales = 0
 ytd-sales = 0
 tot-ptd-cos = 0
 tot-ytd-cos = 0
 tot-ptd-gross = 0
 tot-ytd-gross = 0
 tot-ptd-gen = 0
 tot-ytd-gen = 0.

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.
  if avail period then
    assign v-year = period.yr
           v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.
    if avail period then
    assign v-year = period.yr
           v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   ASSIGN
   x = (56 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (57 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (78 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3.

  view frame r-top.
  view frame head-columns.

  /*  Sales Totals */
  for each account where account.company eq cocode and
                         account.type eq "R" use-index type no-lock:

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
        assign tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
    end.

    if pre-close then
    do:

      for each gltrans no-lock where gltrans.actnum eq account.actnum and
                                     gltrans.company eq cocode:
        if gltrans.tr-date le v-ptd and
           gltrans.tr-date >= period.pst and
           gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign tot-ptd-sales = tot-ptd-sales + gltrans.tr-amt.
        end.
      end.
    end.

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
                              xperiod.pnum = per-loop and
                              xperiod.yr = v-year
                              no-lock no-error.
      if avail xperiod then
        for each glhist no-lock where
            glhist.company EQ account.company AND
            glhist.actnum EQ account.actnum AND
            glhist.period = per-loop and
            glhist.tr-date >= xperiod.pst and
            glhist.tr-date <= xperiod.pend:
          assign tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
        end.
    end.

    assign tot-ytd-sales = tot-ytd-sales + account.cyr-open.

  end.  /* Sales Totals for each */

  assign tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
         tot-ptd-sales = - tot-ptd-sales
         tot-ytd-sales = - tot-ytd-sales.

  put "========  Sales  ========" skip(1).

  /*  Sales Totals */
  for each account where account.company eq cocode and
                         account.type eq "R" no-lock
                         use-index type break by account.actnum:

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
      assign ptd-sales = ptd-sales + glhist.tr-amt.
    end.


    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
                                     gltrans.company = cocode:
        if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign ptd-sales = ptd-sales + gltrans.tr-amt.
        end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-sales = ytd-sales + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((- ptd-sales / tot-ptd-sales) * 100)
             ytd-sales = ytd-sales + ptd-sales + account.cyr-open
             v-ytd-per = ((- ytd-sales / tot-ytd-sales) * 100).
      display account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
                           (- ytd-sales) @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign ptd-sales = 0
             ytd-sales = 0.
    end.
  end.  /* Sales Totals for each */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Sales" at 1
      tot-ptd-sales to 40
      100.00 to 50
      tot-ytd-sales to 65
      100.00 to 75 skip(1).
  put "====  Cost of Sales  ====" skip(1).

  /*  Cost of Sales */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-cos-no and
                         account.actnum <= v-e-cos-no
                         no-lock use-index account break by account.actnum:

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:

      assign ptd-cos = ptd-cos + glhist.tr-amt.
    end.

    if pre-close then
    do:
        for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
          if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
          do:
            if gltrans.period <> period.pnum then
              next.
            assign ptd-cos = ptd-cos + gltrans.tr-amt.
          end.
        end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-cos = ytd-cos + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
             ytd-cos = ytd-cos + ptd-cos + account.cyr-open
             v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
      display account.dscr ptd-cos @ ptd-sales v-ptd-per
                           ytd-cos @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-cos = tot-ptd-cos + ptd-cos
             tot-ytd-cos = tot-ytd-cos + ytd-cos
             ptd-cos = 0
             ytd-cos = 0.
    end.
  end.  /* Cost of Sales */

  assign tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
         tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Cost of Sales" at 1 tot-ptd-cos to 40
       ((tot-ptd-cos / tot-ptd-sales) * 100.00) to 50
      tot-ytd-cos to 65
       ((tot-ytd-cos / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Gross Margin" tot-ptd-gross to 40
      ((tot-ptd-gross / tot-ptd-sales) * 100) to 50
      tot-ytd-gross to 65
      ((tot-ytd-gross / tot-ytd-sales) * 100) to 75 skip(1).
  put "===  Operating Expenses  ===" skip(1).

  /*  Operating Expenses */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-oper-no and
                         account.actnum <= v-e-oper-no
                         no-lock use-index account break by account.actnum:

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
      assign ptd-oper = ptd-oper + glhist.tr-amt.
    end.


    if pre-close then
    do:
        for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
          if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
          do:
            if gltrans.period <> period.pnum then
              next.
            assign ptd-oper = ptd-oper + gltrans.tr-amt.
          end.
        end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-oper = ytd-oper + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((ptd-oper / tot-ptd-sales) * 100)
             ytd-oper = ytd-oper + ptd-oper + account.cyr-open
             v-ytd-per = ((ytd-oper / tot-ytd-sales) * 100).
      display account.dscr ptd-oper @ ptd-sales v-ptd-per
                           ytd-oper @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oper = tot-ptd-oper + ptd-oper
             tot-ytd-oper = tot-ytd-oper + ytd-oper
             ptd-oper = 0
             ytd-oper = 0.
    end.
  end.  /* Operating Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1 tot-ptd-oper to 40
       ((tot-ptd-oper / tot-ptd-sales) * 100.00) to 50
        tot-ytd-oper to 65
       ((tot-ytd-oper / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "General & Administrative" skip(1).

  /*  General/Admin Expenses */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-gen-no and
                         account.actnum <= v-e-gen-no
                         no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
                            period.pst <= v-ptd and
                            period.pend >= v-ptd no-lock no-error.

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
      assign ptd-gen = ptd-gen + glhist.tr-amt.
    end.

    if pre-close then
    do:
        for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
          if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
          do:
            if gltrans.period <> period.pnum then
              next.
            assign ptd-gen = ptd-gen + gltrans.tr-amt.
          end.
        end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-gen = ytd-gen + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
             ytd-gen = ytd-gen + ptd-gen + account.cyr-open
             v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
      display account.dscr ptd-gen @ ptd-sales v-ptd-per
                           ytd-gen @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-gen = tot-ptd-gen + ptd-gen
             tot-ytd-gen = tot-ytd-gen + ytd-gen
             ptd-gen = 0
             ytd-gen = 0.
    end.
  end.  /* Gen & Admin Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total General & Admin" at 1 tot-ptd-gen to 40
       ((tot-ptd-gen / tot-ptd-sales) * 100.00) to 50
        tot-ytd-gen to 65
       ((tot-ytd-gen / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Income Tax Expenses" skip(1).

  /*  Income Tax Expenses */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-inc-no and
                         account.actnum <= v-e-inc-no
                         no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
                            period.pst <= v-ptd and
                            period.pend >= v-ptd no-lock no-error.

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
      assign ptd-inc = ptd-inc + glhist.tr-amt.
    end.


    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
        if gltrans.tr-date le v-ptd and
           gltrans.tr-date >= period.pst and
           gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign ptd-inc = ptd-inc + gltrans.tr-amt.
        end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-inc = ytd-inc + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
             ytd-inc = ytd-inc + ptd-inc + account.cyr-open
             v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
      display account.dscr ptd-inc @ ptd-sales v-ptd-per
                           ytd-inc @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-inc = tot-ptd-inc + ptd-inc
             tot-ytd-inc = tot-ytd-inc + ytd-inc
             ptd-inc = 0
             ytd-inc = 0.
    end.
  end.  /* Operating Expenses */

  assign tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
         tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Income Tax Expense" at 1 tot-ptd-inc to 40
       ((tot-ptd-inc / tot-ptd-sales) * 100.00) to 50
        tot-ytd-inc to 65
       ((tot-ytd-inc / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1
       tot-ptd-exp to 40
       ((tot-ptd-exp / tot-ptd-sales) * 100.00) to 50
       tot-ytd-exp to 65
       ((tot-ytd-exp / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Net Income Before Taxes"
      (tot-ptd-gross - tot-ptd-exp) to 40 format "->>,>>>,>>9.99"
      (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) to 50
      (tot-ytd-gross - tot-ytd-exp) to 65 format "->>,>>>,>>9.99"
      (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip(1).

  /*  Other Expenses */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-oth-no and
                         account.actnum <= v-e-oth-no
                         no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
                            period.pst <= v-ptd and
                            period.pend >= v-ptd no-lock no-error.

    for each glhist no-lock where
        glhist.company EQ account.company AND
        glhist.actnum EQ account.actnum AND
        glhist.period = v-period and
        glhist.tr-date ge period.pst and
        glhist.tr-date le period.pend:
      assign ptd-oth = ptd-oth + glhist.tr-amt.
    end.


    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
        if gltrans.tr-date le v-ptd and
           gltrans.tr-date >= period.pst and
           gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign ptd-oth = ptd-oth + gltrans.tr-amt.
        end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        if avail xperiod then
          for each glhist no-lock where
              glhist.company EQ account.company AND
              glhist.actnum EQ account.actnum AND
              glhist.period = per-loop and
              glhist.tr-date >= xperiod.pst and
              glhist.tr-date <= xperiod.pend:
            assign ytd-oth = ytd-oth + glhist.tr-amt.
          end.
      end.

      assign v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
             ytd-oth = ytd-oth + ptd-oth + account.cyr-open
             v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
      display account.dscr ptd-oth @ ptd-sales v-ptd-per
                           ytd-oth @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oth = tot-ptd-oth + ptd-oth
             tot-ytd-oth = tot-ytd-oth + ytd-oth
             ptd-oth = 0
             ytd-oth = 0.
    end.
  end.  /* Other Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
      
  put "Net Income After Taxes"
      ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) to 40 format "->>,>>>,>>9.99"
      ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) to 50
      ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) to 65 format "->>,>>>,>>9.99"
      ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip. 
