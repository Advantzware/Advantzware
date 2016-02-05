/* b-qthd.i - used in procedure getValueFields of est/b-qthd.w */

  DO:
    IF ipShow EQ 'Next' THEN DO:
      {1}Value[1] = {1}Value[2].
      FOR EACH bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                  AND bQuotehd.loc EQ g_loc
                                  AND bQuotehd.{1} GE {1}Value[1]
                                BY bQuotehd.{1}:
        ASSIGN
          i = i + 1
          {1}Value[2] = bQuotehd.{1}.
        IF i GE recCount THEN LEAVE.
      END. /* each bquotehd */
    END. /* next */
    ELSE DO:
      {1}Value[2] = {1}Value[1].
      FOR EACH bQuotehd NO-LOCK WHERE bQuotehd.company EQ g_company
                                  AND bQuotehd.loc EQ g_loc
                                  AND bQuotehd.{1} LE {1}Value[2]
                                BY bQuotehd.{1} DESCENDING:
        ASSIGN
          i = i + 1
          {1}Value[1] = bQuotehd.{1}.
        IF i GE recCount THEN LEAVE.
      END. /* each bquotehd */
    END. /* else (previous) */
    ASSIGN
      &IF '{1}' NE 'q-no' &THEN
      q-noValue[1] = 0
      q-noValue[2] = 99999
      &ENDIF
      &IF '{1}' NE 'quo-date' &THEN
      quo-dateValue[1] = 1.1.1950
      quo-dateValue[2] = 12.31.2049
      &ENDIF
      &IF '{1}' NE 'cust-no' &THEN
      cust-noValue[1] = ''
      cust-noValue[2] = 'zzzzzzzz'
      &ENDIF
      &IF '{1}' NE 'contact' &THEN
      contactValue[1] = ''
      contactValue[2] = 'zzzzzzzzzzzzzzzzzzzzzzzz'
      &ENDIF
      &IF '{1}' NE 'est-no' &THEN
      est-noValue[1] = ''
      est-noValue[2] = '99999999'
      &ENDIF
      &IF '{1}' NE 'rfq' &THEN
      rfqValue[1] = ''
      rfqValue[2] = 'zzzzzzzz'
      &ENDIF
      &IF '{1}' NE 'part-no' &THEN
      part-noValue[1] = ''
      part-noValue[2] = 'zzzzzzzzzzzzzzzz'
      &ENDIF
      .
  END. /* do */
