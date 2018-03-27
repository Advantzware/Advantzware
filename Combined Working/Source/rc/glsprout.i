/*
01.04.97 by CAH on \\ricky\rprodemo Log#0000:
1.  Added stream param 1.
*/
IF distrib_summary THEN
DO:
  i = 0.
  FOR EACH wksum
      BREAK BY wksum.jsc:
    IF FIRST-OF (wksum.jsc) THEN
    DO:
      {rc/incr.i i}.
      IF i > 1 THEN
      jsc_list = jsc_list + ','.
      IF i < (imax - 1) THEN
      DO:
        jsc_list = jsc_list + STRING(wksum.jsc,'99').
        FIND {2} WHERE {2}.jsc = wksum.jsc NO-LOCK NO-ERROR.
        IF AVAIL {2} THEN
        sum_abbrev[i] = {2}.abbrev.
        ELSE
        sum_abbrev[i] = '*?*'.
      END.
    END.
  END.
  sum_abbrev[imax - 1] = 'OTHER'.
  sum_abbrev[imax] = 'NET'.
  DO i = 1 TO imax:    /* shift captions right in field */
    sum_abbrev[i] = FILL(' ',(12 - LENGTH(sum_abbrev[i])))
      + sum_abbrev[i].
  END.

  hdg_text = ws_glsum_hdg.  /* 9810 CAH to allow for centering */
  page {1}.
  VIEW {1} FRAME hdg-std.
  VIEW {1} FRAME f-sumhdr.
  /*
  display
    h1 h2 sum_abbrev[1 for 7].

  underline {1} h1 h2.
  underline {1}
    sum_abbrev[1]
    sum_abbrev[2]
    sum_abbrev[3]
    sum_abbrev[4]
    sum_abbrev[5]
    sum_abbrev[6]
    sum_abbrev[7].
  down {1} 1.

    */

  FOR EACH wksum
      BREAK BY wksum.per by wksum.acct BY wksum.jsc:

    FORM
      wksum.per
      wksum.acct
      sum_amt
      WITH FRAME f-sum DOWN WIDTH 144 NO-LABELS NO-BOX.

    i = LOOKUP(STRING(wksum.jsc,'99'),jsc_list).
    IF i = 0 THEN
    i = (imax - 1).
    sum_amt[i] = sum_amt[i] + wksum.amount.
    IF LAST-OF (wksum.acct) THEN
    DO:
      DO i = 1 TO (imax - 1): /* cross-foot totals */
        sum_amt[imax] = sum_amt[imax] + sum_amt[i].
      END.
      display {1}
        wksum.per
        wksum.acct
        sum_amt (TOTAL by wksum.per)
        WITH FRAME f-sum.
      DO i = 1 TO imax:
        sum_amt[i] = 0.
      END.
    END.
  END.
END.    /* print summary */
