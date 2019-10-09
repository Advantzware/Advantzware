/* machseq.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.charge_code" THEN
DO:
  FIND job-code
      WHERE job-code.code = {&FIRST-EXTERNAL-TABLE}.charge_code:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  ASSIGN
    job-code_cat = IF NOT AVAILABLE job-code THEN ""
                   ELSE job-code.cat
    job-code_dscr = IF NOT AVAILABLE job-code THEN ""
                    ELSE job-code.dscr.
  DISPLAY job-code_cat job-code_dscr.
END.
