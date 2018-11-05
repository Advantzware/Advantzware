/* rm-rcpt.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.i-no" THEN
DO:
  FIND FIRST item
      WHERE item.company = gcompany
        AND item.i-no = {&FIRST-EXTERNAL-TABLE}.i-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  rm-rcpt_i-name = IF NOT AVAILABLE item THEN ""
                  ELSE item.i-name.
  DISPLAY rm-rcpt_i-name.
END.
