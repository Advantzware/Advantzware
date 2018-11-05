/* rfq.i */
  WHEN "{&FIRST-EXTERNAL-TABLE}.sman" THEN
  DO:
    {custom/getcmpny.i}
    FIND sman where sman.company = gcompany and
                    sman.sman = {&FIRST-EXTERNAL-TABLE}.sman:SCREEN-VALUE
         no-lock no-error.
    sman_sname = IF NOT AVAILABLE sman THEN "" ELSE sman.sname.
    DISPLAY sman_sname.
    IF AVAIL sman THEN {&FIRST-EXTERNAL-TABLE}.comm:SCREEN-VALUE = string(sman.scomm,">>9.99").
  END.
 
