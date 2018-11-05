/* eb.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.procat" THEN
  DO:
    {custom/getcmpny.i}
    FIND fgcat where fgcat.company = gcompany and
                     fgcat.cat = {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
         no-lock no-error.      
    fgcat_dscr = IF NOT AVAILABLE fgcat THEN "" ELSE fgcat.dscr.
    DISPLAY fgcat_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.sman" THEN
  DO:
    {custom/getcmpny.i}
    FIND sman where sman.company = gcompany and
                    sman.sman = {&FIRST-EXTERNAL-TABLE}.sman:SCREEN-VALUE
         no-lock no-error.
    sman_sname = IF NOT AVAILABLE sman THEN "" ELSE sman.sname.
    DISPLAY sman_sname.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.style" THEN
  DO:
    {custom/getcmpny.i}
    FIND style where style.company = gcompany and
                     style.style = {&FIRST-EXTERNAL-TABLE}.style:SCREEN-VALUE
         no-lock no-error.
    style_dscr = IF NOT AVAILABLE style THEN "" ELSE style.dscr.
    DISPLAY style_dscr.
  END.
