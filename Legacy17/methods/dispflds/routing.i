/* routing.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[1]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[1]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[2]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[2]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr2 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr2.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[3]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[3]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr3 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr3.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[4]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[4]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr4 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr4.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[5]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[5]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr5 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr5.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[6]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[6]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr6 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr6.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[7]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[7]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr7 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr7.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[8]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[8]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr8 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr8.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[9]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[9]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr9 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr9.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.m-code[10]" THEN
  DO:
  {custom/getcmpny.i}
  {custom/getloc.i}
    FIND FIRST mach
        WHERE mach.m-code = {&FIRST-EXTERNAL-TABLE}.m-code[10]:SCREEN-VALUE
          AND mach.company eq gcompany
          AND mach.loc     eq gloc
        NO-LOCK NO-ERROR.
    mach_m-dscr10 = IF NOT AVAILABLE mach THEN ""
                         ELSE mach.m-dscr.
    DISPLAY mach_m-dscr10.
  END.
