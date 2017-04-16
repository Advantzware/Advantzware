/* prep.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.cost-type" THEN
  DO:
    {custom/getcmpny.i}
    {custom/getloc.i}
    FIND costtype
        WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    costtype_descr = IF NOT AVAILABLE costtype THEN ""
                     ELSE costtype.descr.
    DISPLAY costtype_descr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.mat-type" THEN
  DO:
    FIND LAST matprep
        WHERE matprep.mat = {&FIRST-EXTERNAL-TABLE}.mat-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    mat_dscr = IF NOT AVAILABLE matprep THEN ""
               ELSE matprep.dscr.
    DISPLAY mat_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.uom" THEN
  DO:
    FIND uom WHERE uom.uom = {&FIRST-EXTERNAL-TABLE}.uom:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    uom_dscr = IF NOT AVAILABLE uom THEN ""
               ELSE uom.dscr.
    DISPLAY uom_dscr.
  END.
