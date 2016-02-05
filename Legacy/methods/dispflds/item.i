/* item.i */
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

  WHEN "fi_mat-type" THEN
  DO:
    FIND mat
        WHERE mat.mat = fi_mat-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    mat_dscr = IF NOT AVAILABLE mat THEN ""
               ELSE mat.dscr.
    DISPLAY mat_dscr.
    {custom/mattypes.i} 
  END.

  WHEN "{&FIRST-EXTERNAL-TABLE}.procat" THEN
  DO:
    {custom/getcmpny.i}
    FIND procat
        WHERE procat.company = gcompany
          AND procat.procat = {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    procat_dscr = IF NOT AVAILABLE procat THEN ""
                  ELSE procat.dscr.
    DISPLAY procat_dscr.
  END.
