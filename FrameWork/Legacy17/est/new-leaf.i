  DEF VAR lv-depts AS CHAR NO-UNDO.
                      
  DO WITH FRAME {&FRAME-NAME}:
    RELEASE item.
    IF ef.leaf[{1}]:SCREEN-VALUE EQ "" THEN
      ASSIGN
       ef.leaf-dscr[{1}]:SCREEN-VALUE = ""
       /* ef.leaf-snum{1}:screen-value = ""
       ef.leaf-bnum{1}:screen-value = ""    */
       ef.leaf-w[{1}]:screen-value = "0.0000"
       ef.leaf-l[{1}]:screen-value = "0.0000" .

    ELSE
    FIND FIRST item NO-LOCK
        WHERE item.company EQ gcompany
          AND item.i-no    EQ ef.leaf[{1}]:SCREEN-VALUE
          AND INDEX("WLF",item.mat-type) GT 0
        NO-ERROR.

    IF AVAIL item THEN DO:
      ASSIGN
       ef.leaf-dscr[{1}]:SCREEN-VALUE = item.i-name
       ef.leaf-snum[{1}]:SCREEN-VALUE = STRING(ef.form-no).
      IF ef.leaf-w[{1}]:SCREEN-VALUE  = "0.0000" THEN DO:
        ef.leaf-w[{1}]:SCREEN-VALUE    = IF item.r-wid NE 0 THEN STRING(item.r-wid)
                                                           ELSE STRING(item.s-wid).
        ef.leaf-l[{1}]:SCREEN-VALUE    = STRING(item.s-len).
      END.
      IF est.est-type EQ 1 OR est.est-type EQ 5 THEN
        ef.leaf-snum[{1}]:SCREEN-VALUE = "1".

      lv-depts = IF item.mat-type EQ "W" THEN "WN,WS" ELSE "FB,FS".

      FIND FIRST mach NO-LOCK
          WHERE est-op.company EQ est.company
            AND LOOKUP(mach.dept[1],lv-depts) GT 0
          NO-ERROR.
      IF AVAIL mach THEN
        ef.leaf-bnum[{1}]:SCREEN-VALUE = IF LOOKUP(mach.dept[1],"WN,FB") GT 0 
                                         THEN "1" ELSE "".
    END.
  END.
