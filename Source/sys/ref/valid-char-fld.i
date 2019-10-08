/* valid-char-fld.i */

DEF VAR ls-name AS cha NO-UNDO.
DEF VAR ls-name-value AS cha NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ls-name = {&nameField}.
  IF CAN-DO(name-fld-list,ls-name) THEN DO:
    ls-name-value = str-init[LOOKUP(ls-name,name-fld-list)].
    IF NOT CAN-DO(ls-name-value,{&tableName}.char-fld:SCREEN-VALUE) THEN
      lv-msg = TRIM({&tableName}.char-fld:SCREEN-VALUE) + " is not on lookup".
  END.
     
  IF lv-msg NE "" AND ls-name EQ "FGWHSBIN" THEN DO:
    lv-msg = "".
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ gcompany
          AND fg-bin.i-no    EQ ""
          AND fg-bin.loc     EQ SUBSTR({&tableName}.char-fld:SCREEN-VALUE,1,5)
          AND fg-bin.loc-bin EQ SUBSTR({&tableName}.char-fld:SCREEN-VALUE,6)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN lv-msg = "FG Bin does not exist".
  END.

  IF lv-msg NE "" AND ls-name EQ "RMWHSBIN" THEN DO:
    lv-msg = "".
    IF {&tableName}.char-fld:SCREEN-VALUE NE "RMITEM" THEN DO:
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ gcompany
            AND rm-bin.loc     EQ SUBSTR({&tableName}.char-fld:SCREEN-VALUE,1,5)
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc-bin EQ SUBSTR({&tableName}.char-fld:SCREEN-VALUE,6)
          NO-LOCK NO-ERROR.
      IF NOT AVAIL rm-bin THEN lv-msg = "RM Bin does not exist".
    END.
  END.

  IF lv-msg EQ "" THEN
    IF ls-name EQ "TSPOST"                        AND
       {&tableName}.log-fld:SCREEN-VALUE EQ "no"      AND
       {&tableName}.char-fld:SCREEN-VALUE EQ "Actual" THEN
      ASSIGN
       {&tableName}.char-fld:SCREEN-VALUE = "Standard"
       lv-msg = "Actual Direct Labor Rate Update may be done only from Touch Screen Data Collection".

  IF lv-msg = "" AND ls-name = "TSPOSTFG" THEN DO:
     DO i = 1 TO NUM-ENTRIES({&tableName}.char-fld:SCREEN-VALUE):
        FIND FIRST mach WHERE mach.company = g_company AND mach.loc = g_loc
                          AND mach.m-code = ENTRY(i,{&tableName}.char-fld:SCREEN-VALUE) 
                          NO-LOCK NO-ERROR.
        IF NOT AVAIL mach THEN 
           lv-msg = "Machine: " + ENTRY(i,{&tableName}.char-fld:SCREEN-VALUE) .
        IF lv-msg <> "" THEN LEAVE.
     END.
  END.

  IF lv-msg EQ "" AND ls-name EQ "FGMASTER" AND
     {&tableName}.char-fld:SCREEN-VALUE NE "FGITEM" AND
     NOT CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ gcompany
                    AND itemfg.i-no    EQ {&tableName}.char-fld:SCREEN-VALUE) THEN
    lv-msg = "FG Item# does not exist, try help...".

  IF lv-msg NE "" THEN DO:
    MESSAGE TRIM({&tableName}.char-fld:LABEL) + " is not valid " +
            "(" + TRIM(lv-msg) + ")" +
            ", please try help..."
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO {&tableName}.char-fld.
    RETURN ERROR.
  END. 
END.
