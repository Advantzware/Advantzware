DEF INPUT PARAM ip-company LIKE company.company NO-UNDO.
DEF OUTPUT PARAM op-shift-list AS CHAR NO-UNDO.
    

FOR EACH shifts NO-LOCK WHERE shifts.company EQ ip-company:
  op-shift-list = op-shift-list + STRING(shifts.shift,"x(5)") + " " +
                                  shifts.description + ",".
END.
IF SUBSTR(op-shift-list,LENGTH(TRIM(op-shift-list)),1) EQ "," THEN
  SUBSTR(op-shift-list,LENGTH(TRIM(op-shift-list)),1) = "".
