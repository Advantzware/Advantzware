
DEF INPUT PARAM ip-run       AS INT NO-UNDO.
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR lv-frame-val AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

{custom/framechk.i}


IF ip-run EQ 1 THEN
FOR EACH tt-frame:
  DELETE tt-frame.
END.

ELSE framechk-i-changed = NO.

ASSIGN
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD
 li           = 0.

DO WHILE VALID-HANDLE(lv-field-hdl):
  lv-frame-val = lv-field-hdl:SCREEN-VALUE NO-ERROR.

  IF NOT ERROR-STATUS:ERROR THEN DO:
    li = li + 1.

    IF ip-run EQ 1 THEN DO:
      CREATE tt-frame.

      ASSIGN
       tt-frame-seq = li
       tt-frame-val = lv-frame-val.
    END.

    ELSE DO:
      FIND FIRST tt-frame WHERE tt-frame-seq EQ li NO-ERROR.

      IF NOT AVAIL tt-frame OR tt-frame-val NE lv-frame-val THEN DO:
        framechk-i-changed = YES.
        LEAVE.
      END.
    END.
  END.

  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.
