
DEF INPUT  PARAM ip-frame-hdl AS HANDLE NO-UNDO.
DEF INPUT  PARAM ip-field-list AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-enabled AS LOG INIT NO NO-UNDO.

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.


ASSIGN
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD.

DO WHILE VALID-HANDLE(lv-field-hdl) AND NOT op-enabled:
  IF lv-field-hdl:NAME NE ""                     AND
     INDEX(ip-field-list,lv-field-hdl:NAME) NE 0 AND
     lv-field-hdl:SENSITIVE                      THEN
    op-enabled = YES.

  ELSE lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.
