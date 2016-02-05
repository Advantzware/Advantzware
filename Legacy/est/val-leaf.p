
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.
DEF INPUT PARAM ip-ext AS INT NO-UNDO.

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.

{sys/inc/var.i SHARED}


ASSIGN
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD.

DO WHILE VALID-HANDLE(lv-field-hdl):
  IF lv-field-hdl:NAME EQ "leaf" THEN DO:
    li = li + 1.

    lv-field-hdl:SCREEN-VALUE = CAPS(lv-field-hdl:SCREEN-VALUE).

    IF li EQ ip-ext OR ip-ext EQ ? THEN DO:
      IF lv-field-hdl:SCREEN-VALUE NE ""                     AND
         NOT CAN-FIND(FIRST item
                      WHERE item.company EQ cocode
                        AND item.i-no    EQ lv-field-hdl:SCREEN-VALUE
                        AND INDEX("WLF",item.mat-type) GT 0) THEN DO:
        MESSAGE "Invalid Leaf/Film, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO lv-field-hdl.
        RETURN ERROR.
      END.

      IF ip-ext NE ? THEN LEAVE.
    END.
  END.

  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.
