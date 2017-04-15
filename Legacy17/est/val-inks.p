
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.
DEF INPUT PARAM ip-fld AS INT NO-UNDO.
DEF INPUT PARAM ip-ext AS INT NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.

DEF TEMP-TABLE w-type NO-UNDO
    FIELD w-typ LIKE item.press-type
    FIELD w-qty AS   INT
    FIELD w-seq AS   INT.


ASSIGN
 li           = 0
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD.

IF ip-fld EQ 2 THEN DO:
  DO WHILE VALID-HANDLE(lv-field-hdl):
    IF lv-field-hdl:TABLE EQ "eb"                                      AND
       lv-field-hdl:NAME EQ "i-code" + TRIM(STRING(ip-fld,">>>>>>>>")) THEN DO:

      li = li + 1.

      IF li NE ip-ext AND lv-field-hdl:SCREEN-VALUE NE "" THEN DO:
        FIND FIRST item
            {sys/look/itemivW.i}
              AND item.i-no EQ lv-field-hdl:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO:
          FIND FIRST w-type WHERE w-typ EQ item.press-type NO-ERROR.
          IF NOT AVAIL w-type THEN DO:
            CREATE w-type.
            ASSIGN
             w-typ = item.press-type
             w-seq = li.
          END.
          w-qty = w-qty + 1.
        END.
      END.
    END.

    lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
  END.

  FOR EACH w-type BREAK BY w-qty BY w-seq DESC:
    IF LAST(w-qty) THEN LEAVE.
    ELSE DELETE w-type.
  END.

  ASSIGN
   li           = 0
   lv-group-hdl = ip-frame-hdl:FIRST-CHILD
   lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE VALID-HANDLE(lv-field-hdl):
    IF lv-field-hdl:TABLE EQ "eb"                                      AND
       lv-field-hdl:NAME EQ "i-code" + TRIM(STRING(ip-fld,">>>>>>>>")) THEN DO:

      li = li + 1.

      IF li EQ ip-ext OR ip-ext EQ ? THEN DO:
        IF lv-field-hdl:SCREEN-VALUE NE "" THEN DO:
          lv-msg = "".

          FIND FIRST item
              {sys/look/itemivW.i}
                AND item.i-no EQ lv-field-hdl:SCREEN-VALUE
              NO-LOCK NO-ERROR.

          IF lv-msg EQ "" AND NOT AVAIL item THEN
            lv-msg = "Invalid entry, try help".

          IF lv-msg EQ "" AND AVAIL w-type AND item.press-type NE w-typ THEN
            lv-msg = "Incompatible Press Type for Ink: "   +
                     TRIM(CAPS(lv-field-hdl:SCREEN-VALUE)) +
                     ", please re-enter".

          IF lv-msg NE "" THEN DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO lv-field-hdl.
            RETURN ERROR.
          END.
        END.

        IF ip-ext NE ? THEN LEAVE.
      END.
    END.

    lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
  END.
END.

ELSE
DO WHILE VALID-HANDLE(lv-field-hdl):
  IF lv-field-hdl:TABLE EQ "eb"                                      AND
     lv-field-hdl:NAME EQ "i-code" + TRIM(STRING(ip-fld,">>>>>>>>")) THEN DO:

    li = li + 1.

    IF li EQ ip-ext OR ip-ext EQ ? THEN DO:
      IF lv-field-hdl:SCREEN-VALUE NE ""                             AND
         NOT CAN-FIND(FIRST item
                      {sys/look/itemivW.i}
                         AND item.i-no EQ lv-field-hdl:SCREEN-VALUE) THEN DO:

        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO lv-field-hdl.
        RETURN ERROR.
      END.

      IF ip-ext NE ? THEN LEAVE.
    END.
  END.

  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.
