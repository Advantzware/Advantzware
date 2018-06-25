/* sys/inc/BitMap.i */

DEFINE VARIABLE cBitMap LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK 
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "BitMap"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "BitMap"
        sys-ctrl.descrip = "Graphics\bigboxes"
        .
END. /* not avail */
cBitMap = sys-ctrl.descrip.
