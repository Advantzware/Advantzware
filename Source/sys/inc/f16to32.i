/* sys/inc/f16to32.i */

DEFINE VARIABLE v-16-or-32         AS DECIMAL   NO-UNDO INITIAL 0.16.
DEFINE VARIABLE li-16-32           AS DECIMAL   NO-UNDO INITIAL 16.
DEFINE VARIABLE v-cecscrn-char     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cecscrn-dec      AS LOG       NO-UNDO.
DEFINE VARIABLE v-cecscrn-decimals AS DECIMAL   NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "CECSCRN"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "CECSCRN"
        sys-ctrl.log-fld  = NO
        sys-ctrl.char-fld = "16th's"
        sys-ctrl.descrip  = "Show estimate dimension in 16th's or 32nd's"
        .   
END.

ASSIGN
    v-cecscrn-char     = sys-ctrl.char-fld
    v-cecscrn-decimals = sys-ctrl.dec-fld
    .

CASE sys-ctrl.char-fld:
    WHEN "32nd's" THEN
    ASSIGN
        K_FRAC     = 3.125
        v-16-or-32 = 0.32
        li-16-32   = 32
        .
    WHEN "Decimal" THEN
    ASSIGN
        K_FRAC        = 1
        v-16-or-32    = 1
        li-16-32      = 1
        v-cecscrn-dec = sys-ctrl.log-fld
        .
END CASE.
