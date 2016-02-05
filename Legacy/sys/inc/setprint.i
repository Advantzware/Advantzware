
DEF VAR v-alloc LIKE itemfg.alloc INIT YES.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SETPRINT"
    NO-ERROR.
IF AVAIL sys-ctrl THEN v-alloc = sys-ctrl.log-fld.

{sys/inc/setfold.i}

