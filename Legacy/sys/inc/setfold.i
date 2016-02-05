
DEF VAR v-allocf LIKE itemfg.alloc INIT YES.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SETFOLD"
    NO-ERROR.
v-allocf = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE v-alloc.

