DEF VAR v-FGFreightClass AS LOG NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "FGFreightClass"
                    NO-LOCK NO-ERROR.


IF AVAIL sys-ctrl THEN
   v-FGFreightClass = sys-ctrl.log-fld.
ELSE
   v-FGFreightClass = NO.
