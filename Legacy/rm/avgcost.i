/* avgcost.i  */
FIND FIRST rm-ctrl WHERE rm-ctrl.company = gcompany NO-LOCK NO-ERROR.
v-avgcost = IF AVAIL rm-ctrl THEN rm-ctrl.avg-lst-cst ELSE NO.
