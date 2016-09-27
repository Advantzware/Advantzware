/*sys/inc/schedule.i */
def var schedule-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
   ASSIGN sys-ctrl.company  = cocode
            sys-ctrl.name     = "SCHEDULE"
            sys-ctrl.char-fld = "None"
            sys-ctrl.descrip  = "Update Due date and Promise date for Schedule?".
                             
END.
ASSIGN schedule-log = sys-ctrl.log-fld
       .
