/* sys/inc/jobduelim.i */
  
IF NOT CAN-FIND(first sys-ctrl WHERE
   sys-ctrl.company eq g_company AND
   sys-ctrl.name    eq "JOBDATESMAX") THEN
do:
   create sys-ctrl.
   assign
    sys-ctrl.company  = g_company
    sys-ctrl.name     = "JOBDATESMAX"
    sys-ctrl.module   = "JU1"
    sys-ctrl.log-fld  = NO
    sys-ctrl.descrip  = "Limit Job Due Date to Integer Value Days from Today?"
    sys-ctrl.char-fld = ""
    sys-ctrl.int-fld  = 180.
end.

       
