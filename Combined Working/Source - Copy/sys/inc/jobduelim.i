/* sys/inc/jobduelim.i */
  
IF NOT CAN-FIND(first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "JOBDUELIM") THEN
do:
   create sys-ctrl.
   assign
    sys-ctrl.company  = g_company
    sys-ctrl.name     = "JOBDUELIM"
    sys-ctrl.log-fld  = YES
    sys-ctrl.descrip  = "Limit Job Due Date to Six Months from Today?"
    sys-ctrl.char-fld = "".
end.

       
