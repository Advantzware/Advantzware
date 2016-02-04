
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "JOBCARD" + "{1}"
    no-lock no-error.

if not avail sys-ctrl then do transaction:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "JOBCARD"
      no-error.

  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.char-fld = "10 Pitch"
     sys-ctrl.log-fld  = no.
  end.

  assign
   sys-ctrl.name     = "JOBCARD" + "{1}"
   sys-ctrl.descrip  = "Job Card or Factory Ticket Format " +
                       if "{1}" eq "" then "" 
                       else "(" + (if "{1}" eq "F" then "Fold" 
                                   else "Corr") + "ware)".
   
 /* if new sys-ctrl then message sys-ctrl.descrip update sys-ctrl.char-fld.*/
end.
