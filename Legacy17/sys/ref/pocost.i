
def var v-pocost  like sys-ctrl.char-fld.
def var v-hold-op like sys-ctrl.log-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "POCOST"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POCOST"
   sys-ctrl.descrip  = "Default Raw Material Cost via?   Hold Overprices?"
   sys-ctrl.char-fld = "JobFile"
   sys-ctrl.log-fld  = no.
  /*message "Default Raw Material Cost via?  (JobFile,Vendor)"
	  update sys-ctrl.char-fld.
  */
  run sys/ref/d-pocost.w  (output sys-ctrl.char-fld).
  MESSAGE "Hold Overprices?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

assign
 v-pocost  = sys-ctrl.char-fld
 v-hold-op = sys-ctrl.log-fld.
