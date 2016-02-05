/* sys/inc/jobpass.i */
  
DEF VAR v-jobpass AS LOG NO-UNDO.
DEF VAR v-jobpass-char AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "JOBPASS"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "JOBPASS"
     sys-ctrl.log-fld  = no
     sys-ctrl.descrip  = "Job File Password?"
     sys-ctrl.char-fld = "yorkie".
end.
ASSIGN v-jobpass = sys-ctrl.log-fld
       v-jobpass-char = sys-ctrl.char-fld.
