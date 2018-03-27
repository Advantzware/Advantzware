/* sys/inc/invexprt.i   Invoice Export/Ftp */
def var inexport-log like sys-ctrl.log-fld   init no no-undo.
def var inexport-cha like sys-ctrl.char-fld          no-undo.
def var inexport-desc like sys-ctrl.descrip          no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INEXPORT"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "INEXPORT"
   sys-ctrl.descrip  = "c:\tmp\"
   sys-ctrl.char-fld = /*if v-print-fmt eq "Frankstn" then "CIT" else "None"*/ "CIT"
   sys-ctrl.log-fld = /*if v-print-fmt eq "Frankstn" then YES else */ NO.
end.
ASSIGN inexport-cha = sys-ctrl.char-fld
       inexport-log = sys-ctrl.log-fld
       inexport-desc = sys-ctrl.descrip.

release sys-ctrl.

