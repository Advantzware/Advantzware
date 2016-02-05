
DEF VAR notepad-log AS LOG NO-UNDO.
DEF VAR notepad-chr AS char NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq gcompany
      and sys-ctrl.name    eq "NOTEPAD"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = gcompany
   sys-ctrl.name    = "NOTEPAD"
   sys-ctrl.descrip  = "Notepad instead of Standard Screen Output?".
end.

 notepad-log = sys-ctrl.log-fld.
 notepad-chr = sys-ctrl.char-fld.
 
