DEF VAR tsdocksec-log AS LOG NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSDOCKSEC"
    no-lock no-error.

if not avail sys-ctrl then DO:
  find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSDOCKSEC"
      no-error.

  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.char-fld = ""
     sys-ctrl.log-fld  = no
     sys-ctrl.int-fld  = 0.
  end.

  assign
   sys-ctrl.name     = "TSDOCKSEC"
   sys-ctrl.descrip  = "TS Add Second to Logout Times of Emp. with Dock Min. if shift change".
end.

tsdocksec-log = sys-ctrl.log-fld.
