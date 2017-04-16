DEF VAR tscomplete-log AS LOG NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSCOMPLETE"
    no-lock no-error.

if not avail sys-ctrl then DO:
  find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSCOMPLETE"
      no-error.

  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.log-fld  = YES
     sys-ctrl.name     = "TSCOMPLETE"
     sys-ctrl.descrip  = "Show Touchscreen Is Opearation Complete Message?".
  end.
end.

tscomplete-log = sys-ctrl.log-fld.
