DEF VAR maxbreak-int AS INT NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "MAXBREAK"
    no-lock no-error.

if not avail sys-ctrl then DO:
  find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "MAXBREAK"
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
   sys-ctrl.name     = "MAXBREAK"
   sys-ctrl.descrip  = "TS Max Break Minutes for not applying Dock Time".
end.

maxbreak-int = sys-ctrl.int-fld * 60.
