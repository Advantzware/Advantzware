def var v-password{1} as char format "x(8)".

if {1} then do:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
	and sys-ctrl.name    eq "SECURITY"
      no-lock no-error.

  if avail sys-ctrl                     and
     v-password{1} ne sys-ctrl.char-fld then
  do on endkey undo, leave with frame passwrd{1} no-attr-space no-labels
				title " {2} " row 9 overlay centered
				color value(col-title) prompt value(col-input):
    update skip(1)
	   "  Password:"
	   v-password{1} blank space(2)
	   skip(1).
    {1} = v-password{1} eq sys-ctrl.char-fld.
  end.
end.
hide frame passwrd{1}.

if keyfunction(lastkey) eq "end-error" then next.
