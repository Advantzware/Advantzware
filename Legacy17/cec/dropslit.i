
find first reftable NO-LOCK
    where reftable.reftable eq "dropslit"
      and reftable.company  eq xest.company
      and reftable.code     eq xest.est-no
    no-error.
if not avail reftable then do:
  create reftable.
  assign
   reftable.reftable = "dropslit"
   reftable.company  = xest.company
   reftable.code     = xest.est-no.
end.

if not vprint and v-drop-rc and j gt 1 then do:
  if avail reftable then
    v-drop-rc = reftable.loc eq "yes".
  else
    message "Drop Slitter if MSF > Minimum?" update v-drop-rc.
end.
DO TRANSACTION:
   FIND CURRENT reftable exclusive-LOCK NO-ERROR.
   reftable.loc = if v-drop-rc then "yes" else "no".
END.
FIND CURRENT reftable NO-LOCK NO-ERROR.

if v-drop-rc and j gt 1 then do:
  find first est-op
      where est-op.company eq xest.company
        and est-op.est-no  eq xest.est-no
        and est-op.s-num   eq xef.form-no
        and est-op.dept    eq "RC"
      no-lock no-error.

  if avail est-op then do:
    find first w-form where w-form.form-no eq xef.form-no no-error.
  
    if not avail w-form then do:
      create w-form.
      assign
       w-form.form-no = xef.form-no.
    end.

    w-form.min-msf = v-drop-rc.
  end.
end.
