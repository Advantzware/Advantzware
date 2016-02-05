do for permx:
   find first permx where task = program-name(1) no-lock no-error.
   if available permx and not can-do( users,userid(ldbname(1)) ) then
   _task:
   do with frame task:
      for each usr-grp where usr-grp.company = cocode and
			     usr-grp.uid     = userid(ldbname(1))
			     no-lock:
	 if can-do(users, usr-grp.usr-grp) then leave _task.
      end.
      pause 0.
      display skip(1) "   You are not authorized to run this procedure.   "
	      skip(1)
	      with color value(col-error) row 19 centered overlay no-box.
      hide frame task.
      readkey pause 0.
      leave.
   end.
end.
