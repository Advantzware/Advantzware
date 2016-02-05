/* ----------------------------------------------- sys/inc/perm.p 10/94 rd  */
/* permissions maintenance                                                    */
/* this checks for user's permissions to accesse.                            */
/* -------------------------------------------------------------------------- */
def input parameter cocode like company.company no-undo.
def input parameter col-error as char no-undo.
def output parameter v-allowed as logical init yes no-undo.
def var v-string as char no-undo.

if substring(program-name(2),length(program-name(2)),1) eq "p" then
  assign v-string = substring(program-name(2),1,length(program-name(2)) - 1) 
		    + "r".
else
  assign v-string = substring(program-name(2),1,length(program-name(2)) - 1) 
		    + "p".

if userid(ldbname(1)) ne ""            and
   (not program-name(2) begins "menu") then do for permx:
   find first permx where permx.task eq program-name(2) no-lock no-error.
   if not avail permx then
     find first permx where permx.task eq v-string no-lock no-error.
   /**if available permx and not can-do(users,userid(ldbname(1)) ) then**/
   if available permx and (not can-do(permx.users,userid(ldbname(1))) or
     (lookup(userid(ldbname(1)),permx.users) = 0 and
     lookup("*",permx.users) = 0)) then
   _task:
   do with frame task:
      for each usr-grp where usr-grp.company = cocode and
			     usr-grp.uid     = userid(ldbname(1))
			     no-lock:
	 if can-do(permx.users, usr-grp.usr-grp) then leave _task.
      end.
      v-allowed = no.
      pause 0.
      repeat on error undo, return:
	display skip(1) "   You are not authorized to run this procedure.   "
		skip(1)
		with color value(col-error) row 19 centered overlay no-box
		frame notauth.
	pause 5 message "Press any key to continue.".
	readkey pause 0.
	leave.
      end.
      hide frame notauth.
   end.
end.
/* end ---------------------------------- copr. 1994  advanced software, inc. */
