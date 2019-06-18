/* lib/userlock.p
 *
 */

{lib/vstlib.i}

define input-output parameter pt_userLock as logical no-undo.

if integer( getStartUpX( "_Startup-LockTable", "(-L)", "Current Size of Lock Table" )) < 100000 or integer( getStartUpX( "_Startup-MaxUsers",  "(-n)", "Maximum Number of Users" )) < 200 then
  pt_userLock = yes.

return.
