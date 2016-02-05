/* patchEnd.i */

FIND CURRENT patchhst EXCLUSIVE-LOCK.
ASSIGN
  patchhst.patch-date = TODAY
  patchhst.patch-time = TIME
  patchhst.user_id = USERID('NoSweat')
  patchhst.completed = YES.
