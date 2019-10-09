/* sys/inc/print1.i */

if tmp-dir = "" then
DO:
   FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

   IF AVAIL users AND users.user_program[2] NE "" THEN
      tmp-dir = users.user_program[2].
   ELSE
      tmp-dir = "c:\tmp".
END.
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.


