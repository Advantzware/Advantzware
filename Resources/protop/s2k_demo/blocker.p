/* blocker.p
 *
 * grab the first feedback record with an exclusive-lock so that victim.p will block
 *
 */

define variable flgName as character no-undo initial "blocker&1.flg".

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

loop: do while true:

  find first feedback exclusive-lock no-error.
  feedback.rating = 0.

  do while true:

    pause 0.5.

    file-info:file-name = flgName.
    if file-info:full-pathname = ? then leave loop.

  end.

end.

quit.
