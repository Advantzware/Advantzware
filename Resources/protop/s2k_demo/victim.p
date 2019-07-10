/* victim.p
 *
 * be the victim of a record lock conflict - a short -lkwtmo may mean that this needs to be relaunched
 *
 */

define variable flgName as character no-undo initial "victim&1.flg".

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

loop: repeat:

  pause 5.

  file-info:file-name = flgName.
  if file-info:full-pathname = ? then leave loop.

  find first feedback exclusive-lock no-error.
  if available feedback then
    do:
      feedback.rating = random( 1, 9 ).
    end.

end.

quit.
