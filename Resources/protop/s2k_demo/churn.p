/* churn.p
 *
 * generate a lot of pointless logical io
 *
 */

define variable flgName as character no-undo initial "churn&1.flg".

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

do while true:

  for each vacation no-lock:	/* 12 records in sports2000 - a nice tiny table to beat the crap out of... */
  end.

  file-info:file-name = search( flgName ).
  if file-info:full-pathname = ? then leave.

end.

quit.
