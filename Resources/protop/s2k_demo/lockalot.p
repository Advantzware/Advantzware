/* lockalot.p
 *
 * generate a lot of pointless record locks
 *
 */

define variable flgName as character no-undo initial "lockalot&1.flg".
define variable lastPO  as integer no-undo.

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

find last purchaseOrder no-lock.
lastPO = purchaseOrder.poNum.

do while true transaction:

  for each purchaseOrder exclusive-lock where poStatus <> "":
  end.

  for each poLine exclusive-lock where poNum <= random( 1, lastPO ):
  end.

  pause 0.5.

  file-info:file-name = flgName.
  if file-info:full-pathname = ? then leave.

end.

quit.
