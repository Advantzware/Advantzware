/* updorder.p
 *
 * randomly update an order's salesRep
 *
 */

define variable flgName as character no-undo initial "updorder&1.flg".
define variable repList as character no-undo.

define variable i as integer no-undo.
define variable n as integer no-undo.

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

for each salesRep no-lock:
  repList = repList + "," +  salesRep.salesRep.
  n = n + 1.
end.

repList = trim( repList, "," ).

loop: do while true:

  for each order no-lock:

    run updSalesRep( order.orderNum, entry( random( 1, n ), repList )).

    file-info:file-name = flgName.
    if file-info:full-pathname = ? then leave loop.

  end.

end.

quit.


procedure updSalesRep:

  define input parameter oNum as integer   no-undo.
  define input parameter sRep as character no-undo.

  define buffer order for order.

  define buffer updOrder for order.

  do for updOrder transaction:

    /* for our purposes there is no point in waiting -- we just want to generate a bunch of bi notes
     */

    find updOrder exclusive-lock where updOrder.orderNum = oNum no-wait no-error.
    if available updOrder then
      do:
        assign
          updOrder.salesRep = sRep
          updOrder.instructions = ""
        .
        do i = 1 to random( 1, 8192 ):
          updOrder.instructions = updOrder.instructions + chr( random( 32, 122 )).
        end.
      end.

    /* we don't want to go completely crazy -- just "crazy enough to generate noticeable update activity"
     */

    pause 0.01.		/* this also increases the chances that the "curr bi cluster" will reflect this activity	*/

  end.

  return.

end.
