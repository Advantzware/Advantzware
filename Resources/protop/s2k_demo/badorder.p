/* badorder.p
 *
 * find orders that are missing order lines, bill to, *and* purchase orders
 *
 * this is deliberately horrible code that will cause very intense db activity
 *
 */

define variable i as integer no-undo.

for each order no-lock
   where not can-find( orderLine of order )
      and not can-find( billTo of order )
      and not can-find( purchaseOrder where string( poNum ) = order.po ) :

  i = i + 1.

end.

display i.
