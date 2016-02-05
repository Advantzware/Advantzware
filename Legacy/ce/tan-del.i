
RELEASE xest.
RELEASE eb.
RELEASE xef.

find first xest
    where xest.company eq xoe-ord.company
      and xest.est-no  eq xoe-ord.est-no
    no-error.

if avail xest THEN
find first eb
    where eb.company  eq xest.company
      and eb.est-no   eq xest.est-no
	  and eb.stock-no eq oe-ordl.i-no
    no-error.

if avail eb then 
find first xef
	where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
      and xef.form-no eq eb.form-no
    no-error.

if AVAIL xef THEN DO:
  
  IF xest.est-type eq 3 then do:
    {ce/tan/blk-del.i eb}
  end.
  else do:
    {ce/com/blk-del.i eb}
  end.

  v-qty-mod = yes.

  /*run oe/tancomup.p.*/
end.
