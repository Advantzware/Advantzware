/* --------------------------------------------------- oe/oe-pric1.i 5/93 rd  */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

if cust.auto-reprice then do:
   find first itemfg where itemfg.company eq cocode
                       and itemfg.i-no    eq v-i-item
                       no-lock no-error.
   class-qty[index("123456789XYZ",trim(itemfg.class)) + 1] =
       class-qty[index("123456789XYZ",trim(itemfg.class)) + 1] + v-i-qty.

   for each tmp-{1} of x{2} where RECID(tmp-{1}) <> RECID({1})
                            {3} no-lock:

       find first itemfg where itemfg.company eq cocode
  	           and itemfg.i-no  eq tmp-{1}.i-no
                  no-lock no-error.
       IF AVAIL itemfg THEN
          class-qty[index("123456789XYZ",trim(itemfg.class)) + 1] =
              class-qty[index("123456789XYZ",trim(itemfg.class)) + 1] + tmp-{1}.{4}.
   end.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

