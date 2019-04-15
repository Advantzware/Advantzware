
DEF TEMP-TABLE tt-cust-part FIELD cust-no  LIKE eb.cust-no
                            FIELD part-no  LIKE eb.part-no
                            FIELD stock-no LIKE eb.stock-no
                            FIELD qty-set  AS   DEC
                            INDEX cust-no cust-no part-no stock-no.


FOR EACH x-eb
    WHERE x-eb.company EQ xest.company 
      AND x-eb.est-no  EQ xest.est-no
      AND x-eb.form-no NE 0
    NO-LOCK BREAK BY x-eb.form-no:
  ll-one-part = FIRST(x-eb.form-no) AND LAST(x-eb.form-no).
  LEAVE.
END.

for each x-eb
    where x-eb.company = xest.company 
      AND x-eb.est-no eq xest.est-no
    BY x-eb.form-no BY x-eb.blank-no:

  if x-eb.form-no eq 0 then tmpstore = {1}.

  else do:
    FIND FIRST tt-cust-part
        WHERE tt-cust-part.cust-no EQ x-eb.cust-no
          AND tt-cust-part.part-no EQ x-eb.part-no
        NO-ERROR.

    IF NOT AVAIL tt-cust-part THEN DO:
      CREATE tt-cust-part.
      ASSIGN
       tt-cust-part.cust-no  = x-eb.cust-no
       tt-cust-part.part-no  = x-eb.part-no
       tt-cust-part.stock-no = x-eb.stock-no.
    END.

    assign
     v-item-no  = tt-cust-part.stock-no
     v-part-qty = if x-eb.cust-% ne 0 then x-eb.cust-% else x-eb.quantityPerSet.

    IF v-part-qty LT 0 THEN v-part-qty = -1 / v-part-qty.

    tt-cust-part.qty-set = tt-cust-part.qty-set + v-part-qty.

    IF v-item-no EQ "" THEN
        RUN fg/GetFGItemID.p (ROWID(x-eb), tmpstore, OUTPUT v-item-no). 
     
    IF ll-one-part THEN v-item-no = tmpstore.

    find first fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq tmpstore
          and fg-set.part-no eq v-item-no
        no-error.
    if not avail fg-set then do:
      find last fg-set use-index s-no no-error.
      y = if avail fg-set then fg-set.s-no else 0.

      find last fg-set
          where fg-set.company eq cocode
            and fg-set.set-no  eq tmpstore
          no-lock no-error.
      x = if avail fg-set then fg-set.line else 0.

      create fg-set.
      assign
       fg-set.company  = cocode
       fg-set.set-no   = tmpstore
       fg-set.part-no  = v-item-no
       fg-set.s-no     = y + 1
       fg-set.line     = x + 1.
    end.

    ASSIGN
     x-eb.stock-no         = v-item-no
     tt-cust-part.stock-no = x-eb.stock-no.

     fg-set.qtyPerSet = tt-cust-part.qty-set.
  end.
end. /* each eb */

for each fg-set
    where fg-set.company eq cocode
      and fg-set.set-no  eq tmpstore
    by fg-set.line desc:
  fg-set.line = fg-set.line + 10000.
end.

x = 0.
for each fg-set
    where fg-set.company eq cocode
      and fg-set.set-no  eq tmpstore
    by fg-set.line:
  assign
   x           = x + 1
   fg-set.line = x.
end.
