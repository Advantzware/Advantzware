/* ----------------------------------------------- cec/com/probeit.v 7/92 cd  */
/*                                                                            */
/* display statement - probe item file                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

if cerunc-dec eq 0 then
   display
     probeit.cust-no at 2
     probeit.part-no at 10 form "x(15)"
     eb.part-dscr1 at 26 form "x(20)"
     probeit.bl-qty @ probeit.yld-qty at 49
     probeit.fact-cost
     probeit.full-cost
     probeit.sell-price form ">>>,>>9.99".
else     
   display
      probeit.cust-no at 2
      probeit.part-no at 10 form "x(15)"
      eb.part-dscr1 at 26 form "x(20)"
      probeit.bl-qty @ probeit.yld-qty at 49
      probeit.fact-cost
      probeit.full-cost
      probeit.sell-price.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
