/* ----------------------------------------------- cec/com/probeit.v 7/92 cd  */
/*                                                                            */
/* display statement - probe item file                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

if cerunc-dec eq 0 then
   display
     probeit.cust-no  
     probeit.part-no  
     probeit.bl-qty @ probeit.yld-qty
     probeit.fact-cost
     probeit.full-cost
     probeit.sell-price form ">>>,>>9.99".
else     
   display
      probeit.cust-no  
      probeit.part-no  
      probeit.bl-qty @ probeit.yld-qty
      probeit.fact-cost
      probeit.full-cost
      probeit.sell-price.


/* end ---------------------------------- copr. 1992  advanced software, inc. */
