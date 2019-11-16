/* ----------------------------------------------- cec/com/probeit.f 7/92 cd  */
/*                                                                            */
/* form statement - probe items file                                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

form
     space(1)
     probeit.cust-no
     probeit.part-no
     /*probeit.bl-qty*/
     probeit.yld-qty
     probeit.fact-cost  format ">,>>>,>>9.99"
     probeit.full-cost  format ">,>>>,>>9.99"
     probeit.sell-price format ">,>>>,>>9.99"
     header
"                                              Fact         Full        Sell"
skip
" Cust. #  Part Number         Quantity       Cost/M       Cost/M      Price/M"
     skip
     with frame probeit-large width 86 stream-io down no-labels no-box no-underline.

form
     space(1)
     probeit.cust-no
     probeit.part-no form "x(18)"
     eb.part-dscr1 form "x(20)"
     /*probeit.bl-qty*/
     probeit.yld-qty
     probeit.fact-cost  format ">,>>>,>>9.99"
     probeit.full-cost  format ">,>>>,>>9.99"
     probeit.sell-price format ">,>>>,>>9.99"
     header
"                                                                Fact         Full       Sell"
skip
" Cust. # Part Number     Description           Quantity       Cost/M       Cost/M    Price/M"
     skip
     with frame probeit-protagon-large width 100 stream-io down no-labels no-box no-underline.

form
     space(1)
     probeit.cust-no
     probeit.part-no
     probeit.bl-qty
     probeit.yld-qty
     probeit.fact-cost  format ">>,>>9.99"
     probeit.full-cost  format ">>,>>9.99"
     probeit.sell-price format ">>,>>9.99"
     header
"                                                   Fact     Full      Sell"
skip
" Cust. #  Part Number                 Quantity    Cost/M    Cost/M   Price/M"
     skip
     with frame probeit width 80 stream-io down no-labels no-box no-underline.

form
     space(1)
     probeit.cust-no
     probeit.part-no form "x(15)"
     eb.part-dscr1 form "x(20)"
     /*probeit.bl-qty*/
     probeit.yld-qty
     probeit.fact-cost  format ">>,>>9.99"
     probeit.full-cost  format ">>,>>9.99"
     probeit.sell-price format ">>,>>9.99"
     header
"                                                             Fact      Full       Sell"
skip
" Cust. # Part Number     Description           Quantity    Cost/M    Cost/M    Price/M"
     skip
     with frame probeit-protagon width 90 stream-io down no-labels no-box no-underline.
     
/* end ---------------------------------- copr. 1992  advanced software, inc. */
