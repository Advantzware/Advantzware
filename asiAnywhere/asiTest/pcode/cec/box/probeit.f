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
     
/* end ---------------------------------- copr. 1992  advanced software, inc. */
