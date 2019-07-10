/* ------------------------------------------------ ce/com/probeit.f 7/92 cd  */
/*                                                                            */
/* form statement - probe items file                                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

form
     probeit.cust-no
     probeit.part-no format "X(15)"
     probeit.bl-qty format ">>>>>>>9"
     probeit.yld-qty format ">>>>>>>9"
     probeit.fact-cost  format ">>>,>>9.99"
     probeit.full-cost  format ">>>,>>9.99"
     probeit.sell-price format ">>>,>>9.99"
     probeit.yrprice
     header
"                          Request    Yield     Fact       Full      Sell"  skip
"Cust. #  Part Number          Qty      Qty     Cost/M     Cost/M    Price/M On"
 skip
     with frame probeit stream-io width 200 down no-labels no-box no-underline.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
