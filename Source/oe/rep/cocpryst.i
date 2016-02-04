 /* oe/rep/cocxprnt.i  */  
put "<FArial>"  SKIP
    "<P14><C+40><B>Packing Slip</B> " SKIP
    "<C1><#1><R+7><C+45><IMAGE#1=" ls-full-img1  SKIP /* APC package */  
    "<P12><C1><R-1><b> Prystup Packaging Products " SKIP
    "<C1> P.O. Box 1039" SKIP
    "<C1> Livingston, AL 35470 </b>" SKIP

    "<=1><R+11><P10>"
    "<FCourier New>" SKIP
     cust.name                    at 17
     v-ship-name                  at 63
     cust.addr[1]                 at 17
     v-ship-addr[1]               at 63
     cust.addr[2]                 at 17
     v-ship-addr[2]               at 63
     v-cust-addr3                 at 17
     v-ship-addr3                 at 63
     skip(1)
     v-po-no[1]                   at 33
     v-po-no[2]                   at 77
     skip(2)
     v-ord-date                   at 9
     oe-bolh.bol-date             at 23
     v-carrier-dscr               at 37 format "x(20)" 
     v-fob                        at 58
     v-ord-bol                    at 78
     skip(4)
    .
 v-printline = v-printline + 20.
