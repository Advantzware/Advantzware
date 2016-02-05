/* ---------------------------------------------- oe/rep/bolcolor.i 07/09 GDM */
/* N-K BOLFMT = ALLPKG2                                                         */
/* -------------------------------------------------------------------------- */ 
 
   put  "<FArial>"  SKIP
          "<P14><C+40><B>Delivery Receipt</B>     Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" SKIP
          "<C1><#1><R+5><C+25>"
          "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"

          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+3><C3>"  
         "<=1><R+4><C3>" v-comp-add1 SKIP
         "<=1><R+5><C3>" v-comp-add2 SKIP
         "<=1><R+6><C3>" v-comp-add3 SKIP
         "<=1><R+7><C3>" v-comp-add4 SKIP
         "<=1><R+8><C3>" v-comp-add5 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         "<=1><R+9><C3>" lv-email SKIP(1).

/*    IF v-shp2brk                                      */
/*      THEN                                            */
/*       PUT                                            */
/*        "<FCourier New>"                              */
/*                "Ship To: "    AT 59 v-ship-id   SKIP */
/*                v-ship-name    AT 45 skip             */
/*                v-ship-addr[1] AT 45 SKIP             */
/*                v-ship-addr[2] AT 45 SKIP             */
/*                v-ship-addr3   AT 45 SKIP.            */
/*                                                      */
/*      ELSE                                            */
      PUT
        "<R14><C3><#2>" SKIP
        "<FArial>"
               "<=#2>Ship To: " AT 8 v-ship-id SKIP
               "<=#2><R+1>" v-ship-name SKIP
               "<=#2><R+2>" v-ship-addr[1] SKIP
               "<=#2><R+3>" v-ship-addr[2] SKIP
               "<=#2><R+4>" v-ship-addr3 SKIP.
/*                SPACE(5) "Sold To: " v-comp-Id   "Ship To: " AT 59 v-ship-id   SKIP */
/*                SPACE(5) v-comp-name    v-ship-name    AT 45 skip                   */
/*                SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP                   */
/*                SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP                   */
/*                SPACE(5) v-comp-addr3   v-ship-addr3   AT 45 SKIP.                  */

   PUT
        "<R7><C50><#3>" SKIP
        "<FArial><P14><=#3>" "<P10>" SKIP
                "<=#3><B>Receipt#: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+5>Acct Code: " v-cust-no SKIP
                "<=#3><R+7>Customer: " v-comp-name SKIP
                "<=#3><R+8>Contact: " v-contact SKIP
                "<=#3><R+9>Phone: " v-phone-num SKIP
                 SKIP 
                "<R20><C1><#4><FROM><R24><C80><RECT>" SKIP
                "<R22><C1><FROM><R22><C80><LINE>" SKIP    
                "<R20><C12><FROM><R24><C12><LINE>" SKIP
                "<R20><C38><FROM><R24><C38><LINE>" SKIP
                "<R20><C46><FROM><R24><C46><LINE>" SKIP
                "<R20><C70><FROM><R24><C70><LINE>" SKIP
                "<FArial><=4><R+1><C2>Date<C13>Terms<C39>Tax<C47>Carrier<C71>Freight Terms" SKIP 
                "<FCourier New><=4><R+3><C2>" oe-bolh.bol-date "<C13>" v-terms "<C39>" v-tax "<C47>" carrier.dscr FORMAT "x(28)" "<C71>" v-frt-terms SKIP
                "<R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
                "<R25><C14><FROM><R27><C14><LINE>" SKIP
                "<R25><C26><FROM><R27><C26><LINE>" SKIP
                "<R25><C56><FROM><R27><C56><LINE>" SKIP  
                "<R25><C61><FROM><R27><C61><LINE>" SKIP
                "<R25><C67><FROM><R27><C67><LINE>" SKIP            
                "<R25><C74><FROM><R27><C74><LINE>" SKIP 
        "<FArial><R25><C74> Release" SKIP
        "<FArial><R26><C2>FG# / Order#<C15>PO#<C27>Customer Part#/Description<C57>Units<C62>Count<C68>Total<C76>P/C" SKIP
        "<FCourier New>"                                  
        .

        v-printline = v-printline + 17.
