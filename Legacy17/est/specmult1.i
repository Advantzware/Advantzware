/* oe/rep/bolhugh1.i */

      put 
         "<FArial>"  SKIP
          "<P18><C+30><B>Specification Sheet</B> " SKIP
          "<C1><#1><R+5><C+25>"  SKIP 
          "<P11><=1><R+4>" SKIP 
         "<FCourier New>" 
         space(5) "<FArial>" "Spec #:    "  "<B><FCourier New>" eb.est-no FORMAT "x(10)" "</B>" 
                "<FArial><C45>" "Sample #:    "  /* AT 117 */ "<B><FCourier New><C60>" v-sample-no "</B>" SKIP(1)
               "<FCourier New>" 

         space(5) "<FArial>"  "Cust:         " "<B><FCourier New>" v-customer FORMAT "x(25)" "</B>" 
                    "<FArial><C45>Date Receive:     " /* AT 92 */ "<B><FCourier New><C60>" v-date-rec "</B>" SKIP(1)

         space(5) "<FArial>"  "Attn:         " "<B><FCourier New>" v-attn "</B>"
                    "<FArial><C45>Date Due:          " /* AT 78 */"<FCourier New><B><C60>" v-date-due    "</B>" skip(1)

         space(5) "<FArial>" "Ship To:"  "<FArial><C45>" 
                "Sample Sent:      " /* AT 96 */ "<FCourier New><B><C60>" v-sample-sent "</B>" SKIP(1)

         SPACE(12) "<B>" v-ship-name    "</B>" "</B><FArial><C45>" 
                "# of Samples:     "  /*AT 78 */ "<FCourier New><B><C60>" v-no-samples  "</B>" SKIP(1)

         SPACE(12) "<B>" v-ship-addr[1] "</B>" "</B><FArial><C45>" 
               /* "Quantity:         " */ /*AT 78*/ "<FCourier New><B><C60>" ""            "</B>" SKIP(1)

         SPACE(12) "<B>" v-ship-addr[2] "</B>" "</B><FArial>" 
                "<FArial><C45>Caliper Form1:     " /* AT 78 */"<FCourier New><B><C60>" v-caliper1 "</B>" SKIP(1)

         SPACE(12) "<B>" v-ship-addr3   "</B>" "</B><FArial>" 
                  "<FArial><C45>" "Caliper Form2:       " /* AT 86 */ "<FCourier New><B><C60>" v-caliper2 "</B>"      SKIP(1)

         SPACE(5) "<FArial>" "Shipped From: " "<B>" "Multicell Packaging, Inc."  "</B><FArial>" 
                  "<FArial><C45>" "Part No.:            " /* AT 78 */ "<FCourier New><B><C60>" v-part "</B>"      SKIP(1)
         
         SPACE(12) "<FArial><C45>" "No. Cells:           " /* AT 101 */ "<FCourier New><B><C60>" IF v-no-cells GT "" THEN v-no-cells ELSE STRING((v-strips1 + 1) * (v-strips2 + 1)) "</B>"      SKIP(1)
                "<|10><R5><c3><#4><FROM><R65><C81><RECT>" SKIP
                "<R29><c3><FROM><R29><C81><LINE>" SKIP 
                "<R36><c3><FROM><R36><C81><LINE>" SKIP 
                "<R43><c3><FROM><R43><C81><LINE>" SKIP 
                "<R49><c3><FROM><R49><C81><LINE>" SKIP 
   
        "<FArial><=5><R+29>      Long Strip: " SKIP(1)
        SPACE(20) "<FArial><=5><R+31>           Length                      Height                      Strips                       Slots                      Interior                  End" SKIP(1)
        "<FCourier New>"     
        "<B>" SPACE(2) v-eb-len1 SPACE(10) v-eb-wid1 SPACE(6) v-strips1 SPACE(6) v-slot1 SPACE(6) scr-in-cell-length SPACE(2) scr-end-cell-l2 SKIP(2)
        "</B><FArial><=5><R+35>      Short Strip:  " SKIP(1)
        SPACE(20) "<FArial><=5><R+37>           Length                      Height                      Strips                       Slots                      Interior                  End" SKIP(1)
        "<FCourier New>"  
        "<B>" SPACE(2) v-eb-len2 SPACE(10) v-eb-wid2 SPACE(6) v-strips2 SPACE(6) v-slot2 SPACE(6) scr-in-cell-width SPACE(2) scr-end-cell-w2 SKIP(1)
        "</B><FArial><=5><R+44>      Special Inst: " SKIP(1)
        "<FArial><=5><R+48>      Mfg Inst: " space(450) "Shipping Instructions: " SKIP(1)
        .
            v-printline = v-printline + 16.
    

