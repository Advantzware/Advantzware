/* ------------------------------------------- oe/rep/ackallpack.i GDM 04200907*/
/* ORDER ACKNOLEDGEMENT for N-K-1-ACKHEAD = AllPacking                          */
/* ------------------------------------------------------------------------- */


      if not v-print-head       or
          v-print-fmt eq "HOP" THEN PUT
          /*format header*/
          "<C58>" oe-ord.ord-no  SPACE(5)
          oe-ord.ord-date  FORMAT "99/99/99" skip
          /* company.name at 10 */ skip(1)
          /* company.addr[1] at 10 */ v-salesman at 61 skip
          /* company.addr[2] at 10 */ skip(1)
          /* company.city at 10 company.state company.zip */ skip
          oe-ord.po-no at 61 skip(2) v-fob at 4 oe-ord.terms-d at 32
          v-shipvia at 58 format "x(22)"
          skip(5)
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 skip
          v-addr3          at 10 v-sold-addr3        at 54 skip
          v-cust-phone   at 10 skip(4)
           .
        else
        PUT  /*format header*/
          "<C62>ACKNOWLEDGEMENT" skip
          "Order No." at 60 "Order Date" at 70 skip
          "---------" at 60 "----------" at 70 skip
          oe-ord.ord-no at 61
          oe-ord.ord-date at 72 FORMAT "99/99/99" skip
          company.name at 10 skip
           company.addr[1] at 10 "Salesman:" at 61 v-salesman at 71 skip
          company.addr[2] at 10 skip
          company.city at 10 company.state company.zip "Customer PO#" at 61 skip
          "------------" at 61 skip
          oe-ord.po-no at 61 skip(2)
          "F.O.B." at 4 "Terms" at 32 "Ship VIA" at 58 skip
          "------" at 4 "-----" at 32 "--------" at 58 skip
          v-fob at 4 oe-ord.terms-d at 32 v-shipvia at 58 format "x(22)" skip(2)
          "Bill To:" at 5 "Sold To:" at 49 skip
          "--------" at 5 "--------" at 49 skip
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 skip
          v-addr3          at 10 v-sold-addr3        at 54 skip
          v-cust-phone   at 10 skip(1)
          "No." at 1 "Item Number" at 5 "Description" at 22
          "Ordered" at 60 "Price Per M" to 80 skip
          FILL("-",80) FORMAT "x(80)"
           .

