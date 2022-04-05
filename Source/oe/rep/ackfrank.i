/* oe/rep/ackfrank.i */

PUT "<FCourier New>"   /*<R+8>*/
    "<C3><R2><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP. 
    /*"<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP.  regular */
           
PUT "<=1><R+8><P10>"  /*bigger address */
    /*"<P10>"  /*regular font*/ */
    SKIP(1)
   "Bill To:" AT 6 /*SPACE(57) "Ship To:" */ SKIP
   SPACE(5) oe-ord.cust-name /*v-ship-adr[1] AT 65*/ skip
   SPACE(5) oe-ord.addr[1]  /*v-ship-adr[2]  AT 65 */ SKIP
   SPACE(5) oe-ord.addr[2] /*v-ship-adr[3]   AT 65 */ SKIP
   SPACE(5) v-addr3  /* v-ship-adr[4] AT 65*/  SKIP.

    /*
 IF lv-display-comp THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".
      */

v-printline = v-printline + 14.
PUT "<||3><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
    "<R8><C50><FROM><R8><C80><LINE>" SKIP      
    "<R4><C61><FROM><R6><C61><LINE>" SKIP
    "<R6><C65><FROM><R8><C65><LINE>" SKIP
    "<R8><C65><FROM><R10><C65><LINE>" SKIP
    .

PUT "<=#3><P10>" SKIP
    "<=#3> Customer ID       <B>ACKNOWLEDGEMENT</B>"
    "<=#3><R+2> Telephone                 Fax" 
    "<=#3><R+4> Contact               Order Date"
    "<=3><R+1> " oe-ord.cust-no  space(16) oe-ord.ord-no SPACE(5)
    "<=3><R+3> " v-cust-phone  space(6) cust.fax
    "<=3><R+5> " cust.contact FORM "x(15)" space(7) oe-ord.ord-date .

PUT "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
"<R21><C1><FROM><R21><C80><LINE>" SKIP    
"<R19><C11><FROM><R23><C11><LINE>" SKIP
"<R19><C22><FROM><R23><C22><LINE>" SKIP
"<R19><C38><FROM><R23><C38><LINE>" SKIP
"<R19><C49><FROM><R23><C49><LINE>" SKIP
"<R19><C65><FROM><R23><C65><LINE>" SKIP
"<R19><C73><FROM><R23><C73><LINE>" SKIP
.

PUT "<=4><R+1> Ready Date     FOB           Ship Via           Terms       Sales Person      Order#   Quote#" SKIP
"<=4><R+3> " oe-ord.due-date FORM "99/99/9999" space(2)
cust.fob-code FORM "x(14)" SPACE(1)
v-shipvia /*carrier.carrier*/ FORM "x(17)" SPACE(1)
oe-ord.terms-d /*terms.dscr*/ FORM "x(12)" SPACE(1) v-salesman FORM "x(18)" "<C65.5>" oe-ord.ord-no space(3) v-q-no SKIP.

PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C14.2><FROM><R26><C14.2><LINE>" SKIP
       "<R24><C27><FROM><R26><C27><LINE>" SKIP
     /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
       "<R24><C46><FROM><R26><C46><LINE>" SKIP
       "<R24><C56><FROM><R26><C56><LINE>" SKIP
       "<R24><C65.3><FROM><R26><C65.3><LINE>" SKIP
       "<R24><C69><FROM><R26><C69><LINE>" SKIP
       .

PUT "<=5><R+1> Customer Part#        PO#            Description         Ordered  Unit Price UOM   Ext. Price" SKIP(1).
v-printline = v-printline + 6.
