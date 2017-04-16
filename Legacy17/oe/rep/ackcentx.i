/* oe/rep/ackxprnt.i */

PUT "<FMS Mincho>"   /*<R+8>*/
    "<C3><R1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP. 
    /*"<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP.  regular */
           
PUT "<=1><R+10><P10>"  /*bigger address */
    /*"<P10>"  /*regular font*/ */
    SKIP(1)
   "Bill To:" AT 6 /*SPACE(57) "Sold To:"  */ SKIP
   SPACE(5) oe-ord.cust-name 
/*    ( IF oe-ord.sold-name = "" THEN oe-ord.cust-name ELSE oe-ord.sold-name) AT 71*/  skip
   SPACE(5) oe-ord.addr[1] 
    /*(IF oe-ord.sold-addr[1] = "" THEN oe-ord.addr[1] ELSE oe-ord.sold-addr[1]) AT 71*/ SKIP
   SPACE(5) oe-ord.addr[2] 
    /*(IF oe-ord.sold-addr[2] = "" THEN oe-ord.addr[2] ELSE oe-ord.sold-addr[2]) AT 71 */ SKIP
   SPACE(5) v-addr3  /*v-sold-addr3 AT 71*/  SKIP.
    /*
 IF lv-display-comp THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".
      */

v-printline = v-printline + 14.
PUT "<||3><R4><C50><#3><FROM><R12><C80><RECT>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
    "<R8><C50><FROM><R8><C80><LINE>" SKIP      
    "<R10><C50><FROM><R10><C80><LINE>" SKIP      
    "<R4><C61><FROM><R6><C61><LINE>" SKIP
    "<R6><C65><FROM><R12><C65><LINE>" SKIP
    .

PUT "<=#3><P10>" SKIP
    "<=#3> Customer ID          <B>ACKNOWLEDGEMENT</B>"
    "<=#3><R+2> Telephone                    Fax" 
    "<=#3><R+4> Contact                   Order Date "
    "<=3><R+6> Status                Date and Time"
    "<=3><R+1> " oe-ord.cust-no  space(16) oe-ord.ord-no SPACE(5)
    "<=3><R+3> " v-cust-phone  space(10) cust.fax
    "<=3><R+5> " cust.contact FORM "x(15)" space(11) oe-ord.ord-date .
 IF LENGTH(lv-prt-sts) < 38 THEN /*revised*/
      PUT  "<=3><R+7> " lv-prt-sts FORM "x(38)" SPACE(12)
     lv-prt-date space(1) lv-prt-time.
 else /*Original*/
      PUT   "<=3><R+7> " lv-prt-sts FORM "x(38)" SPACE(13)
           lv-prt-date space(1) lv-prt-time.


/* same as request - no space between hd & line
PUT "<R11><#4><R17><C80><RECT>" SKIP
"<R13><C1><FROM><R13><C80><LINE>" SKIP
"<R15><C1><FROM><R15><C80><LINE>" SKIP

"<R11><C20><FROM><R15><C20><LINE>" SKIP
"<R11><C30><FROM><R15><C30><LINE>" SKIP
"<R11><C40><FROM><R15><C40><LINE>" SKIP
"<R11><C50><FROM><R15><C55><LINE>" SKIP
"<R11><C60><FROM><R15><C60><LINE>" SKIP
"<R11><C70><FROM><R15><C70><LINE>" SKIP
.
*/  

PUT "<R16><C77><FROM><R+2><C+3><RECT><||3>"
    "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
    "<R21><C1><FROM><R21><C80><LINE>" SKIP    
    "<R19><C11><FROM><R23><C11><LINE>" SKIP
    "<R19><C22><FROM><R23><C22><LINE>" SKIP
    "<R19><C38><FROM><R23><C38><LINE>" SKIP
    "<R19><C49><FROM><R23><C49><LINE>" SKIP
    "<R19><C65><FROM><R23><C65><LINE>" SKIP
    "<R19><C72><FROM><R23><C72><LINE>" SKIP
    .
PUT "<=4><R+1> Ready Date         FOB             Ship Via             Terms          Sales Person         Order#      Quote#" SKIP
"<=4><R+3> " oe-ord.due-date FORM "99/99/9999" space(4)
cust.fob-code FORM "x(14)" SPACE(2)
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(3)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" space(1) v-salesman FORM "x(20)" space(1) oe-ord.ord-no space(6) v-q-no SKIP.


PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C12.3><FROM><R26><C12.3><LINE>" SKIP
       "<R24><C17.3><FROM><R26><C17.3><LINE>" SKIP
       /*"<R24><C23><FROM><R26><C23><LINE>" SKIP */
       "<R24><C46><FROM><R26><C46><LINE>" SKIP
       "<R24><C56><FROM><R26><C56><LINE>" SKIP
       "<R24><C65><FROM><R26><C65><LINE>" SKIP
       "<R24><C69><FROM><R26><C69><LINE>" SKIP
       .

PUT "<=5>    Customer"
    "<R+1><C1>   Part#/PO#     Status               Description                   Ordered      Unit Price  UOM    Ext. Price" SKIP(1).
v-printline = v-printline + 6.
