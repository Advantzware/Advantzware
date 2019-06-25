/* ------------------------------------------- oe/rep/acksimkn.i GDM 04200907*/
/* ORDER ACKNOLEDGEMENT for N-K-1-ACKHEAD = SIMKINS                          */
/* ------------------------------------------------------------------------- */

PUT "<FArial>".
     PUT "" /*"<C1><#1><R+5><C+25><IMAGE#1=" ls-full-img1 */ SKIP. /* pacific package */ 
           
PUT "<=1>" SKIP.
PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
    "<=2><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"     
   "<P10><=2><R+2>"
     v-comp-add1 AT 8 SKIP
     v-comp-add2  AT 8  SKIP
     v-comp-add3  AT 8 SKIP
     v-comp-add4  AT 8 skip
     v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
     lv-email AT 8 SKIP(1)
   "<FCourier New>"
    "Bill To:" SPACE(30) SKIP.

IF TRIM(oe-ord.cust-name) NE ""
  THEN PUT SPACE(5) oe-ord.cust-name SKIP.

IF TRIM(oe-ord.addr[1])  NE ""
  THEN PUT SPACE(5) oe-ord.addr[1] SKIP.

IF TRIM(oe-ord.addr[2])  NE ""
  THEN PUT SPACE(5) oe-ord.addr[2] SKIP.

IF TRIM(v-addr3)  NE ""
  THEN PUT SPACE(5) v-addr3 SKIP.

/*
   "Bill To:" SPACE(30) /*"Sold To:" */ SKIP
   SPACE(5) oe-ord.cust-name 
    ( IF oe-ord.sold-name = "" THEN oe-ord.cust-name ELSE oe-ord.sold-name) AT 45 FORM "x(30)" skip
   SPACE(5) oe-ord.addr[1] 
    (IF oe-ord.sold-addr[1] = "" THEN oe-ord.addr[1] ELSE oe-ord.sold-addr[1]) AT 45 FORM "x(30)" SKIP
   SPACE(5) oe-ord.addr[2] 
    (IF oe-ord.sold-addr[2] = "" THEN oe-ord.addr[2] ELSE oe-ord.sold-addr[2]) AT 45 FORM "x(30)" SKIP
   SPACE(5) v-addr3  v-sold-addr3 AT 45 SKIP.
*/


 IF lv-display-comp THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".

ASSIGN v-printline = v-printline + 14.
PUT 
    "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
PUT 
    "<R6><C50><FROM><R6><C80><LINE>" SKIP      
    "<R8><C50><FROM><R8><C80><LINE>" SKIP      
    "<R4><C61><FROM><R6><C61><LINE>" SKIP
    "<R6><C65><FROM><R8><C65><LINE>" SKIP
    .

PUT "<FArial><P12><=#3><R-2> <B>Acknowledgement</B> " "<P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                      Fax" 
    "<=#3><R+4>                            Order Date      <FCourier New>"
    "<=3><R+1> " oe-ord.cust-no  SPACE(5) cust.contact
    "<=3><R+3> " v-cust-phone  SPACE(5) cust.fax
    "<=3><R+5> " SPACE(11) oe-ord.ord-date .



PUT 
    "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
    "<R21><C1><FROM><R21><C80><LINE>"          SKIP 
    "<R19><C11><FROM><R23><C11><LINE>"         SKIP 
    "<R19><C22><FROM><R23><C22><LINE>"         SKIP 
    "<R19><C38><FROM><R23><C38><LINE>"         SKIP 
    "<R19><C52><FROM><R23><C52><LINE>"         SKIP 
    "<R19><C63><FROM><R23><C63><LINE>"         SKIP 
    "<R19><C72><FROM><R23><C72><LINE>"         SKIP 
    .

PUT 
    "<FArial><=4><R+1> Date Req.             FOB                     Ship Via                              Terms                        Sales Person           Order#       Quote#" SKIP
    "<FCourier New><=4><R+3> " lv-due-date FORMAT "99/99/9999" SPACE(2)
    oe-ord.fob-code FORMAT "x(11)" SPACE(2) 
    v-shipvia       FORMAT "x(20)" SPACE(1)
    oe-ord.terms-d  FORMAT "x(15)" SPACE(4) 
    v-salesman                     SPACE(8) 
    oe-ord.ord-no                  SPACE(2) 
    v-q-no SKIP
    .

PUT 
    "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
    "<R24><C15><FROM><R26><C15><LINE>"     SKIP
    "<R24><C30><FROM><R26><C30><LINE>"     SKIP
    "<R24><C53><FROM><R26><C53><LINE>"     SKIP
    "<R24><C63><FROM><R26><C63><LINE>"     SKIP
    "<R24><C75><FROM><R26><C75><LINE>"         SKIP
    .


PUT "<FArial><=5><R24><C14>     Customer Part#".
PUT "<=5><R25>    Customer PO                   FG Item #                            Description                                Ordered                      Price              UOM" SKIP(1).

PUT "<FCourier New>"          .
v-printline = v-printline + 6.
