/* ------------------------------------------- ar/rep/invloyln.i 04200903 GDM */
/* PRINT INVOICE   Xprint form for Allwest                                    */
/* -------------------------------------------------------------------------- */
/*****************************************************************************
  PUT "<FTimes New Roman>".
  PUT "<C+20><#1><B><P20>" skip
      space(24) v-comp-add1 FORM "x(50)"  "</B>" SKIP(1)
     "<P10>" space(65) v-comp-add2 FORM "x(60)" SKIP
     space(75) v-comp-add3 FORM "x(60)" SKIP
     space(62) v-comp-add4 FORM "x(60)" "<P10>"SKIP.
*****************************************************************************/
/*PUT
/*   "<C7><#1><R+11><C+25><IMAGE#1=" ls-full-img1 SKIP. */
  "<C2><R3><#1><R+60><C+50><IMAGE#1=" ls-full-img1 . */
PUT
  "<C1><R2.5><#1><R+50><C+28><IMAGE#1=" ls-full-img1 .

PUT
   "<P10><C28><R4>" v-cust-add1
   /*"<C28><R+1>" v-cust-add2*/                 /*Task# 12231307*/
   "<C28><R+1>" v-cust-add3
   "<C28><R+1>" v-cust-phn
   "<C28><R+1>" v-fax-arcd "<P10>".

PUT 
  "<=1>" SKIP
  "<C1><#2>"
  "<P10><=2><R+11>"
  "<FCourier New><B>"
  SPACE(5) "Bill To:" SPACE(45) "Ship To:" SKIP
  SPACE(12) v-soldto-name v-shipto-name AT 65 SKIP.

IF TRIM(v-soldto-addr[1]) NE ""
  THEN PUT SPACE(12) v-soldto-addr[1].
  ELSE DO:
   PUT SPACE(12) v-soldto-addr[2].
   ASSIGN  v-soldto-addr[2] = "".
  END.

IF TRIM(v-shipto-addr[1]) NE ""
  THEN PUT v-shipto-addr[1] AT 65 SKIP.
  ELSE DO:
   PUT v-shipto-addr[2] AT 65 SKIP.
   ASSIGN v-shipto-addr[2] = "".
  END.

IF TRIM(v-soldto-addr[2]) NE ""
  THEN PUT SPACE(12) v-soldto-addr[2].
  ELSE DO:
   PUT SPACE(12) v-addr3.
   ASSIGN v-addr3 = "".
  END.

IF TRIM(v-shipto-addr[2]) NE ""
  THEN PUT v-shipto-addr[2] AT 65 SKIP.
  ELSE DO:
   PUT v-sold-addr3 AT 65 SKIP.
   ASSIGN v-sold-addr3 = "".
  END.

IF TRIM(v-addr3) NE ""
  THEN PUT SPACE(12) v-addr3.
  
IF TRIM(v-sold-addr3) NE ""
  THEN PUT v-sold-addr3 AT 65 SKIP.


ASSIGN v-printline = v-printline + 15.

PUT "<|10><R4><C54><#3><FROM><R9><C79><RECT>" SKIP.

PUT 
  "<R7><C54><FROM><R7><C79><LINE>" SKIP
  "<R8><C54><FROM><R8><C79><LINE>" SKIP
  "<R7><C66><FROM><R9><C66><LINE>" SKIP.  
        
PUT "<FArial><P12><=#3><R-2> <P10>" SKIP
    "<=#3><C54><B>                       INVOICE#                    " SKIP
    "<=#3><C53.8><R+1><P14>                 "ar-inv.inv-no "</B><P10>"
    "<=#3><C52><R+3>              DATE               " "<C69>"v-inv-date SKIP
    "<=#3><C53><R+4>       Customer ID:          " "<C69.6>"v-custno "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C80.75><RECT>" SKIP
    "<R22><C1><FROM><R22><C80.75><LINE>"          SKIP
    "<R21><C11><FROM><R24><C11><LINE>"         SKIP
    "<R21><C22><FROM><R24><C22><LINE>"         SKIP
    "<R21><C38><FROM><R24><C38><LINE>"         SKIP
    "<R21><C52><FROM><R24><C52><LINE>"         SKIP
    "<R21><C72><FROM><R24><C72><LINE>"         SKIP
    .

ASSIGN v-printline = v-printline + 3.

FIND FIRST sman NO-LOCK
  WHERE sman.company EQ ar-inv.company 
    AND sman.sman    EQ v-salesman NO-ERROR.

ASSIGN v-salesname = IF AVAIL sman 
                       THEN sman.sname ELSE "".

PUT 
  "<FArial><=4><B>  <C3>Ship Date             <C15>FOB                    <C27>Ship Via                                 <C43>Terms                          <C57>Sales Person                      <C74.5>BOL#" SKIP
  "<FCourier New><=4><R+2></B> " v-date-ship FORMAT "99/99/9999" 
 SPACE(2)
  v-fob          FORMAT "x(12)" 
 SPACE(1)
  v-shipvia      FORMAT "x(18)" 
 SPACE(1)
  ar-inv.terms-d FORMAT "x(15)" 
 SPACE(6) 
  v-salesname    FORMAT "x(15)"
 SPACE(3) 
 lv-bol-no  "</B>"
 SKIP.

PUT 
   "<|10><B><R25><C1><#5><FROM><R63.5><C80.75><RECT>" SKIP 
   "<C1><R27><FROM><R27><C80.75><LINE>"             SKIP 
   "<C61><R59><FROM><R59><C80.75><LINE>"            SKIP

    "<R25><C15><FROM><R60><C15><LINE>"           SKIP 
    "<R25><C48.5><FROM><R60><C48.5><LINE>"           SKIP 
   /* "<R28><C48.5><FROM><R60><C48.5><LINE>"       SKIP */
    /* "<R25><C57><FROM><R60><C57><LINE>"           SKIP */
    "<R25><C57.5><FROM><R60><C57.5><LINE>"           SKIP 
    "<R25><C71><FROM><R59><C71><LINE>"           SKIP 
    "</B>".   

PUT 
  "<R60><C13><P8></B>" .

/*PUT 
  "<FArial><R61><C1><#9><FROM><R63><C61><RECT> " 
  "<R61><C34.5><FROM><R63><C34.5><LINE>"         
  "<R61><C61><FROM><R63><C61><LINE>" SKIP .*/
PUT 
  "<FArial><R60><C1><#9><FROM><R63.5><C61><RECT> " 
  "<R60><C17><FROM><R63.5><C17><LINE>"  
  "<R60><C32><FROM><R63.5><C32><LINE>"
  "<R60><C45><FROM><R63.5><C45><LINE>"    
  "<R59><C61><FROM><R63><C61><LINE>" SKIP .

/*PUT
  "<=9><R61><P6> THIS IS YOUR INVOICE - MONTHLY STATEMENTS NOT RENDERED. IF"
  "                         1 1/2% PENALTY PER MONTH WILL BE CHARGED ($1.00"         
 SKIP
  "<=9><R61.5> LEGAL ACTION IS REQUIRED FOR COLLECTION OF THIS INVOICE. THE"
  "                       MINIMUM)   TO   YOUR  ACCOUNT ON BALANCES OVER" 
 SKIP
  "<=9><R62> BUYER IS RESPONSIBLE FOR ALL LEGAL EXPENSES INCURRED."
  "                                  30 DAYS."                
  .*/

PUT
  "<R63><P10><B></B>" 
  " " SKIP  
  "<R64><C19><FROM><R65><C70><FILLRECT>"
"<R64><C20><P11><B></B><P10>".  

PUT "<FArial><=5><B>   <C4>CUST PO#                     <C25>CUSTOMER PART #                                                                            <C62>PRICE" SKIP
    "   <C4.3>ORDER#                           <C26>DESCRIPTION                                                      <C49.3>QUANTITY            <C62.4>UOM                       <C73>AMOUNT"    SKIP
    "</B>"SKIP(1).

ASSIGN v-printline = v-printline + 3.
           
PUT "<FCourier New></B>".
