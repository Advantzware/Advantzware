/******************************************************************************** 
 ORIGINAL FORM = ar/rep/crdbmemo2.i 
 GDM - 04210922 
 stdcrmemo10.i
********************************************************************************/

PUT "<C2><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP.
    "<FArial>".
PUT "<=1>" SKIP.
/*
PUT "<C1><R5><#2>" 
    "<P10><=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
            "<P10><=2><R+5>"
            space(3) v-comp-add1 SKIP
            space(3) v-comp-add2 SKIP
            space(3) v-comp-add3 SKIP
            space(3) v-comp-add4 SKIP
            space(3) v-comp-add5 SKIP
            space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" lv-email FORMAT "X(48)" SKIP(1).
*/
IF v-memo-name NE ""  
  THEN
    PUT 
    "<FCourier New><C1><R+8><#2>"
/*             space(7) "Bill To:"    SPACE(47) /*"Ship To:"            */ SKIP */
            SPACE(7) v-memo-name           /*v-shipto-name AT 63   */ SKIP
            SPACE(7) v-memo-addr[1]        /*v-shipto-addr[1] AT 63*/ SKIP.

IF v-memo-addr[2] NE "" 
  THEN PUT SPACE(7) v-memo-addr[2]        /*v-shipto-addr[2] AT 63*/ SKIP.

IF v-memo-city  NE "" OR 
   v-memo-state NE "" OR
   v-memo-zip   NE ""
  THEN
    PUT SPACE(7) v-memo-city + ", " + 
                 v-memo-state + " " + 
                 v-memo-zip FORMAT "x(30)"  /*v-sold-addr3 AT 63    */ SKIP.
            
          
v-printline = v-printline + 18.
/*
IF lv-display-comp THEN
    PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C2><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 
*/
PUT "<P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C62><FROM><R6><C62><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
/*     "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP */
    .
        
PUT "<FArial><P12><=#3><C50><R-2> <B>Credit/Debit Memo #: " ar-cash.check-no FORMAT ">>>>>>>>>9" "<P10>       Pg: </B>" string(PAGE-NUM - v-page-num,">>9") SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4>                  Credit/Debit Memo Date<FCourier New>"    

    "<=3><R+1> " cust.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " space(12) ar-cash.check-date  .

    
PUT "<R16><C1><#4><FROM><R20><C80><RECT><||3>" SKIP
    "<R18><C1><FROM><R18><C80><LINE><||3>" SKIP    
    "<R16><C11><FROM><R20><C11><LINE><||3>" SKIP
    "<R16><C22><FROM><R20><C22><LINE><||3>" SKIP
    "<R16><C41><FROM><R20><C41><LINE><||3>" SKIP
    "<R16><C58><FROM><R20><C58><LINE><||3>" SKIP
    "<R16><C68><FROM><R20><C68><LINE><||3>" SKIP     
    .

v-printline = v-printline + 5.


PUT "<FArial><=4><R+1>     Ship Date               FOB                          Ship Via                                   Terms                         S.Person                   BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship              SPACE (4)
                                v-fob     FORMAT "x(12)" SPACE(1)
                                v-shipvia FORMAT "x(22)" SPACE(1)
                                v-terms   FORMAT "x(19)" SPACE(4) 
                                v-s-man   FORMAT "x(8)"  SPACE(5) 
                                v-bol-no  FORMAT "x(8)"  SKIP.


PUT 
    "<R21><C1><#5><FROM><R23><C80><RECT><||3>" SKIP    

    "<R21><C10><FROM><R23><C10><LINE><||3>"    SKIP 
    "<R21><C20><FROM><R23><C20><LINE><||3>"    SKIP
    "<R21><C27><FROM><R23><C27><LINE><||3>"    SKIP
    "<R21><C43><FROM><R23><C43><LINE><||3>"    SKIP
/*     "<R26><C62><FROM><R28><C62><LINE><||3>"    SKIP */
    "<R21><C71><FROM><R23><C71><LINE><||3>"    SKIP.

/* PUT "<FArial><=5><R+1>   Invoice #          PO#         Order               Item#/CustPart#                              Memo Description                          Total Applied" */ 
PUT "<FArial><=5><R21>                                                                                                                                                                                              Total "
    SKIP.                                                                                                                                                                  
PUT "<FArial><=5><R+1>   Invoice #             PO#               Order                Item#/CustPart#                                Memo Description                        Applied"
    SKIP(1).

v-printline = v-printline + 4.
           

PUT "<FCourier New>".
