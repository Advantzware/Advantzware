/* ------------------------------------------- cec/quote/quosmkt2.i 05/18 GDM */
/* N-K = QUOPRINT - SIMKINS Xprint FORmat                                     */
/* -------------------------------------------------------------------------- */

  ASSIGN
    v-bill[1] = bill[1]
    v-bill[2] = bill[2]
    v-bill[3] = bill[3]
    v-bill[4] = bill[4]
    v-shp2[1] = xquo.shipto[1]
    v-shp2[2] = xquo.shipto[2]
    v-shp2[3] = xquo.shipto[3]
    v-shp2[4] = xquo.shipto[4]
    v-shp2[5] = xquo.shipto[5].

  PUT "<C+25><#1>"
      "<=1>" SKIP 
      "<C1><#2><Farial>"
      "<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
      "<P10><=2><R+5>"
       v-comp-add1 AT 8 SKIP
       v-comp-add2 AT 8 SKIP
       v-comp-add3 AT 8 SKIP
       v-comp-add4 AT 8 SKIP
       v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
       lv-email AT 8 SKIP(3)
      "<FCourier New>"
      "Bill To:"  SPACE(40) .

  IF v-prt-shp2 
    THEN PUT "Ship To:"  v-shp2[5] SKIP.
    ELSE PUT SKIP.

  PUT SPACE(5) v-bill[1].
    
  IF v-prt-shp2
    THEN PUT v-shp2[1] AT 55 SKIP.
    ELSE PUT SKIP.

  IF TRIM(v-bill[2]) NE "" 
   THEN PUT SPACE(5) v-bill[2].
   ELSE DO:
    IF TRIM(v-bill[3]) NE "" THEN DO:
      PUT SPACE(5) v-bill[3].
      ASSIGN  v-bill[3] = "".
    END.
    ELSE do:      
     PUT SPACE(5) v-bill[4].
      v-bill[4] = "".
    END.
   END.

  IF v-prt-shp2 AND
     TRIM(v-shp2[2]) NE ""
    THEN PUT v-shp2[2] AT 55 SKIP.  
    ELSE DO:
     IF v-prt-shp2 AND
        TRIM(v-shp2[3]) NE "" THEN DO: 
       PUT v-shp2[3] AT 55 SKIP.
       ASSIGN  v-shp2[3] = "".
     END.
       ELSE 
        IF v-prt-shp2 AND
          TRIM(v-shp2[4]) NE "" THEN DO: 
           PUT v-shp2[4] AT 55 SKIP.
         ASSIGN v-shp2[4] = "".
       END.    
    END.

  IF NOT v-prt-shp2 THEN PUT SKIP.  
  
  IF TRIM(v-bill[3]) NE "" 
    THEN PUT SPACE(5) v-bill[3].
    ELSE DO:      
     PUT SPACE(5) v-bill[4].
      v-bill[4] = "".
    END.

  IF v-prt-shp2 AND
     TRIM(v-shp2[3]) NE "" 
    THEN PUT v-shp2[3] AT 55 SKIP.
    ELSE DO:
     IF v-prt-shp2 AND
        TRIM(v-shp2[4]) NE "" THEN DO: 
       PUT v-shp2[4] AT 55 SKIP.
       ASSIGN v-shp2[4] = "".
     END.
    END.

  IF NOT v-prt-shp2 THEN PUT SKIP.

  IF TRIM(v-bill[3]) NE "" 
    THEN PUT SPACE(5) v-bill[4].

  IF v-prt-shp2 AND
     TRIM(v-shp2[4]) NE "" 
    THEN PUT v-shp2[4] AT 55 SKIP.
    ELSE PUT SKIP. 

  IF lv-display-comp THEN
     PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
         "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)". 

  v-printline = v-printline + 15.
  PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP
      "<R6><C50><FROM><R6><C80><LINE>"       
      "<R4><C62><FROM><R6><C62><LINE>" 
      "<R6><C65><FROM><R8><C65><LINE>"
      "<FArial><P12><=#3>"
       "<=#3><R-2> <B>Quotation#: " v-first-q-no "</B>" "           Page#: " + string(PAGE-NUM /*- lv-pg-num*/ ,">>9") /*+ " of " + string(lv-tot-pg)*/ FORM "x(30)"
       "<P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact FORMAT "x(25)"
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C42><FROM><R27><C42><LINE>" SKIP
   "<R23><C54><FROM><R27><C54><LINE>" SKIP
/*    "<R23><C70><FROM><R27><C70><LINE>" SKIP */
   .
/*PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP*/
   PUT "<FArial><=4><R+1> Quote Date         FOB                                Ship Via                                Terms                                     Sales Person" SKIP
   "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORMAT "x(11)" SPACE(2)
   carrier.dscr  FORMAT "x(23)" SPACE(1)
   terms.dscr    FORMAT "x(15)" SPACE(1) 
   sman.sname                   SPACE(2) /*v-over-under*/ SKIP. 

   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
       "<R28><C7><FROM><R30><C7><LINE>"             SKIP 
       "<R28><C20><FROM><R30><C20><LINE>"           SKIP 
       "<R28><C45><FROM><R30><C45><LINE>"           SKIP 
       "<R28><C60><FROM><R30><C60><LINE>"           SKIP 
/*        "<R28><C63><FROM><R30><C63><LINE>"           SKIP */
       "<R28><C72><FROM><R30><C72><LINE>"           SKIP.
  
   PUT "<FArial><=5><R+1> Est#/Qt#    Part#/Description               Item/Style/Color/Board                             Quantity                          Price                UOM " SKIP.
   PUT "<FCourier New>".

lv-pg-num = PAGE-NUM.

