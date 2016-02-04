/* ------------------------------------------- oe/rep/relallws.i GDM 04200906*/
/* REALSE TICKET PRINT for N-K-1-RELPRINT = Allwest                          */
/* ------------------------------------------------------------------------- */

PUT 
    "<FArial>" SKIP
    "<P14><C+35><B>Pick Ticket</B> " SKIP
    "<=1>" SKIP
    "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */
    "<=1><C3><FGCOLOR=" TRIM(lv-comp-color) + ">"
    "<=1><C3><R+1><P20><FGCOLOR=" TRIM(lv-other-color) + ">" FORMAT "x(6)" 
    "<P10></B>"
    "<=2><R50>" "<FGCOLOR=" + TRIM(lv-comp-color) + ">" FORMAT "x(15)"        
    "<P10><=1><R+12>"
    "<FCourier New>"
    "Sold To:" SPACE(30) "Ship To:"           SKIP
    SPACE(5) cust.name shipto.ship-name AT 45 SKIP.

IF TRIM(cust.addr[1]) NE ""
  THEN PUT SPACE(5) cust.addr[1].
  ELSE DO:
   PUT  SPACE(5) cust.addr[2].
   /*ASSIGN cust.addr[2] = "".*/
  END.

IF TRIM(shipto.ship-addr[1]) NE ""
  THEN PUT shipto.ship-addr[1] AT 45 SKIP.
  ELSE DO:
   PUT shipto.ship-addr[2] AT 45 SKIP.
   /*ASSIGN shipto.ship-addr[2] = "".*/
  END.

IF TRIM(cust.addr[2]) NE ""
  THEN PUT SPACE(5) cust.addr[2].
  ELSE
      IF  TRIM(cust.city)  NE ""  OR
          TRIM(cust.state) NE "" OR
          TRIM(cust.zip)   NE ""
       THEN 
        PUT 
          SPACE(5) 
          cust.city  + " " + 
          cust.state + " " + 
          cust.zip  FORMAT "x(30)".  

IF TRIM(shipto.ship-addr[2]) NE ""
  THEN PUT shipto.ship-addr[2] AT 45 SKIP.
  ELSE 
   IF TRIM(shipto.ship-city)  NE "" OR
      TRIM(shipto.ship-state) NE "" OR 
      TRIM(shipto.ship-zip)   NE ""
     THEN 
      PUT shipto.ship-city + " " + 
          shipto.ship-state + " " + 
          shipto.ship-zip AT 45 FORMAT "x(30)" SKIP.
   
 IF TRIM(cust.addr[2]) NE "" AND
    (TRIM(cust.city)   NE ""  OR
     TRIM(cust.state)  NE "" OR
     TRIM(cust.zip)    NE "")
   THEN 
    PUT SPACE(5) cust.city  + " " + cust.state + " " + cust.zip  FORMAT "x(30)".

IF TRIM(shipto.ship-addr[2]) NE "" AND
   (TRIM(shipto.ship-city)   NE "" OR
    TRIM(shipto.ship-state)  NE "" OR 
    TRIM(shipto.ship-zip)    NE "")
  THEN 
   PUT 
    shipto.ship-city + " " + 
    shipto.ship-state + " " + 
    shipto.ship-zip AT 45 FORMAT "x(30)" SKIP.

PUT
    "<R4><C50><#3>" 
  SKIP
    "<FArial><P14><=#3><P12>" 
  SKIP
    "<=#3><B>Ticket #: " STRING(oe-relh.release#,"->>>>>>9") "</B><P10>" 
  SKIP(1)
    "<=#3><R+2>Print Date:" v-ticket-date FORMAT "99/99/9999"  
  SKIP
    "<=#3><R+3>Ship Date:" oe-relh.rel-date  
  SKIP
  SKIP     
    "<|10><R19><C1><#4><FROM><R23><C80><RECT>" 
  SKIP
    "<R21><C1><FROM><R21><C80><LINE>" 
  SKIP
    "<R19><C12><FROM><R23><C12><LINE>" 
  SKIP
    "<R19><C25><FROM><R23><C25><LINE>" 
  SKIP 
/*"<R19><C35><FROM><R23><C35><LINE>" SKIP */
    "<R19><C34><FROM><R23><C34><LINE>" 
  SKIP
    "<R19><C57><FROM><R23><C57><LINE>" 
  SKIP
    "<FArial><=4><R+1>    Pallets/Bags              Weight                    FOB               Ship Via                                                    Freight Terms" 
  SKIP
    "<FCourier New><=4><R+3> " v-pallets SPACE(10) v-weight FORM "->>>>>9" space(9) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   
  SKIP
    "<|10><R24><C1><#5><FROM><R26><C80><RECT>" 
  SKIP    
    "<R24><C7><FROM><R26><C7><LINE>" 
  SKIP
    "<R24><C18><FROM><R26><C18><LINE>" 
  SKIP
    "<R24><C24><FROM><R26><C24><LINE>" 
  SKIP
    "<R24><C35><FROM><R26><C35><LINE>" 
  SKIP
    "<R24><C57><FROM><R26><C57><LINE>" 
  SKIP
/*                    "<R24><C64><FROM><R26><C64><LINE>" SKIP */
    "<R24><C71><FROM><R26><C71><LINE>" 
 SKIP                                                                                                     /*job#*/
/*"<FArial><=5><R+1> Order#            PO#                Bin#           FG#                   Description                                        Units        Order Qty. Release Qty" SKIP(1)*/
    "<FArial><=5><R+1> Order#            PO#                Bin#           FG#                   Description                                           Units / Count            Release Qty" 
 SKIP(1)
    "<FCourier New>"          
    .

ASSIGN v-printline = v-printline + 16.
