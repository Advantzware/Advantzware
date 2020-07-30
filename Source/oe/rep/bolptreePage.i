 
 Define Variable hNotesProcs as Handle NO-UNDO.
 DEFINE VARIABLE iLineCount AS INTEGER NO-UNDO .
 DEFINE VARIABLE iUnitCount AS INTEGER NO-UNDO .
 DEFINE VARIABLE dWeightCount AS DECIMAL NO-UNDO .
 DEFINE BUFFER bf-oe-boll FOR oe-boll .
 /*IF glShipNotesExpanded THEN do:                                                                                    */      
 /*    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.                                                             */
 /*    RUN GetNoteOfType IN hNotesProcs (oe-bolh.rec_key, "ES", OUTPUT ship_note).                                    */
 /*    DELETE OBJECT hNotesProcs.                                                                                     */
 /*END.                                                                                                               */
 /*ELSE DO:                                                                                                           */
 /*    ship_note = oe-bolh.ship-i[1] + " " + oe-bolh.ship-i[2] + "  " + oe-bolh.ship-i[3] + " " + oe-bolh.ship-i[4] . */
 /*END.                                                                                                               */
 
 find first carrier NO-LOCK
     where carrier.company eq oe-bolh.company
     and carrier.carrier eq oe-bolh.carrier
    no-error.
 
 IF oe-bolh.frt-pay EQ "C" THEN
     cFrightDscr = "COLLECT" .
  ELSE IF oe-bolh.frt-pay EQ "B" THEN
     cFrightDscr = "BILLABLE" .
  ELSE IF oe-bolh.frt-pay EQ "P" THEN
     cFrightDscr = "PREPAID"         .
   ELSE IF oe-bolh.frt-pay EQ "T" THEN
     cFrightDscr = "3RD PARTY"        .
  FIND FIRST tt-temp-report NO-ERROR .
  

 
 PUT  "<FArial>".
   PUT "<FArial><R2><C2><B><p7> STRAIGHT BILL OF LADING - SHORT FORM - <p6> Original - Not Negotlable <c60><p6>  BOL#: " oe-bolh.bol-no "</B>"  SKIP.
   PUT "<FArial><R3><C2><B><p7> RECEIVED,subject to the classifications and tariffs in effect on the date of issue of this Origional Bill of Lading." 
       "<R2.5><c60><p6>   Date: " oe-bolh.bol-date "</B>"   .
   PUT "<FArial><R4><C2><B><p8> " ( IF AVAIL carrier THEN carrier.dscr ELSE "") FORMAT "x(20)" "<c40><p7> Carrier <R3><c60><p6>   Page: 1 of 1 </B>" SKIP.
   PUT  SKIP(3)"<R5><c2><FROM><R5><C77><LINE>"  .
   PUT "<FArial><R5.5><C2><B><p5> the property described below, in apparent good order, except as noted (contents and condition of contents of packages unknown),marked, consigned,"
       " and destined as indicated below, which said carrier (the word </B>"  .
   PUT "<FArial><R6><C2><B><p5> carrier being understood throughout this contract as meaning any person or corporation in possession of the property under the contract)agrees " 
       "to carry to its usual place of delivery as said destination, if on its </B>"  .
   PUT "<FArial><R6.5><C2><B><p5> route, otherwise to deliver to another carrier on the route to said destinaton. It is mutually agreed, as to each carrier of all or any of said"  
       "property over all or any portion of said route to destination, and as to each </B>"  .
   PUT "<FArial><R7><C2><B><p5> party at any time interested in all or any of said property, that every service to be performed her under shall be subject to all the terms and" 
       "condition of the UnIform Domestic Straight Bill of Lading set forth </B>"   .
   PUT "<FArial><R7.5><C2><B><p5> (1) in the Uniform Freight Classification in effect on the date hereof, if this is a rail, or a rail-water shipment, or (2) In the applicable major" 
       "carrier classification or tariff if this is a motor carrier shipment. Shipper </B>"  .
   PUT "<FArial><R8><C2><B><p5> hereby certifies that he is familliar with all the terms and conditions of said bill of lading, including those on the attachment thereof, set forth" 
       "in the classification or tarriff which governs the transportation of this </B>" . 
   PUT "<FArial><R8.5><C2><B><p5> shipment, and the said terms and conditions are hereby agreed to by the shipper and accepted for himself and his assigns. </B>" SKIP.  
   PUT "<||><R9.5><C2><FROM><R10.5><C77><RECT>" "<R9.5><c2><B><p8> SHIPPER (FORM)  </B>"   .
   PUT "<FArial><R11><C2><B><p7> " lv-comp-name FORMAT "x(30)" " <c45> PRO #: " (IF AVAIL tt-temp-report THEN tt-temp-report.key-01 ELSE "") FORMAT "x(30)"  " </B>" SKIP.
   PUT "<FArial><R12><C2><B><p7> " v-comp-add1 FORMAT "x(30)"  "</B>" .
   PUT "<FArial><R13><C2><B><p7> " v-comp-add2  FORMAT "x(30)"  "</B>" .
   PUT "<FArial><R14><C2><B><p7> " v-comp-add3  FORMAT "x(30)"  "</B>" .

   
   FIND FIRST bf-oe-boll NO-LOCK
       WHERE bf-oe-boll.b-no EQ oe-bolh.b-no 
        AND bf-oe-boll.lot-no NE "" NO-ERROR . 
   IF NOT AVAIL bf-oe-boll THEN
        FIND FIRST bf-oe-boll NO-LOCK
       WHERE bf-oe-boll.b-no EQ oe-bolh.b-no 
        AND bf-oe-boll.ord-no NE 0 NO-ERROR .
  IF AVAIL bf-oe-boll THEN
      FIND first oe-ord NO-LOCK
      where oe-ord.company eq bf-oe-boll.company
        and oe-ord.ord-no  eq bf-oe-boll.ord-no 
      NO-ERROR.

   PUT "<||><R15><C2><FROM><R16><C77><RECT>" "<R15><C2><B><p8> CONSIGNEE (SHIP TO) </B>" .
   PUT "<FArial><R16.4><C2><B><p7> " v-ship-name FORMAT "x(30)" "</B>"  .
   /*PUT "<FArial><R17.5><C2><B><p7> ATTN:" (IF AVAIL oe-ord THEN oe-ord.contact ELSE "") FORMAT "x(20)" "</B>" SKIP.*/
   PUT "<FArial><R17.3><C2><B><p7> " v-ship-addr[1] FORMAT "x(30)" "</B>"  .
   PUT "<FArial><R18.2><C2><B><p7> " v-ship-addr[2] FORMAT "x(30)" "</B>"  .
   PUT "<FArial><R19.1><C2><B><p7> " v-ship-addr3 FORMAT "x(30)" "</B>"  .
   PUT  SKIP(3) "<R20><C2><FROM><R20><C77><LINE>" SKIP.

   

   PUT "<FArial><R20.4><C2><B><p7> CUST.ORDER#: " (IF AVAIL bf-oe-boll THEN bf-oe-boll.lot-no ELSE "") FORMAT "x(20)" 
             " <c45> OUR ORDER#: "   (IF AVAIL bf-oe-boll THEN bf-oe-boll.ord-no ELSE 0) FORMAT ">>>>>>>>" "</B>" SKIP.
   
   PUT "<FArial><R21.2><C2><B><p7> Special Instructions: </B>"   SKIP.  
   PUT "<FArial><R22.0><C2><B><p7> </B>" oe-bolh.ship-i[1] FORMAT "x(200)"  .
   PUT "<FArial><R22.8><C2><B><p7> </B>" oe-bolh.ship-i[2] FORMAT "x(200)"  . 
   PUT "<FArial><R23.5><C2><B><p7> </B>" oe-bolh.ship-i[3] FORMAT "x(200)"  .
   PUT "<FArial><R24.2><C2><B><p7> </B>" oe-bolh.ship-i[4] FORMAT "x(200)"  .
   PUT "<||><R25><C2><FROM><R32><C42><RECT>" "<R25><C2><B><p8> THIRD PARTY FREIGHT CHARGES BILL TO </B>"
       "<||><R25><C42><FROM><R32><C60><RECT>" "<R25><C43><B><p8> Freight charges are: </B>" SKIP.
   PUT "<||><R25><C42><FROM><R32><C77><RECT>"  SKIP. 
   PUT  SKIP(3) "<R26><C2><FROM><R26><C42><LINE>"  SKIP.    
   PUT "<FArial><R26.5><C2><B><p8> " (IF AVAIL tt-temp-report AND oe-bolh.frt-pay EQ "T" THEN tt-temp-report.key-04 ELSE "") FORMAT "x(30)" "</B>" "<R26><C42><B><p13> " cFrightDscr FORMAT "X(15)" "</B>" SKIP. 
   PUT "<FArial><R27.5><C2><B><p8> " (IF AVAIL tt-temp-report AND oe-bolh.frt-pay EQ "T" THEN tt-temp-report.key-05 ELSE "") FORMAT "x(30)"  " </B>" SKIP.    
   PUT  SKIP(3) "<R27.5><C42><FROM><R27.5><C59><LINE>" SKIP .
   PUT "<FArial><R28.5><C2><B><p8> "  (IF AVAIL tt-temp-report AND oe-bolh.frt-pay EQ "T" THEN tt-temp-report.key-06 ELSE "") FORMAT "x(30)" "</B>" SKIP
       "<R29.5><C2><B><p8> "  (IF AVAIL tt-temp-report AND oe-bolh.frt-pay EQ "T" THEN string(tt-temp-report.key-07 + ", " + tt-temp-report.key-08 + "  " + tt-temp-report.key-09) ELSE "") FORMAT "x(35)" "</B>" SKIP .
 PUT "<FArial><R25.5><C61><B><p5> Subject  to section T of conditions of  </B>" SKIP.      
   PUT "<FArial><R26><C60><B><p5> applicable bill of lading. If this shiphment   </B>" SKIP.   
   PUT "<FArial><R26.5><C60><B><p5> is to be deliverd to the consignee  </B>" SKIP.    
   PUT "<FArial><R27><C60><B><p5> without recourse on the consigner,the  </B>" SKIP.  
   PUT "<FArial><R27.5><C60><B><p5> consigner shall sign the following statement:  </B>" SKIP.    
   PUT "<FArial><R28.5><C60><B><p5> The carrier shall not make delivery of  </B>" SKIP.     
   PUT "<FArial><R29><C60><B><p5> this shippment without payment of freight </B>" SKIP.    
   PUT "<FArial><R29.5><C60><B><p5> and all other lawful charges.  </B>" SKIP.  
   PUT "<R31.5><C62><FROM><R31.5><C74><LINE>" SKIP .
   PUT "<FArial><R31.5><C63><B><p5> (signature of consigner)  </B>" SKIP.      
  PUT "<||><R32><C2><FROM><R34><C77><RECT>"  SKIP.
   PUT "<FArial><R32><C3><B><p7> NO. <R32><C58><B><p7> WEIGHT  CLASS  NMFC        SUB </B>" SKIP.
   PUT "<FArial><R33><C2><B><p8> PKGS <R33><C7><B><p8> DESCRIPTION OF ARTICLES, KIND OF PACKAGE, SPECIAL MARKS AND EXCEPTIONS"
        "<R33><C63><B><p6>  (subject to correction) </B>" SKIP. 
  iLineCount = 0 .
  oe-Boll-Loop:
  FOR EACH tt-boll where tt-boll.company eq oe-bolh.company and tt-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ordl
	    where oe-ordl.company eq tt-boll.company
	      and oe-ordl.ord-no  eq tt-boll.ord-no
          AND oe-ordl.i-no    EQ tt-boll.i-no
          AND oe-ordl.line    EQ tt-boll.LINE  
	    NO-LOCK BREAK BY tt-boll.qty-case:
        
        FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ tt-boll.company
            AND itemfg.i-no EQ tt-boll.i-no NO-ERROR .            
       
            iLineCount = iLineCount + 1 .
          PUT "<P8><C2>" tt-boll.cases 
          "<C8>" oe-ordl.i-name FORMAT "x(30)" 
          "<C25>" oe-ordl.part-no FORMAT "x(15)"
          "<C40>" oe-ordl.part-dscr1 FORMAT "x(30)"
          "<C59>" tt-boll.weight 
          "<C63>" (IF AVAIL itemfg THEN itemfg.frt-class ELSE "") FORMAT "x(5)"
          "<C66>" ( IF AVAIL tt-temp-report AND iLineCount EQ 1 THEN tt-temp-report.key-02 ELSE "") FORMAT "x(10)"
          "<C72>" ( IF AVAIL tt-temp-report AND iLineCount EQ 1 THEN tt-temp-report.key-03 ELSE "")  FORMAT "x(10)" SKIP.              
        
     iUnitCount = iUnitCount + tt-boll.cases .
     dWeightCount = dWeightCount + tt-boll.weight .
     
     IF iLineCount GE 10 THEN LEAVE oe-Boll-Loop.
   END.

   
   
   PUT "<||><R49><C2><FROM><R50><C77><RECT>" "<FArial><R49><C3><B><p8>"  oe-bolh.tot-pallet   "<c60>"  STRING(string(dWeightCount) + " " + "LBS")   "<c70> TOTALS </B>" SKIP.
   PUT "<FArial><R50><C2><B><p7> If the shipment moves between two ports by a carrier by water, the law requires that the bill of lading" 
         " shall state whether it is carrier's or shipper's weight. </B>" SKIP.                                                              
   PUT "<FArial><R51><C2><B><p7> NOTE - Where the rate is dependent on value, shippers are required to state specificelly in writing"
       "the agreed or declared value of the property.     </B>" SKIP. 
   PUT "<FArial><R52><C2><B><p7> The agreed or declared value of the property is hereby stated by the shipper to be not exceeding  </B>" SKIP.    
   PUT "<FArial><R53><C15><B><p8> PER     </B>" SKIP.
   PUT "<FArial><R54><C2><B><p7> The fiber boxes used for this shipment conform to the specifications set forth in the box maker's certificate thereof," 
       "and all other requirements of Uniform Freight Classifications.  </B>" SKIP.
   PUT "<FArial><R55><C2><B><p7> Shipper's imprint in leu of stamp:not a part of bill of lading approved by the interstate Commerce Commission." .
   PUT "<FArial><R56><C2><B><p7> SHIPPER'S CERTIFICATION This is to certify that the above named materials are property classified,described," 
       " packaged, marked and labeled, and are in proper condition     </B>" SKIP.                    
   PUT "<FArial><R57><C2><B><p7> for transportation according to the applicable regulations of the Department of Transportation. </B>" SKIP.       
   PUT "<FArial><R58><C2><B><p7>   Per  </B>" SKIP. 
   PUT "<R59><C4><FROM><R59><C30><LINE>" SKIP .  
   PUT "<||><R59><C2><FROM><R66><C80><RECT>" "<FArial><R59><C2><B><p9> SHIPPER: " lv-comp-name FORMAT "x(30)" 
       " <R59><C45><B><p8>  Recevied by: <p9> " v-ship-name FORMAT "x(25)" "</B>" SKIP.
   PUT   "<R61><C45><FROM><R61><C75><LINE>" SKIP .
   PUT  "<R62><C7><FROM><R62><C30><LINE>" SKIP .
   PUT "<FArial><R61><C45><B><p8> Carrier/Driver: " (IF AVAIL carrier THEN carrier.dscr ELSE "") FORMAT "x(20)"  "</B>" SKIP.

   PUT "<FArial><R61><C2><B><p9> PER:    "   oe-bolh.USER-ID FORMAT "x(8)" "</B>" SKIP
       "<R62><C2><B><p9> Shipper Phone # / Fax # / E-mail  </B>" SKIP
       "<R63><C2><B><p9> " v-comp-add4  v-comp-add5    "</B>" SKIP 
       "<R64><C2>" lv-email "<C50><B><p9>Receiving & Carrier Signatures           Date  </B>" 
       "<R64><C45><FROM><R64><C75><LINE>" .
   
