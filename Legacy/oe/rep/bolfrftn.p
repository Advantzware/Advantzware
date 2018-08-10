/* ---------------------------------------------- oe/rep/bolfrftn.p          */
/* PRINT Fibrex BOL                                                           */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
{sys/form/r-top.i}    

DEFINE BUFFER xoe-bolh     FOR oe-bolh.
DEFINE BUFFER xoe-boll     FOR oe-boll.
DEFINE BUFFER xitemfg      FOR itemfg.
DEFINE BUFFER xxreport     FOR report.

{oe/rep/oe-lad.i}

{oerep/r-bolx.i}

DEFINE VARIABLE v-salesman          AS   CHARACTER FORMAT "x(26)" NO-UNDO.
DEFINE VARIABLE v-fob               AS   CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE v-tot-cases         AS   INTEGER FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-palls         AS   INTEGER FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-wt            AS   DECIMAL FORMAT "->>,>>>,>>9" NO-UNDO.

DEFINE VARIABLE v-tot-pkgs          AS   INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE v-pal-cnt           AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-ord-qty           LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE v-bol-qty           LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE v-ship-qty          LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE v-bol-wt            AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-part-dscr         AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-part-comp         AS   CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE v-part-qty          AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-ord-no            LIKE oe-boll.ord-no NO-UNDO.
DEFINE VARIABLE v-po-no             LIKE oe-bolh.po-no NO-UNDO.
DEFINE VARIABLE v-job-no            AS   CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE v-phone-num         AS   CHARACTER FORMAT "x(13)" NO-UNDO.

DEFINE VARIABLE v-ship-name  LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE v-ship-addr  LIKE shipto.ship-addr NO-UNDO.
DEFINE VARIABLE v-ship-city  LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE v-ship-state LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE v-ship-zip   LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE v-ship-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-name  LIKE company.NAME NO-UNDO.
DEFINE VARIABLE v-comp-addr  LIKE company.addr NO-UNDO.
DEFINE VARIABLE v-comp-city  LIKE company.city NO-UNDO.
DEFINE VARIABLE v-comp-state LIKE company.state NO-UNDO.
DEFINE VARIABLE v-comp-zip   LIKE company.zip NO-UNDO.
DEFINE VARIABLE v-comp-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-cust-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-1          LIKE oe-boll.cases INIT 1 NO-UNDO.

DEFINE VARIABLE v-terms LIKE oe-ord.terms-d NO-UNDO.
DEFINE VARIABLE v-frt-terms AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone LIKE carr-mtx.del-zone NO-UNDO.
DEFINE VARIABLE v-lines AS INTEGER NO-UNDO.
DEFINE VARIABLE v-job-po            AS   CHARACTER NO-UNDO.
DEFINE VARIABLE v-line1 AS CHARACTER FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE v-i AS INTEGER NO-UNDO.

/* gdm - 03060901 */
DEFINE VARIABLE v-txt1     AS CHARACTER FORMAT "x(220)" EXTENT 30 NO-UNDO.
DEFINE VARIABLE v-frtclass LIKE itemfg.frt-class NO-UNDO.

/* gdm - 04160903 */
DEFINE VARIABLE v-lot# AS CHARACTER NO-UNDO.
DEFINE BUFFER b-rh FOR rm-rcpth.
DEFINE BUFFER b-rd FOR rm-rdtlh.

DEFINE TEMP-TABLE w2 NO-UNDO
    FIELD cases            AS   INTEGER FORMAT ">9"
    FIELD cas-cnt          AS   INTEGER FORMAT ">>>>9"
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INTEGER 
    FIELD dscr LIKE oe-ordl.part-dscr1
    FIELD partial          AS INTEGER
    FIELD unitcount        AS INTEGER 
    FIELD qty-sum          AS INTEGER .

DEFINE TEMP-TABLE w3 NO-UNDO
    FIELD ship-i           AS   CHARACTER FORMAT "x(60)".

DEFINE VARIABLE ls-image1 AS cha NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

FIND FIRST tt-bolx NO-ERROR.

IF AVAILABLE tt-bolx AND tt-bolx.print-logo THEN
   ls-full-img1 = tt-bolx.logo-file + ">".
ELSE IF NOT AVAILABLE tt-bolx THEN
   ASSIGN
      ls-image1 = "images\fibrelog.bmp"
      FILE-INFO:FILE-NAME = ls-image1
      ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEFINE VARIABLE v-tel AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact AS cha FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEFINE VARIABLE v-line-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-quo-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-t-tax      AS   DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab    AS   CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline AS INTEGER NO-UNDO.
DEFINE VARIABLE ll-display-comp AS LOG NO-UNDO.
DEFINE VARIABLE ll-consol-bolls AS LOG NO-UNDO.
DEFINE VARIABLE lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-bolfmt-int AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-comp-color AS cha NO-UNDO.
DEFINE VARIABLE lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-cusx-add1 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add2 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add3 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add4 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add5 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-name AS cha NO-UNDO.
DEFINE VARIABLE icountpallet AS INTEGER NO-UNDO .

DEFINE BUFFER b-itemfg     FOR itemfg.
DEFINE BUFFER bf-ttboll FOR tt-boll.
DEFINE VARIABLE v-tot-case-qty AS INTEGER NO-UNDO.
FORM w2.i-no                         FORMAT "x(15)"
     w2.job-po                       AT 17 FORMAT "x(15)"
     w2.dscr                         AT 33 FORMAT "x(30)"
     w2.cases                        TO 70 FORMAT "->>>>"
     icountpallet                      TO 77 FORMAT "->>>>>>"
     tt-boll.qty                     TO 85 FORMAT "->>>>>>"
     bf-ttboll.p-c                   AT 92
    WITH FRAME bol-mid DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 110.

FORM oe-ordl.i-no                         FORMAT "x(15)"
     v-job-po                       AT 17 FORMAT "x(15)"
     v-part-dscr                    AT 33 FORMAT "x(30)"
     w2.cases                       TO 70 FORMAT "->>>9"
     w2.cas-cnt                    TO 77 FORMAT "->>>>>9"
     tt-boll.qty                    TO 85 FORMAT "->>>>>9"
     tt-boll.p-c                    AT 92
    WITH FRAME bol-mid2 DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 100.

/* gdm - 04160923 
form 
     v-job-po                       at 17 format "x(15)"     
    with frame bol-mid3 down no-box no-labels stream-io width 100.
**********************/

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "BOLFMT" NO-LOCK NO-ERROR.
ASSIGN
 ll-display-comp = AVAILABLE sys-ctrl AND sys-ctrl.log-fld
 ll-consol-bolls = AVAILABLE sys-ctrl AND sys-ctrl.int-fld NE 0
 lv-bolfmt-int   = IF AVAILABLE sys-ctrl THEN sys-ctrl.int-fld ELSE 0.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST cust WHERE cust.company = cocode AND
                      cust.active = "X" NO-LOCK NO-ERROR.
IF AVAILABLE cust THEN
DO:
   IF cust.addr[2] EQ '' THEN
      ASSIGN v-comp-add1 = ''
             v-comp-add2 = cust.addr[1].
   ELSE
      ASSIGN v-comp-add1 = cust.addr[1]
             v-comp-add2 = cust.addr[2].

   ASSIGN v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
          v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
          v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999")
          lv-comp-name = cust.NAME   
          v-cusx-add1 = v-comp-add1
          v-cusx-add2 = v-comp-add2
          v-cusx-add3 = v-comp-add3
          v-cusx-add4 = v-comp-add4
          v-cusx-add5 = v-comp-add5
          v-cusx-name = lv-comp-name.
END.
FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.

{sa/sa-sls01.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.

v-printline = 0.

FOR EACH xxreport WHERE xxreport.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh)   EQ xxreport.rec-id,
    FIRST cust
    WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-bolh.cust-no
    NO-LOCK
    BREAK BY oe-bolh.bol-no:
      
    IF FIRST-OF(oe-bolh.bol-no) THEN DO:
    FIND FIRST carrier
        WHERE carrier.company EQ oe-bolh.company
          AND carrier.carrier EQ oe-bolh.carrier
        NO-LOCK NO-ERROR.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    ASSIGN
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone.
     
    IF shipto.broker THEN 
       ASSIGN
       v-comp-add1 = cust.addr[1]
       v-comp-add2 = cust.addr[2]
       v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
       v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
       v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
       lv-comp-name = cust.NAME .

    ELSE
       ASSIGN
          v-comp-add1 = v-cusx-add1
          v-comp-add2 = v-cusx-add2    
          v-comp-add3 = v-cusx-add3    
          v-comp-add4 = v-cusx-add4                
          v-comp-add5 = v-cusx-add5
          lv-comp-name = v-cusx-name.
        
    ASSIGN
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

    IF TRIM(v-comp-addr3) EQ "," THEN v-comp-addr3 = "".
              
    IF v-comp-addr[2] EQ "" THEN
      ASSIGN
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    IF v-ship-addr[2] EQ "" THEN
      ASSIGN
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
    IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

    ASSIGN
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".

    FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK,
        FIRST oe-ord
	    WHERE oe-ord.company EQ oe-boll.company
	      AND oe-ord.ord-no  EQ oe-boll.ord-no
	    NO-LOCK:

      IF NOT AVAILABLE carrier THEN
      FIND FIRST carrier WHERE carrier.company = oe-ord.company
        AND carrier.carrier = oe-ord.carrier NO-LOCK NO-ERROR.

      DO i = 1 TO 3:
        IF oe-ord.sman[i] NE "" THEN
           v-salesman = TRIM(v-salesman) + " " + oe-ord.sman[i] + ",".
      END.

      ASSIGN v-terms = oe-ord.terms-d
/*              v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"          */
/*                            else if cust.frt-pay eq "B" then "Bill"        */
/*                            else if cust.frt-pay eq "C" then "Collect"     */
/*                            else if cust.frt-pay eq "T" then "Third Party" */
/*                            else ""                                        */
             v-frt-terms = IF oe-bolh.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF oe-bolh.frt-pay EQ "B" THEN "Bill"
                           ELSE IF oe-bolh.frt-pay EQ "C" THEN "Collect"
                           ELSE IF oe-bolh.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""
             v-zone = cust.del-zone.
             
      IF v-terms EQ "" THEN
      DO:
        FIND FIRST terms WHERE terms.t-code EQ oe-ord.terms NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
           ASSIGN v-terms = terms.dscr.
      END.
      
      ASSIGN
         v-salesman = TRIM(v-salesman)
         v-po-no = oe-boll.po-no
         v-job-no = IF oe-boll.job-no = "" THEN ""
                    ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).

      IF v-salesman GT '' THEN
        IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
          substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-bolh.lot-no" AND
           reftable.rec_key  EQ oe-bolh.rec_key
           USE-INDEX rec_key
           NO-LOCK NO-ERROR.

      IF AVAILABLE reftable THEN
         ASSIGN v-fob = reftable.CODE.

      IF v-fob = "" THEN
        ASSIGN v-fob = oe-ord.fob-code.

      ASSIGN v-fob = (IF v-fob BEGINS "O" THEN "Origin" 
                      ELSE IF v-fob BEGINS "d" THEN "Destination" 
                      ELSE "").

      LEAVE.
    END.

    EMPTY TEMP-TABLE w3.

    FOR EACH tt-boll:
        DELETE tt-boll.
    END.
    
  END. /* first-of(oe-bolh.bol-no) */

  DO i = 1 TO 4:
    IF oe-bolh.ship-i[i] NE "" THEN DO:
      FIND FIRST w3 WHERE w3.ship-i EQ oe-bolh.ship-i[i] NO-ERROR.
      IF NOT AVAILABLE w3 THEN CREATE w3.
      w3.ship-i = oe-bolh.ship-i[i].
    END.
  END.
  ASSIGN v-tot-cases = 0 .
  FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:
    IF ll-consol-bolls THEN DO:
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).
    
      IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
        
    END.

    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
         /* ASSIGN tt-boll.rec_id = STRING(RECID(oe-boll))*/.
    END.
    ASSIGN v-tot-cases = v-tot-cases + oe-boll.cases + (if oe-boll.partial GT 0 THEN 1 else 0).
    oe-boll.printed = YES.
  END.

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     /* end of dup loop */

     {oe/rep/bolfrftn.i}
     {oe/rep/bolfrftn2.i}

     v-last-page = PAGE-NUMBER.

     IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

     /* gdm - 03060901 */
     ASSIGN        
         /*v-txt1[1] = " The property described below in apparent good order, except as noted (contents and condition of contents of packages unknown), marked consigned               If charges are to be pre-" */
         /*v-txt1[2] = " and destined as indicated below, which said carrier (the word carrier being understood throughout this contract as meaning any person or corporation              paid,write or stamp here" */
         /*v-txt1[3] = " in possession of the property under the contract) agrees to carry to its usual place of delivery at said destination, if on its route, otherwise to deliver                  To be Prepaid" */

         v-txt1[1] = " The property described below, in apparent good order, except as noted (contents and condition of contents of packages unknown), marked, consigned" 
         v-txt1[2] = " and destined as indicated below, which said carrier (the word carrier being understood throughout this contract as meaning any person or corporation              Prepaid, Collect,"  
         v-txt1[3] = " in possession of the property under the contract) agrees to carry to its usual place of delivery at said destination, if on its route, otherwise to deliver                    or 3rd Party "
         v-txt1[4] = " to another carrier on the route to said destination.  It is mutually agreed, as to each carrier of all or any of said property over all or any portion of said" 
         v-txt1[5] = " route to destination, and as to each party at any time interested in all or any of said property, that every service to be performed hereunder shall" 
         v-txt1[6] = " be subjected to all terms and conditions of the Uniform Domestic Straight Bill of Lading set forth (I) in Uniform Freight Classification in effect on the"
         v-txt1[7] = " date hereof, if this is a rail or rail-water shipment, (2) in the applicable motor carrier classification or tariff if this is a motor carrier shipment." 
         v-txt1[8] = " Shipper hereby certifies that he is familiar with all the terms and conditions of the said bill of lading, including those on the back therof, set forth in the"
         v-txt1[9] = " classification or tariff which governs the transportation of this shipment, and the said terms and conditions are hereby agreed to by the shipper and "
         v-txt1[10] = " accepted for himself and his assigns."
         v-txt1[11] = "      NO. OF               KINDS OF PACKAGES,DESCRIPTION OF               WEIGHT                  CLASS                  NO. OF                KINDS OF PACKAGES,DESCRIPTION OF                WEIGHT               CLASS"
         v-txt1[12] = "      UNITS              ARTICLES SPECIAL MARKS & EXCEPTIONS        (SUB TO CORR)       OR RATE                UNITS                ARTICLES SPECIAL MARKS & EXCEPTIONS         (SUB TO CORR)      OR RATE"
         v-txt1[13] = " If the shipping moves between two port by a carrier by water, the law requires that the bill of lading shall state whether it is 'carrier' or shipper's weight."
         v-txt1[14] = " Note:  Where the rate is dependent on value, the shippers are required to state specifically in writing the agreed to declared value of the property."
         v-txt1[15] = " The agreed or declared value is hereby specifically stated by the shipper to be not exceeding   _________________  per   ______________."       
         v-txt1[16] = "   Shipper                                                                                          Carrier / Driver                                                                                          Customer"
         v-txt1[17] = "Subject TO SECTION 7 of Condition of applicable"
         v-txt1[18] = "bill of lading if this shipment is to be delivered to the"
         v-txt1[19] = "consignor without recourse on the consignor,the"
         v-txt1[20] = "consignor shall sign the following statement. The"
         v-txt1[21] = "carrier shall not make delivery of this shipment"
         v-txt1[22] = "without payment of freight and all other lawful" 
         v-txt1[23] = "charges."
         v-txt1[24] = "   X                                                                                                X                                                                                                         X       "
         .

     IF v-printline >= 34 THEN DO:
        ASSIGN v-printline = 0.
        PAGE {1}.
        {oe/rep/bolfrftn.i}

     END.
         
     PUT "<FArial><P10><R46><C25><b> Total Unit:"  STRING(v-tot-cases,"->>>>>9")   "                  Total Pallets:" v-tot-palls FORMAT "->>>>>9" 
         "</b><P7><R47><C13> SHORTAGES OR DAMAGE SHOULD BE NOTED ON RECEIPT OF SHIPMENT, OTHERWISE CLAIMS WILL NOT BE ALLOWED." SKIP
         "<|10><R48><C1><#6><FROM><R54><C81><RECT>" 
         "<R48><C1>" v-txt1[1]    
         "<R48.5><C1>" v-txt1[2]  
         "<R49><C1>" v-txt1[3]    
         "<R49.5><C1>" v-txt1[4]  
         "<R50><C1>" v-txt1[5]    
         "<R50.5><C1>" v-txt1[6]  
         "<R51><C1>" v-txt1[7]    
         "<R51.5><C1>"            
         "<R52><C1>" v-txt1[8]    
         "<R52.5><C1>" v-txt1[9]  
         "<R53><C1>" v-txt1[10]   
         "<R48><C65.5><FROM><R54><C65.5><LINE>"  

         "<|10><R54><C1><#7><FROM><R59><C81><RECT>" 
         "<P6><R54><C1>" v-txt1[11]     
         "<P6><R54.5><C1>" v-txt1[12]   
/*
         "<P7><R56><C1>"  v-tot-palls      /* # UNITS */
         "<P7><R56><C8>"  " DESCRIPTION"   /* DESCRPT */
         "<P7><R56><C28>" v-tot-wt         /* WEIGHT  */
         "<P7><R56><C35>" v-tot-palls      /* CLASS   */
         "<P7><R56><C42>" v-tot-palls      /* # UNITS */
         "<P7><R56><C49>" v-tot-palls      /* DESCRPT */
         "<P7><R56><C69>" v-tot-palls      /* WEIGHT  */
         "<P7><R56><C76>" v-tot-palls      /* CLASS   */
*/

         "<R54><C7><FROM><R59><C7><LINE>"         
         "<R54><C27><FROM><R59><C27><LINE>"         
         "<R54><C34><FROM><R59><C34><LINE>"         
         "<R54><C41><FROM><R59><C41><LINE>"         
         "<R54><C48><FROM><R59><C48><LINE>"         
         "<R54><C68><FROM><R59><C68><LINE>"         
         "<R54><C75><FROM><R59><C75><LINE>"         
         "<R55><C1><FROM><R55><C81><LINE>"
         "<R57><C1><FROM><R57><C81><LINE>"

         "<|10><R59><C1><#8><FROM><R65><C81><RECT>" 
         "<R59><C64><FROM><R65><C64><LINE>"
         "<R61><C1><FROM><R61><C64><LINE>"
         "<R62.5><C1><FROM><R62.5><C64><LINE>"
         

         "<R59><C1>"   v-txt1[13] 
         "<R59.5><C1>" v-txt1[14] 
         "<R60><C1>"   v-txt1[15] 
         "<P6><R61.5><C1>"   v-txt1[16]

         "<P5><R59.5><C65>"  v-txt1[17]          
         "<R60><C65>"    v-txt1[18]  
         "<R60.5><C65>"  v-txt1[19]           
         "<R61><C65>"    v-txt1[20]  
         "<R61.5><C65>"  v-txt1[21] 
         "<R62><C65>"    v-txt1[22] 
         "<R62.5><C65>"  v-txt1[23] 
         "<R63.5><C66> _______________________________" 
         "<R64><C68>  (Signature of Consignor)"
         "<R63.5><C1>"  v-txt1[24]   
         .
         
                        

/*
     PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
         "<=8><R+1> Total Units       :" v-tot-palls
         "<=8><R+3> Total Weight      :" v-tot-wt
         "<P9><R58><C1>"
         "__________________________________________________________________________________________________________________" 
         "<R59><C1>" "<B>  Signature of Receipt </B>" 
         "<R60><C1>" "Customer ________________________________________                       Carrier _______________________________________" 
         "<R62><C1>" "Date ____________________________________________                       Date _________________________________________".

   
     v-printline = v-printline + 14.
*/   
     PAGE.
     v-printline = 0.
   
     FOR EACH report WHERE report.term-id EQ v-term-id,
         FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK:
         DELETE report.
     END.

  END.  /* last-of*/

  oe-bolh.printed = YES.
END. /* for each oe-bolh */

RETURN.

PROCEDURE create-tt-boll.
  DEFINE INPUT PARAMETER ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEFINE INPUT PARAMETER ip-cases    LIKE oe-boll.cases NO-UNDO.

  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.LINE 
        AND (tt-boll.qty-sum EQ (ip-qty-case * ip-cases))
      NO-LOCK NO-ERROR.

  IF NOT AVAILABLE tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
    /* tt-boll.rec_id = STRING(RECID(oe-boll))*/
     tt-boll.qty-sum = ip-qty-case * ip-cases
     tt-boll.partial  = 0 .
   
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight)
   tt-boll.partial = tt-boll.partial + oe-boll.partial 
   tt-boll.unitCount =  tt-boll.unitCount + 1 .

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.

/* gdm - 04160923 */
PROCEDURE get_lot_no:
    ASSIGN v-lot# = "".
    
    IF tt-boll.job-no NE "" THEN DO:
        RELEASE reftable.

        FIND FIRST job NO-LOCK
            WHERE job.company EQ tt-boll.company
              AND job.job-no  EQ tt-boll.job-no
              AND job.job-no2 EQ tt-boll.job-no2 NO-ERROR.
        IF AVAILABLE job THEN
            FIND FIRST reftable NO-LOCK
               WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job.job,"999999999")
                 AND reftable.code2    EQ tt-boll.i-no
                 USE-INDEX reftable NO-ERROR.
            IF NOT AVAILABLE reftable THEN
               FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ tt-boll.company
                  AND job-hdr.job-no  EQ tt-boll.job-no
                  AND job-hdr.job-no2 EQ tt-boll.job-no2
                  AND job-hdr.i-no    EQ tt-boll.i-no NO-ERROR.

            IF AVAILABLE reftable OR AVAILABLE job-hdr THEN
                FOR EACH rm-rcpth NO-LOCK
                  WHERE rm-rcpth.company   EQ tt-boll.company
                    AND rm-rcpth.job-no    EQ tt-boll.job-no
                    AND rm-rcpth.job-no2   EQ tt-boll.job-no2
                    AND rm-rcpth.rita-code EQ "I" USE-INDEX job,
                 EACH rm-rdtlh NO-LOCK
                  WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                    AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                    AND rm-rdtlh.s-num     EQ (IF AVAILABLE reftable 
                                               THEN reftable.val[12]
                                               ELSE job-hdr.frm)
                    AND rm-rdtlh.tag       NE "",
                 EACH b-rd NO-LOCK
                  WHERE b-rd.company   EQ rm-rdtlh.company
                    AND b-rd.tag       EQ rm-rdtlh.tag
                    AND b-rd.loc       EQ rm-rdtlh.loc
                    AND b-rd.loc-bin   EQ rm-rdtlh.loc-bin
                    AND b-rd.rita-code EQ "R"
                    AND b-rd.tag2      NE "" USE-INDEX tag,
                 FIRST b-rh NO-LOCK
                  WHERE b-rh.r-no      EQ b-rd.r-no
                    AND b-rh.rita-code EQ b-rd.rita-code
                    AND b-rh.i-no      EQ rm-rcpth.i-no:

                    v-lot# = b-rd.tag2.                
                    
                END. /* for each */

        END. /* get lot # */
END PROCEDURE.
/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
