/* ------------------------------------------- oe/rep/bolallws.p GDM 04200904*/
/* BOL PRINT for N-K-1-BOLFMT = Allwest                                      */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

DEFINE BUFFER xoe-bolh FOR oe-bolh.
DEFINE BUFFER xoe-boll FOR oe-boll.
DEFINE BUFFER xitemfg  FOR itemfg.
DEFINE BUFFER xxreport FOR report.

{oe/rep/oe-lad.i}

DEFINE VARIABLE v-salesman   AS CHARACTER FORMAT "x(26)" NO-UNDO.
DEFINE VARIABLE v-fob        AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE v-tot-cases  AS INTEGER   FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-palls  AS INTEGER   FORMAT "->>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-wt     AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.

DEFINE VARIABLE v-tot-pkgs   AS INTEGER   FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE v-pal-cnt    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ord-qty    LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE v-bol-qty    LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE v-ship-qty   LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE v-bol-wt     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-part-dscr  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-part-comp  AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE v-part-qty   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ord-no     LIKE oe-boll.ord-no NO-UNDO.
DEFINE VARIABLE v-po-no      LIKE oe-bolh.po-no NO-UNDO.
DEFINE VARIABLE v-job-no     AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE v-phone-num  AS CHARACTER FORMAT "x(13)" NO-UNDO.
DEFINE VARIABLE v-ship-phone AS CHARACTER FORMAT "X(13)" NO-UNDO.

DEFINE VARIABLE v-ship-name  LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE v-ship-addr  LIKE shipto.ship-addr NO-UNDO.
DEFINE VARIABLE v-ship-city  LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE v-ship-state LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE v-ship-zip   LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE v-ship-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-name  LIKE company.NAME NO-UNDO.
DEFINE VARIABLE v-comp-addr  LIKE company.addr NO-UNDO.
DEFINE VARIABLE v-comp-city  LIKE company.city NO-UNDO.
DEFINE VARIABLE v-comp-state LIKE company.state NO-UNDO.
DEFINE VARIABLE v-comp-zip   LIKE company.zip NO-UNDO.
DEFINE VARIABLE v-comp-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-cust-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-1          LIKE oe-boll.cases INIT 1 NO-UNDO.

DEFINE VARIABLE v-terms      LIKE oe-ord.terms-d NO-UNDO.
DEFINE VARIABLE v-frt-terms  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone       LIKE carr-mtx.del-zone NO-UNDO.
DEFINE VARIABLE v-job-var    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE w2 NO-UNDO
    FIELD cases   AS INTEGER FORMAT ">9"
    FIELD cas-cnt AS INTEGER FORMAT ">>>>9".

DEFINE TEMP-TABLE w3 NO-UNDO
    FIELD ship-i AS CHARACTER FORMAT "x(60)".

DEFINE VARIABLE v-tel            AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax            AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact        AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5      AS cha       FORM "x(30)" NO-UNDO.

DEFINE VARIABLE v-line-total     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-quo-total      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-t-tax          AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab        AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no           LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-display-comp  AS LOG       NO-UNDO.
DEFINE VARIABLE lv-comp-name     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email         AS cha       FORM "x(40)" NO-UNDO.
DEFINE VARIABLE v-cusx-add1      AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add2      AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add3      AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add4      AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-add5      AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-email     AS cha       NO-UNDO.
DEFINE VARIABLE v-cusx-name      AS cha       NO-UNDO.

DEFINE VARIABLE lv-comp-color    AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color   AS cha       INIT "BLACK" NO-UNDO.
DEFINE VARIABLE lv-bolfmt-int    AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-weight         LIKE oe-boll.weight NO-UNDO.
DEFINE VARIABLE v-job-po         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-phone          AS cha       NO-UNDO.
DEFINE VARIABLE v-shipto-contact LIKE shipto.contact NO-UNDO.
DEFINE VARIABLE v-ship-i         AS cha       EXTENT 4 FORM "x(60)" NO-UNDO.
DEFINE VARIABLE v-tmp-lines      AS DECIMAL   NO-UNDO.

DEFINE VARIABLE v-txt1           AS CHARACTER FORMAT "x(220)" EXTENT 30 NO-UNDO.

ASSIGN 
    tmpstore = FILL("-",130).

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BOLFMT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN 
    ASSIGN lv-display-comp = sys-ctrl.log-fld 
        lv-bolfmt-int   = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
        lv-bolfmt-int   = 0.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

IF lv-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN
        ASSIGN v-comp-add1  = cust.addr[1]
            v-comp-add2  = cust.addr[2]
            v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email     = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            v-cusx-add1  = v-comp-add1
            v-cusx-add2  = v-comp-add2
            v-cusx-add3  = v-comp-add3
            v-cusx-add4  = v-comp-add4
            v-cusx-add5  = v-comp-add5
            v-cusx-email = lv-email
            v-cusx-name  = lv-comp-name.
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
  
    IF FIRST-OF(oe-bolh.bol-no) THEN 
    DO:
        FIND FIRST carrier
            WHERE carrier.company EQ oe-bolh.company
            AND carrier.carrier EQ oe-bolh.carrier
            NO-LOCK NO-ERROR.

        RUN oe/custxship.p (oe-bolh.company,
            oe-bolh.cust-no,
            oe-bolh.ship-id,
            BUFFER shipto).

        ASSIGN
            v-ship-name      = shipto.ship-name
            v-ship-addr[1]   = shipto.ship-addr[1]
            v-ship-addr[2]   = shipto.ship-addr[2]
            v-ship-addr3     = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
            v-phone-num      = cust.area-code + cust.phone
            v-ship-phone     = IF shipto.area-code + shipto.phone <> "" THEN
                      "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx")
                      ELSE ""
            v-phone          = IF oe-bolh.area-code + oe-bolh.phone <> "" THEN 
              "(" + oe-bolh.area-code + ")" + string(oe-bolh.phone,"xxx-xxxx")
              ELSE ""
            v-shipto-contact = oe-bolh.contact.

        IF v-phone = "" THEN v-phone = "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx").
        IF v-shipto-contact = "" THEN v-shipto-contact = shipto.contact.

        IF shipto.broker THEN 
        DO:
            ASSIGN 
                v-comp-add1  = cust.addr[1]
                v-comp-add2  = cust.addr[2]
                v-comp-add3  = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
                v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
                v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
                lv-email     = "Email:  " + cust.email   
                lv-comp-name = cust.NAME .
            /* sold to address from order */
            FIND FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-boll THEN 
            DO:
                FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                    AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN
                    ASSIGN lv-comp-name = oe-ord.sold-name
                        v-comp-add1  = oe-ord.sold-addr[1]
                        v-comp-add2  = oe-ord.sold-addr[2]
                        v-comp-add3  = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.        
            END.
        END.
        ELSE
            ASSIGN v-comp-add1  = v-cusx-add1
                v-comp-add2  = v-cusx-add2    
                v-comp-add3  = v-cusx-add3    
                v-comp-add4  = v-cusx-add4                
                v-comp-add5  = v-cusx-add5
                lv-email     = v-cusx-email
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

            ASSIGN 
                v-terms     = oe-ord.terms-d
                v-frt-terms = IF cust.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF cust.frt-pay EQ "B" THEN "Bill"
                           ELSE IF cust.frt-pay EQ "C" THEN "Collect"
                           ELSE IF cust.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""
                v-zone      = cust.del-zone.
             
            IF v-terms EQ "" THEN
            DO:
                FIND FIRST terms WHERE terms.t-code EQ oe-ord.terms NO-LOCK NO-ERROR.
                IF AVAILABLE terms THEN
                    ASSIGN v-terms = terms.dscr.
            END.
      
            ASSIGN
                v-salesman = TRIM(v-salesman)
                v-po-no    = oe-boll.po-no
                v-job-no   = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).

            IF v-salesman GT '' THEN
                IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
                    substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

            v-fob = IF oe-ord.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".
      
            LEAVE.
        END.

        EMPTY TEMP-TABLE w3.
    
    END. /* first-of(oe-bolh.bol-no) */

    DO i = 1 TO 4:
        IF oe-bolh.ship-i[i] NE "" THEN 
        DO:
            FIND FIRST w3 WHERE w3.ship-i EQ oe-bolh.ship-i[i] NO-ERROR.
            IF NOT AVAILABLE w3 THEN CREATE w3.
            w3.ship-i = oe-bolh.ship-i[i].
        END.
    END.

    FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:
        CREATE report.
        ASSIGN
            report.term-id  = v-term-id
            report.key-01   = oe-boll.i-no
            report.key-02   = STRING(oe-boll.ord-no,"9999999999")
            report.rec-id   = RECID(oe-boll)
            oe-boll.printed = YES.
    END.

    IF LAST-OF(oe-bolh.bol-no) THEN 
    DO:
        IF v-comp-addr[2] = "" THEN
            ASSIGN v-comp-addr[2] = v-comp-addr3
                v-comp-addr3   = "".
        IF v-ship-addr[2] = "" THEN
            ASSIGN v-ship-addr[2] = v-ship-addr3
                v-ship-addr3   = "".
     
        {oe/rep/bolallws2.i}
        {oe/rep/bolallws.i}

        v-last-page = PAGE-NUMBER.

        IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

        PUT "<R44><C51><#8><FROM><R+4><C+30><RECT> " 
            "<=8><R+1> Total Pallets       :" v-tot-palls 
            "<=8><R+2> Total Weight        :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
        ASSIGN 
            v-ship-i = "".
        IF v-print-shipnotes THEN
            ASSIGN v-ship-i[1] = oe-bolh.ship-i[1]
                v-ship-i[2] = oe-bolh.ship-i[2]
                v-ship-i[3] = oe-bolh.ship-i[3]
                v-ship-i[4] = oe-bolh.ship-i[4].
  
        PUT "<FArial><R42.5><C1><P12><B> Shipping Instructions: </B> <P9> " 
            "<R43><C1>" v-ship-i[1] AT 7 
            "<R44><C1>" v-ship-i[2] AT 7 
            "<R45><C1>" v-ship-i[3] AT 7 
            "<R46><C1>" v-ship-i[4] AT 7 .

        ASSIGN        
            v-txt1[1]  = " The property described below, in apparent good order, except as noted (contents and condition of contents of packages unknown), marked, consigned" 
            v-txt1[2]  = " and destined as indicated below, which said carrier (the word carrier being understood throughout this contract as meaning any person or corporation              Prepaid, Collect,"  
            v-txt1[3]  = " in possession of the property under the contract) agrees to carry to its usual place of delivery at said destination, if on its route, otherwise to deliver                    or 3rd Party "
            v-txt1[4]  = " to another carrier on the route to said destination.  It is mutually agreed, as to each carrier of all or any of said property over all or any portion of said" 
            v-txt1[5]  = " route to destination, and as to each party at any time interested in all or any of said property, that every service to be performed hereunder shall" 
            v-txt1[6]  = " be subjected to all terms and conditions of the Uniform Domestic Straight Bill of Lading set forth (I) in Uniform Freight Classification in effect on the"
            v-txt1[7]  = " date hereof, if this is a rail or rail-water shipment, (2) in the applicable motor carrier classification or tariff if this is a motor carrier shipment." 
            v-txt1[8]  = " Shipper hereby certifies that he is familiar with all the terms and conditions of the said bill of lading, including those on the back therof, set forth in the"
            v-txt1[9]  = " classification or tariff which governs the transportation of this shipment, and the said terms and conditions are hereby agreed to by the shipper and "
            v-txt1[10] = " accepted for himself and his assigns."
            v-txt1[11] = "      NO. OF               KINDS OF PACKAGES,DESCRIPTION OF               WEIGHT                  CLASS                  NO. OF                KINDS OF PACKAGES,DESCRIPTION OF                WEIGHT                CLASS"
            v-txt1[12] = "   PACKAGES       ARTICLES SPECIAL MARKS & EXCEPTIONS        (SUB TO CORR)       OR RATE               PACKAGES          ARTICLES SPECIAL MARKS & EXCEPTIONS         (SUB TO CORR)      OR RATE"         
            v-txt1[13] = " If the shipping moves between two port by a carrier by water, the law requires that the bill of lading shall state whether it is 'carrier' or shipper's weight."
            v-txt1[14] = " Note:  Where the rate is dependent on value, the shippers are required to state specifically in writing the agreed to declared value of the property."
            v-txt1[15] = " The agreed or declared value is hereby specifically stated by the shipper to be not exceeding   _________________  per   ______________."       
            v-txt1[16] = "   per                                                                                              Shipper Agent                                                                                             per"
            v-txt1[17] = "Subject TO SECTION 7 of Condition of applicable"
            v-txt1[18] = "bill of lading if this shipment is to be delivered to the"
            v-txt1[19] = "consignor without recourse on the consignor,the"
            v-txt1[20] = "consignor shall sign the following statement. The"
            v-txt1[21] = "carrier shall not make delivery of this shipment"
            v-txt1[22] = "without payment of freight and all other lawful" 
            v-txt1[23] = "charges."         
            v-txt1[24] = "  Permanent post-office address of shipper                                                                   (This Bill of Lading is to be signed "
            v-txt1[25] = "  The fibre boxes used for this shipment conform to the specifications set foth           by the shipper and agent of the      "
            v-txt1[26] = "  box maker's Certificate thereon.                                                                                   carrier issuing same).               ".
         

        PUT "<FArial><P7>" SKIP
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
            "<P7><R61.5><C1><B>" "per" 
            "<C25><R61><P10>" "Shipper Agent"  
            "<C48><R61.5><P7>" "per"  
            "</B>"


            "<P5><R59.5><C65>"  v-txt1[17]          
            "<R60><C65>"    v-txt1[18]  
            "<R60.5><C65>"  v-txt1[19]           
            "<R61><C65>"    v-txt1[20]  
            "<R61.5><C65>"  v-txt1[21] 
            "<R62><C65>"    v-txt1[22] 
            "<R62.5><C65>"  v-txt1[23] 
            "<R63.5><C66> _______________________________" 
            "<R64><C68>  (Signature of Consignor)"
            "<R63><C1><P7>" SPACE(2) "Permanent post-office address of shipper" 
            "<C48>"  "(This Bill of Lading is to be signed "
            "<R63.5><C1>"  SPACE(2) "The fibre boxes used for this shipment conform to the specifications set foth"
            "<C48>"  "by the shipper and agent of the"
            "<R64><C1>"    SPACE(2) "box maker's Certificate thereon."
            "<C48>"   "carrier issuing same)."
            .

        v-printline = v-printline + 18.
 
        PAGE.
        v-printline = 0.

        FOR EACH report WHERE report.term-id EQ v-term-id,
            FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK:
            DELETE report.
        END.

    END.  /* last-of*/

    oe-bolh.printed = YES.
END. /* for each oe-bolh */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

