/* ----------------------------------------------- oe/rep/ackm3cpk.p 10/10 YSK */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

def var v-salesman as char format "x(3)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T" NO-UNDO.
def var v-len as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-ans as log init no NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var pagebreak as int init 28 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
/*ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
*/
DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-billinst AS cha FORM "x(70)" EXTENT 4 NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-line-print AS INT INIT 44 NO-UNDO.
DEF VAR lv-due-date AS DATE NO-UNDO.
DEF VAR v-prev-order AS CHAR FORMAT "x(6)" NO-UNDO.

DEF VAR v-count AS INT FORMAT ">>" INIT 0 NO-UNDO.
DEF VAR v-note-lin AS CHAR FORMAT "x(50)" NO-UNDO .
DEF VAR r AS INT INIT 0 NO-UNDO .
DEF VAR v-count2 AS INT FORMAT ">>" INIT 0 NO-UNDO.
DEF VAR v-reltype AS cha NO-UNDO.
DEF VAR v-lot-no AS CHAR FORMAT "x(15)" no-undo.
DEF VAR v-type-code AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR v-upc-no AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-frt-pay AS CHAR FORMAT "x(10)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ACKHEAD" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode no-lock no-error.
{custom/notesdef.i}

 /*
  format
    v-line         to  3 format ">>9"
    oe-ordl.i-no   at  5
    oe-ordl.i-name at 22
    oe-ordl.qty    to 66 format "->>,>>>,>>9"
    oe-ordl.price  to 77 format "->>>,>>9.99" space(0)
    oe-ordl.pr-uom to 80 format "x(3)"
    with frame detail no-labels no-box no-underline down stream-io width 90.

  format
    v-line to 3 format ">>9"
    oe-ordm.charge at  5 space(2)
    oe-ordm.dscr format "x(30)"
    oe-ordm.tax at 65
    oe-ordm.amt to 78 format "->>,>>9.99" skip(1)
    with frame detailm no-labels no-box no-underline down stream-io width 90.

  format
    lcnt format ">9" to 10
    oe-rel.qty          space(3)
    oe-rel.rel-date     FORMAT "99/99/99"
    with frame sched-rel no-labels no-box no-underline down stream-io width 90.

  format
    shipto.ship-name at 18 skip
    shipto.ship-addr at 18 skip
    v-addr4          at 18 skip
    with frame shipto-rel no-labels no-box no-underline down stream-io width 90.

 */

 ASSIGN v-comp-add1 = ""
        v-comp-add2 = ""
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        lv-email = ""
        lv-comp-name = "".

 IF lv-display-comp THEN DO:
        FIND FIRST cust WHERE cust.company = cocode AND
                              cust.active = "X" NO-LOCK NO-ERROR.
      /*  ASSIGN v-comp-add1 = company.addr[1]
               v-comp-add2 = company.addr[2]
               v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
               v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
               v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
               lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
               lv-comp-name = company.NAME   
               .
      */ 
       IF AVAIL cust THEN
          ASSIGN v-comp-add1 = cust.addr[1]
                 v-comp-add2 = cust.addr[2]
                 v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
                 v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
                 v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
                 lv-email    = "Email:  " + cust.email 
                 lv-comp-name = cust.NAME   
                 .
 END.

  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  
  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sman[1].
      else
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].

      if oe-ord.fob-code eq "ORIG" then
        v-fob = "Origin".
      else
        v-fob = "Destination".

      find FIRST carrier
          where carrier.company eq oe-ord.company
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.
      if avail carrier then
        v-shipvia = carrier.dscr.
      else
        v-shipvia = "".
      v-printline = 0.
      assign
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1.
      IF oe-ord.sold-city = "" AND oe-ord.sold-state = ""
         AND oe-ord.sold-zip = "" THEN v-sold-addr3 = v-addr3.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.
      /* get q-no for first item */
      v-q-no = 0.
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl AND oe-ordl.est-no <> "" THEN DO:
         FIND last quotehd WHERE quotehd.company = oe-ordl.company
                              AND quotehd.est-no = oe-ordl.est-no
                              NO-LOCK NO-ERROR.
         IF AVAIL quotehd THEN DO:
            FIND FIRST quoteitm OF quotehd WHERE quoteitm.part-no = oe-ordl.part-no
                     NO-LOCK NO-ERROR.
            IF AVAIL quoteitm THEN DO:
                v-q-no = quoteitm.q-no.
               FIND FIRST quoteqty WHERE quoteqty.company = oe-ordl.company
                                     AND quoteqty.loc = quoteitm.loc
                                     AND quoteqty.q-no = quoteitm.q-no
                                     AND quoteqty.LINE = quoteitm.LINE
                                     AND quoteqty.qty = oe-ordl.qty
                                     NO-LOCK NO-ERROR.
               IF AVAIL quoteqty THEN v-q-no = quoteqty.q-no.
            END.
         END.
      END.
      lv-due-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE oe-ord.due-date.
      
/*{oe/rep/ackxprnt.i}*/
v-upc-no = "" .
      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:

        FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

        IF AVAIL itemfg THEN
            ASSIGN v-upc-no = itemfg.upc-no .
        
        v-prev-order = "" .
        IF AVAIL oe-ord THEN
            ASSIGN
            v-prev-order = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                                     ELSE STRING(oe-ord.pord-no).
        
        v-type-code = "" .
        IF oe-ordl.type-code EQ "O" THEN
            v-type-code = "Original" .
        IF oe-ordl.type-code EQ "R" THEN
            v-type-code = "Repeat" .
        IF oe-ordl.type-code EQ "Q" THEN
            v-type-code = "Quality/Re-work" .
        IF oe-ordl.type-code EQ "N" THEN
            v-type-code = "New" .
        IF oe-ordl.type-code EQ "C" THEN
            v-type-code = "Repeat with Change" .
        IF oe-ordl.type-code EQ "T" THEN
            v-type-code = "Inhouse Transfer" .
        IF oe-ordl.type-code EQ "X" THEN
            v-type-code = "Complete Re-run" .

        v-frt-pay = "" .
        IF oe-ord.frt-pay EQ "P" THEN
            v-frt-pay = "Prepaid" .
        IF oe-ord.frt-pay EQ "C" THEN
            v-frt-pay = "Collect" .
        IF oe-ord.frt-pay EQ "B" THEN
            v-frt-pay = "Bill" .
        IF oe-ord.frt-pay EQ "T" THEN
            v-frt-pay = "3rd Party" .



        PUT SKIP(2)
            "<FCalibri><P20><R3><C34><U><B>Order Master<P28></B></U>" SKIP(2)
            "<P14><B><C4>Sales Order: " oe-ordl.ord-no 
            "<C32>Customer Code: " oe-ord.sold-id
            "<C56>Customer: " oe-ord.sold-name "</B>" SKIP(1)

            "<||3><C2><FROM><C80><LINE><||3>" SKIP(1)

            "<P11><C4>Estimate: " oe-ordl.est-no FORMAT "x(8)"
            "<C50>Customer Contact: " OE-ORD.CONTACT SKIP(.5)
            "<C4>Previous Order: " v-prev-order
            "<C50>Over %: " OE-ORD.OVER-PCT SKIP(.5)
            "<C4>Sales: " OE-ORD.SMAN
            "<C50>Under %: " OE-ORD.UNDER-PCT SKIP(.5)
            "<C4>CSR: " oe-ord.user-id
            "<C50>Order Date: " OE-ORD.ORD-DATE SKIP(1)

            "<||3><C2><FROM><C80><LINE><||3>" SKIP(1)

            "<C4>Purchase Order: " "<B> " OE-ORD.PO-NO "</B>"
            "<C40>Date Given: _______________________________________" SKIP(.5)
            "<C4>Requested Mfg Date:  " "<B> " oe-ordl.PROM-DATE "</B>"
            "<C40>Scheduling Sign-off: ________________________________" SKIP(.5)
            "<C4>Freight Charge: " v-frt-pay
            "<C40>F.O.B.: " OE-ORD.FOB-CODE 
            "<C61>Carrier: " OE-ORD.CARRIER SKIP(.5)
            "<C4>Payment Terms: " OE-ORD.TERMS  SKIP(1)

            "<||3><C2><FROM><C80><LINE><||3>" 
            "<#1><C52><FROM><R+12><C52><LINE><||3>"

            SKIP(1)
            "<R-12><C4>Item: " "<B> " oe-ordl.i-no "</B>"
            "<C27>Description: " "<B> " OE-ORDL.I-NAME FORMAT "x(25)" "</B>"
            "<C61>New/Repeat: " "<B> " v-type-code "</B>" SKIP(.5)
            "<C4>Quantity Ordered: " "<B> " oe-ordl.qty "</B>"
            "<C27>Description: " OE-ORDL.PART-DSCR1 FORMAT "x(25)"
            "<C61>Pharmacode: "  v-upc-no SKIP(.5)
            "<C4>UOM: " OE-ORDL.PR-UOM
            "<C27>Description: " OE-ORDL.PART-DSCR2 FORMAT "x(25)"
            "<C53>Date Planned: _______________________" SKIP(.5)
            "<C4>Price: " "<B> " OE-ORDL.PRICE "</B>"
            "<C27>Description: " OE-ORDL.PART-DSCR3 FORMAT "x(25)"
            "<C53>Date Materials Allocated: ______________" SKIP(.5)
            "<C4>Cost: " oe-ordl.cost
            "<C53>Date Scheduled: _____________________" SKIP(.5)

            "<C4><B><U>Misc Charges:</U></B>" SKIP(.5) .

           
            
         if v-printline ge lv-line-print THEN do:
            PAGE .
            PUT SKIP(4) .
            assign v-printline = 2.
         end.

             FOR EACH oe-ordm OF oe-ord WHERE oe-ordm.company EQ oe-ord.company
                 NO-LOCK:
                 
                 IF OE-ORDM.CHARGE NE "" THEN 
                     ASSIGN v-count = v-count + 1 
                            r = r + 1.

                 IF r EQ 1 THEN
                     v-note-lin = "Notes: ___________________________________________" .
                 ELSE
                     v-note-lin = "_________________________________________________".

                  if v-printline ge lv-line-print THEN do:
                      PAGE .
                      PUT SKIP(4) .
                      assign v-printline = 2.
                  end.

                 PUT 
                     "<C4>" v-count ") " OE-ORDM.CHARGE ":" OE-ORDM.AMT 
                     "<C40>" v-note-lin SKIP.
                 v-printline = v-printline + 1.
             END.
             IF v-count < 4 THEN DO WHILE v-count < 4:
                 v-count = v-count + 1.
                 v-printline = v-printline + 1.
                 PUT "<C40>_________________________________________________" SKIP.
             
             END.
             
             v-count = 0 .
             v-note-lin = "".
             r = 0.
             
              if v-printline ge lv-line-print THEN do:
                  PAGE .
                  PUT SKIP(4) .
                  assign v-printline = 2.
              end.

             PUT SKIP(1) 
            "<C4><B><U>Releases:</U>" SKIP(.5) .
             
             v-printline = v-printline + 1.

             if v-printline ge lv-line-print THEN do:
                  PAGE .
                  PUT SKIP(4) .
                  assign v-printline = 2.
             end.

             PUT
                 "<C9.5>Quantity<C21>PO#<C33>Lot#<C45>Date<C52>Ship-to Address (City/State)</B>" SKIP(.5) .
             
             v-printline = v-printline + 1.

             if v-printline ge lv-line-print THEN do:
                  PAGE .
                  PUT SKIP(4) .
                  assign v-printline = 2.
             end.
             
             FOR EACH oe-rel WHERE oe-rel.company EQ oe-ordl.company 
                 AND oe-rel.ord-no  EQ oe-ordl.ord-no 
                 AND oe-rel.i-no    EQ oe-ordl.i-no  
                 AND oe-rel.line    EQ oe-ordl.line NO-LOCK BY oe-rel.rel-date:
                
                 FIND FIRST reftable WHERE
                       reftable.reftable EQ "oe-rel.lot-no" AND
                       reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
                       NO-LOCK NO-ERROR.
                
                  IF AVAIL reftable THEN
                     ASSIGN
                        v-lot-no      = reftable.CODE .
                  ELSE v-lot-no      = "" .
                  

                 IF oe-rel.tot-qty NE 0 OR oe-rel.po-no NE "" OR reftable.CODE NE "" OR oe-rel.rel-date NE ? OR  oe-rel.ship-addr[1] NE "" THEN 
                     ASSIGN v-count2 = v-count2 + 1 .

                 if v-printline ge lv-line-print THEN do:
                  PAGE .
                  PUT SKIP(4) .
                  assign v-printline = 2.
                 end.
                 PUT
                   "<C4>" v-count2 ") " "<C6> " oe-rel.tot-qty "<C17>" oe-rel.po-no "<C30>" v-lot-no
                   "<C43>" oe-rel.rel-date "<C52>" TRIM(oe-rel.ship-addr[1]) FORMAT "x(22)" "(" trim(oe-rel.ship-city) "/" trim(oe-rel.ship-state) FORMAT "x(2)" ")" SKIP
                   .
                  v-printline = v-printline + 1.   

                 if v-printline ge lv-line-print THEN do:
                  PAGE .
                  PUT SKIP(4) .
                  assign v-printline = 2.
                 end.
             END.
             v-count2 = 0 .
             v-lot-no = "" .
          PAGE .
          
      end. /* each oe-ordl */
  END.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
