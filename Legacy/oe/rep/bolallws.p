/* ------------------------------------------- oe/rep/bolallws.p GDM 04200904*/
/* BOL PRINT for N-K-1-BOLFMT = Allwest                                      */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format "->,>>>,>>9" NO-UNDO.
def var v-tot-palls         as   int format "->>,>>>,>>9" NO-UNDO.
def var v-tot-wt            as   dec format "->>,>>>,>>9" NO-UNDO.

def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
def var v-pal-cnt           as   DEC NO-UNDO.
def var v-ord-qty           like oe-ordl.qty NO-UNDO.
def var v-bol-qty           like oe-boll.qty NO-UNDO.
def var v-ship-qty          like oe-ordl.ship-qty NO-UNDO.
def var v-bol-wt            as   DEC NO-UNDO.
def var v-part-dscr         as   char format "x(30)" NO-UNDO.
def var v-part-comp         as   char format "x" NO-UNDO.
def var v-part-qty          as   DEC NO-UNDO.
def var v-ord-no            like oe-boll.ord-no NO-UNDO.
def var v-po-no             like oe-bolh.po-no NO-UNDO.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.
DEF VAR v-ship-phone        AS   CHAR FORMAT "X(13)" NO-UNDO.

def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-comp-name  like company.NAME NO-UNDO.
def var v-comp-addr  like company.addr NO-UNDO.
def var v-comp-city  like company.city NO-UNDO.
def var v-comp-state like company.state NO-UNDO.
def var v-comp-zip   like company.zip NO-UNDO.
def var v-comp-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-job-var AS CHAR NO-UNDO.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
DEF VAR v-cusx-add1 AS cha NO-UNDO.
DEF VAR v-cusx-add2 AS cha NO-UNDO.
DEF VAR v-cusx-add3 AS cha NO-UNDO.
DEF VAR v-cusx-add4 AS cha NO-UNDO.
DEF VAR v-cusx-add5 AS cha NO-UNDO.
DEF VAR v-cusx-email AS cha NO-UNDO.
DEF VAR v-cusx-name AS cha NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-phone AS cha NO-UNDO.
DEF VAR v-shipto-contact LIKE shipto.contact NO-UNDO.
DEF VAR v-ship-i AS cha EXTENT 4 FORM "x(60)" NO-UNDO.
DEF VAR v-tmp-lines AS DEC NO-UNDO.

DEF VAR v-txt1     AS CHAR FORMAT "x(220)" EXTENT 30 NO-UNDO.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
            lv-bolfmt-int = 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

IF lv-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
   IF AVAIL cust THEN
      ASSIGN v-comp-add1 = cust.addr[1]
             v-comp-add2 = cust.addr[2]
             v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
             v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
             v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
             lv-email    = "Email:  " + cust.email 
             lv-comp-name = cust.NAME   
             v-cusx-add1 = v-comp-add1
             v-cusx-add2 = v-comp-add2
             v-cusx-add3 = v-comp-add3
             v-cusx-add4 = v-comp-add4
             v-cusx-add5 = v-comp-add5
             v-cusx-email = lv-email
             v-cusx-name = lv-comp-name.
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
{sa/sa-sls01.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-printline = 0.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,
    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    NO-LOCK
    break by oe-bolh.bol-no:
  
    if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone
     v-ship-phone   = IF shipto.area-code + shipto.phone <> "" THEN
                      "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx")
                      ELSE ""
    v-phone = IF oe-bolh.area-code + oe-bolh.phone <> "" THEN 
              "(" + oe-bolh.area-code + ")" + string(oe-bolh.phone,"xxx-xxxx")
              ELSE ""
    v-shipto-contact = oe-bolh.contact.

    IF v-phone = "" THEN v-phone = "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx").
    IF v-shipto-contact = "" THEN v-shipto-contact = shipto.contact.

    if shipto.broker then DO:
       ASSIGN v-comp-add1 = cust.addr[1]
              v-comp-add2 = cust.addr[2]
              v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
              v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
              v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
              lv-email    = "Email:  " + cust.email   
              lv-comp-name = cust.NAME .
       /* sold to address from order */
       FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
       IF AVAIL oe-boll THEN DO:
          FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                              AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
          IF AVAIL oe-ord THEN
             ASSIGN lv-comp-name = oe-ord.sold-name
                    v-comp-add1 = oe-ord.sold-addr[1]
                    v-comp-add2 = oe-ord.sold-addr[2]
                    v-comp-add3 = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.        
       END.
    END.
    ELSE
       ASSIGN v-comp-add1 = v-cusx-add1
              v-comp-add2 = v-cusx-add2    
              v-comp-add3 = v-cusx-add3    
              v-comp-add4 = v-cusx-add4                
              v-comp-add5 = v-cusx-add5
              lv-email    = v-cusx-email
              lv-comp-name = v-cusx-name.
    assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

      assign v-terms = oe-ord.terms-d
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      ASSIGN
         v-salesman = trim(v-salesman)
         v-po-no = oe-boll.po-no
         v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    EMPTY TEMP-TABLE w3.
    
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
        ASSIGN v-comp-addr[2] = v-comp-addr3
               v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
        ASSIGN v-ship-addr[2] = v-ship-addr3
               v-ship-addr3 = "".
     
     {oe/rep/bolallws2.i}
     {oe/rep/bolallws.i}

    v-last-page = page-number.

  IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

  PUT "<R44><C51><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Pallets       :" v-tot-palls 
    "<=8><R+2> Total Weight        :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
  ASSIGN v-ship-i = "".
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

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

