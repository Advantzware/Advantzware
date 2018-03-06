/* ---------------------------------------------- oe/rep/bolfibrex.p          */
/* PRINT Fibrex BOL                                                           */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
{sys/form/r-top.i}    

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

{oerep/r-bolx.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format "->,>>>,>>9" NO-UNDO.
def var v-tot-palls         as   int format "->,>>>,>>9" NO-UNDO.
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
DEF VAR v-lines AS INT NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-line1 AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-i AS INT NO-UNDO.

/* gdm - 03060901 */
DEF VAR v-txt1     AS CHAR FORMAT "x(220)" EXTENT 30 NO-UNDO.
DEF VAR v-frtclass LIKE itemfg.frt-class NO-UNDO.

/* gdm - 04160903 */
DEF VAR v-lot# AS CHAR NO-UNDO.
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9"
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INT 
    FIELD dscr LIKE oe-ordl.part-dscr1
    FIELD partial          AS INTEGER
    FIELD unitcount        AS INTEGER 
    FIELD qty-sum          AS INTEGER .

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(180)" NO-UNDO.

FIND FIRST tt-bolx NO-ERROR.

IF AVAIL tt-bolx AND tt-bolx.print-logo THEN
   ls-full-img1 = tt-bolx.logo-file + ">".
ELSE IF NOT AVAIL tt-bolx THEN
   ASSIGN
      ls-image1 = "images\fibrelog.bmp"
      FILE-INFO:FILE-NAME = ls-image1
      ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
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
DEF VAR ll-display-comp AS LOG NO-UNDO.
DEF VAR ll-consol-bolls AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-cusx-add1 AS cha NO-UNDO.
DEF VAR v-cusx-add2 AS cha NO-UNDO.
DEF VAR v-cusx-add3 AS cha NO-UNDO.
DEF VAR v-cusx-add4 AS cha NO-UNDO.
DEF VAR v-cusx-add5 AS cha NO-UNDO.
DEF VAR v-cusx-name AS cha NO-UNDO.
DEFINE VARIABLE icountpallet AS INTEGER NO-UNDO .

def buffer b-itemfg     for itemfg.
DEF BUFFER bf-ttboll FOR tt-boll.
DEF VAR v-tot-case-qty AS INT NO-UNDO.
form w2.i-no                         format "x(15)"
     w2.job-po                       at 17 format "x(15)"
     w2.dscr                         at 33 format "x(30)"
     w2.cases                        to 70 format "->>>>"
     icountpallet                      to 77 format "->>>>>>"
     tt-boll.qty                     to 85 format "->>>>>>"
     bf-ttboll.p-c                   at 92
    with frame bol-mid down no-box no-labels stream-io width 110.

form oe-ordl.i-no                         format "x(15)"
     v-job-po                       at 17 format "x(15)"
     v-part-dscr                    at 33 format "x(30)"
     w2.cases                       to 70 format "->>>9"
     icountpallet                    to 77 format "->>>>>9"
     tt-boll.qty                    to 85 format "->>>>>9"
     tt-boll.p-c                    at 92
    with frame bol-mid2 down no-box no-labels stream-io width 100.

/* gdm - 04160923 
form 
     v-job-po                       at 17 format "x(15)"     
    with frame bol-mid3 down no-box no-labels stream-io width 100.
**********************/

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
ASSIGN
 ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld
 ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0
 lv-bolfmt-int   = IF AVAIL sys-ctrl THEN sys-ctrl.int-fld ELSE 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

FIND FIRST cust WHERE cust.company = cocode AND
                      cust.active = "X" NO-LOCK NO-ERROR.
IF AVAIL cust THEN
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
     v-phone-num    = cust.area-code + cust.phone.
     
    if shipto.broker then 
       assign
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
/*              v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"          */
/*                            else if cust.frt-pay eq "B" then "Bill"        */
/*                            else if cust.frt-pay eq "C" then "Collect"     */
/*                            else if cust.frt-pay eq "T" then "Third Party" */
/*                            else ""                                        */
             v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                           else if oe-bolh.frt-pay eq "B" then "Bill"
                           else if oe-bolh.frt-pay eq "C" then "Collect"
                           else if oe-bolh.frt-pay eq "T" then "Third Party"
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
         v-job-no = IF oe-boll.job-no = "" THEN ""
                    ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-bolh.lot-no" AND
           reftable.rec_key  EQ oe-bolh.rec_key
           USE-INDEX rec_key
           NO-LOCK NO-ERROR.

      IF AVAIL reftable THEN
         ASSIGN v-fob = reftable.CODE.

      IF v-fob = "" THEN
        ASSIGN v-fob = oe-ord.fob-code.

      ASSIGN v-fob = (if v-fob begins "O" then "Origin" 
                      ELSE IF v-fob BEGINS "d" THEN "Destination" 
                      ELSE "").

      LEAVE.
    end.

    EMPTY TEMP-TABLE w3.

    FOR EACH tt-boll:
        DELETE tt-boll.
    END.
    
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    IF ll-consol-bolls THEN DO:
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).
    
     /* IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).*/
        
    END.

    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
         /* ASSIGN tt-boll.rec_id = STRING(RECID(oe-boll))*/.
    END.

    oe-boll.printed = YES.
  END.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     /* end of dup loop */

     {oe/rep/bolfibrex.i}
     {oe/rep/bolfibrex2.i}

     v-last-page = page-number.

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
        {oe/rep/bolfibrex.i}

     END.
         

     PUT "<FArial><P7><R47><C13> SHORTAGES OR DAMAGE SHOULD BE NOTED ON RECEIPT OF SHIPMENT, OTHERWISE CLAIMS WILL NOT BE ALLOWED." SKIP
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
   
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
         delete report.
     end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

RETURN.

PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.

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

  IF NOT AVAIL tt-boll THEN DO:
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
        IF AVAIL job THEN
            FIND FIRST reftable NO-LOCK
               WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job.job,"999999999")
                 AND reftable.code2    EQ tt-boll.i-no
                 USE-INDEX reftable NO-ERROR.
            IF NOT AVAIL reftable THEN
               FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ tt-boll.company
                  AND job-hdr.job-no  EQ tt-boll.job-no
                  AND job-hdr.job-no2 EQ tt-boll.job-no2
                  AND job-hdr.i-no    EQ tt-boll.i-no NO-ERROR.

            IF AVAIL reftable OR AVAIL job-hdr THEN
                FOR EACH rm-rcpth NO-LOCK
                  WHERE rm-rcpth.company   EQ tt-boll.company
                    AND rm-rcpth.job-no    EQ tt-boll.job-no
                    AND rm-rcpth.job-no2   EQ tt-boll.job-no2
                    AND rm-rcpth.rita-code EQ "I" USE-INDEX job,
                 EACH rm-rdtlh NO-LOCK
                  WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                    AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                    AND rm-rdtlh.s-num     EQ (IF AVAIL reftable 
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
