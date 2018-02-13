/* ---------------------------------------------- oe/rep/invpremx.p */
/* PRINT INVOICE   Xprint form for Premier Pkg  (PremierX and PremierS)       */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-copy-title AS cha NO-UNDO.
DEF INPUT PARAM ip-print-s AS LOG NO-UNDO. /* for PremierS */
{sys/inc/var.i shared}

{oe/rep/invoice.i}

def var v-salesman as char format "x(14)" NO-UNDO.
def var v-salesname as char format "x(30)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO. 
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-email as char format "x(130)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-t-weight like inv-line.t-weight NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
DEF SHARED VAR s-print-zero-qty AS LOG NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-date-ord as date NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS INT NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
DEF VAR v-pc AS cha NO-UNDO. /* partial or complete */

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .
DEF BUFFER b-inv-head FOR inv-head.

def TEMP-TABLE w-sman NO-UNDO
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
DEF VAR v-inv-qtys AS DEC NO-UNDO.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax NO-UNDO
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEFINE VARIABLE lFirstLine AS LOGICAL     NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(150)" NO-UNDO.
/*ASSIGN ls-image1 = "images\premiercan.jpg"          */
/*       FILE-INFO:FILE-NAME = ls-image1.             */
/*       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.

DEF VAR iDaysToPay AS INT NO-UNDO.
DEF VAR cInvDate AS CHAR NO-UNDO.
DEF VAR dOrigQty AS DEC NO-UNDO.
DEFINE VARIABLE cOrigUOM AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cXMLShipTo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lChkImage AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cTaxCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompanyID AS CHARACTER NO-UNDO.

/* rstark 05181205 */
{XMLOutput/XMLOutput.i &XMLOutput=XMLInvoice &Company=cocode}
RUN XMLOutput (lXMLOutput,'','','Header').
/* rstark 05181205 */

/* rstark 05291402 */
&SCOPED-DEFINE sysCtrlcXML cXMLInvoice
{XMLOutput/XMLOutput.i &cXMLOutput={&sysCtrlcXML} &Company=cocode &c=c}
/* rstark 05291402 */

    find first company where company.company = cocode no-lock no-error.
    IF company.company EQ '004' THEN 
        ASSIGN 
            cCurCode = 'CAD'
            cTaxCode = 'HST'
            cCompanyID = 'GST# 70523 1090 RT0001'
            .
    ELSE IF company.company EQ '006' THEN 
        ASSIGN 
            cCurCode = 'AUD'
            cTaxCode = 'GST'
            cCompanyID = 'ABN 11 620 887 149'
            .
    ELSE IF company.company EQ '005' THEN
        ASSIGN 
            cCurCode = 'USD'
            cTaxCode = 'Sales Tax'
            cCompanyID = ''
            .
    ELSE 
        ASSIGN 
            cCurCode = 'USD'
            cTaxCode = 'Sales Tax'
            cCompanyID = ''
            .
  RUN sys/ref/nk1look.p (INPUT company.company, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
            INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
            OUTPUT cRtnChar, OUTPUT lRecFound).

IF cRtnChar NE "" THEN DO:
    ASSIGN 
        lChkImage = YES
        ls-full-img1 = SEARCH(ls-full-img1)
        ls-full-img1 = cRtnChar + ">".
END.
DO:
    ASSIGN 
        ls-image1 = SEARCH("images\premierinv.jpg")
        FILE-INFO:FILE-NAME = ls-image1.
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
END.

    
    ASSIGN v-comp-add1 = ""
           v-comp-add2 = ""
           v-comp-add3 = " "
           v-comp-add4 = ""
           .
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by (IF v-sort THEN "" ELSE report.key-02)
              BY report.key-03:

      FIND FIRST cust WHERE cust.company = xinv-head.company
                        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

      /* rstark 05291402 */
      IF ccXMLOutput NE '' THEN DO:
        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ  cocode
              AND sys-ctrl.name    EQ 'cXMLInvoice' NO-ERROR.
        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
            FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
                WHERE sys-ctrl-shipto.cust-vend EQ YES
                  AND sys-ctrl-shipto.cust-vend-no EQ xinv-head.cust-no
                  AND sys-ctrl-shipto.log-fld EQ YES
                NO-ERROR.
            IF AVAIL sys-ctrl-shipto THEN 
                ASSIGN 
                    cXMLIdentity = sys-ctrl-shipto.char-fld
                    cXMLDTD = 'http://xml.cxml.org/schemas/cXML/1.2.025/InvoiceDetail.dtd'.
        END.
        {XMLOutput/cXMLCust.i &cXMLSysCtrl={&sysCtrlcXML} &Company=xinv-head.company &Customer=xinv-head.cust-no}
        FIND FIRST terms 
            WHERE terms.company EQ xinv-head.company
              AND terms.t-code EQ xinv-head.terms
            NO-LOCK NO-ERROR.
        IF AVAIL terms THEN
            iDaysToPay = terms.net-days.
        /*changing to today to handle Ariba 3 day constraints*/
        cInvDate = STRING(YEAR(TODAY),'9999')
                             + '-'
                             + STRING(MONTH(TODAY),'99')
                             + '-'
                             + STRING(DAY(TODAY),'99')
                             + 'T'
                             + STRING(0,'hh:mm:ss')
                             + '-05:00'.
      END.
      /* rstark 05291402 */
      
      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip
              cXMLShipTo = xinv-head.sold-no.

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
     /*   find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then */
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-bolh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip
                cXMLShipTo = shipto.ship-id.

       FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ xinv-head.company
              AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
              AND sys-ctrl-shipto.cust-vend EQ YES
              AND sys-ctrl-shipto.cust-vend-no EQ xinv-head.cust-no
            NO-ERROR.
        IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN 
            cXMLShipTo = TRIM(sys-ctrl-shipto.char-fld) + oe-bolh.ship-id.
      end. /* avail oe-bolh */  
      IF /*NOT v-reprint OR*/ xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).
        
        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        if inv-head.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        find FIRST carrier where carrier.company = inv-head.company and
          carrier.carrier = inv-head.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
        assign
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
      IF AVAIL cust  THEN
          ASSIGN v-email = cust.email.
      ELSE v-email = "" .
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error.
        if not avail stax then
        find first stax where stax.tax-group eq inv-head.tax-gr
            no-lock no-error.
   
        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        assign v-tot-pallets = 0.
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no AND
            (s-print-zero-qty OR
             NOT(xinv-line.ship-qty EQ 0 AND xinv-line.inv-qty EQ 0))
          break by xinv-line.i-no:

         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty)
                v-tot-pallets = 0
                v-pc = "C". /* complete*/
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no 
                /*oe-bolh.ord-no = xinv-line.ord-no*/ :
           v-pc = "P". /* partial*/ 
           FOR EACH oe-boll fields(cases partial p-c) NO-LOCK WHERE
              oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xinv-line.i-no AND
              oe-boll.ord-no = xinv-line.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0).
              IF oe-boll.p-c THEN v-pc = "C". /*complete*/
              
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                        /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                         (if oe-boll.partial gt 0 then 1 else 0) */.
         END. /* each oe-bolh */
         
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then
           do:
             find first eb where eb.company = xinv-line.company and
               eb.est-no = xinv-line.est-no and
               eb.e-num = xinv-line.e-num and
               eb.form-no = xinv-line.form-no and
               eb.blank-no = xinv-line.blank-no no-lock no-error.

             IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                  AND fg-set.set-no = xinv-line.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN
                  ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                  eb.est-no = xinv-line.est-no AND
                  eb.e-num = xinv-line.e-num AND
                  eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = xinv-line.company AND
                    fg-set.set-no = xinv-line.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.part-qty NE 0 THEN
                   ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
                 ELSE
                   ASSIGN v-part-qty = 1 / v-set-qty.


                IF eb.cas-cnt = 0 THEN
                   ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 ELSE
                   ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
               END. /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN
             DO:
               IF eb.cas-cnt = 0 THEN
                 ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
               ELSE
                 ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             END. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each xinv-line */
    
                        /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        find first oe-bolh where oe-bolh.company = inv-head.company and
                                 oe-bolh.bol-no = inv-head.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.

        find first inv-line where inv-line.r-no = inv-head.r-no
                                  no-lock no-error.
        if avail inv-line then
        do:
          assign v-price-head = inv-line.pr-uom.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date
                   cXMLPayloadID = oe-ord.spare-char-3
                   cXMLProcessID = STRING(oe-ord.ord-no)
                   .
            
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.
        

        
        /* rstark 05181205 */
        XMLLineNumber = 0.
        RUN XMLOutput (lXMLOutput,'InvoiceHeader','','Row').
        RUN XMLOutput (lXMLOutput,'RemitTo','PREMIER PACKAGING','Col').
        RUN XMLOutput (lXMLOutput,'Remit_1','3254 RELIABLE PARKWAY','Col').
        RUN XMLOutput (lXMLOutput,'Remit_2','CHICAGO, IL 60686','Col').
        RUN XMLOutput (lXMLOutput,'Customer',inv-head.cust-name,'Col').
        RUN XMLOutput (lXMLOutput,'Bill_1',inv-head.addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_2',inv-head.addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_3',v-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'EMail',v-email,'Col').
        RUN XMLOutput (lXMLOutput,'ShipName',v-shipto-name,'Col').
        RUN XMLOutput (lXMLOutput,'Ship_1',v-shipto-addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_2',v-shipto-addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_3',v-sold-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceNo',inv-head.inv-no,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceDate',v-inv-date,'Col').
        RUN XMLOutput (lXMLOutput,'ShipDate',v-date-ship,'Col').
        RUN XMLOutput (lXMLOutput,'FOB',v-fob,'Col').
        RUN XMLOutput (lXMLOutput,'ShipVia',v-shipvia,'Col').
        RUN XMLOutput (lXMLOutput,'Terms',xinv-head.terms-d,'Col').
        RUN XMLOutput (lXMLOutput,'SalesRep',v-salesname,'Col').
        RUN XMLOutput (lXMLOutput,'BOL',xinv-head.bol-no,'Col').
        RUN XMLOutput (lXMLOutput,'/InvoiceHeader','','Row').
        XMLPage = NO.
        /* rstark 05181205 */

        /* rstark 05291402 */
        cXMLLineNumber = 0. 
        RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + cXMLProduction + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequest','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequestHeader ' 
                                + 'invoiceDate="' + cInvDate + '" '
                                + 'invoiceID="' + STRING(inv-head.inv-no) + '" '
                                + 'operation="new" purpose="standard"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailHeaderIndicator/','','Row').  
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineIndicator isShippingInLine="yes" /','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="billTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',inv-head.cust-name,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',inv-head.addr[1],'Col').
        IF inv-head.addr[2] NE '' THEN 
            RUN cXMLOutput (clXMLOutput,'Street',inv-head.addr[2],'Col').
        RUN cXMLOutput (clXMLOutput,'City',inv-head.city,'Col').
        RUN cXMLOutput (clXMLOutput,'State',inv-head.state,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',inv-head.zip,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="Premier Packaging" role="remitTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3254 Reliable Pkwy','Col').
        RUN cXMLOutput (clXMLOutput,'City','Chicago','Col').
        RUN cXMLOutput (clXMLOutput,'State','IL','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','60686','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="' + cXMLShipTo + '" role="shipTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',v-shipto-name,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[1],'Col').
        IF v-shipto-addr[2] NE "" AND v-shipto-addr[2] NE '345 Court Street' THEN
             RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[2],'Col').
        RUN cXMLOutput (clXMLOutput,'City',v-shipto-city,'Col').
        RUN cXMLOutput (clXMLOutput,'State',v-shipto-state,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',v-shipto-zip,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
        RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
        RUN cXMLOutput (clXMLOutput,'State','KY','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row'). 
        RUN cXMLOutput (clXMLOutput,'PaymentTerm payInNumberOfDays="' + STRING(iDaysToPay)  + '" /','','Row').
/*         RUN cXMLOutput (clXMLOutput,'Extrinsic name="invoiceSubmissionMethod"','','Row'). */
/*         RUN cXMLOutput (clXMLOutput,'','CSVUpload','Col').                                */
/*         RUN cXMLOutput (clXMLOutput,'/Extrinsic','','Row').                               */
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequestHeader ','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrder','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrderInfo','','Row').
        RUN cXMLOutput (clXMLOutput,'OrderReference orderID="' + v-po-no + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + cXMLPayloadID + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'/DocumentReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrderInfo','','Row').
        XMLPage = NO.
        /* rstark 05291402 */

        {oe/rep/invpremx.i}  /* xprint form */

        ASSIGN
        v-subtot-lines = 0
        v-t-tax = 0.
        lFirstLine = YES.
        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:

          IF NOT s-print-zero-qty AND
             inv-line.ship-qty EQ 0 AND inv-line.inv-qty EQ 0 THEN
             NEXT.

          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          v-pc = "P". /* partial*/ 
          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        /*and oe-boll.b-no = inv-line.b-no*/
                        and oe-boll.i-no = inv-line.i-no use-index bol-no:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            IF oe-boll.p-c THEN v-pc = "C". /*complete*/

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */

            
          end. /* each oe-boll */

          IF v-printline > 62 THEN do:           
                PAGE.
                {oe/rep/invpremx.i}
                v-printline = 39.
          END.

            assign v-line = v-line + 1
                    /* v-printline = v-printline + 2 */.  
            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = inv-line.ord-no and
                                     oe-ordl.i-no = inv-line.i-no
                                     no-lock no-error.
            if avail oe-ordl THEN DO:
            
              assign v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty).
              IF oe-ordl.spare-char-2 NE "" THEN DO:
                    ASSIGN 
                        dOrigQty = oe-ordl.spare-dec-1
                        cOrigUom = oe-ordl.spare-char-2
                        .
                    IF cOrigUom EQ 'CS' 
                        AND dOrigQty NE inv-line.qty
                        AND oe-ordl.cas-cnt NE 0 THEN 
                            dOrigQty = inv-line.inv-qty / oe-ordl.cas-cnt.
                    ELSE
                        dOrigQty = inv-line.inv-qty.
                           
                END.
            END.
            else
              assign v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty.
            assign v-inv-qty = inv-line.qty
                   v-ship-qty = inv-line.ship-qty
                   v-i-no = inv-line.i-no
                   v-i-dscr = inv-line.i-name
                   v-price = inv-line.price * (1 - (inv-line.disc / 100))
                   v-t-price = inv-line.t-price
                   v-subtot-lines = v-subtot-lines + inv-line.t-price.
            /* PremierS switch*/
            IF ip-print-s THEN v-inv-qtys = inv-line.inv-qty / inv-line.cas-cnt.

                if inv-line.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else inv-line.t-price) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne inv-line.t-price then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.
            
            ASSIGN v-po-no  = inv-line.po-no
                   v-ord-no = inv-line.ord-no
                   v-price-head = inv-line.pr-uom.

            PUT space(1)
                v-po-no 
                inv-line.part-no  SPACE(1)
                v-i-dscr FORM "x(30)". 
            /* PremierS switch*/
            IF ip-print-s THEN
                PUT v-inv-qtys  format "->>9.99" SPACE(3).
            ELSE PUT v-ship-qty  format "->>>>>9" SPACE(3).
            PUT v-price  format ">>>,>>9.9999"                
                inv-line.t-price  format "->>>,>>9.99"                
                SKIP
                v-ord-no SPACE(10)
                inv-line.i-no SPACE(1)
                inv-line.part-dscr1  SPACE(11)
                v-pc  FORM "x" SPACE(7)
                v-price-head SKIP
                inv-line.part-dscr2 AT 33 SKIP.

             v-printline = v-printline + 3.
             
             /* rstark 05181205 */

             XMLLineNumber = XMLLineNumber + 1.
             RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             RUN XMLOutput (lXMLOutput,'Column_1',v-po-no,'Col').
             RUN XMLOutput (lXMLOutput,'Column_2',inv-line.part-no,'Col').
             RUN XMLOutput (lXMLOutput,'Column_3',v-i-dscr,'Col').
             /* PremierS switch*/
             IF ip-print-s THEN RUN XMLOutput (lXMLOutput,'Column_4',v-inv-qtys,'Col').
             ELSE RUN XMLOutput (lXMLOutput,'Column_4',v-ship-qty,'Col').
             RUN XMLOutput (lXMLOutput,'Column_5','','Col').
             RUN XMLOutput (lXMLOutput,'Column_6',v-price,'Col').
             RUN XMLOutput (lXMLOutput,'Column_7',inv-line.t-price,'Col').
             RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             
             XMLLineNumber = XMLLineNumber + 1.
             RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             RUN XMLOutput (lXMLOutput,'Column_1',v-ord-no,'Col').
             RUN XMLOutput (lXMLOutput,'Column_2',inv-line.i-no,'Col').
             RUN XMLOutput (lXMLOutput,'Column_3',inv-line.part-dscr1,'Col').
             RUN XMLOutput (lXMLOutput,'Column_4','','Col').
             RUN XMLOutput (lXMLOutput,'Column_5',v-pc,'Col').
             RUN XMLOutput (lXMLOutput,'Column_6',v-price-head,'Col').
             RUN XMLOutput (lXMLOutput,'Column_7','','Col').
             RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

             XMLLineNumber = XMLLineNumber + 1.
             RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             RUN XMLOutput (lXMLOutput,'Column_1','','Col').
             RUN XMLOutput (lXMLOutput,'Column_2','','Col').
             RUN XMLOutput (lXMLOutput,'Column_3',inv-line.part-dscr2,'Col').
             RUN XMLOutput (lXMLOutput,'Column_4','','Col').
             RUN XMLOutput (lXMLOutput,'Column_5','','Col').
             RUN XMLOutput (lXMLOutput,'Column_6','','Col').
             RUN XMLOutput (lXMLOutput,'Column_7','','Col').
             RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

             XMLLineNumber = XMLLineNumber + 1.
             /* rstark 05181205 */
             
             /* rstark 05291402 */

             RUN cXMLOutput (clXMLOutput,'InvoiceDetailItem invoiceLineNumber="' + STRING(inv-line.LINE) 
                             + '" quantity="' + STRING(IF dOrigQty NE 0 THEN dOrigQty ELSE inv-line.inv-qty) + '"','','Row').
             RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',inv-line.pr-uom,'Col').
             RUN cXMLOutput (clXMLOutput,'UnitPrice','','Row'). 
             RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
             RUN cXMLOutput (clXMLOutput,'',STRING(v-price),'Col').
             RUN cXMLOutput (clXMLOutput,'/Money','','Row').
             RUN cXMLOutput (clXMLOutput,'/UnitPrice','','Row'). 
             RUN cXMLOutput (clXMLOutput,'InvoiceDetailItemReference lineNumber="' + STRING(inv-line.LINE) + '"','','Row').
             RUN cXMLOutput (clXMLOutput,'ItemID','','Row'). 
             RUN cXMLOutput (clXMLOutput,'SupplierPartID',inv-line.i-no,'Col').
             RUN cXMLOutput (clXMLOutput,'/ItemID','','Row'). 
             RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
             RUN cXMLOutput (clXMLOutput,'',inv-line.i-name,'Col').
             RUN cXMLOutput (clXMLOutput,'/Description','','Row').
             RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItemReference','','Row').
             RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row'). 
             RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
             RUN cXMLOutput (clXMLOutput,'',STRING(inv-line.t-price),'Col').
             RUN cXMLOutput (clXMLOutput,'/Money','','Row').
             RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row'). 
             RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineShipping','','Row').
             RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
             RUN cXMLOutput (clXMLOutput,'Contact addressID="' + cXMLShipTo + '" role="shipTo"','','Row').
             RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
             RUN cXMLOutput (clXMLOutput,'',v-shipto-name,'Col').
             RUN cXMLOutput (clXMLOutput,'/Name','','Row').
             RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
             RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[1],'Col').
             IF v-shipto-addr[2] NE "" AND v-shipto-addr[2] NE '345 Court Street' THEN 
                 RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[2],'Col').
             RUN cXMLOutput (clXMLOutput,'City',v-shipto-city,'Col').
             RUN cXMLOutput (clXMLOutput,'State',v-shipto-state,'Col').
             RUN cXMLOutput (clXMLOutput,'PostalCode',v-shipto-zip,'Col').
             RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
             RUN cXMLOutput (clXMLOutput,'','US','Col').
             RUN cXMLOutput (clXMLOutput,'/Country','','Row').
             RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
             RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
             RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
             RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
             RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
             RUN cXMLOutput (clXMLOutput,'/Name','','Row').
             RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
             RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
             RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
             RUN cXMLOutput (clXMLOutput,'State','KY','Col').
             RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
             RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
             RUN cXMLOutput (clXMLOutput,'','US','Col').
             RUN cXMLOutput (clXMLOutput,'/Country','','Row').
             RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
             RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
             RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row').
             RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
             IF lFirstLine THEN DO: 
                 RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-freight),'Col').
                 lFirstLine = NO.
             END.
             ELSE
                RUN cXMLOutput (clXMLOutput,'','0','Col').
             RUN cXMLOutput (clXMLOutput,'/Money','','Row').
             RUN cXMLOutput (clXMLOutput,'/InvoiceDetailLineShipping','','Row').
             RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItem','','Row'). 
            
             /* rstark 05291402 */
            
             put skip(1).
             v-printline = v-printline + 1.
        end. /* each inv-line */
        
        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
            inv-misc.bill = "Y" break by ord-no with frame detailm:
            
              
          IF v-printline > 62 THEN do:
              
                PAGE.                
                {oe/rep/invpremx.i}
                v-printline = 39.
          END.
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.

            /* rstark 05181205 */
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2','','Col').
            RUN XMLOutput (lXMLOutput,'Column_3','** Miscellaneous Items **','Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            XMLLineNumber = XMLLineNumber + 1.
            /* rstark 05181205 */

          end.

            put inv-misc.charge AT 17 inv-misc.dscr inv-misc.amt AT 85 SKIP.

            /* rstark 05181205 */
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2',inv-misc.charge,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',inv-misc.dscr,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7',inv-misc.amt,'Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            /* rstark 05181205 */

            ASSIGN
            v-subtot-lines = v-subtot-lines + inv-misc.amt
            v-printline = v-printline + 1.
            if inv-misc.tax and avail stax then
            do i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.company eq "yes" then v-t-price
                              else inv-misc.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
              end.
            end.

            if v-t-price ne inv-misc.amt then do:
              create w-tax.
              assign
               w-dsc     = "******ITEM TOTAL:"
               w-tax     = v-t-price
               v-lines   = v-lines + 1.
            end.

        end. /* each inv-misc */

        IF v-prntinst THEN 
        DO:
            do i = 1 to 4:
                if inv-head.bill-i[i] ne "" then 
                do:
                    put inv-head.bill-i[i] at 10 skip.
                    assign 
                        v-printline = v-printline + 1.
             

            /* rstark 05181205 */
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2',inv-head.bill-i[i],'Col').
            RUN XMLOutput (lXMLOutput,'Column_3','','Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            /* rstark 05181205 */

          end.
         end. /* 1 to 4 */
         IF v-printline > 63 THEN 
            DO:              
                PAGE.                
                {oe/rep/invpremx.i}
                v-printline = 39.
            END.
         
        end.

        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr <> "" and
           inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                         ELSE inv-head.t-inv-freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end.
      end. /* DO TRANSACTION */

    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = 0.
    FOR EACH b-inv-head WHERE b-inv-head.company = inv-head.company
                          AND b-inv-head.inv-no  = inv-head.inv-no
                          AND b-inv-head.multi-invoice = no
                          AND b-inv-head.f-bill  = TRUE
                        NO-LOCK.
       v-inv-freight = v-inv-freight + b-inv-head.t-inv-freight.
    END.
   
    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        "<=8><R+2> " cTaxCode FORMAT "x(9)" "    :" inv-head.t-inv-tax FORM "->>,>>9.99"
        "<=8><R+3>" "" 
        "<=8><R+4> Total Invoice:" inv-head.t-inv-rev FORM "->>,>>9.99" .

    PUT "<FArial><R58><C1><P12><B> THANK YOU. </B> <P9> " SKIP.
    PUT "<FArial><R60><C1><P12> All currencies displayed in " cCurCode FORMAT "x(3)" ". <P9> " SKIP.
    v-printline = v-printline + 8.
    IF v-printline > 63 THEN 
        DO:              
            PAGE.                
               
            v-printline = 39.
        END.
    /* rstark 05181205 */
    RUN XMLOutput (lXMLOutput,'Last',STRING(PAGE-NUM),'Page').
    RUN XMLOutput (lXMLOutput,'InvoiceFooter','','Row').
    RUN XMLOutput (lXMLOutput,'SubTotal',v-subtot-lines,'Col').
    RUN XMLOutput (lXMLOutput,'Freight',v-inv-freight,'Col').
    RUN XMLOutput (lXMLOutput,'SalesTax',inv-head.t-inv-tax,'Col').
    RUN XMLOutput (lXMLOutput,'TotalInvoice',inv-head.t-inv-rev,'Col').
    RUN XMLOutput (lXMLOutput,'/InvoiceFooter','','Row').
    /* rstark 05181205 */

    /* rstark 05291402 */
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrder','','Row'). 
    RUN cXMLOutput (clXMLOutput,'InvoiceDetailSummary','','Row').
    RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-subtot-lines),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Tax','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-tax),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
    RUN cXMLOutput (clXMLOutput,'','Sales Tax','Col').
    RUN cXMLOutput (clXMLOutput,'/Description','','Row').
    RUN cXMLOutput (clXMLOutput,'TaxDetail category="SalesTax" '+
                                'percentageRate="0"','','Row').
    RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-subtot-lines),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-tax),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row').    
    RUN cXMLOutput (clXMLOutput,'/Tax','','Row').
    RUN cXMLOutput (clXMLOutput,'SpecialHandlingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'','0','Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/SpecialHandlingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'ShippingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-inv-freight),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/ShippingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'GrossAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-rev),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/GrossAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-rev),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailSummary','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequest','','Row').
    RUN cXMLOutput (clXMLOutput,'/Request','','Row').
    /* rstark 05291402 */

    IF v-printline <= 66 THEN page.
    
    {XMLOutput/XMLOutput.i &c=c &XMLClose} /* rstark 05291402 */
    /* Create eddoc for invoice if required */
    RUN ed/asi/o810hook.p (recid(inv-head), no, no).     
    FIND FIRST edmast NO-LOCK
          WHERE edmast.cust EQ inv-head.cust-no
          NO-ERROR.
   IF AVAIL edmast THEN DO: 
     FIND FIRST edcode NO-LOCK
       WHERE edcode.partner EQ edmast.partner
     NO-ERROR.
     IF NOT AVAIL edcode THEN 
       FIND FIRST edcode NO-LOCK
            WHERE edcode.partner EQ edmast.partnerGrp
            NO-ERROR.
   END.  
   IF AVAIL edcode AND edcode.sendFileOnPrint THEN    
    RUN ed/asi/write810.p (INPUT cocode).    
  end. /* each xinv-head */

{XMLOutput/XMLOutput.i &XMLClose} /* rstark 05181205 */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
