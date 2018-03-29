/* ---------------------------------------------- oe/rep/invsthpk.p */
/* PRINT INVOICE   Xprint form for South Pak             */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEF INPUT PARAM  ip-lines-per-page AS INT NO-UNDO.

DEF STREAM st-fax.

{sys/inc/var.i shared}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
{oe/rep/invoice.i}

def var v-salesman as char format "x(14)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-id as char format "x(30)" NO-UNDO.
def var v-shipto-phone as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-soldto-city as char format "x(15)" NO-UNDO.
def var v-soldto-state as char format "x(2)" NO-UNDO.
def var v-soldto-zip as char format "x(10)" NO-UNDO.

def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-t-weight like inv-line.t-weight NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS DECIMAL NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .

def workfile w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-due-date like oe-ord.due-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR tax-dscr-1 AS CHAR NO-UNDO.
DEF VAR tax-dscr-2 AS CHAR NO-UNDO.
DEF VAR tax-dscr-3 AS CHAR NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\southpak.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.

/* vARIABLE FOR EXCEL OUTPUT */
DEFINE SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.
/* skb 1/24/07 - Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE 						  NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE 							NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE 							NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE 							NO-UNDO. 
DEFINE VARIABLE v-cell                          AS CHARACTER  							NO-UNDO.
DEFINE VARIABLE t-dwg                           AS CHAR       							NO-UNDO.
DEFINE VARIABLE t-name                          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE t-fnd                           AS LOGICAL  INIT "False"    NO-UNDO.
DEFINE VARIABLE t-seq                           AS INTEGER  								NO-UNDO.
DEFINE VARIABLE inRowCount                      AS INTEGER    							NO-UNDO    INITIAL 1.
DEFINE VARIABLE chFile AS CHAR NO-UNDO.
DEFINE VARIABLE LvLineCnt AS INT NO-UNDO.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.
DEFINE VARIABLE LvCtr as int no-undo.
DEF VAR vcTemplateFile AS CHAR NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

DEFINE VARIABLE CurActivePrinter AS CHAR NO-UNDO.
DEFINE VARIABLE AdobePrinter     AS CHAR NO-UNDO.
define variable CommandString    AS CHAR NO-UNDO.
define variable WshNetwork as com-handle.
DEFINE VARIABLE LvFirstTimePrint AS LOGICAL INIT NO NO-UNDO.
/**************************** Excel Initilization Starts *********************************/

DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

assign CurActivePrinter = SESSION:PRINTER-NAME
       AdobePrinter     = "PDFcamp Printer".

/* Capture the current active printer */
CREATE "WScript.Network" WshNetwork NO-ERROR.
IF NOT(VALID-HANDLE(WshNetwork)) THEN
DO :
  MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

IF LvOutputSelection = "Email" THEN
DO:
   WshNetwork:SetDefaultPrinter(AdobePrinter). 
END.
CREATE "Excel.Application" chExcelApplication NO-ERROR.
chExcelApplication:VISIBLE = TRUE.
IF LvOutputSelection = "Email" or LvOutputSelection = "Printer" THEN
chExcelApplication:VISIBLE = FALSE.

IF NOT(VALID-HANDLE(chExcelApplication)) THEN
DO :
  MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

FILE-INFO:FILE-NAME = "Template\invoice-sp.xlt".

/* Set the Excel Template to be used. */
ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
if search (chFile) = ? then do:
   MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
           'cannot be found. Please verify that the file exists.'
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
   apply 'CLOSE':U to this-procedure.
end.

CurrDir = SUBSTRING (chFile, 1, INDEX (chFile, "Template\invoice-sp.xlt") - 2)
          no-error.

for each tt-filelist:
  delete tt-filelist.
end.
/**************************** Excel Initilization End *********************************/
    
    find first company where company.company = cocode no-lock no-error.

    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.
for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      
            
      FIND FIRST cust WHERE cust.company = xinv-head.company
                        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

      assign  v-shipto-name = xinv-head.sold-name
      				v-shipto-id   = xinv-head.sold-no
      				v-shipto-phone = ""
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip.

      v-del-no = 0.
      
      find first oe-bolh where oe-bolh.company = xinv-head.company and
           oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-bolh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
        			  v-shipto-id = shipto.ship-id
        			  v-shipto-phone = shipto.area-code + shipto.phone
        			  v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */
      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
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
          v-soldto-city = inv-head.city
          v-soldto-state = inv-head.state
          v-soldto-zip = inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
      
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
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).
         v-tot-pallets = 0.
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                                    oe-boll.b-no = oe-bolh.b-no AND
                                    oe-boll.i-no = xinv-line.i-no AND
                                    oe-boll.ord-no = xinv-line.ord-no :

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0).
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date.
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
                 ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
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

                 IF AVAIL fg-set AND fg-set.QtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
          if avail oe-ord then
          do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date
                   v-due-date = oe-ord.due-date.
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.
        
        /* Print Header Information */
        
        ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)
               chExcelApplication:ScreenUpdating = FALSE.
        
        /************ Sold To *********************/
        /* Sold To Customer  */
        chExcelApplication:Goto("R7C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = inv-head.cust-no.
        
        /* Sold To Customer Name */
        chExcelApplication:Goto("R8C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = inv-head.cust-name.
        /* Sold To Add1 */
        chExcelApplication:Goto("R9C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = inv-head.addr[1].
        /* Sold To Add2 */
        chExcelApplication:Goto("R10C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = inv-head.addr[2].
        /* City */
        chExcelApplication:Goto("R11C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-soldto-city.
        
        /* State */
        chExcelApplication:Goto("R11C11") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-soldto-State.

        /* Zip */
        chExcelApplication:Goto("R11C13") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-soldto-zip.

        
        /************ Ship To *********************/
        chExcelApplication:Goto("R7C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-id.
        
        chExcelApplication:Goto("R7C28") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-phone.
        
        /* Sold To Customer Name */
        chExcelApplication:Goto("R8C18") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-name.
        /* Sold To Add1 */
        chExcelApplication:Goto("R9C18") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-addr[1].
        /* Sold To Add2 */
        chExcelApplication:Goto("R10C18") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-addr[2].

        /* City */
        chExcelApplication:Goto("R11C18") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-city.
        /*State */        
        chExcelApplication:Goto("R11C28") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-state.

        chExcelApplication:Goto("R11C30") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-zip.
        
        /* Invoice# */
        chExcelApplication:Goto("R5C34") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = inv-head.inv-no.
        
        /* Date */
        chExcelApplication:Goto("R5C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-inv-date.

        /* Due Date */
        chExcelApplication:Goto("R5C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-due-date.

        /* Cust PO */
        chExcelApplication:Goto("R8C34") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-po-no.
        
        /* Terms */
        chExcelApplication:Goto("R8C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = xinv-head.terms-d.

        /* Our Order# */
        chExcelApplication:Goto("R8C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-ord-no.
 
        /* Ship Via */
        chExcelApplication:Goto("R11C34") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipvia.
        
        /* Freight Terms */
        chExcelApplication:Goto("R11C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-fob.
        
        /* SalesPerson */
        chExcelApplication:Goto("R11C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-salesman
               v-subtot-lines = 0
               v-t-tax = 0
               inrowcount = 14.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-no
        BY inv-line.i-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1.

          find first oe-ordl where oe-ordl.company = cocode and
                                   oe-ordl.ord-no = inv-line.ord-no and
                                   oe-ordl.i-no = inv-line.i-no
                                   no-lock no-error.

          assign v-inv-qty = inv-line.qty
                 v-ship-qty = inv-line.ship-qty
                 v-i-no = inv-line.i-no
                 v-i-dscr = inv-line.part-dscr1
                 v-price = inv-line.price * (1 - (inv-line.disc / 100))
                 v-t-price = inv-line.t-price
                 v-subtot-lines = v-subtot-lines + inv-line.t-price.

          IF AVAIL oe-ordl THEN DO:
            v-bo-qty = inv-line.ship-qty + oe-ordl.t-ship-qty.

            FIND FIRST oe-ord
                WHERE oe-ord.company EQ oe-ordl.company
                  AND oe-ord.ord-no  EQ oe-ordl.ord-no
                NO-LOCK NO-ERROR.

            v-bo-qty = IF v-bo-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))
                       THEN 0
                       ELSE (oe-ordl.qty - v-bo-qty).
          END.

          ELSE
            v-bo-qty = IF inv-line.qty - inv-line.ship-qty LT 0
                       THEN 0 ELSE (inv-line.qty - inv-line.ship-qty).

          FOR EACH oe-boll
              WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                             WHERE oe-bolh.b-no   EQ oe-boll.b-no
                               AND oe-bolh.posted EQ YES)
              NO-LOCK:

            IF oe-boll.p-c THEN v-bo-qty = 0.
          END. /* each oe-boll */
                                  
          FOR EACH oe-boll
              WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                             WHERE oe-bolh.b-no   EQ oe-boll.b-no
                               AND oe-bolh.posted EQ YES)
              NO-LOCK:
                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

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

          if inv-line.tax and avail stax then
          do i = 1 to 3:
            if stax.tax-code[i] ne "" then do:
              create w-tax.
              assign
                w-dsc      = stax.tax-dscr[i]
                w-tax      = round((if stax.accum-tax then v-t-price
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
            
          v-price-head = inv-line.pr-uom.
          
          ASSIGN inrowcount = inrowcount + 1.

          /* Line# */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C1".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = inv-line.line.

          /* Item Desc */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C2".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-i-dscr .

          /* Line Item PO# */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C19".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = inv-line.po-no .

          /* Customer Item */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C24".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = inv-line.part-no.

          /* Southpak Item */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C28".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-i-no.
          
          /* Order Qty */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C32".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-inv-qty.
          
          /* Ship Qty */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C35".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = inv-line.ship-qty.
          
          /* B/O */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C38".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-bo-qty.

          /* UM */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C41".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-price-head.

          /* Price */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C43".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = v-price .

          /* Ext Amount */
          ASSIGN v-cell = "R" + STRING(inrowcount) + "C46".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = inv-line.t-price .

          if inv-line.tax then
          do :
            /* Tax */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C50".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = "Y" .
          end.
          else do :
            /* Tax */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C50".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = "N" .
          end.
          
          v-printline = v-printline + 2.
          IF v-printline >= 50 THEN DO:
             PAGE.
             v-printline = 24.
          END.
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          IF v-printline >= 50 THEN DO:
               PAGE.
               v-printline = 24.
          END.

          if first(inv-misc.ord-no) then
          do:
            ASSIGN inrowcount = inrowcount + 2.

            /* Item Desc */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C2".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = "** Miscellaneous Items **"
                   inrowcount = inrowcount + 1
                   v-printline = v-printline + 2.
          end.

            /* Item Desc */
            ASSIGN inrowcount = inrowcount + 1
                   v-cell = "R" + STRING(inrowcount) + "C2".

            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = inv-misc.charge + "-" +  inv-misc.dscr

            /* Ext Amount */
                   v-cell = "R" + STRING(inrowcount) + "C46".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = inv-misc.amt
                   v-subtot-lines = v-subtot-lines + inv-misc.amt
                   v-printline = v-printline + 1.

            if inv-misc.tax and avail stax then
            do i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.accum-tax then v-t-price
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

        if v-prntinst then do:
          
       {custom/notesprt.i inv-head v-inst 4}
         DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:                
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                   
                 END.
                 
                 ASSIGN inrowcount = inrowcount + 1
                        v-cell = "R" + STRING(inrowcount) + "C23".

                 chExcelApplication:Goto(v-cell) NO-ERROR.
                 ASSIGN chExcelApplication:ActiveCell:Value = v-inst[i].

                 v-printline = v-printline + 1.
              END.
           END.
        end.

            IF v-printline >= 50 THEN
               v-printline = 24.

        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr <> "" and
           inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" AND stax.tax-frt[i] then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.accum-tax then v-frt-tax
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
    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
    
    RUN util/GetBottomRow.p (INPUT 14, INPUT 30, INPUT 6, INPUT inrowcount, OUTPUT inrowcount).
    ASSIGN v-cell = STRING(inrowcount) + ":328". 
    chExcelApplication:Rows(v-cell):SELECT.
    chExcelApplication:SELECTION:DELETE.

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-subtot-lines.

    ASSIGN inrowcount = inrowcount + 1
           tax-dscr-1 = ""
           tax-dscr-2 = ""
           tax-dscr-3 = "".

    IF AVAIL cust THEN
    DO:
       FIND FIRST stax WHERE
            stax.company EQ cocode AND
            stax.tax-group EQ cust.tax-gr
            NO-LOCK NO-ERROR.

       IF AVAIL stax THEN
       DO:
          ASSIGN tax-dscr-1 = stax.tax-dscr[1]
                 tax-dscr-2 = stax.tax-dscr[2]
                 tax-dscr-3 = stax.tax-dscr[3].
          RELEASE stax.
       END.
    END.

    v-cell = "R" + STRING(inrowcount) + "C35".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-1
           v-cell = "R" + STRING(inrowcount) + "C45".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[1].

    ASSIGN inrowcount = inrowcount + 1
           v-cell = "R" + STRING(inrowcount) + "C35".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-2
           v-cell = "R" + STRING(inrowcount) + "C45".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[2].

    ASSIGN inrowcount = inrowcount + 1
           v-cell = "R" + STRING(inrowcount) + "C35".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-3
           v-cell = "R" + STRING(inrowcount) + "C45".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[3].
            
    ASSIGN inrowcount = inrowcount + 1
           v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-inv-freight
           inrowcount = inrowcount + 1
           v-cell = "R" + STRING(inrowcount) + "C45".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = inv-head.t-inv-rev.

    chExcelApplication:Goto("R15C1") NO-ERROR.
    os-delete value(v-dir + STRING(inv-head.inv-no) + ".xls").     
    os-delete value(v-dir + "asi.pdf").
    os-delete value(v-dir + STRING(inv-head.inv-no) + ".pdf").
    IF LvOutputSelection = "PRINTER" THEN
     DO:
       IF LvFirstTimePrint = NO THEN
       DO :
         chExcelApplication:Dialogs(8):Show.
         chWorkbook:Close(no) no-error.
         ASSIGN LvFirstTimePrint = YES.
       END.
       ELSE DO :
         chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
         chWorkbook:Close(no) no-error. 
       END.
    END.

    IF LvOutputSelection = "Email" THEN
    DO:
      chExcelApplication:ActiveSheet:SaveAs(v-dir + STRING(inv-head.inv-no) + ".xls") no-error. 	   

      chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
   	  chWorkbook:Close(no) no-error.   
   	  pause 3.
      		
   	  OS-DELETE VALUE(v-dir + STRING(inv-head.inv-no) + ".xls").
       		
      OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + STRING(inv-head.inv-no) + ".pdf").
       		
      ASSIGN LvCtr = LvCtr + 1.
      CREATE tt-filelist.
      ASSIGN tt-FileCtr  = LvCtr
             tt-FileName = v-dir + STRING(inv-head.inv-no) + ".pdf".
    END.
    ELSE IF LvOutputSelection = "Screen" THEN
    DO:
      chExcelApplication:ActiveSheet:Protect("advance4me").
    END.
    
    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook NO-ERROR.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chHyper NO-ERROR.

end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
/* Clean up and extra processing */
/* Merge all the PDF Files */

ASSIGN CommandString = CurrDir + "\util\pdftk ".

os-delete VALUE(v-dir + "Invoice.pdf").

FOR EACH tt-filelist :
  assign CommandString = CommandString + " " + tt-FileName .
END.

assign CommandString = CommandString + " cat output " + v-dir + "Invoice.pdf".
os-command silent value(CommandString).
FOR EACH tt-filelist :
  os-delete value(tt-FileName).
END.
WshNetwork:SetDefaultPrinter(CurActivePrinter).
IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
DO:
  chExcelApplication:Quit() no-error.
END.

chExcelApplication:ScreenUpdating = TRUE.

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.
