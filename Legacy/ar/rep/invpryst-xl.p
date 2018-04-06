/* ---------------------------------------------- ar/rep/invxprnt.p   */
/* PRINT INVOICE   Xprint form for Pacific PKG             */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEF INPUT PARAM  ip-lines-per-page AS INT NO-UNDO.
DEF STREAM st-fax.
DEF BUFFER b-ar-invl FOR ar-invl.

{sys/inc/var.i shared}

{ar/rep/invoice.i}

def var v-salesman as char format "x(14)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-id as char no-undo.
def var v-shipto-phone as char no-undo.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-soldto-city as char format "x(15)" NO-UNDO.
def var v-soldto-state as char format "x(2)" NO-UNDO.
def var v-soldto-zip as char format "x(10)" NO-UNDO.

def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.

def var v-t-weight like ar-invl.t-weight NO-UNDO.
def var v-inv-no as int NO-UNDO.
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
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF BUFFER xar-inv FOR ar-inv.

def TEMP-TABLE w-sman
  field sman as char format "x(4)".

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
def var v-po-no like ar-invl.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-due-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF VAR v-cartot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax
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
DEF VAR vcTemplateFile AS CHAR NO-UNDO.


FIND FIRST ar-inv NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\prystup.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.

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
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

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

IF LvOutputSelection = "email" THEN
assign CurActivePrinter = SESSION:PRINTER-NAME
       AdobePrinter     = "PDFcamp Printer".

vcTemplateFile   = "Template\invoice-py.xlt".

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

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

IF NOT(VALID-HANDLE(chExcelApplication)) THEN
DO :
  MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

FILE-INFO:FILE-NAME = vcTemplateFile.

/* Set the Excel Template to be used. */
ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
if search (chFile) = ? then do:
   MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
           'cannot be found. Please verify that the file exists.'
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
   apply 'CLOSE':U to this-procedure.
end.

CurrDir = SUBSTRING (chFile, 1, INDEX (chFile, "Template\invoice-py.xlt") - 2)
          no-error.

chExcelApplication:VISIBLE = TRUE.

IF LvOutputSelection = "Email" or LvOutputSelection = "Printer" THEN
chExcelApplication:VISIBLE = FALSE.

for each tt-filelist :
  delete tt-filelist.
end.

/**************************** Excel Initilization End *********************************/

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
   
    find first company where company.company = cocode no-lock no-error.

     ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        .
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
           .
 END.
    

    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock,
        FIRST cust WHERE cust.company = ar-inv.company
                     AND cust.cust-no = ar-inv.cust-no NO-LOCK 

        break by ar-inv.cust-no
              by ar-inv.inv-no:
     
        IF ip-multi-faxout AND FIRST-OF(ar-inv.cust-no) THEN DO:
           OUTPUT CLOSE.
           OUTPUT STREAM st-fax CLOSE.
           OUTPUT TO value("c:\temp\fax\fx" + cust.cust-no + ".xpr") PAGE-SIZE value(ip-lines-per-page).
           OUTPUT STREAM st-fax TO value("c:\temp\fax\fx" + cust.cust-no + ".txt").
           PUT STREAM st-fax UNFORMATTED "FAX#:" trim(cust.fax-prefix) cust.fax SKIP.
           PUT CONTROL "<PRINT=NO>".       
           PUT UNFORMATTED "<EXPORT=c:\temp\fax\fx" trim(cust.cust-no) ".tif,BW>" .
           /* PUT "FAX#:" cust.fax SKIP.*/
        END.

        find first carrier where carrier.company eq cocode
             and carrier.carrier eq ar-inv.carrier no-lock no-error.
        if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
        else assign v-shipvia = "".
        
        assign
        v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
        v-soldto-city = ar-inv.city
        v-soldto-state = ar-inv.state
        v-soldto-zip = ar-inv.zip.

        find first shipto where shipto.company eq cocode
             and shipto.cust-no eq ar-inv.cust-no
             and shipto.ship-id eq ar-inv.ship-id no-lock no-error.

        IF AVAIL shipto THEN 
           assign  v-shipto-name = shipto.ship-name
           			   v-shipto-id   = shipto.ship-id
           			   v-shipto-phone = shipto.area-code + shipto.phone
                   v-shipto-addr[1] = shipto.ship-addr[1]
                   v-shipto-addr[2] = shipto.ship-addr[2]
                   v-shipto-city = shipto.ship-city
                   v-shipto-state = shipto.ship-state
                   v-shipto-zip = shipto.ship-zip
                   v-sold-addr3 = v-shipto-city + ", " + v-shipto-state + "  " + v-shipto-zip
           .

        if ar-inv.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        assign
          v-line = 1
          v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.
        if not avail stax then
        find first stax where stax.tax-group eq ar-inv.tax-code
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
       for each ar-invl NO-LOCK where ar-invl.x-no  eq ar-inv.x-no  
                     break by ar-invl.i-no:
         do i = 1 to 3:
          if ar-invl.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = ar-invl.sman[i].
          end.
         end.

         assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
                v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                            ar-invl.qty, 2) * ar-invl.inv-qty).

         ASSIGN
          v-tot-pallets = 0
          v-date-ship   = TODAY.
        
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = ar-invl.i-no AND
              oe-boll.ord-no = ar-invl.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0).
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                .

         END. /* each oe-bolh */

         if last-of(ar-invl.i-no) then do:
           if ar-invl.est-no ne "" then
           do:
             find first eb where eb.company = ar-invl.company and
               eb.est-no = ar-invl.est-no and
               eb.form-no = ar-invl.form-no and
               eb.blank-no = ar-invl.blank-no no-lock no-error.

             IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                                       AND fg-set.set-no = ar-invl.i-no:
                    ASSIGN v-set-qty = v-set-qty + fg-set.qtyPerSet.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                              eb.est-no = ar-invl.est-no AND
                              eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = ar-invl.company AND
                    fg-set.set-no = ar-invl.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.qtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.qtyPerSet / v-set-qty.
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
        end. /* each ar-invl */
    
                        /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        assign v-po-no = ar-inv.po-no
               v-bill-i = ar-inv.bill-i[1]
               v-ord-no = ar-inv.ord-no
               v-ord-date = ar-inv.ord-date
               v-due-date = ar-inv.due-date.

        find first ar-invl where ar-invl.x-no = ar-inv.x-no no-lock no-error.
        if avail ar-invl then
        do:
           assign v-price-head = ar-invl.pr-uom
                  /*v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no*/
                  v-ord-no = ar-invl.ord-no
                  lv-bol-no = ar-invl.bol-no.
        end.      

        IF v-po-no = "" THEN
        DO:
           find first oe-ord where
                oe-ord.company = cocode and
                oe-ord.ord-no = ar-invl.ord-no
                no-lock no-error.

           if avail oe-ord then
           DO:
              assign v-po-no = oe-ord.po-no.
              RELEASE oe-ord.
           END.
        END.

        /* display heder info 
         view frame invhead-comp.  /* Print headers */  */
        IF v-salesman = "" THEN v-salesman = cust.sman.
        v-inv-date = ar-inv.inv-date.
       
        ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)
               chExcelApplication:ScreenUpdating = FALSE.

        /************ Sold To *********************/
  /*      /* Sold To Customer Name */
        chExcelApplication:Goto("R7C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.cust-no. */
        
        /* Sold To Customer Name */
        chExcelApplication:Goto("R8C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.cust-name.
        /* Sold To Add1 */
        chExcelApplication:Goto("R9C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.addr[1].
        /* Sold To Add2 */
        chExcelApplication:Goto("R10C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.addr[2].
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
     /*   chExcelApplication:Goto("R7C1") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-id. */
        
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
        ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.inv-no.
        
        /* Date */
        chExcelApplication:Goto("R5C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-inv-date.

        /* Due Date */
        chExcelApplication:Goto("R5C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-due-date.

        /* Cust PO */
        chExcelApplication:Goto("R8C34") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = lv-bol-no /*v-po-no*/ .
        
        /* Terms */
        chExcelApplication:Goto("R8C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = SUBSTRING(ar-inv.terms-d,1,15).

        /* Our Order# */
        chExcelApplication:Goto("R8C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-salesman /*ar-inv.ord-no*/ .

        /* Ship Via */
        chExcelApplication:Goto("R11C34") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-shipvia.
        
        /* Freight Terms */
        chExcelApplication:Goto("R11C40") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-fob.
        
     /*   /* SalesPerson */
        chExcelApplication:Goto("R11C46") NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-salesman */
             ASSIGN inrowcount = 14
               v-subtot-lines = 0
               v-cartot-lines = 0
               v-t-tax = 0
                .
               
        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no
            BY ar-invl.i-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".
                 v-line = v-line + 1
                 .

            v-beeler-lines = 0.
            
            assign v-inv-qty = ar-invl.qty
                   v-ship-qty = ar-invl.ship-qty
                   v-i-no = ar-invl.i-no
                 /*  v-i-dscr = ar-invl.part-dscr1*/
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt
                   v-cartot-lines = v-cartot-lines + ar-invl.ship-qty .
            /*IF v-i-dscr = "" THEN
                    v-i-dscr = ar-invl.i-dscr .*/

            find first oe-ordl where oe-ordl.company = cocode and
                                   oe-ordl.ord-no = ar-invl.ord-no and
                                   oe-ordl.i-no = ar-invl.i-no
                                   no-lock no-error.

            IF AVAIL oe-ordl THEN DO:

                ASSIGN v-i-dscr = oe-ordl.i-name  .

              FIND FIRST oe-ord
                  WHERE oe-ord.company EQ oe-ordl.company
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                  NO-LOCK NO-ERROR.

              v-bo-qty = IF oe-ordl.t-ship-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))
                         THEN 0
                         ELSE (oe-ordl.qty - oe-ordl.t-ship-qty).
            END.

            ELSE
              ASSIGN
                  v-i-dscr = ar-invl.part-dscr1
                  v-bo-qty = IF ar-invl.qty - ar-invl.ship-qty LT 0
                                    THEN 0 ELSE (ar-invl.qty - ar-invl.ship-qty).

            IF v-i-dscr = "" THEN
                v-i-dscr = ar-invl.i-dscr .

            FOR EACH oe-boll
                WHERE oe-boll.company EQ ar-invl.company
                  AND oe-boll.ord-no  EQ ar-invl.ord-no
                  AND oe-boll.i-no    EQ ar-invl.i-no
                  AND oe-boll.po-no   EQ ar-invl.po-no
                  AND CAN-FIND(FIRST oe-bolh
                               WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                 AND oe-bolh.posted EQ YES)
                NO-LOCK:

              IF oe-boll.p-c THEN v-bo-qty = 0.
            END. /* each oe-boll */

                if ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.accum-tax then v-t-price
                                                                  else ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne ar-invl.amt then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.
            
            v-price-head = ar-invl.pr-uom.
            
            ASSIGN inrowcount = inrowcount + 1.

            /* Line# */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C1".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.line.


            /* Item Desc */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C2".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = v-i-dscr .


            /* Line Item PO# */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C19".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = v-i-no /*ar-invl.po-no*/ .


            /* Customer Item */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C26".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.ord-no /*ar-invl.part-no*/ .

            /* Southpak Item */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C30".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.lot-no /*v-i-no*/ .
          
            /* Order Qty */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C34".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.po-no /*v-inv-qty*/ .
          
        /*    /* Ship Qty */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C35".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.ship-qty.
          
            /* B/O */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C38".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = v-bo-qty.*/

            /* UM */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C41".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.inv-qty /*ar-invl.ship-qty*/ .


            /* Price */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C45".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = v-price . 

            /* Ext Amount */
            ASSIGN v-cell = "R" + STRING(inrowcount) + "C48".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-invl.amt .

       /*     if ar-invl.tax then
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
            end. */
                 
            v-printline = v-printline + 1.

      
            put skip(1).
            v-printline = v-printline + 1.

            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
            END.

        end. /* each ar-invl */

        /*if v-prntinst then do:
         do i = 1 to 4:
          if ar-inv.bill-i[i] ne "" then do:
            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
            END.

            ASSIGN inrowcount = inrowcount + 1.

            ASSIGN v-cell = "R" + STRING(inrowcount) + "C23".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = ar-inv.bill-i[i].            
            
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.   /* v-prntinst */ */

        v-frt-tax = ar-inv.freight.        
        IF ar-inv.tax-code <> "" and
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" AND stax.tax-frt[i] then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.accum-tax then v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end.

    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                      ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                         ELSE FILL(" ",5) ) +
                      fill(" ",6) + ":" +
                      string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.
    
    RUN util/GetBottomRow.p (INPUT 14, INPUT 30, INPUT 6, INPUT inrowcount, OUTPUT inrowcount).
    ASSIGN v-cell = STRING(inrowcount) + ":99". 
    chExcelApplication:Rows(v-cell):SELECT.
    chExcelApplication:SELECTION:DELETE.

    ASSIGN v-cell = "R" + STRING(inrowcount) + "C39".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-cartot-lines.

    ASSIGN inrowcount = inrowcount + 1 .

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
            stax.company EQ ar-inv.company AND
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

  /*  v-cell = "R" + STRING(inrowcount) + "C35".
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
            
    ASSIGN inrowcount = inrowcount + 1                                                  */
           v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = v-inv-freight.
    
    ASSIGN inrowcount = inrowcount + 1
           v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = (v-subtot-lines + v-inv-freight) .

    chExcelApplication:Goto("R15C1") NO-ERROR.
    os-delete value(v-dir + STRING(ar-inv.inv-no) + ".xls").     
    os-delete value(v-dir + "asi.pdf").
    os-delete value(v-dir + STRING(ar-inv.inv-no) + ".pdf").
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
      chExcelApplication:ActiveSheet:SaveAs(v-dir + STRING(ar-inv.inv-no) + ".xls") no-error. 	   
      NO-RETURN-VALUE chWorkbook:ExportAsFixedFormat(0, v-dir + "invoice.pdf").
/*       chExcelApplication:ActiveWindow:SelectedSheets:PrintOut(). */
   	  chWorkbook:Close(no) no-error.   
   	  pause 3.
      		
/*       OS-DELETE VALUE(v-dir + STRING(ar-inv.inv-no) + ".xls").                          */
/*                                                                                         */
/*       OS-RENAME value(v-dir + "asi.pdf") VALUE(v-dir + STRING(ar-inv.inv-no) + ".pdf"). */
       		
      ASSIGN LvCtr = LvCtr + 1.
      CREATE tt-filelist.
      ASSIGN tt-FileCtr  = LvCtr
          		tt-FileName = v-dir + STRING(ar-inv.inv-no) + ".pdf"
       		.
    END.
    ELSE IF LvOutputSelection = "Screen" THEN
    DO:
      chExcelApplication:ActiveSheet:Protect("advance4me").
    END.
    
    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook NO-ERROR.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chHyper NO-ERROR.



    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.

    END. /* DO TRANSACTION avail ar-inv */ 
 
end. /* each report, ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
/* Clean up and extra processing */
/* Merge all the PDF Files */

/* ASSIGN CommandString = CurrDir + "\util\pdftk ".                               */
/*                                                                                */
/* os-delete value(v-dir + "Invoice.pdf").                                        */
/*                                                                                */
/* FOR EACH tt-filelist :                                                         */
/*   assign CommandString = CommandString + " " + tt-FileName .                   */
/* END.                                                                           */
/*                                                                                */
/* assign CommandString = CommandString + " cat output " + v-dir + "Invoice.pdf". */
/* os-command silent value(CommandString).                                        */
/* FOR EACH tt-filelist :                                                         */
/*   os-delete value(tt-FileName).                                                */
/* END.                                                                           */


WshNetwork:SetDefaultPrinter(CurActivePrinter).
IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
DO:
  chExcelApplication:Quit() no-error.
END.

chExcelApplication:ScreenUpdating = TRUE.

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.
