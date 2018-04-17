/* ---------------------------------------------- oe/rep/bolxprt2.p 10/02 YSK */
/* PRINT Xprint BOL 2 like Dayton                                             */
/* -------------------------------------------------------------------------- */
/* Output selection for the report */
DEFINE SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-palls         as   int format "->,>>>,>>9".
def var v-tot-wt            as   dec format "->>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-pal-cnt           as   dec.
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-ord-date            like oe-ord.ord-date.
def var v-po-no             like oe-bolh.po-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-id    like shipto.ship-id.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".

def var v-shipcontact as char no-undo.
def var v-frt-acct-num like ar-ctrl.freight no-undo.

def var v-comp-id   like company.company.
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-city  like company.city.
def var v-comp-state like company.state.
def var v-comp-zip   like company.zip.
def var v-comp-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-custcontact as char no-undo.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-lines AS INT NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.

DEF VAR v-cases AS DEC NO-UNDO.
DEF VAR v-partial AS DEC NO-UNDO.
DEF VAR v-total-count AS DEC NO-UNDO.
DEF VAR v-weight AS DEC NO-UNDO.

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

DEF VAR lv-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-cusx-add1 AS cha NO-UNDO.
DEF VAR v-cusx-add2 AS cha NO-UNDO.
DEF VAR v-cusx-add3 AS cha NO-UNDO.
DEF VAR v-cusx-add4 AS cha NO-UNDO.
DEF VAR v-cusx-add5 AS cha NO-UNDO.
DEF VAR v-cusx-email AS cha NO-UNDO.
DEF VAR v-cusx-name AS cha NO-UNDO.

DEF VAR lv-bolfmt-int AS INT NO-UNDO.
def buffer b-itemfg     for itemfg.
DEF VAR iError AS INT NO-UNDO.

/* skb 1/2/07 - Variables for excel Automation  */
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
DEFINE VARIABLE LvFirstTimePrint AS LOGICAL INIT NO NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist NO-UNDO
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

DEFINE VARIABLE CurActivePrinter AS CHAR NO-UNDO.
DEFINE VARIABLE AdobePrinter     AS CHAR NO-UNDO.
define variable CommandString    AS CHAR NO-UNDO.
define variable WshNetwork as com-handle.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

for each tt-filelist :
  delete tt-filelist.
end.

IF LvOutputSelection = "email" THEN
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
  /* message "Email Option Selected, please make sure printer is changed to PDF Camp before clicking on OK" view-as alert-box information. */
END.
CREATE "Excel.Application" chExcelApplication NO-ERROR.

IF NOT(VALID-HANDLE(chExcelApplication)) THEN
DO :
  MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

RUN UTIL/CurrDir.p (output CurrDir).
ASSIGN chFile = CurrDir + "\Template\po-sp.xlt" no-error.
  
chExcelApplication:VISIBLE = TRUE.
IF LvOutputSelection = "Email" or LvOutputSelection = "Printer" THEN
chExcelApplication:VISIBLE = FALSE.

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

find first ar-ctrl no-lock where ar-ctrl.company = cocode no-error.
if available ar-ctrl then
assign v-frt-acct-num = ar-ctrl.freight
.

find first company where company.company = cocode no-lock no-error.
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
 
  IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-city = cust.city
            v-comp-state = cust.state
            v-comp-zip = cust.zip
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
            v-cusx-name = lv-comp-name
            .
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

{sa/sa-sls01.i}
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,
    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    NO-LOCK
    break by oe-bolh.bol-no :
      
    
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
        v-ship-id      = shipto.ship-id
        v-ship-name    = shipto.ship-name
        v-ship-addr[1] = shipto.ship-addr[1]
        v-ship-addr[2] = shipto.ship-addr[2]
        v-ship-addr3   = shipto.ship-city + ", " +
                         shipto.ship-state + "  " +
                         shipto.ship-zip
        v-ship-city = shipto.ship-city
        v-ship-state = shipto.ship-state
        v-ship-zip = shipto.ship-zip
        v-phone-num    = cust.area-code + cust.phone .
     
      if shipto.broker then 
      assign
        v-comp-add1 = cust.addr[1]
        v-comp-add2 = cust.addr[2]
        v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                         cust.zip
        v-comp-city = cust.city
        v-comp-state = cust.state
        v-comp-zip = cust.zip
        v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
        v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
        lv-email    = "Email:  " + cust.email   
        lv-comp-name = cust.NAME 
        v-custcontact  = shipto.contact.

      ELSE 
        ASSIGN 
          v-comp-add1 = v-cusx-add1
          v-comp-add2 = v-cusx-add2    
          v-comp-add3 = v-cusx-add3    
          v-comp-add4 = v-cusx-add4                
          v-comp-add5 = v-cusx-add5
          lv-email    = v-cusx-email
          lv-comp-name = v-cusx-name.
        
                        
    assign
       v-comp-id      = cust.cust-no
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
       v-ship-addr3   = ""
       v-ship-city = ""
       v-ship-state = ""
       v-ship-zip = ""
       .

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

      /* skb - 1/11/07 - Get Ship Contact */
      assign
        v-shipcontact = oe-ord.contact
        v-ord-date = oe-ord.ord-date.
      
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
      
      v-salesman = trim(v-salesman).
      v-po-no = oe-boll.po-no.
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      if shipto.broker then 
         assign
           v-comp-add1 = oe-ord.sold-addr[1]
           v-comp-add2 = oe-ord.sold-addr[2]
           v-comp-add3 = oe-ord.sold-city + ", " +
                         oe-ord.sold-state + "  " +
                         oe-ord.sold-zip
           v-comp-city = oe-ord.sold-city
           v-comp-state = oe-ord.sold-state
           v-comp-zip = oe-ord.sold-zip
           v-comp-add4 = ""
           v-comp-add5 = ""
           lv-email    = ""
           lv-comp-name = oe-ord.sold-name .

      LEAVE.
    end.
    /* end for each oe-boll */

    
  
  end. /* first-of(oe-bolh.bol-no) */

  /*do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.*/

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

     /*IF lv-bolfmt-int = 1 THEN
       for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break BY oe-boll.i-no BY oe-boll.ord-no BY oe-boll.line
                      BY oe-boll.po-no BY oe-boll.job-no BY oe-boll.job-no2:  
         IF FIRST-OF(oe-boll.i-no) THEN DO:
            FOR EACH w2.
                DELETE w2.
            END.
         END.
         if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
            find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
            if not avail w2 then create w2.
            ASSIGN w2.cas-cnt = oe-boll.qty-case
                   w2.cases   = w2.cases + oe-boll.cases.
         end.

         if oe-boll.partial ne 0 then do:
            find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
            if not avail w2 then create w2.
            ASSIGN w2.cas-cnt = oe-boll.partial
                   w2.cases   = w2.cases + 1.
         end.*/

         /*IF LAST-OF(oe-boll.i-no) THEN DO:
            i = 0.
            FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt:
                ln-cnt = ln-cnt + 2.          
                find first oe-ordl where oe-ordl.company eq cocode
                               and oe-ordl.ord-no  eq oe-boll.ord-no
                               and oe-ordl.i-no    eq oe-boll.i-no
                               and oe-ordl.line    eq oe-boll.LINE no-lock no-error.
                IF AVAIL oe-ordl THEN DO:
                  /* IF oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.*/
                 /*  IF oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.*/
                END.
                
                IF LAST-OF(w2.cases * w2.cas-cnt) THEN ln-cnt = ln-cnt + 1.
                /*MESSAGE oe-boll.i-no oe-boll.ord-no oe-boll.LINE w2.cases w2.cas-cnt ln-cnt skip
                    oe-ordl.part-dscr1 "," oe-ordl.part-dscr2 VIEW-AS ALERT-BOX.*/
            END.
         END.
     END. */ /* bol summary page calc*/
     /*ELSE
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break by report.key-01
                by report.key-02:                 
          ln-cnt = ln-cnt + 3.          
          find first oe-ordl where oe-ordl.company eq cocode
                               and oe-ordl.ord-no  eq oe-boll.ord-no
                               and oe-ordl.i-no    eq oe-boll.i-no
                               and oe-ordl.line    eq oe-boll.LINE no-lock no-error.
          IF AVAIL oe-ordl THEN DO:
          /*   IF oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.
             IF oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.*/
          END.
     END.*/

     /*FOR EACH w2.
         DELETE w2.
     END.*/
     /* end of dup loop */
      
    /*  end of getting total page per po */
     
     RUN UTIL/CurrDir.p (output CurrDir).
     ASSIGN chFile = CurrDir + "\Template\bol-sp.xlt" no-error.

     ASSIGN chWorkbook =chExcelApplication:Workbooks:Open(chfile)
            chExcelApplication:ScreenUpdating = FALSE
            inRowCount = 14
            LvLineCnt = 0.
     
     {oe/rep/bolsth22-xl.i}
     {oe/rep/bolsthp2-xl.i}
     
     os-delete value(v-dir + "asi.pdf").
     os-delete value(v-dir + string(oe-bolh.bol-no) + ".xls").     
     os-delete value(v-dir + string(oe-bolh.bol-no) + ".pdf").
     
     RUN util/GetBottomRow.p (INPUT 14, INPUT 30, INPUT 3, INPUT inrowcount, OUTPUT inrowcount).
     ASSIGN v-cell = STRING(inrowcount) + ":351". 
     chExcelApplication:Rows(v-cell):SELECT.
     chExcelApplication:SELECTION:DELETE.
     ASSIGN v-cell = "R15C1".
     chExcelApplication:Goto(v-cell) NO-ERROR.
     
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
     ELSE IF LvOutputSelection = "Email" THEN
     DO:
       WshNetwork:SetDefaultPrinter(AdobePrinter).
       chExcelApplication:ActiveSheet:SaveAs(v-dir + string(oe-bolh.bol-no) + ".xls")no-error. 	   
       chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
       chWorkbook:Close(no) no-error.   
       chExcelApplication:Quit() no-error.
       OS-DELETE VALUE(v-dir + string(oe-bolh.bol-no) + ".xls").

       		
       OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + string(oe-bolh.bol-no) + ".pdf").
       
       ASSIGN LvCtr = LvCtr + 1.
       CREATE tt-filelist.
       ASSIGN tt-FileCtr  = LvCtr
              tt-FileName = v-dir + string(oe-bolh.bol-no) + ".pdf".
     END.
     
     RELEASE OBJECT chWorkbook NO-ERROR.
     RELEASE OBJECT chWorkSheet NO-ERROR.
     RELEASE OBJECT chHyper NO-ERROR.
     
     IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.
      
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
       delete report.
     end.

  END.  /* last-of*/

  oe-bolh.printed = yes.

end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

/* Merge all the PDF Files */

ASSIGN CommandString = CurrDir + "\util\pdftk ".
os-delete value(v-dir + "bol.pdf").

FOR EACH tt-filelist :
  assign CommandString = CommandString + " " + tt-FileName .
END.
assign CommandString = CommandString + " cat output " + v-dir + "bol.pdf".

os-command silent value(CommandString).

WshNetwork:SetDefaultPrinter(CurActivePrinter).
IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
DO:
  chExcelApplication:Quit() no-error.
END.
ELSE DO :
chExcelApplication:ActiveSheet:Protect("advance4me").

END.

chExcelApplication:ScreenUpdating = TRUE.

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.

      

