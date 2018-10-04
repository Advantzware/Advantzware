/* ------------------------------------------- cec/quote/quoknight-xl.p        */
/* -------------------------------------------------------------------------- */

DEFINE SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.

{sys/inc/var.i shared}
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>>9.9<<<<" NO-UNDO.
DEF VAR lv-part-dscr2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-i-coldscr AS cha NO-UNDO.
DEF VAR ll-prt-dscr2 AS LOG NO-UNDO.
def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{est/printquo.i}
def var numfit as int no-undo.
def var sold as ch extent 5 FORM "x(30)" no-undo.
def var bill as ch extent 5 FORM "x(30)" no-undo.
def var ship as ch extent 5 FORM "x(30)" no-undo.
def var tot as de no-undo.
def var v-over-under as char no-undo.
def var v-comp-name like company.name extent 4.
def var trim-size like quoteitm.size no-undo.
def var temp-trim-size like quoteitm.size no-undo.
def var cc as int no-undo.
def var v-first-q-no like quotehd.q-no no-undo.
def var v-line like quoteitm.line no-undo.
def var v-rels as int NO-UNDO.
DEF VAR lv-yes-printed AS LOG NO-UNDO.
def var v-part like quoteitm.part-no no-undo.
def var v-board as char no-undo.
def var v-last as log initial no no-undo.
DEF VAR v-first AS LOG INITIAL YES NO-UNDO.
DEF VAR lv-fg# AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 20 NO-UNDO.
	

DEFINE VARIABLE chrX           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE intPageNum     AS INTEGER    NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 20 NO-UNDO.    
{custom/notesdef.i}
DEF VAR idx AS INT NO-UNDO.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR style-dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-est-no AS cha NO-UNDO.
DEF VAR lv-two-box AS LOG NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-uom LIKE xqqty.uom NO-UNDO.
DEF VAR lv-chg-amt LIKE quotechg.amt NO-UNDO.

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

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist
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

/* Capture the current active printer */
CREATE "WScript.Network" WshNetwork.

CREATE "Excel.Application" chExcelApplication .

assign CurActivePrinter = SESSION:PRINTER-NAME
       AdobePrinter     = "PDFcamp Printer"
      /* Disable screen updating so it will go faster */
       chExcelApplication:ScreenUpdating = False.

find first company where company.company = cocode no-lock no-error.

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.

FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

find first est where est.company = xquo.company
                   AND est.est-no eq xquo.est-no no-lock no-error.
find first sman
      where sman.company eq cocode
        and sman.sman    eq xquo.sman
      no-lock no-error.
find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq xquo.carrier
      no-lock no-error.
find first terms
      where terms.company eq cocode
        and terms.t-code  eq xquo.terms
      no-lock no-error.
find first cust
      where cust.company eq xquo.company
        and cust.cust-no eq xquo.cust-no
      no-lock no-error.

if avail cust then
    v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                   trim(string(cust.under-pct,">>9.9<%")).
assign
   sold[5] = trim(string(xquo.sold-no))
   ship[5] = trim(string(xquo.ship-id))
   bill[5] = trim(string(xquo.cust-no))
   v-first-q-no = xquo.q-no
   v-line-total = 0.

do i = 1 to 4:
   assign
    sold[i] = xquo.soldto[i]
    ship[i] = xquo.shipto[i]
    bill[i] = xquo.billto[i].
end.

if (xquo.shipto[1] eq xquo.soldto[1] and
    xquo.shipto[2] eq xquo.soldto[2] and
    xquo.shipto[3] eq xquo.soldto[3] and
    xquo.shipto[4] eq xquo.soldto[4]) then
    assign
     ship[1] = ""
     ship[2] = ""
     ship[3] = ""
     ship[4] = ""
     ship[5] = "SAME".


RUN UTIL/CurrDir.p (output CurrDir).

chFile = CurrDir + "\Template\quoteknight.xlt" NO-ERROR.
   
chExcelApplication:VISIBLE = TRUE.

if not ch-multi then do:
     
   ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)
          inRowCount = 18.

   {cec/quote/quoknight2-xl.i}    
   {cec/quote/quoknight-xl.i 1}
   
   inrowcount = inrowcount + 4.
   
   os-delete value(v-dir + "quote.xls").     
   os-delete value(v-dir + "asi.pdf").
   os-delete value(v-dir + "quote.pdf").
   	 
   IF LvOutputSelection = "PRINTER" THEN
   DO:
      NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
      chWorkbook:Close(no) no-error.
   END.
   ELSE IF LvOutputSelection = "Email" THEN
   DO:
      WshNetwork:SetDefaultPrinter(AdobePrinter). 
      chExcelApplication:ActiveSheet:SaveAs(v-dir + "quote.xls") no-error. 	   
      NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,). 
      chWorkbook:Close(no) no-error.   
      chExcelApplication:Quit() no-error.
       
      pause 3.
      OS-DELETE VALUE(v-dir + "quote.xls").
      OS-RENAME VALUE(v-dir + "asi.pdf") value(v-dir + "quote.pdf").
      ASSIGN LvCtr = LvCtr + 1.
      CREATE tt-filelist.
      ASSIGN tt-FileCtr  = LvCtr
      	      tt-FileName = v-dir + "quote.pdf".
   END.
   
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorkSheet NO-ERROR.
   RELEASE OBJECT chHyper NO-ERROR.

   v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

   release est.
end. /* not ch-multi */

else do: /* ch-multi is yes*/

    for each report where report.term-id eq v-term-id,
        first xquo where recid(xquo) eq report.rec-id
        no-lock
      break by report.key-01
            by report.key-02
            by report.key-03
      TRANSACTION:

      find first est
          where est.company eq xquo.company
            AND est.est-no  EQ xquo.est-no
          no-lock no-error.
      find first sman
          where sman.company eq cocode
            and sman.sman    eq xquo.sman
          no-lock no-error.
      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq xquo.carrier
          no-lock no-error.
      find first terms
          where terms.company eq cocode
            and terms.t-code  eq xquo.terms
          no-lock no-error.
      find first cust
          where cust.company eq xquo.company
            and cust.cust-no eq xquo.cust-no
          no-lock no-error.

      if avail cust then
         v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                        trim(string(cust.under-pct,">>9.9<%")).

      assign
       sold[5] = trim(string(xquo.sold-no))
       ship[5] = trim(string(xquo.ship-id))
       bill[5] = trim(string(xquo.cust-no)).

      do i = 1 to 4:
        assign
         sold[i] = xquo.soldto[i]
         ship[i] = xquo.shipto[i]
         bill[i] = xquo.billto[i].
      end.

      if first-of(report.key-01) THEN
         v-first-q-no = xquo.q-no.
    
      IF v-first THEN
         ASSIGN
            chWorkbook =chExcelApplication:Workbooks:Open(chfile)
            inRowCount = 18
            v-first = NO.

       {cec/quote/quoknight2-xl.i}
      
      ASSIGN
        v-last = last-of(report.key-01)
        v-line-total = 0.
  
      {cec/quote/quoknight-xl.i 2}  
      v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
      
      IF LAST-OF(report.key-01) THEN DO:

        ASSIGN
           inrowcount = inrowcount + 4
           v-cell = "R" + string(inrowcount) + "C2".

        chExcelApplication:Goto("R19C1") NO-ERROR.
        os-delete value(v-dir + "quote.xls").     
     	os-delete value(v-dir + "asi.pdf").
     	os-delete value(v-dir + "quote.pdf").
	 
     	IF LvOutputSelection = "PRINTER" THEN
     	DO:
           NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
           chWorkbook:Close(no) no-error.
     	END.
     	ELSE IF LvOutputSelection = "Email" THEN
     	DO:
           WshNetwork:SetDefaultPrinter(AdobePrinter). 
       	   chExcelApplication:ActiveSheet:SaveAs(v-dir + "quote.xls") no-error. 	   
       	   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
       	   chWorkbook:Close(no) no-error.   
       	   chExcelApplication:Quit() no-error.
       	   
       	   pause 3.
       	   
       	   OS-DELETE VALUE(v-dir + "quote.xls").
       	   
       	   OS-RENAME value(v-dir + "asi.pdf") value(v-dir + "quote.pdf").
       	   
       	   CREATE tt-filelist.
       	   ASSIGN LvCtr = LvCtr + 1
                  tt-FileCtr  = LvCtr
                  tt-FileName = v-dir + "quote.pdf".
     	END.
        
        RELEASE OBJECT chWorkbook NO-ERROR.
     	RELEASE OBJECT chWorkSheet NO-ERROR.
     	RELEASE OBJECT chHyper NO-ERROR.
      END.
    end.
end.

/* Merge all the PDF Files */
ASSIGN CommandString = CurrDir + "\util\pdftk ".
os-delete value(v-dir + "quote.pdf").

FOR EACH tt-filelist :
   assign CommandString = CommandString + " " + tt-FileName.
END.

assign CommandString = CommandString + " cat output " + v-dir + "quote.pdf".

os-command silent value(CommandString).

WshNetwork:SetDefaultPrinter(CurActivePrinter).

IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
   chExcelApplication:Quit() no-error.

chExcelApplication:ScreenUpdating = TRUE.

chExcelApplication:Goto("R1C1") NO-ERROR.

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
