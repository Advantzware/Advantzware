/*----------------------------------------------- oe/rep/cocacpi.p */
/* Print Unipak COC (Certificate of Compliance)                   */
/*----------------------------------------------------------------*/

{sys/inc/var.i shared}

{oe/rep/oe-lad.i}

def var v-bol-qty    like oe-boll.qty NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
DEF VAR viWorkSheetCount AS INT NO-UNDO.

DEFINE SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE VAR CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE VAR AdobePrinter         AS CHAR         NO-UNDO.
DEFINE VAR vcTemplateFile       AS CHAR    NO-UNDO.
DEFINE VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
define var WshNetwork           as com-handle.
DEFINE VARIABLE chFile          AS CHAR NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.
DEF VAR CommandString AS CHAR NO-UNDO.
DEF VAR v-rel-date AS DATE INIT 12/31/2999 NO-UNDO.
DEF VAR v-manuf-date AS DATE INIT 12/31/2999 NO-UNDO.
DEF VAR v-fg-rctd-po-no LIKE fg-rctd.po-no NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR mypict AS COM-HANDLE.
DEF VAR LvCtr as int no-undo.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE tt-filelist
       FIELD tt-FileCtr         AS INT
       FIELD tt-FileName        AS CHAR
       INDEX filelist           IS PRIMARY 
             TT-FILECTR.


FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

run InitializeExcel.
run MainLoop.
run Cleanup.


PROCEDURE FillData:

{sa/sa-sls01.i}

for each report  where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id no-lock:

  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.b-no    eq oe-bolh.b-no
      no-lock:

    create xreport.
    assign
     xreport.term-id = v-term-id
     xreport.key-01  = report.key-01
     xreport.key-02  = report.key-02
     xreport.key-03  = report.key-03
     xreport.key-04  = report.key-04
     xreport.key-06  = oe-boll.i-no
     xreport.rec-id  = recid(oe-boll).
  end.

  delete report.
end.

for each report where report.term-id eq v-term-id no-lock,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-06:

  if LAST-OF(report.key-06) then
  DO:
     viWorkSheetCount = viWorkSheetCount + 1.
     IF viWorkSheetCount GT 1 THEN
        chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:item(1)) NO-ERROR.
  END.
END.

ASSIGN
   viWorkSheetCount = 0
   FILE-INFO:FILE-NAME = "signature\" + USERID("nosweat") + ".jpg".

for each report where report.term-id eq v-term-id no-lock,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-06 DESC:

  /* accumulate bol quantity. */
  ASSIGN v-bol-qty = v-bol-qty + oe-boll.qty.

  /* If last of item */
  if LAST-OF(report.key-06) then do:
    
    viWorkSheetCount = viWorkSheetCount + 1.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate no-error.

    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no
          and oe-ordl.line    eq oe-boll.line
        no-lock no-error.

    ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item(viWorkSheetCount)
      chWorkSheet:name = STRING(oe-bolh.bol-no) + "-" +
                         report.key-06
      v-cust-addr3 = cust.city + ", " +
                     cust.state + "  " +
                     cust.zip. 

    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    FOR EACH fg-rctd WHERE
        fg-rctd.company EQ cocode AND
        fg-rctd.i-no EQ oe-ordl.i-no AND
        fg-rctd.job-no EQ oe-ordl.job-no AND
        fg-rctd.job-no2 EQ oe-ordl.job-no2
        NO-LOCK
        USE-INDEX i-no
        BY fg-rctd.rct-date:

        ASSIGN v-manuf-date = fg-rctd.rct-date
               v-fg-rctd-po-no = fg-rctd.po-no.
        LEAVE.
    END.

    IF v-manuf-date EQ 12/31/2999 THEN
       FOR EACH fg-rctd WHERE
           fg-rctd.company EQ cocode AND
           fg-rctd.i-no EQ oe-ordl.i-no AND
           fg-rctd.po-no EQ STRING(oe-ordl.po-no-po)
           NO-LOCK
           USE-INDEX i-no
           BY fg-rctd.rct-date:

           ASSIGN v-manuf-date = fg-rctd.rct-date
                  v-fg-rctd-po-no = fg-rctd.po-no.
           LEAVE.
       END.

    ASSIGN
/*       chWorkSheet:Range("C48"):value = IF v-manuf-date NE 12/31/2999 THEN  */
/*                                           STRING(v-manuf-date) ELSE ""     */
      chWorkSheet:Range("I6"):value = TODAY
      chWorkSheet:Range("C12"):value = cust.NAME
      chWorkSheet:Range("C13"):value = cust.addr[1]
      chWorkSheet:Range("C14"):value = cust.addr[2]
      chWorkSheet:Range("C15"):value = v-cust-addr3

        /* Customer PO Number */
      chWorkSheet:Range("C26"):VALUE = oe-boll.po-no
        /* Quantity Shipped */
      chWorkSheet:Range("C40"):VALUE = v-bol-qty.

      /* ACP Manufacturing Lot Number */
     IF oe-boll.job-no <> "" THEN
         chWorkSheet:Range("C38"):VALUE = oe-boll.job-no.
     ELSE
         chWorkSheet:Range("C38"):VALUE = v-fg-rctd-po-no.


    IF AVAIL oe-ordl THEN
    DO:
       ASSIGN
        /* Customer Part Number */
         chWorkSheet:Range("C28"):VALUE = oe-ordl.part-no
         /* ACP Order Number */
         chWorkSheet:Range("C36"):VALUE = oe-ordl.ord-no.

       FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no NO-LOCK NO-ERROR.

       /* Customer Part Description */
/*        IF AVAIL eb AND eb.part-dscr1 NE "" THEN           */
/*           chWorkSheet:Range("C30"):VALUE = eb.part-dscr1. */
/*        ELSE                                               */
          chWorkSheet:Range("C30"):VALUE = oe-ordl.i-name.
/*        IF AVAIL eb AND eb.part-dscr2 NE "" THEN           */
/*           chWorkSheet:Range("C31"):VALUE = eb.part-dscr2. */
/*        ELSE                                               */
          chWorkSheet:Range("C31"):VALUE = oe-ordl.part-dscr1.
/*        IF AVAIL eb AND eb.part-dscr3 NE "" THEN           */
/*           chWorkSheet:Range("C32"):VALUE = eb.part-dscr3. */
/*        ELSE                                               */
          chWorkSheet:Range("C32"):VALUE = oe-ordl.part-dscr2.
    
       /* Drawing Number */
       IF AVAIL eb AND eb.cad-no NE "" THEN
          chWorkSheet:Range("C34"):VALUE = eb.cad-no.
       ELSE
          chWorkSheet:Range("C34"):VALUE = itemfg.cad-no.

       for each oe-rel no-lock
           where oe-rel.company   eq oe-ordl.company
             and oe-rel.ord-no    eq oe-ordl.ord-no
             and oe-rel.i-no      eq oe-ordl.i-no
             and oe-rel.line      eq oe-ordl.line:
      
           RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

           IF index("A,B,P",v-type) > 0 THEN
           DO:
              IF oe-rel.rel-date LT v-rel-date THEN
                 v-rel-date = oe-rel.rel-date.
           END.
       END.

/*        IF v-rel-date NE 12/31/2999 THEN                  */
/*           chWorkSheet:Range("C50"):VALUE =  v-rel-date.  */
    END.

    /* Signature image */
    IF SEARCH(FILE-INFO:FULL-PATHNAME) NE ? THEN DO:
       mypict = chExcelApplication:Range("B45"):Parent:Pictures:Insert(FILE-INFO:FULL-PATHNAME).
       mypict:TOP = chExcelApplication:Range("B45"):TOP.
       mypict:LEFT = chExcelApplication:Range("B45"):LEFT.
       RELEASE OBJECT mypict.
    END.

    ASSIGN
       v-bol-qty = 0
       v-rel-date = 12/31/2999
       v-manuf-date = 12/31/2999
       chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$1:$I$48".
  end.
end. /* for each report */


chWorkbook:WorkSheets(1):Activate no-error.

OS-DELETE value(v-dir + "cofc.xls").     
OS-DELETE value(v-dir + "asi.pdf").
OS-DELETE value(v-dir + "cofc.pdf").

IF LvOutputSelection = "PRINTER" THEN
DO:
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
   chWorkbook:Close(no) no-error.
END.
ELSE IF LvOutputSelection = "Email" THEN
DO:
   /*WshNetwork:SetDefaultPrinter(AdobePrinter).*/
   chExcelApplication:ActiveSheet:SaveAs(v-dir + "cofc.xls") no-error. 	   
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,). 
   chWorkbook:Close(no) no-error.   
   chExcelApplication:Quit() no-error.
   pause 3.
   OS-DELETE VALUE(v-dir + "cofc.xls").
   OS-RENAME value(v-dir + "asi.pdf") value(v-dir + "cofc.pdf").
   LvCtr = LvCtr + 1.
   CREATE tt-filelist.
   ASSIGN tt-FileCtr  = LvCtr
          tt-FileName = v-dir + "cofc.pdf".
END.

END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

   /* Capture the current active printer. */
  IF LvOutputSelection = "email" THEN
    assign 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
  vcTemplateFile   = "template\acpi-cofc.xlt".

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication connect no-error.

  /* If Excel is running close it. */
  if valid-handle (chExcelApplication) then
  do:
    chExcelApplication:Quit()         no-error.
    run CleanUp.
  end.


  /* Network connection checks. */
  CREATE "WScript.Network" WshNetwork NO-ERROR.
  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Switch Printer to PDFCamp Printer. */
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter).

  /* Start a new session of Excel. */
  /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
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

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" or 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.
  
  /* Clear tt-FileList. */
  empty temp-table tt-filelist.

END PROCEDURE.

PROCEDURE MainLoop:

   /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(1):Activate no-error.
  chWorkSheet      = chExcelApplication:Sheets:item(1).

  /*Fill in Data*/
  run FillData.

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.

  /* Reset the Active Printer to the Original Printer. */
  if CurActivePrinter <> '' then
    WshNetwork:SetDefaultPrinter(CurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:Quit() no-error.
  
  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.


