/*----------------------------------------------- oe/rep/cocuni.p */
/* Print Prystup COC (Certificate of Compliance)                   */
/*----------------------------------------------------------------*/

{sys/inc/var.i shared}

DEFINE BUFFER bf-oe-boll FOR oe-boll .

{oe/rep/oe-lad.i}

DEFINE        VARIABLE iBolQty            LIKE oe-boll.qty NO-UNDO.
DEFINE        VARIABLE viWorkSheetCount   AS INTEGER          NO-UNDO.    
DEFINE SHARED VARIABLE LvOutputSelection  AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE CurActivePrinter   AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE AdobePrinter       AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE vcTemplateFile     AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE WshNetwork         AS COMPONENT-HANDLE.
DEFINE        VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE        VARIABLE CurrDir            AS CHARACTER        NO-UNDO.
DEFINE        VARIABLE CommandString      AS CHARACTER        NO-UNDO. 
DEFINE        VARIABLE iLvCtr             AS INTEGER          NO-UNDO.
DEFINE        VARIABLE cDirPath           AS CHARACTER        FORMAT "X(80)" NO-UNDO.
DEFINE        VARIABLE iTotalCases        LIKE oe-boll.cases NO-UNDO.   
DEFINE        VARIABLE cFob               AS CHARACTER        FORMAT "x(7)".
DEFINE        VARIABLE cTerms             AS CHARACTER        FORMAT "x(12)".
DEFINE        VARIABLE cName              AS CHARACTER        FORMAT "x(30)".
DEFINE        VARIABLE dPrice             AS DECIMAL          FORMAT ">>>>>>>>".
DEFINE        VARIABLE dTotPrice          AS DECIMAL          FORMAT ">>>>>>>>>>".
DEFINE        VARIABLE cDscr              AS CHARACTER        FORMAT "x(30)".

DEFINE        VARIABLE iOrdNo             AS INTEGER          FORMAT ">>>>>>>".
DEFINE        VARIABLE dPartQty           AS DECIMAL. 
DEFINE        VARIABLE v-qty-alf          AS CHARACTER        FORMAT "x(10)".
DEFINE        VARIABLE cBin               AS CHARACTER        FORMAT "x(22)" .
DEFINE        VARIABLE cases              AS INTEGER          NO-UNDO.
DEFINE        VARIABLE iQtyCases          AS INTEGER          NO-UNDO.
DEFINE        VARIABLE pallet             AS INTEGER          NO-UNDO.  
DEFINE        VARIABLE cShipName          LIKE shipto.ship-name.
DEFINE        VARIABLE cShipAddr          LIKE shipto.ship-addr. 
DEFINE        VARIABLE cShipAddr3         AS CHARACTER        FORMAT "x(30)".
DEFINE        VARIABLE cCustAddr3         AS CHARACTER        FORMAT "x(30)".

DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr  AS INTEGER
    FIELD tt-FileName AS CHARACTER
    INDEX filelist IS PRIMARY 
    TT-FILECTR.

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    cDirPath = users.user_program[2] + "\".
ELSE
    cDirPath = "c:\tmp\".

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

PROCEDURE FillData:

    {sa/sa-sls01.i}
    iTotalCases = 0.
    FOR EACH report EXCLUSIVE WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id :

        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ cocode
            AND oe-boll.b-no    EQ oe-bolh.b-no :
        
            RELEASE oe-rel.
       
            FIND FIRST oe-rell NO-LOCK
                WHERE oe-rell.company EQ cocode
                AND oe-rell.r-no    EQ oe-boll.r-no
                AND oe-rell.ord-no  EQ oe-boll.ord-no
                AND oe-rell.i-no    EQ oe-boll.i-no
                AND oe-rell.line    EQ oe-boll.line
                NO-ERROR.
            IF AVAILABLE oe-rell THEN 
            DO:
                FIND FIRST oe-relh OF oe-rell NO-LOCK.
                FIND FIRST oe-rel NO-LOCK
                    WHERE oe-rel.company EQ cocode
                    AND oe-rel.ord-no  EQ oe-relh.ord-no
                    AND oe-rel.line    EQ oe-rell.line
                    AND oe-rel.link-no EQ oe-rell.r-no
                    AND oe-rel.ship-no EQ oe-relh.ship-no
                    AND oe-rel.i-no    EQ oe-rell.i-no
                    NO-ERROR.
                IF NOT AVAILABLE oe-rel THEN
                    FIND FIRST oe-rel NO-LOCK
                        WHERE oe-rel.company  EQ cocode
                        AND oe-rel.ord-no   EQ oe-relh.ord-no
                        AND oe-rel.line     EQ oe-rell.line
                        AND oe-rel.rel-date EQ oe-relh.rel-date
                        AND oe-rel.ship-no  EQ oe-relh.ship-no
                        AND oe-rel.i-no     EQ oe-rell.i-no
                        NO-ERROR.
            END.
       
       
    
            CREATE xreport.
            ASSIGN
                xreport.term-id = v-term-id
                xreport.key-01  = report.key-01
                xreport.key-02  = report.key-02
                xreport.key-03  = report.key-03
                xreport.key-04  = /*report.key-04*/ STRING(oe-boll.ord-no )
                xreport.key-05  = /*IF AVAIL oe-rel THEN oe-rel.po-no ELSE "" */ oe-boll.po-no  /* ticket 16064 */ 
                xreport.key-06  = oe-boll.i-no
                xreport.key-07  = STRING(oe-boll.cases)
                xreport.key-08  = STRING(oe-boll.qty-case)
                xreport.rec-id  = RECID(oe-boll).
        END.

        DELETE report.
    END.

    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
        FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
        FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        BY report.key-04
        BY report.key-06:

        IF LAST-OF(report.key-06) THEN
        DO:
            viWorkSheetCount = viWorkSheetCount + 1.
            IF viWorkSheetCount GT 1 THEN
                chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:ITEM(1)) NO-ERROR.
        END.
    END.

    ASSIGN
        viWorkSheetCount    = 0
        FILE-INFO:FILE-NAME = "signature\" + USERID("nosweat") + ".jpg".

    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK,
        FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
        FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        BY report.key-04
        BY report.key-05
        BY report.key-06
        BY report.key-07
        BY report.key-08:          

        iBolQty = iBolQty + oe-boll.qty.
        iTotalCases = iTotalCases + oe-boll.cases.

        IF FIRST-OF(report.key-06) THEN 
        DO:
  
            viWorkSheetCount = viWorkSheetCount + 1.

            /* Go to the Active Sheet. */
            chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.
    
            ASSIGN
                chWorkSheet      = chExcelApplication:Sheets:ITEM(viWorkSheetCount)
                chWorkSheet:name = STRING(oe-bolh.bol-no) + "-" +
                         report.key-06 .

  
            FIND FIRST carrier NO-LOCK
                WHERE carrier.company EQ cocode
                AND carrier.carrier EQ oe-bolh.carrier
                NO-ERROR.

            RUN oe/custxship.p (oe-bolh.company,
                oe-bolh.cust-no,
                oe-bolh.ship-id,
                BUFFER shipto).

            ASSIGN
                cShipName    = shipto.ship-name
                cShipAddr[1] = shipto.ship-addr[1]
                cShipAddr[2] = shipto.ship-addr[2]
                cShipAddr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
                cCustAddr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.                       
                      
                      
            IF TRIM(cShipAddr3) EQ "," THEN cShipAddr3 = "".
            IF TRIM(cCustAddr3) EQ "," THEN cCustAddr3 = "".

            ASSIGN
                cFob      = ""
                cDscr     = ""
                cName     = ""
                cTerms    = ""
                dPrice    = 0
                dTotPrice = 0      
                cases     = 0   
                iQtyCases = 0
                pallet    = 0      
                .

            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ oe-boll.ord-no
                NO-ERROR.

            IF AVAILABLE oe-ord THEN 
            DO:
                IF NOT AVAILABLE carrier THEN
                    FIND FIRST carrier NO-LOCK
                        WHERE carrier.company EQ cocode
                        AND carrier.carrier EQ oe-ord.carrier
                        NO-ERROR.

                ASSIGN
                    cFob   = IF oe-ord.fob-code BEGINS "ORIG" THEN "ORIG"
                                                     ELSE "DEST"        
                    cTerms = oe-ord.terms     
                    iOrdNo = oe-ord.ord-no          .

            END.
    
            ASSIGN
                chWorkSheet:Range("AD3"):VALUE  = oe-bolh.bol-no      
                chWorkSheet:Range("A11"):VALUE  = cust.NAME
                chWorkSheet:Range("A12"):VALUE  = cust.addr[1]
                chWorkSheet:Range("A13"):VALUE  = cust.addr[2]
                chWorkSheet:Range("A14"):VALUE  = cCustAddr3 
                chWorkSheet:Range("R11"):VALUE  = cShipName
                chWorkSheet:Range("R12"):VALUE  = cShipAddr[1]
                chWorkSheet:Range("R13"):VALUE  = cShipAddr[2]
                chWorkSheet:Range("R14"):VALUE  = cShipAddr3                                                       
                chWorkSheet:Range("A18"):VALUE  = TODAY                                                       
                chWorkSheet:Range("E18"):VALUE  = oe-bolh.bol-date     
                chWorkSheet:Range("J18"):VALUE  = oe-boll.ord-no         
                chWorkSheet:Range("Q18"):VALUE  = oe-boll.po-no
                chWorkSheet:Range("Y18"):VALUE  = cTerms 
                chWorkSheet:Range("AH18"):VALUE = cFob
                chWorkSheet:Range("AL18"):VALUE = "Prystup"
                chWorkSheet:Range("AR18"):VALUE = oe-bolh.carrier    .         
   
        END.  
  
        iBolQty = iBolQty + oe-boll.qty. 
  
        ASSIGN
            cases     = oe-boll.cases  
            iQtyCases = oe-boll.qty-case 
            pallet    = pallet +  oe-boll.tot-pallets  .
     
     
        IF FIRST-OF(report.key-06) THEN 
        DO:
    
            ASSIGN 
                iBolQty = 0 .
            FOR EACH bf-oe-boll WHERE bf-oe-boll.bol-no EQ oe-boll.bol-no 
                AND bf-oe-boll.i-no EQ report.key-06 
                AND bf-oe-boll.po-no EQ string(report.key-05)
                AND bf-oe-boll.ord-no EQ INTEGER(report.key-04) NO-LOCK:
      
                iBolQty = iBolQty + bf-oe-boll.qty .      
            END.
            v-qty-alf = TRIM(STRING(iBolQty,">>>>>>>>>9")).
    
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ oe-boll.ord-no
                AND oe-ordl.i-no    EQ oe-boll.i-no
                AND oe-ordl.line    EQ oe-boll.line
                NO-ERROR.
            ASSIGN
                cName     = IF AVAILABLE oe-ordl AND oe-ordl.i-name NE "" THEN oe-ordl.i-name ELSE itemfg.i-name 
                cDscr     = IF AVAILABLE oe-ordl AND oe-ordl.part-dscr1 NE "" THEN oe-ordl.part-dscr1 ELSE itemfg.part-dscr1
                dPrice    = IF AVAILABLE oe-ordl  THEN oe-ordl.price ELSE 0
                dTotPrice = IF AVAILABLE oe-ordl  THEN oe-ordl.t-price ELSE 0
                iOrdNo    = IF AVAILABLE oe-ordl THEN oe-ordl.ord-no ELSE oe-boll.ord-no  . 
    
            ASSIGN
                chWorkSheet:Range("A24"):VALUE = STRING(v-qty-alf) 
                chWorkSheet:Range("J24"):VALUE = STRING(itemfg.i-no)
                chWorkSheet:Range("J25"):VALUE = STRING(cName)
                chWorkSheet:Range("J26"):VALUE = STRING(cDscr).        
                                 
            ASSIGN
                i    = 0
                j    = 0
                cBin = ""
                .

            FIND FIRST fg-set
                WHERE fg-set.company EQ cocode
                AND fg-set.set-no  EQ itemfg.i-no
                NO-ERROR.

            IF itemfg.isaset THEN 
            DO WHILE AVAILABLE fg-set:
                i = i + 1.
     
                IF i GT 8 THEN 
                DO:
       
                    ASSIGN
                        i    = 1
                        j    = 0
                        cBin = ""  .
                END.

                {sys/inc/part-qty.i dPartQty fg-set} 
               
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ fg-set.part-no
                    NO-ERROR.
           
                IF AVAILABLE fg-bin THEN
                    cBin = fg-bin.loc-bin.

          
                ASSIGN
                    chWorkSheet:Range("A" + string(27 + i)):VALUE = STRING(iBolQty * dPartQty) 
                    chWorkSheet:Range("J" + string(27 + i)):VALUE = STRING(fg-set.part-no)  + (IF cBin NE "" THEN ("  -  " + cBin) ELSE "")
                    .   
    
                FIND NEXT fg-set NO-LOCK
                    WHERE fg-set.company EQ cocode
                    AND fg-set.set-no  EQ itemfg.i-no
                    NO-ERROR.
            END.   
   
            iBolQty = 0.
        END.
  
        IF LAST-OF(report.key-08) THEN 
        DO:
            
            ASSIGN      
                chWorkSheet:Range("J" + string(27 + i + 2)):VALUE = STRING(pallet) +  " Pallet(s) @ " +  string(cases) + " cases  @" + STRING(iQtyCases) + "/case "
                .  
     
            ASSIGN 
                pallet    = 0 
                cases     = 0
                iQtyCases = 0
                .  
        END.
         
    END. /* for each report */       
      
   
END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

    /* Capture the current active printer. */
    IF LvOutputSelection = "email" THEN
        ASSIGN 
            CurActivePrinter = SESSION:PRINTER-NAME
            /*AdobePrinter     = "PDFcamp Printer"*/.
  
    RUN sys/ref/getFileFullPathName.p ("Template\PrystupXLT.xlt", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

   
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

    /* If Excel is running close it. */
    IF NOT VALID-HANDLE (chExcelApplication) THEN    
    /* Start a new session of Excel. */
    /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    DO :
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
    FILE-INFO:FILE-NAME = vcTemplateFile.

    /* Make Excel visible. */
    ASSIGN
        chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.
  
    /* Clear tt-FileList. */
    EMPTY TEMP-TABLE tt-filelist.

END PROCEDURE.

PROCEDURE MainLoop:

    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:OPEN(chfile)  NO-ERROR.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    chWorkSheet      = chExcelApplication:Sheets:ITEM(1).

    /*Fill in Data*/
    RUN FillData.
    RUN OutputFiles.
    /* enable screen updating */
    chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE OutputFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iFileCtr AS INTEGER NO-UNDO. 

chWorkbook:WorkSheets(1):Activate NO-ERROR.

OS-DELETE VALUE(cDirPath + "cofc.xls").     
OS-DELETE VALUE(cDirPath + "asi.pdf").
OS-DELETE VALUE(cDirPath + "cofc.pdf").

IF LvOutputSelection = "PRINTER" THEN DO:
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,FALSE,).
    chWorkbook:CLOSE(NO) NO-ERROR.
END.
ELSE IF LvOutputSelection = "Email" THEN DO:
/*     gchWshNetwork:SetDefaultPrinter(gcAdobePrinter). */
    chExcelApplication:ActiveSheet:SaveAs(cDirPath + "cofc.xls") /*NO-ERROR*/.

    NO-RETURN-VALUE chWorkbook:ExportAsFixedFormat(0, cDirPath + "cofc.pdf").
/*     NO-RETURN-VALUE gchWorkbook:PrintOut(,,,,,TRUE,,cDirPath + "asi.pdf"). */
 
    chWorkbook:CLOSE(NO) NO-ERROR.
/*     gchExcelApplication:QUIT() NO-ERROR. */
    PAUSE 3.
    OS-DELETE VALUE(cDirPath + "cofc.xls").
    OS-RENAME VALUE(cDirPath + "asi.pdf") VALUE(cDirPath + "cofc.pdf").
    iFileCtr = iFileCtr + 1.
    CREATE tt-filelist.
    ASSIGN 
        tt-FileCtr  = iFileCtr
        tt-FileName = cDirPath + "cofc.pdf".
END.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.

    /* Reset the Active Printer to the Original Printer. */
    IF CurActivePrinter NE '' THEN
        WshNetwork:SetDefaultPrinter(CurActivePrinter).

    /* For E-mail and Printer jobs, close Excel. */
    IF LvOutputSelection = "PRINTER" OR 
        LvOutputSelection = "EMAIL" THEN
        chExcelApplication:QUIT() NO-ERROR.
  
    /* Release created objects. */
    RELEASE OBJECT WshNetwork         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.


