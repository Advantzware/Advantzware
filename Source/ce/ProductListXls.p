/*----------------------------------------------- oe/rep/cocacpi.p */
/* Print Soule COC (Certificate of Compliance)                   */
/*----------------------------------------------------------------*/
DEFINE INPUT PARAMETER iprwRowidEb AS ROWID NO-UNDO.

{sys/inc/var.i shared}    

DEFINE VARIABLE viWorkSheetCount   AS INTEGER          NO-UNDO.   
DEFINE VARIABLE LvOutputSelection  AS CHARACTER        INIT "on-Screen" NO-UNDO.
DEFINE VARIABLE CurActivePrinter   AS CHARACTER        NO-UNDO.
DEFINE VARIABLE AdobePrinter       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE vcTemplateFile     AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE WshNetwork         AS COMPONENT-HANDLE.
DEFINE VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CurrDir            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE CommandString      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-dir              AS CHARACTER        FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE iRowCount          AS INTEGER          NO-UNDO.
DEFINE VARIABLE iQtyOnHand         AS INTEGER          NO-UNDO.

DEFINE TEMP-TABLE tt-temp-table LIKE eb
    FIELD board LIKE ef.board. 

DEFINE BUFFER bf-eb FOR eb.
FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    v-dir = users.user_program[2] + "\".
ELSE
    v-dir = "c:\tmp\".

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

PROCEDURE FillData:

    DEFINE VARIABLE iSheetQty AS INTEGER NO-UNDO.

    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ iprwRowidEb NO-ERROR .   
    
    IF NOT AVAIL eb THEN RETURN.
     
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ eb.company
        AND bf-eb.est-no EQ eb.est-no BY bf-eb.form-no:
    
        FIND FIRST ef NO-LOCK
            WHERE ef.company EQ bf-eb.company
            AND ef.est-no EQ bf-eb.est-no 
            AND ef.form-no EQ bf-eb.form-no NO-ERROR .
     
        CREATE tt-temp-table.
        BUFFER-COPY bf-eb TO tt-temp-table.
        ASSIGN         
            tt-temp-table.board = IF AVAILABLE ef THEN ef.board ELSE "" .         
    END.
     
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ eb.company 
        AND cust.cust-no EQ eb.cust-no NO-ERROR.
     
    ASSIGN
        viWorkSheetCount = 1. 
   
    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.   
    
    ASSIGN
        chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
        chWorkSheet:name = "Purchasing List" .
      
    ASSIGN
        chWorkSheet:Range("B2"):value = (IF AVAILABLE cust THEN cust.NAME ELSE eb.cust-no )
        chWorkSheet:Range("B3"):value = eb.part-no
        chWorkSheet:Range("B4"):value = TODAY
        chWorkSheet:Range("D3"):value = eb.Plate-no
        chWorkSheet:Range("D4"):value = eb.est-no
        .
    iRowCount = 8.
     
    FOR EACH tt-temp-table NO-LOCK
        BREAK BY tt-temp-table.board :
        iSheetQty = 0.
        FOR EACH probe WHERE probe.company EQ tt-temp-table.company AND 
            ASI.probe.est-no = tt-temp-table.est-no 
            AND probe.probe-date NE ? NO-LOCK :
            iSheetQty =  iSheetQty + probe.gsh-qty .
        END.
      
        IF FIRST-OF(tt-temp-table.board) AND tt-temp-table.board NE "" THEN
        DO:
      
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ tt-temp-table.company
                AND ITEM.i-no EQ tt-temp-table.board NO-ERROR .
                 
            ASSIGN 
                chWorkSheet:Range("A" + STRING(iRowCount)):value = ITEM.i-name .
            
            IF AVAILABLE ITEM THEN     
                chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(ITEM.s-len) + "x" + STRING(ITEM.s-wid).
            
            chWorkSheet:Range("C" + STRING(iRowCount)):value = "RM Item#" . 
            iQtyOnHand = 0.
            FOR EACH rm-bin FIELDS(qty )
                WHERE rm-bin.company EQ tt-temp-table.company
                AND rm-bin.i-no EQ tt-temp-table.board
                NO-LOCK:
                ASSIGN
                    iQtyOnHand = iQtyOnHand + rm-bin.qty.
            END.  
            chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(iQtyOnHand) . 
            chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(iSheetQty - iQtyOnHand) .
            
            iRowCount = iRowCount + 1.
        END. 
      
      
        FOR EACH estPacking NO-LOCK 
            WHERE estPacking.company = tt-temp-table.company 
            AND estPacking.estimateNo = tt-temp-table.est-no  
            AND estPacking.FormNo     = tt-temp-table.form-no 
            AND estPacking.BlankNo    = tt-temp-table.blank-No BREAK BY estPacking.rmItemID :                    
          
            IF FIRST-OF(estPacking.rmItemID) THEN
            DO:          
                FIND FIRST ITEM NO-LOCK
                    WHERE ITEM.company EQ tt-temp-table.company
                    AND ITEM.i-no EQ estPacking.rmItemID NO-ERROR .
                 
                ASSIGN 
                    chWorkSheet:Range("A" + STRING(iRowCount)):value = ITEM.i-name .
            
                IF AVAILABLE ITEM THEN     
                    chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(estPacking.dimLength) + "x" + STRING(estPacking.dimWidth).
            
                chWorkSheet:Range("C" + STRING(iRowCount)):value = "" . 
                iQtyOnHand = 0.
                FOR EACH rm-bin FIELDS(qty )
                    WHERE rm-bin.company EQ tt-temp-table.company
                    AND rm-bin.i-no EQ tt-temp-table.board
                    NO-LOCK:
                    ASSIGN
                        iQtyOnHand = iQtyOnHand + rm-bin.qty.
                END.  
            
                chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(iQtyOnHand) . 
                chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(estPacking.quantity) .
            
                iRowCount = iRowCount + 1.                
            END.          
        END.      
    END.
     
    /*chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$1:$N$48".*/
     
    chWorkbook:WorkSheets(1):Activate NO-ERROR.

END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

    /* Capture the current active printer. */
    IF LvOutputSelection = "email" THEN
        ASSIGN 
            CurActivePrinter = SESSION:PRINTER-NAME
            AdobePrinter     = "PDFcamp Printer".
  
    RUN sys/ref/getFileFullPathName.p ("Template\PurchasingList.xlt", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

    /* If Excel is running close it. */
    IF VALID-HANDLE (chExcelApplication) THEN
    DO:
        chExcelApplication:Quit()         NO-ERROR.
        RUN CleanUp.
    END.


    /* Network connection checks. */
    CREATE "WScript.Network" WshNetwork NO-ERROR.
    IF NOT(VALID-HANDLE(WshNetwork)) THEN
    DO :
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.    
  
    /* Start a new session of Excel. */
    /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    DO :
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
    /* Make Excel visible. */
    ASSIGN
        chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.      

END PROCEDURE.

PROCEDURE MainLoop:

    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    chWorkSheet      = chExcelApplication:Sheets:item(1).

    /*Fill in Data*/
    RUN FillData.

    /* enable screen updating */
    chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.

    /* Reset the Active Printer to the Original Printer. */
    IF CurActivePrinter <> '' THEN
        WshNetwork:SetDefaultPrinter(CurActivePrinter).

    
    /* Release created objects. */
    RELEASE OBJECT WshNetwork         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.


