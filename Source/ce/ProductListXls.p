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
DEFINE VARIABLE cPrintItem         AS CHARACTER        NO-UNDO.

DEFINE TEMP-TABLE tt-temp-table LIKE eb
    FIELD board LIKE ef.board
    FIELD rwRowid AS ROWID . 

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
    DEFINE VARIABLE dWastePer AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyPerMaterial AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLayerPad AS DECIMAL NO-UNDO.
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.

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
            tt-temp-table.board = IF AVAILABLE ef THEN ef.board ELSE "" 
            tt-temp-table.rwRowid = ROWID(eb).         
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
      
        IF  tt-temp-table.board NE "" THEN
        DO:
            IF LOOKUP(tt-temp-table.board,cPrintItem) EQ 0 THEN 
            do:
                FIND FIRST ITEM NO-LOCK
                    WHERE ITEM.company EQ tt-temp-table.company
                    AND ITEM.i-no EQ tt-temp-table.board NO-ERROR .
                          
                ASSIGN 
                    chWorkSheet:Range("A" + STRING(iRowCount)):value = ITEM.i-name .
                
                IF AVAILABLE ITEM THEN     
                    chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(ITEM.r-wid) .
                
               /*IF ITEM.mat-type EQ "B" THEN*/
               chWorkSheet:Range("C" + STRING(iRowCount)):value = ITEM.i-name . 
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
                
                RUN pGetWastePer( BUFFER tt-temp-table, OUTPUT dWastePer) .
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dWastePer) .
                iRowCount = iRowCount + 1.
                cPrintItem = cPrintItem + tt-temp-table.board + "," . 
            END.
            
            
            IF tt-temp-table.cas-no NE "" AND LOOKUP(tt-temp-table.cas-no,cPrintItem) EQ 0 THEN
            DO:
                 FIND FIRST ITEM NO-LOCK
                    WHERE ITEM.company EQ tt-temp-table.company
                    AND ITEM.i-no EQ tt-temp-table.cas-no NO-ERROR .
                 
                ASSIGN 
                    chWorkSheet:Range("A" + STRING(iRowCount)):value = ITEM.i-name .                                         
                  
                chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(tt-temp-table.cas-len) + "x" + STRING(tt-temp-table.cas-wid) + "x" + STRING(tt-temp-table.cas-dep).
                
                iQtyOnHand = 0.
                FOR EACH rm-bin FIELDS(qty )
                    WHERE rm-bin.company EQ tt-temp-table.company
                    AND rm-bin.i-no EQ estPacking.rmItemID
                    NO-LOCK:
                    ASSIGN
                        iQtyOnHand = iQtyOnHand + rm-bin.qty.
                END.  
                
                chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(iQtyOnHand) .    
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dWastePer) .           
               iRowCount = iRowCount + 1. 
               cPrintItem = cPrintItem + tt-temp-table.cas-no + "," .
            END.
            IF tt-temp-table.layer-pad NE "" AND LOOKUP(tt-temp-table.layer-pad,cPrintItem) EQ 0 THEN
            DO:
                 FIND FIRST ITEM NO-LOCK
                    WHERE ITEM.company EQ tt-temp-table.company
                    AND ITEM.i-no EQ tt-temp-table.layer-pad NO-ERROR .                 
                
                IF AVAILABLE ITEM THEN
                DO:
                    dLayerPad = 0.
                    RUN find-depth-reftable(tt-temp-table.rwRowid, OUTPUT rwRowid).
                    FIND reftable WHERE ROWID(reftable) EQ rwRowid NO-ERROR.
                    IF AVAIL reftable THEN
                    ASSIGN
                     dLayerPad  = decimal(reftable.val[1]).
                    ASSIGN 
                    chWorkSheet:Range("A" + STRING(iRowCount)):value = ITEM.i-name                     
                    chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(tt-temp-table.lp-len) + "x" + STRING(tt-temp-table.lp-wid) + "x" + string(dLayerPad).
                END.
                iQtyOnHand = 0.
                FOR EACH rm-bin FIELDS(qty )
                    WHERE rm-bin.company EQ tt-temp-table.company
                    AND rm-bin.i-no EQ estPacking.rmItemID
                    NO-LOCK:
                    ASSIGN
                        iQtyOnHand = iQtyOnHand + rm-bin.qty.
                END.  
                
                chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(iQtyOnHand) .                  
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dWastePer) .          
               iRowCount = iRowCount + 1. 
               cPrintItem = cPrintItem + tt-temp-table.layer-pad + "," .
            END.
        END.           
      
        FOR EACH estPacking NO-LOCK 
            WHERE estPacking.company = tt-temp-table.company 
            AND estPacking.estimateNo = tt-temp-table.est-no  
            AND estPacking.FormNo     = tt-temp-table.form-no 
            AND estPacking.BlankNo    = tt-temp-table.blank-No BREAK BY estPacking.rmItemID :                    
          
            IF FIRST-OF(estPacking.rmItemID) AND LOOKUP( estPacking.rmItemID,cPrintItem) EQ 0 THEN
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
                    AND rm-bin.i-no EQ estPacking.rmItemID
                    NO-LOCK:
                    ASSIGN
                        iQtyOnHand = iQtyOnHand + rm-bin.qty.
                END.  
                IF iQtyOnHand LT 0 THEN iQtyOnHand = 0 .
                chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(iQtyOnHand) . 
                
                 dQtyPerMaterial = 0.
                 FOR EACH estCostMaterial NO-LOCK 
                     WHERE estCostMaterial.estimateNo EQ tt-temp-table.est-no 
                     AND estCostMaterial.formNo EQ tt-temp-table.form-no
                     AND estCostMaterial.blankNo EQ tt-temp-table.blank-no
                     AND estCostMaterial.itemID EQ estPacking.rmItemID
                     BY estCostMaterial.formNo DESCENDING
                     BY estCostMaterial.blankNo
                     BY estCostMaterial.sequenceOfMaterial:
                     dQtyPerMaterial =  dQtyPerMaterial + estCostMaterial.quantityRequiredTotal .
                 END.
        
                chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(dQtyPerMaterial) .
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dWastePer) .
                            
                iRowCount = iRowCount + 1.
                cPrintItem = cPrintItem + estPacking.rmItemID + "," .
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

PROCEDURE pGetWastePer:
   DEFINE PARAMETER BUFFER ip-tt-temp-table FOR tt-temp-table.
   DEFINE OUTPUT PARAMETER opdWasterPer AS DECIMAL NO-UNDO.
        
    FOR EACH est-op WHERE est-op.company = ip-tt-temp-table.company 
      AND est-op.est-no = ip-tt-temp-table.est-no
      AND est-op.s-num = ip-tt-temp-table.form-no 
      AND (est-op.b-num = ip-tt-temp-table.blank-no OR est-op.b-num EQ 0)
      AND est-op.line < 500           
       NO-LOCK :
       opdWasterPer = opdWasterPer + est-op.op-spoil.
    END.
END PROCEDURE.

PROCEDURE find-depth-reftable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.

  DEFINE BUFFER b-eb FOR eb.
  DEFINE BUFFER b-rt FOR reftable.
  DEFINE BUFFER b-item FOR ITEM.

  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO TRANSACTION:
     FIND FIRST b-rt
          WHERE b-rt.reftable EQ "cedepth"
            AND b-rt.company  EQ b-eb.company
            AND b-rt.loc      EQ b-eb.est-no
            AND b-rt.code     EQ STRING(b-eb.form-no,"9999999999")
            AND b-rt.code2    EQ STRING(b-eb.blank-no,"9999999999")
          NO-LOCK NO-ERROR.
     IF NOT AVAIL b-rt THEN DO:
        CREATE b-rt.
        ASSIGN
           b-rt.reftable = "cedepth"
           b-rt.company  = b-eb.company
           b-rt.loc      = b-eb.est-no
           b-rt.code     = STRING(b-eb.form-no,"9999999999")
           b-rt.code2    = STRING(b-eb.blank-no,"9999999999").

        IF eb.layer-pad NE "" THEN
        DO:
           find FIRST b-item where
                b-item.company = b-eb.company and
                b-item.i-no = b-eb.layer-pad and
                b-item.mat-type = "5"
                no-lock no-error.

           IF AVAIL b-item THEN
           DO:
              ASSIGN
                 b-rt.val[1] = b-item.case-d
                 b-rt.val[2] = b-item.case-d.
              RELEASE b-item.
           END.
        END.
    END.

    op-rowid = ROWID(b-rt).
    RELEASE b-rt.
  END.
END PROCEDURE.

