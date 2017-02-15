/* ---------------------------------------------- est/specprem */
/* PRINT Premier spec sheet                                                     */
/* -------------------------------------------------------------------------- */
/*                        */

/*shared variables*/
{sys/inc/var.i shared}
DEFINE SHARED VARIABLE v-term-id AS CHAR NO-UNDO.

/*Buffers*/
DEFINE BUFFER gbf-report FOR report.

DEFINE VARIABLE ghExcel AS COM-HANDLE.
DEFINE VARIABLE ghExcelWorkbook AS COM-HANDLE.
DEFINE VARIABLE ghExcelWorksheet AS COM-HANDLE.

DEFINE VARIABLE gcOutputFile AS CHAR NO-UNDO.
DEFINE VARIABLE gcOutputFileName AS CHAR INIT "SpecPrem" NO-UNDO.
DEFINE VARIABLE gcTemplateFile AS CHAR INIT "SpecPremTemplate.xltx" NO-UNDO.
DEFINE VARIABLE giMaxMach AS INT INIT 5 NO-UNDO.
DEFINE VARIABLE giMaxNotes AS INT INIT 6 NO-UNDO.
DEFINE VARIABLE giMaxQtys AS INT INIT 5 NO-UNDO.
DEFINE VARIABLE gcNoteCodes AS CHAR INIT "SN,BN" NO-UNDO.

DEFINE VARIABLE gxEstNo LIKE eb.est-no NO-UNDO.
DEFINE VARIABLE gxFormNo LIKE eb.form-no NO-UNDO.
DEFINE VARIABLE gxBlankNo LIKE eb.blank-no NO-UNDO.
DEFINE VARIABLE gxOrdNo LIKE eb.ord-no NO-UNDO.
DEFINE VARIABLE gxEstLoc LIKE est.loc NO-UNDO.
DEFINE VARIABLE gxCustNo LIKE eb.cust-no NO-UNDO.
DEFINE VARIABLE gxShipId LIKE eb.ship-id NO-UNDO.
DEFINE VARIABLE gxINo LIKE eb.stock-no NO-UNDO.
DEFINE VARIABLE gxPartNo LIKE eb.part-no NO-UNDO.
DEFINE VARIABLE gxMCode LIKE est-op.m-code NO-UNDO EXTENT 10.
DEFINE VARIABLE gxMDscr LIKE est-op.m-dscr NO-UNDO EXTENT 10.
DEFINE VARIABLE glCuttingDie AS LOG NO-UNDO.
DEFINE VARIABLE glPrintPlate AS LOG NO-UNDO.
DEFINE VARIABLE gxNOutDC LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE gxNOutPR LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE gxVend LIKE e-itemfg-vend.vend-no NO-UNDO.
DEFINE VARIABLE gxVendItem LIKE e-itemfg-vend.vend-item NO-UNDO.
DEFINE VARIABLE gxPurMan LIKE eb.pur-man NO-UNDO.
DEFINE VARIABLE gxNote  LIKE notes.note_text NO-UNDO EXTENT 10.
DEFINE VARIABLE gxQty LIKE quoteqty.qty NO-UNDO EXTENT 10.

DEFINE VARIABLE giCount AS INTEGER NO-UNDO.


FOR EACH gbf-report WHERE gbf-report.term-id EQ v-term-id:
    /*initialize variables*/

    /*Find records required for report and fill variables*/
    FIND FIRST eb 
        WHERE recid(eb) EQ gbf-report.REC-ID
        NO-LOCK NO-ERROR.
    IF AVAIL eb THEN DO:
        ASSIGN
            gxEstNo = eb.est-no
            gxFormNo = eb.form-no
            gxBlankNo = eb.blank-no
            gxOrdNo = eb.ord-no
            gxCustNo = eb.cust-no
            gxShipId = eb.ship-id
            gxINo = eb.stock-no
            gxPartNo = eb.part-no
            gxPurMan = eb.pur-man.
        FIND FIRST est 
            WHERE est.company EQ cocode 
              AND est.est-no  EQ gxEstNo
            NO-LOCK NO-ERROR.
        FIND FIRST ef
            WHERE ef.company EQ cocode
              AND ef.est-no  EQ gxEstNo
              AND ef.form-no EQ gxFormNo
        NO-LOCK NO-ERROR.
    END.
    IF NOT AVAIL eb OR NOT AVAIL est OR NOT AVAIL ef THEN
      NEXT.
/*     RUN calc-values (INPUT ROWID(est), ROWID(eb)). */
    ASSIGN gxEstLoc = est.loc.
    IF gxOrdNo GT 0 THEN DO:
        FIND FIRST oe-ord 
            WHERE oe-ord.company EQ cocode
              AND oe-ord.ord-no  EQ gxOrdNo
            NO-LOCK NO-ERROR.
    END.
    FIND FIRST probe
        WHERE probe.company EQ cocode
          AND probe.est-no EQ gxEstNo
          AND probe.gsa-war GT 0
        NO-LOCK NO-ERROR.
    FIND LAST quotehd
        WHERE quotehd.company EQ cocode
          AND quotehd.loc EQ gxEstLoc
          AND quotehd.est-no EQ gxEstNo
        NO-LOCK NO-ERROR.
    giCount = 0.
    FOR EACH quoteitm OF quotehd
        WHERE quoteitm.part-no  EQ gxPartNo
        NO-LOCK,
        EACH quoteqty OF quoteitm
        NO-LOCK:
        giCount = giCount + 1.
        IF giCount LE giMaxQtys THEN
            gxQty[giCount] = quoteqty.qty.
    END.        
    FIND FIRST cust 
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ gxCustNo
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
        FIND FIRST shipto 
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ gxCustNo
            AND shipto.ship-id EQ gxShipId
        NO-LOCK NO-ERROR.
    FIND FIRST est-qty 
        WHERE est-qty.company EQ cocode
          AND est-qty.est-no  EQ gxEstNo
        NO-LOCK NO-ERROR.
    ASSIGN 
        giCount = 0
        glCuttingDie = NO
        glPrintPlate = NO
        gxMCode = ""
        gxMDscr = "".
    FOR EACH est-op 
        WHERE est-op.company EQ cocode
          AND est-op.est-no  EQ gxEstNo
          AND est-op.s-num   EQ gxFormNo
          AND (est-op.b-num   EQ gxBlankNo OR est-op.b-num EQ 0)
          AND est-op.qty    EQ est-qty.eqty
          AND est-op.LINE    LT 500
        NO-LOCK
        BY est-op.line:
        giCount = giCount + 1.
        FIND FIRST mach 
            WHERE mach.company EQ cocode
              AND mach.m-code EQ est-op.m-code
            NO-LOCK NO-ERROR.
        IF AVAIL mach THEN DO:
            IF NOT glCuttingDie THEN DO: 
                glCuttingDie = (mach.dept[1] EQ "DC" OR
                                mach.dept[2] EQ "DC" OR
                                mach.dept[3] EQ "DC" OR 
                                mach.dept[4] EQ "DC").
                IF glCuttingDie THEN
                    gxNOutDC = est-op.n-out.
            END.
            IF NOT glPrintPlate THEN DO:
                glPrintPlate = (mach.dept[1] EQ "PR" OR
                                mach.dept[2] EQ "PR" OR
                                mach.dept[3] EQ "PR" OR 
                                mach.dept[4] EQ "PR").
                IF glPrintPlate THEN
                    gxNOutPR = est-op.n-out.
            END.
        END.
        IF giCount LE giMaxMach THEN
            ASSIGN 
                gxMCode[giCount] = est-op.m-code
                gxMDscr[giCount] = est-op.m-dscr.
    END.
    giCount = 0.
    FOR EACH notes
        WHERE notes.rec_key EQ est.rec_key
/*           AND lookup(notes.note_code,gcNoteCodes) GT 0 */
        NO-LOCK:
        giCount = giCount + 1.
        IF giCount LE giMaxNotes THEN
            gxNote[giCount] = notes.note_text.
    END.
    
    IF gxPurMan THEN DO:
        giCount = 0.
        FOR EACH e-itemfg-vend 
            WHERE e-itemfg-vend.company EQ cocode
              AND e-itemfg-vend.est-no EQ gxEstNo
              AND e-itemfg-vend.eqty EQ eb.eqty
              AND e-itemfg-vend.form-no EQ gxFormNo
              AND e-itemfg-vend.blank-no EQ gxBlankNo 
              AND e-itemfg-vend.vend-no NE ""
            NO-LOCK:
            giCount = giCount + 1.
            IF giCount EQ 1 THEN
                ASSIGN 
                    gxVend = e-itemfg-vend.vend-no
                    gxVendItem = e-itemfg-vend.vend-item.
            ELSE DO:
                ASSIGN 
                    gxVend = "Multiple"
                    gxVendItem = "Multiple".
                LEAVE.
            END.
    
        END.
    END.
        
      
    

    /*output report with buffers and variables*/

    /*initialize Excel*/
    RUN CreateExcelSheet.

    /*write data to Excel "Data" sheet*/
    RUN BuildExcelDataSheet(1,2, gxCustNo).
    RUN BuildExcelDataSheet(2,2, cust.NAME).
    RUN BuildExcelDataSheet(3,2, gxEstNo).
    RUN BuildExcelDataSheet(4,2, gxINo).
    RUN BuildExcelDataSheet(5,2, gxPartNo).
    RUN BuildExcelDataSheet(6,2, eb.len).
    RUN BuildExcelDataSheet(7,2, eb.wid).
    RUN BuildExcelDataSheet(8,2, eb.dep).
    RUN BuildExcelDataSheet(9,2, gxOrdNo).

    IF AVAIL oe-ord THEN
        RUN BuildExcelDataSheet(10,2, oe-ord.due-date).
    
    RUN BuildExcelDataSheet(11,2, eb.part-dscr1).
    RUN BuildExcelDataSheet(12,2, eb.part-dscr2).

    IF AVAIL quotehd THEN
        RUN BuildExcelDataSheet(13,2,quotehd.q-no). 

    RUN BuildExcelDataSheet(14,2, eb.cad-no).
    RUN BuildExcelDataSheet(15,2, eb.style).
    RUN BuildExcelDataSheet(16,2, eb.flute).
    RUN BuildExcelDataSheet(17,2, eb.test).

    /*adder descriptions 1-3*/
    RUN BuildExcelDataSheet(18,2, ef.adder[7]). 
    RUN BuildExcelDataSheet(19,2, ef.adder[8]).
    RUN BuildExcelDataSheet(20,2, ef.adder[9]).

    /*Inks 1-6*/
    RUN BuildExcelDataSheet(21,2, eb.i-code[1]).
    RUN BuildExcelDataSheet(22,2, eb.i-%[1]).
    RUN BuildExcelDataSheet(23,2, eb.i-code[2]).
    RUN BuildExcelDataSheet(24,2, eb.i-%[2]).
    RUN BuildExcelDataSheet(25,2, eb.i-code[3]).
    RUN BuildExcelDataSheet(26,2, eb.i-%[3]).
    RUN BuildExcelDataSheet(27,2, eb.i-dscr[1]).
    RUN BuildExcelDataSheet(28,2, eb.i-dscr[2]).
    RUN BuildExcelDataSheet(29,2, eb.i-dscr[3]).
/*     RUN BuildExcelDataSheet(30,2, eb.i-%[5]).    */
/*     RUN BuildExcelDataSheet(31,2, eb.i-code[6]). */
/*     RUN BuildExcelDataSheet(32,2, eb.i-%[6]).    */

    RUN BuildExcelDataSheet(33,2, STRING(glCuttingDie) ).
    RUN BuildExcelDataSheet(34,2, eb.die-no).
    RUN BuildExcelDataSheet(35,2,STRING(glPrintPlate)).
    RUN BuildExcelDataSheet(36,2, eb.plate-no).
    RUN CheckTabBoxes(eb.tab-in).
    IF gxMcode[1] NE "" THEN DO:
        RUN BuildExcelDataSheet(37,2, gxMCode[1]).
        RUN BuildExcelDataSheet(38,2, gxMDscr[1]).
        RUN BuildExcelDataSheet(39,2, gxMCode[2]).
        RUN BuildExcelDataSheet(40,2, gxMDscr[2]).
        RUN BuildExcelDataSheet(41,2, gxMCode[3]).
        RUN BuildExcelDataSheet(42,2, gxMDscr[3]).
        RUN BuildExcelDataSheet(43,2, gxMCode[4]).
        RUN BuildExcelDataSheet(44,2, gxMDscr[4]).
        RUN BuildExcelDataSheet(45,2, gxMCode[5]).
        RUN BuildExcelDataSheet(46,2, gxMDscr[5]).
    END.

    RUN BuildExcelDataSheet(47,2, eb.cas-no). /*packing code*/
    RUN BuildExcelDataSheet(48,2, eb.tr-no). /*pallet number*/
    RUN BuildExcelDataSheet(49,2, eb.tr-cnt). /*count*/
    RUN BuildExcelDataSheet(50,2, eb.stack-code).

    RUN BuildExcelDataSheet(51,2, gxShipID).
    IF AVAIL shipto THEN DO:
        RUN BuildExcelDataSheet(52,2, shipto.ship-addr[1]).
        RUN BuildExcelDataSheet(53,2, shipto.ship-addr[2]).
        RUN BuildExcelDataSheet(54,2, shipto.ship-city).
        RUN BuildExcelDataSheet(55,2, shipto.ship-state).
        RUN BuildExcelDataSheet(56,2, shipto.ship-zip).
        RUN BuildExcelDataSheet(57,2, shipto.contact).
        RUN BuildExcelDataSheet(58,2, shipto.dock-hour).
        RUN BuildExcelDataSheet(59,2, shipto.loc).
        RUN BuildExcelDataSheet(60,2, shipto.phone).
        RUN BuildExcelDataSheet(61,2, shipto.phone-country).
        RUN BuildExcelDataSheet(62,2, shipto.area-code).
    END.
    RUN BuildExcelDataSheet(63,2, eb.num-up).
/*     RUN BuildExcelDataSheet(63,2, gxNOutDC). */
/*     RUN BuildExcelDataSheet(64,2, gxNOutPR). */
    RUN BuildExcelDataSheet(65,2, gxVend).
    RUN BuildExcelDataSheet(66,2, gxVendItem).
    RUN BuildExcelDataSheet(67,2, STRING(gxPurMan,"YES/NO")).
    RUN BuildExcelDataSheet(68,2, STRING(AVAIL(probe),"YES/NO")). /*gsa warehouse markup gt 0*/

    giCount = 0.
    DO WHILE giCount LT giMaxQtys:
        giCount = giCount + 1.
        RUN BuildExcelDataSheet(68 + giCount,2, gxQty[giCount]).        
    END.

    /*Print Notes - these will be concatenated in the Excel template*/
    giCount = 0.
    DO WHILE giCount LT giMaxNotes:
        giCount = giCount + 1.
        RUN BuildExcelDataSheet(giCount,6, gxNote[giCount]).
    END.

    RUN SaveExcel(gxEstNo + STRING(gxFormNo) + STRING(gxBlankNo) ).
    RUN OpenExcel.
END. /* for each gbf-report */

RUN ReleaseExcelObjects.

FOR EACH gbf-report WHERE gbf-report.term-id EQ v-term-id EXCLUSIVE-LOCK:
  DELETE gbf-report.
END.

PROCEDURE GetOutputDir:
    DEFINE OUTPUT PARAMETER opcOutDir AS CHAR NO-UNDO.

    DEFINE BUFFER bf-users FOR users.

    FIND FIRST bf-users WHERE
       bf-users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL bf-users AND bf-users.user_program[2] NE "" THEN
     opcOutDir = users.user_program[2].
  ELSE
     opcOutDir = "c:\tmp\".
  IF R-INDEX(opcOutDir,"\") NE LENGTH(opcOutDir)
      THEN opcOutDir = opcOutDir + "\".

END PROCEDURE.

PROCEDURE CreateExcelSheet:
    DEFINE VARIABLE cCurrDir AS CHAR NO-UNDO.
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO.
    DEFINE VARIABLE cOutDir AS CHAR NO-UNDO.
  
    RUN util/CurrDir.p (OUTPUT cCurrDir).
    RUN GetOutputDir (OUTPUT cOutDir).
  
    ASSIGN
        cFileName = cCurrDir + "\Template\"  + gcTemplateFile NO-ERROR.

   /* Create excel object*/
    IF NOT VALID-HANDLE(ghExcel) THEN
        CREATE "excel.application" ghExcel.

    /* Open an Excel    */
    ghExcel:Workbooks:OPEN(cFileName).

    /* open  sheet*/
    ASSIGN 
        ghExcelWorkbook  = ghExcel:ActiveWorkbook
        ghExcelWorksheet = ghExcel:Sheets:Item("Data")  /* 3rd Sheet */
        ghExcel:ScreenUpdating = no
        gcOutputFile = cOutDir + gcOutputFileName.

  /*============*/

END PROCEDURE. /* CreateExceSheet */

PROCEDURE BuildExcelDataSheet:
  DEFINE INPUT PARAMETER ipiRowNum AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipiColNum AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipcCellValue AS CHAR NO-UNDO.
  
  IF ipcCellValue = "" THEN RETURN.
  
  ghExcelWorkSheet:Cells(ipiRowNum,ipiColNum):VALUE = ipcCellValue.

END PROCEDURE. /*BuildExcelSheet */

PROCEDURE SaveExcel:
    DEFINE INPUT PARAMETER ipcFileAppend AS CHAR NO-UNDO.
    gcOutputFile = gcOutputFile + ipcFileAppend + ".xls".
    ghExcelWorkbook:SaveAs(gcOutputFile, -4143,,,,,,, TRUE).
      
END PROCEDURE.

PROCEDURE CheckTabBoxes:
  DEFINE INPUT PARAMETER iplTabIn AS LOG NO-UNDO.

  IF iplTabIn EQ YES THEN
    ASSIGN 
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Inside"):VALUE = 1
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Outside"):VALUE = 0
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_NA"):VALUE = 0
      .
  ELSE IF iplTabIn EQ NO THEN
    ASSIGN
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Inside"):VALUE = 0
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Outside"):VALUE = 1
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_NA"):VALUE = 0
      .
  ELSE
    ASSIGN
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Inside"):VALUE = 0
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_Outside"):VALUE = 0
      ghExcelWorkbook:Sheets(1):CheckBoxes("cb_NA"):VALUE = 1
      .

END PROCEDURE. /*BuildExcelSheet */

PROCEDURE OpenExcel:
  /* Open the saved file    */
  ghExcel:Workbooks:OPEN(gcOutputFile).
  ghExcel:VISIBLE = YES.
  ghExcel:ScreenUpdating = YES.
  
END PROCEDURE. /*ReOpenExcel*/

PROCEDURE ReleaseExcelObjects:
    RELEASE OBJECT ghExcelWorkSheet.
    RELEASE OBJECT ghExcelWorkbook.
    RELEASE OBJECT ghExcel .

END PROCEDURE.  /*ReleaseExcelObject */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */


