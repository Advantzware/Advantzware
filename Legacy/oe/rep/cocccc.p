&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/rep/cocccc.p
    Purpose     : CCC BOL Cert

    Syntax      :

    Description :

    Author(s)   :  BV
    Created     :  02/24/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{sys/inc/var.i shared}

{oe/rep/oe-lad.i}

DEFINE SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.

DEFINE VARIABLE gcCurActivePrinter AS CHAR NO-UNDO.
/* DEFINE VARIABLE gcAdobePrinter AS CHAR NO-UNDO. */
DEFINE VARIABLE gchExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE gchWshNetwork AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE gchWorkBook  AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE gchWorksheet AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE gcTempDir AS CHARACTER FORMAT "X(200)" NO-UNDO.
DEFINE VARIABLE gcFile AS CHARACTER NO-UNDO.
DEF VAR lv-print-img AS LOG NO-UNDO.
DEFINE VARIABLE cCertFormat AS CHARACTER NO-UNDO .
DEF VAR cBolcert-char AS CHAR FORMAT "X(200)" NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr         AS INT
    FIELD tt-FileName        AS CHAR
    INDEX filelist           IS PRIMARY TT-FILECTR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   gcTempDir = users.user_program[2] + "\".
ELSE
   gcTempDir = "c:\tmp\".

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* RELEASE OBJECTS */
  RELEASE OBJECT gchWorkbook         NO-ERROR.
  RELEASE OBJECT gchWorkSheet        NO-ERROR.

  /* Reset the Active Printer to the Original Printer. */
  if gcCurActivePrinter <> '' then
    gchWshNetwork:SetDefaultPrinter(gcCurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    gchExcelApplication:Quit() no-error.
  
  /* Release created objects. */
  RELEASE OBJECT gchWshNetwork         NO-ERROR.
  RELEASE OBJECT gchExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillData Procedure 
PROCEDURE FillData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iWorksheetCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iWorksheetTotal AS INTEGER NO-UNDO.

DEFINE VARIABLE dtManDate AS DATE NO-UNDO.
DEFINE VARIABLE cCoating AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInks AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBoard AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dWeight AS DECIMAL NO-UNDO.
DEFINE VARIABLE xCaseCountFull LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE xCaseCountPartial LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE xCaseCountTail LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE xQtyPerCaseFull LIKE oe-boll.qty-case NO-UNDO.
DEFINE VARIABLE xQtyPerCasePartial LIKE oe-boll.qty-case NO-UNDO.
DEFINE VARIABLE xQtyPerCaseTail LIKE oe-boll.qty-case NO-UNDO.
DEFINE VARIABLE iNoteLine AS INTEGER     NO-UNDO.
DEFINE VARIABLE iInkCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE v-ord-fb  AS CHAR FORMAT "x(13)" NO-UNDO.

DEFINE BUFFER bf-oe-boll FOR oe-boll.
DEFINE BUFFER bf-job-mat FOR job-mat.
DEFINE BUFFER bf-item FOR ITEM.
DEFINE BUFFER bf-job FOR job.

RUN PrepareReportTT.
RUN PrepareExcelFile(OUTPUT iWorksheetTotal).

/*FIND FIRST sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.NAME = "BOLCERT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN cBolcert-char = sys-ctrl.char-fld.*/

ASSIGN
   iWorksheetCount = 0.

FOR EACH report 
    WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll 
        WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK,
    FIRST oe-bolh 
        WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
    FIRST cust    
        WHERE cust.cust-no EQ oe-bolh.cust-no NO-LOCK
    BREAK BY report.key-01
          BY report.key-02
          BY report.key-03
          BY report.key-04
          BY report.key-06 DESC:
    ASSIGN
        cCoating = ""
        cInks = ""
        cBoard = ""
        dWeight = 0.

    /* If last of item */
    IF LAST-OF(report.key-06) THEN DO:
        iWorksheetCount = iWorksheetCount + 1.
        
        /* Establish Active Sheet. */
        gchWorkbook:WorkSheets(iWorksheetCount + 2):Activate NO-ERROR.

        /*Always overwriting Data sheet*/
        gchWorkSheet = gchExcelApplication:Sheets:item(1).

        FOR EACH bf-oe-boll OF oe-bolh 
            WHERE bf-oe-boll.i-no EQ oe-boll.i-no NO-LOCK:
            /*Sum up Shipping Data points - runs for each oe-boll*/
            IF bf-oe-boll.partial GT 0 THEN 
                ASSIGN 
                    xCaseCountPartial = 1
                    xQtyPerCasePartial = bf-oe-boll.partial.
           
            IF bf-oe-boll.lot-no MATCHES "TAIL*" THEN 
                ASSIGN
                    xCaseCountTail = 1
                    xQtyPerCaseTail = bf-oe-boll.qty-case.
            ELSE
                ASSIGN 
                    xCaseCountFull = xCaseCountFull + bf-oe-boll.cases
                    xQtyPerCaseFull = bf-oe-boll.qty-case.
        END.    
        /*Get Main data buffers*/
        FIND FIRST shipto
            WHERE shipto.company EQ oe-bolh.company
              AND shipto.cust-no EQ oe-bolh.cust-no
              AND shipto.ship-id  EQ oe-bolh.ship-id
            NO-LOCK NO-ERROR.
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ oe-boll.company
              AND oe-ordl.ord-no  EQ oe-boll.ord-no
              AND oe-ordl.i-no    EQ oe-boll.i-no
              AND oe-ordl.line    EQ oe-boll.line
            NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN DO:
            FIND FIRST eb 
                WHERE eb.company EQ oe-ordl.company 
                  AND eb.est-no EQ oe-ordl.est-no 
                  AND eb.form-no EQ oe-ordl.form-no 
                  AND eb.blank-no EQ oe-ordl.blank-no 
                NO-LOCK NO-ERROR.
            FIND FIRST bf-job
                WHERE bf-job.company EQ oe-ordl.company
                  AND bf-job.job-no EQ oe-ordl.job-no
                  AND bf-job.job-no2 EQ oe-ordl.job-no2
                NO-LOCK.
            IF AVAIL bf-job THEN DO:
                ASSIGN 
                    cBoard = ""
                    dWeight = 0.
                FOR EACH bf-job-mat
                    WHERE bf-job-mat.company EQ bf-job.company
                      AND bf-job-mat.job EQ bf-job.job
                      AND bf-job-mat.job-no EQ bf-job.job-no
                      AND bf-job-mat.job-no2 EQ bf-job.job-no2
                      AND bf-job-mat.frm EQ oe-ordl.form-no
                    NO-LOCK,
                FIRST bf-item
                    WHERE bf-item.company EQ bf-job-mat.company
                      AND bf-item.i-no EQ bf-job-mat.rm-i-no
                      AND LOOKUP(bf-item.mat-type,"B,P") GT 0    /*Ticket - 28664 */
                    NO-LOCK:    
                    IF cBolcert-char EQ "CCC2" THEN DO:
                       IF AVAIL bf-item THEN DO: 
                            cBoard = STRING(bf-item.cal)  + " - " + STRING(bf-item.procat,"X(5)").
                       END.

                     /*  IF AVAIL bf-item AND bf-item.i-code EQ "R" THEN DO:
                        ASSIGN  cBoard =(IF bf-item.cal NE 0 THEN STRING(bf-item.cal,"9.99<<<") ELSE "") + " - " + STRING(bf-item.i-no,"X(10)").
                        IF  bf-item.s-len NE 0 OR bf-item.r-wid NE 0 OR  bf-item.s-wid NE 0 THEN 
                            cBoard = cBoard + " - " + STRING(bf-item.s-len,"9999.99<<") + " X " +
                            (if bf-item.r-wid ne 0 then STRING(bf-item.r-wid,"9999.99<<") ELSE STRING(bf-item.s-wid,"9999.99<<")).
                        
                       END.
                       ELSE DO:
                        ASSIGN  cBoard = (IF bf-item.cal NE 0 THEN STRING(bf-item.cal,"9.99<<<") ELSE "") + " - " + STRING(bf-item.i-no,"X(15)").
                        IF bf-job-mat.len NE 0 OR bf-job-mat.wid NE 0 THEN 
                                cBoard = cBoard + " - " + string(bf-job-mat.len,"9999.99<<") + " X " + STRING(bf-job-mat.wid,"9999.99<<").
                                                
                       END.*/

                    END.
                    ELSE DO:
                      ASSIGN cBoard = bf-item.i-name.
                    END.

                    ASSIGN 
                        /*cBoard = bf-item.i-name*/
                        dWeight = bf-job-mat.basis-w.
                    LEAVE.
                END. /*eacb bf-job-mat, bf-item*/
            END. /*avail bf-job*/
        END. /*avail oe-ordl*/

        /*get manufactured date as last receipt*/
        FOR EACH fg-rctd 
            WHERE fg-rctd.company EQ cocode 
              AND fg-rctd.i-no EQ oe-ordl.i-no 
              AND fg-rctd.job-no EQ oe-ordl.job-no
              AND fg-rctd.job-no2 EQ oe-ordl.job-no2
            NO-LOCK
            USE-INDEX i-no
            BY fg-rctd.rct-date:
            dtManDate = fg-rctd.rct-date.
            LEAVE.
        END. /*each fg-rctd*/
        IF dtManDate EQ 12/31/2999 THEN
            FOR EACH fg-rctd 
                WHERE fg-rctd.company EQ cocode 
                  AND fg-rctd.i-no EQ oe-ordl.i-no 
                  AND fg-rctd.po-no EQ STRING(oe-ordl.po-no-po)
                NO-LOCK
                USE-INDEX i-no
                BY fg-rctd.rct-date:
                dtManDate = fg-rctd.rct-date.
                LEAVE.
            
            END. /*each fg-rctd*/
        IF AVAIL eb THEN DO:
            FIND FIRST ef OF eb NO-LOCK NO-ERROR.
            FIND FIRST style
                WHERE style.company EQ eb.company
                  AND style.style EQ eb.style
                NO-LOCK NO-ERROR.
            /*build inks and coating*/
            IF eb.i-col > 0 OR eb.i-coat > 0 THEN 
                DO iInkCount = 1 TO EXTENT(eb.i-code2):
                    IF eb.i-code2[iInkCount] NE "" THEN DO:
                        FIND FIRST ITEM 
                            WHERE ITEM.company EQ cocode
                              AND ITEM.i-no EQ eb.i-code2[iInkCount]
                            NO-LOCK NO-ERROR.
                        IF AVAIL ITEM THEN
                            IF ITEM.mat-type EQ "I" THEN 
                                cInks = cInks + eb.i-dscr2[iInkCount] + ",".
                            ELSE 
                                cCoating = cCoating + eb.i-dscr2[iInkCount] + ",".

                    END. /*eb.i-code2[inkCount] ne ""*/
                END. /*do iInkCount*/
                
        END. /*avail eb*/

        /*Clean up inks and coating*/
        IF cInks EQ "" THEN cInks = "N/A".
        ELSE
            cInks = TRIM(cInks,",").
        IF cCoating EQ "" THEN cCoating = "N/A".
        ELSE
            cCoating = TRIM(cCoating,",").
                            

        FIND FIRST fgcat
            WHERE fgcat.company EQ itemfg.company
              AND fgcat.procat EQ itemfg.procat
            NO-LOCK NO-ERROR.

        v-ord-fb = "".
        v-ord-fb = STRING(oe-boll.ord-no) .
        FIND FIRST job-hdr WHERE job-hdr.company EQ cocode      /*Task# 08081404*/
          AND job-hdr.job-no EQ oe-boll.job-no
          AND job-hdr.job-no2 EQ oe-boll.job-no2
          AND job-hdr.i-no EQ oe-boll.i-no NO-LOCK NO-ERROR.
        IF AVAIL job-hdr THEN
            v-ord-fb = v-ord-fb +  "-" + trim(string(job-hdr.frm)) + trim(string(job-hdr.blank-no)) .

        
        /*write data to excel Data sheet*/
        ASSIGN
            gchWorkSheet:Range("B1"):VALUE = oe-boll.po-no
            gchWorkSheet:Range("B2"):VALUE = STRING(v-ord-fb)
            gchWorkSheet:Range("B3"):VALUE = IF AVAIL oe-ordl THEN STRING(oe-ordl.qty) ELSE ""
            gchWorkSheet:Range("B4"):VALUE = STRING(dtManDate,"99/99/9999")
            gchWorkSheet:Range("B5"):VALUE = IF AVAIL eb THEN STRING(eb.num-up) ELSE ""
            gchWorkSheet:Range("B6"):VALUE = cust.NAME
            gchWorkSheet:Range("B7"):VALUE = IF AVAIL shipto THEN shipto.ship-addr[1] ELSE cust.addr[1]
            gchWorkSheet:Range("B8"):VALUE = IF AVAIL shipto THEN shipto.ship-addr[2] ELSE cust.addr[2]
            gchWorkSheet:Range("B9"):VALUE = IF AVAIL shipto THEN shipto.ship-city ELSE cust.city
            gchWorkSheet:Range("B10"):VALUE = IF AVAIL shipto THEN shipto.ship-state ELSE cust.state
            gchWorkSheet:Range("B11"):VALUE = IF AVAIL shipto THEN shipto.ship-zip ELSE cust.zip
            gchWorkSheet:Range("B12"):VALUE = IF AVAIL fgcat THEN fgcat.dscr ELSE ""
            gchWorkSheet:Range("B13"):VALUE = itemfg.part-no
            gchWorkSheet:Range("B14"):VALUE = IF AVAIL eb THEN eb.spc-no ELSE ""
            gchWorkSheet:Range("B15"):VALUE = IF AVAIL style THEN style.dscr ELSE ""
            gchWorkSheet:Range("B16"):VALUE = cCoating
            gchWorkSheet:Range("B17"):VALUE = IF AVAIL eb THEN eb.part-dscr1 ELSE ""
            gchWorkSheet:Range("B18"):VALUE = string(cBoard,"x(30)")
            gchWorkSheet:Range("B19"):VALUE = STRING(dWeight)
            gchWorkSheet:Range("B20"):VALUE = cInks
            gchWorkSheet:Range("B21"):VALUE = IF AVAIL eb THEN eb.adhesive ELSE ""
            gchWorkSheet:Range("B22"):VALUE = STRING(oe-bolh.bol-date)
            gchWorkSheet:Range("B23"):VALUE = itemfg.prod-no
            gchWorkSheet:Range("B24"):VALUE = STRING(xCaseCountFull)
            gchWorkSheet:Range("B25"):VALUE = STRING(xQtyPerCaseFull)
/*             gchWorkSheet:Range("B26"):VALUE = "Calculated" */
            gchWorkSheet:Range("B27"):VALUE = STRING(xCaseCountPartial)
            gchWorkSheet:Range("B28"):VALUE = STRING(xQtyPerCasePartial)
/*             gchWorkSheet:Range("B29"):VALUE = "Calculated" */
            gchWorkSheet:Range("B30"):VALUE = STRING(xCaseCountTail)
            gchWorkSheet:Range("B31"):VALUE = STRING(xQtyPerCaseTail)
/*             gchWorkSheet:Range("B32"):VALUE = "Calculated" */
            gchWorkSheet:Range("B33"):VALUE = STRING(oe-bolh.tot-pal).

        /*Get Notes*/
        iNoteLine = 1.
        FOR EACH notes 
            WHERE notes.rec_key EQ cust.rec_key
              AND notes.note_code EQ "CA"
            NO-LOCK:
            iNoteLine = iNoteLine + 1.
            gchWorkSheet:Range("D" + TRIM(STRING(iNoteLine,">9"))):VALUE = 
                notes.note_text.
        END.

        /*Copy & PasteSpecial Values for a copy of the formula template sheet*/
        gchWorkbook:WorkSheets(iWorksheetCount + 2):UsedRange:COPY NO-ERROR.
        gchWorkbook:WorkSheets(iWorksheetCount + 2):UsedRange:PasteSpecial(-4163, -4142, False, False) NO-ERROR.

    END. /*last of item*/

    ASSIGN
        xCaseCountFull = 0
        xCaseCountPartial = 0
        xCaseCountTail = 0
        xQtyPerCaseFull = 0
        xQtyPerCasePartial = 0
        xQtyPerCaseTail = 0
        dtManDate = 12/31/2999
        .
END. /* for each report */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTemplateFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iWorksheetTotal AS INTEGER NO-UNDO.

/* Capture the current active printer. */
IF LvOutputSelection = "email" THEN
    ASSIGN 
        gcCurActivePrinter = SESSION:PRINTER-NAME
/*         gcAdobePrinter     = "PDFcamp Printer" */
    .
  
FIND FIRST sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.NAME = "BOLCERT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN
    cBolcert-char = sys-ctrl.char-fld .
IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "CCCWPP" THEN lv-print-img = YES.
    ELSE lv-print-img = NO.
IF AVAIL sys-ctrl THEN
    cCertFormat = sys-ctrl.char-fld .

FOR EACH report  
    WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh 
        WHERE RECID(oe-bolh) EQ report.rec-id NO-LOCK:
         /* tests where customer specific forms have CCCWPP*/
    DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ cocode
            AND sys-ctrl-shipto.NAME EQ "BOLCERT"
            AND sys-ctrl-shipto.cust-vend-no = oe-bolh.cust-no NO-ERROR.
        IF AVAIL sys-ctrl-shipto THEN do:
            cBolcert-char = sys-ctrl-shipto.char-fld .
            IF sys-ctrl-shipto.char-fld = "CCCWPP" THEN
                lv-print-img = YES.
            ELSE lv-print-img = NO.
           cCertFormat = sys-ctrl-shipto.char-fld  .
        END.
    END. /*end of Do block to test for CCCWPP per customer*/ 
    LEAVE .
 END.

IF cCertFormat EQ "CCCWPP"  THEN
    cTemplateFile = "template\WPPBOLCert.xlt".
ELSE IF cCertFormat EQ "CCCW" THEN
    cTemplateFile = "template\CCWBOLCert.xlt".
ELSE
    cTemplateFile = "template\CCCBOLCert.xlt". 

/* Connect to the running Excel session. */
CREATE "Excel.Application" gchExcelApplication CONNECT NO-ERROR.

/* If Excel is running close it. */
IF NOT VALID-HANDLE (gchExcelApplication) THEN
    /* Start a new session of Excel. */
    CREATE "Excel.Application" gchExcelApplication NO-ERROR.

/* Network connection checks. */
CREATE "WScript.Network" gchWshNetwork NO-ERROR.
IF NOT(VALID-HANDLE(gchWshNetwork)) THEN
    DO:
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
/* /* Switch Printer to PDFCamp Printer. */             */
/* IF LvOutputSelection = "Email" THEN                  */
/*     gchWshNetwork:SetDefaultPrinter(gcAdobePrinter). */
 
/* Check if Excel got initialized. */
IF NOT (VALID-HANDLE (gchExcelApplication)) THEN
    DO:
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

FILE-INFO:FILE-NAME = cTemplateFile.

/* Set the Excel Template to be used. */
ASSIGN gcFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
IF SEARCH (gcFile) = ? THEN DO:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
        'cannot be found. Please verify that the file exists.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* Make Excel visible. */
ASSIGN
    gcFile = FILE-INFO:FULL-PATHNAME
    gchExcelApplication:VISIBLE = 
        IF LvOutputSelection = "Email" OR LvOutputSelection = "Printer" THEN  
            FALSE
        ELSE 
            TRUE.

/* Clear ttFileList. */
EMPTY TEMP-TABLE tt-filelist.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Open our Excel Template. */  
  ASSIGN gchWorkbook = gchExcelApplication:Workbooks:OPEN(gcFile)  NO-ERROR.
  
  /* Do not display Excel error messages. */
  gchExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

  /* Disable screen updating so it will go faster */
  gchExcelApplication:ScreenUpdating = FALSE.

  /* Go to the Active Sheet. */
  gchWorkbook:WorkSheets(1):Activate NO-ERROR.
  gchWorkSheet = gchExcelApplication:Sheets:ITEM(1).

  /*Fill in Data*/
  RUN FillData.
  RUN OutputFiles.
  /* enable screen updating */
  gchExcelApplication:ScreenUpdating = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputFiles Procedure 
PROCEDURE OutputFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iFileCtr AS INTEGER NO-UNDO.

gchWorkbook:WorkSheets(2):DELETE NO-ERROR. /*delete template sheet with formulas*/
gchWorkbook:WorkSheets(1):DELETE NO-ERROR. /*delete Data Sheet*/
gchWorkbook:WorkSheets(1):Activate NO-ERROR.

OS-DELETE VALUE(gcTempDir + "cofc.xls").     
OS-DELETE VALUE(gcTempDir + "asi.pdf").
OS-DELETE VALUE(gcTempDir + "cofc.pdf").

IF LvOutputSelection = "PRINTER" THEN DO:
   NO-RETURN-VALUE gchWorkbook:PrintOut(,,,,,FALSE,).
    gchWorkbook:CLOSE(NO) NO-ERROR.
END.
ELSE IF LvOutputSelection = "Email" THEN DO:
/*     gchWshNetwork:SetDefaultPrinter(gcAdobePrinter). */
    gchExcelApplication:ActiveSheet:SaveAs(gcTempDir + "cofc.xls") /*NO-ERROR*/.

    NO-RETURN-VALUE gchWorkbook:ExportAsFixedFormat(0, gcTempDir + "cofc.pdf").
/*     NO-RETURN-VALUE gchWorkbook:PrintOut(,,,,,TRUE,,gcTempDir + "asi.pdf"). */
 
    gchWorkbook:CLOSE(NO) NO-ERROR.
/*     gchExcelApplication:QUIT() NO-ERROR. */
    PAUSE 3.
    OS-DELETE VALUE(gcTempDir + "cofc.xls").
    OS-RENAME VALUE(gcTempDir + "asi.pdf") VALUE(gcTempDir + "cofc.pdf").
    iFileCtr = iFileCtr + 1.
    CREATE tt-filelist.
    ASSIGN 
        tt-FileCtr  = iFileCtr
        tt-FileName = gcTempDir + "cofc.pdf".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepareExcelFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepareExcelFile Procedure 
PROCEDURE PrepareExcelFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiWorksheetCount AS INTEGER NO-UNDO.

/*count needed worksheets and create copies*/
FOR EACH report 
    WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll 
        WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST oe-bolh 
        WHERE oe-bolh.b-no  EQ oe-boll.b-no NO-LOCK,
    FIRST cust    
        WHERE cust.cust-no EQ oe-bolh.cust-no NO-LOCK
    BREAK BY report.key-01
          BY report.key-02
          BY report.key-03
          BY report.key-04
          BY report.key-06:
    
    /*##PN Make copies of the "COC" sheet that has formulas & rename sheets*/
    /*##PN These copies will copy/pasted as values once the data sheet is filled in*/
    /*##PN This allows for easy template edit and design*/
    IF LAST-OF(report.key-06) THEN DO:
        opiWorksheetCount = opiWorksheetCount + 1.
        gchWorkbook:WorkSheets(2):COPY(gchExcelApplication:Sheets:item(2)) NO-ERROR.
        gchWorksheet = gchExcelApplication:Sheets:item(3).
        gchWorkSheet:name = STRING(oe-bolh.bol-no) + "-" +
                         report.key-06.
    END. /*last-of(report.key-06)*/
END. /*each report*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepareReportTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepareReportTT Procedure 
PROCEDURE PrepareReportTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sa/sa-sls01.i}

FOR EACH report  
    WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh 
        WHERE RECID(oe-bolh) EQ report.rec-id NO-LOCK:
    FOR EACH oe-boll
        WHERE oe-boll.company EQ cocode
          AND oe-boll.b-no    EQ oe-bolh.b-no
        NO-LOCK:
        
        CREATE xreport.
        ASSIGN
            xreport.term-id = v-term-id
            xreport.key-01  = report.key-01
            xreport.key-02  = report.key-02
            xreport.key-03  = report.key-03
            xreport.key-04  = report.key-04
            xreport.key-06  = oe-boll.i-no
            xreport.rec-id  = RECID(oe-boll).
    END.
    DELETE report.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

