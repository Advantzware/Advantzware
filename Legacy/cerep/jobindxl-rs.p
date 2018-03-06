&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : cerep/jobindxl-rs.p 
    
    Purpose     : Generates Factory Ticket Using Excel Template

    Syntax      : run cerep/jobindxl-rs.p (
                    input vcBegJobNo,   /* Starting Job-No  */
                    input vcEndJobNo,   /* Ending Job-No    */
                    input viBegJobNo2,  /* Starting Job-No2 */
                    input viEndJobNo2,  /* Ending Job-No2   */
                    input vlRS,         /* Process RS?      */
                    input vlPR,         /* Process PR?      */
                    input vlDC,         /* Process DC?      */
                    input vlGL,         /* Process GL?      */
                    input vlSW)         /* Process SW?      */
                    no-error.

    Description : Factory Ticket Using Excel Template

    Author(s)   : Dennis Dizon
    
    Created     : Apr 9, 2007
    
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
DEF INPUT PARAM icBegJobNo        AS CHAR NO-UNDO.
DEF INPUT PARAM icEndJobNo        AS CHAR NO-UNDO.
DEF INPUT PARAM iiBegJobNo2       AS INTE NO-UNDO.
DEF INPUT PARAM iiEndJobNo2       AS INTE NO-UNDO.
DEF INPUT PARAM ilRS              AS LOG  NO-UNDO.
DEF INPUT PARAM ilPR              AS LOG  NO-UNDO.
DEF INPUT PARAM ilDC              AS LOG  NO-UNDO.
DEF INPUT PARAM ilGL              AS LOG  NO-UNDO.
DEF INPUT PARAM ilSW              AS LOG  NO-UNDO.

/* Buffers */
DEF BUFFER b-job-hdr  FOR job-hdr.
DEF BUFFER b2-job-hdr FOR job-hdr.
DEF BUFFER b-job-mat  FOR job-mat.
DEF BUFFER b-eb       FOR eb.
DEF BUFFER b2-eb      FOR eb.
DEF BUFFER b-ef       FOR ef.
DEF BUFFER b-job-mch  FOR job-mch.
DEF BUFFER b-itemfg   FOR itemfg.
DEF BUFFER b-item     FOR item.
DEF BUFFER b-rt       FOR reftable.
DEF BUFFER b-est-flm  FOR est-flm.
DEF BUFFER b-prep     FOR prep.
DEF BUFFER b-reftable FOR reftable.

/* Includes */
{sys/inc/var.i shared}

/* Temp-Tables */
DEF TEMP-TABLE tt-ink NO-UNDO
         FIELD form-no   AS INTE
         FIELD blank-no  AS INTE
         FIELD i-pass    AS INTE
         FIELD i-unit    AS INTE
         FIELD i-code    AS CHAR
         FIELD i-dscr    AS CHAR
         FIELD i-name    AS CHAR
         FIELD loc-bin   AS CHAR
         FIELD q-onh     AS INTE
         FIELD i-seq     AS INTE
         FIELD i-qty     AS DECI FORMAT '->>,>>9.9999'
         FIELD rm-dscr   AS CHAR 
         INDEX i-code    i-code
         INDEX i-unit    i-unit.

DEFINE TEMP-TABLE ttRmBin NO-UNDO LIKE rm-bin
    FIELD rct-date      LIKE rm-rcpth.trans-date
    .


/* Variables */
DEF VAR viLoop      AS INTE NO-UNDO.
DEF VAR vcMachines  AS CHAR NO-UNDO.
DEF VAR vcRMItem    AS CHAR NO-UNDO.
DEF VAR vcJobNo     AS CHAR NO-UNDO.
DEF VAR vlPage2     AS LOG  NO-UNDO INIT FALSE.
DEF VAR vlSheet2    AS LOG  NO-UNDO INIT FALSE.
DEF VAR viNumUp     AS INTE NO-UNDO.
DEF VAR viSheets    AS INTE NO-UNDO.
DEF VAR viRS        AS INTE NO-UNDO.
DEF VAR viPR        AS INTE NO-UNDO.
DEF VAR viDC        AS INTE NO-UNDO.
DEF VAR viGL        AS INTE NO-UNDO.
DEF VAR viWN        AS INTE NO-UNDO.
DEF VAR viSW        AS INTE NO-UNDO.
DEF VAR viSheetNo   AS INTE NO-UNDO.
DEF VAR viLine      AS INTE NO-UNDO.
DEF VAR viGLSeq     AS INTE NO-UNDO.
DEF VAR viWNSeq     AS INTE NO-UNDO.
DEF VAR vcItemNo    AS CHAR NO-UNDO.
DEF VAR vcItemNm    AS CHAR NO-UNDO.
DEF VAR vcItemDesc  AS CHAR NO-UNDO.
DEF VAR viJobQty    AS INTE NO-UNDO.
DEF VAR vcDeptNotes AS CHAR NO-UNDO.
DEF VAR vcRecKey    AS CHAR NO-UNDO.
DEF VAR viUnitNo    AS INTE NO-UNDO.
DEF VAR vcBinLocs   AS CHAR NO-UNDO.
DEF VAR viCount     AS INTE NO-UNDO.
DEF VAR vcRange     AS CHAR NO-UNDO.
DEF VAR vlWN2       AS LOG  NO-UNDO.
DEF VAR viSheetQty  AS DECI NO-UNDO.
DEF VAR viOldLine   AS INTE NO-UNDO.
DEF VAR viOldLoop   AS INTE NO-UNDO.
DEF VAR v-unit AS INT NO-UNDO.

/* VARIABLE FOR EXCEL OUTPUT */
DEFINE NEW SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter     AS CHAR NO-UNDO.

/* Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE   NO-UNDO.
DEFINE            VARIABLE v-cell               AS CHARACTER    NO-UNDO.
DEFINE            VARIABLE t-dwg                AS CHAR         NO-UNDO.
DEFINE            VARIABLE t-name               AS CHARACTER    NO-UNDO   FORMAT "x(40)" .
DEFINE            VARIABLE t-fnd                AS LOGICAL      NO-UNDO   INIT "False"    .
DEFINE            VARIABLE t-seq                AS INTEGER      NO-UNDO.
DEFINE            VARIABLE inRowCount           AS INTEGER      NO-UNDO   INITIAL 1.
DEFINE            VARIABLE chFile               AS CHAR         NO-UNDO.
DEFINE            VARIABLE LvLineCnt            AS INT          NO-UNDO.
DEFINE            VARIABLE CurrDir              AS CHAR         NO-UNDO.
DEFINE            VARIABLE LvCtr                AS INT          NO-UNDO.
DEFINE            VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE            VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
DEFINE            VARIABLE CommandString        AS CHAR         NO-UNDO.
DEFINE            VARIABLE WshNetwork           AS COM-HANDLE.
DEFINE            VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT NO.

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                       FIELD tt-FileCtr         AS INT
                       FIELD tt-FileName        AS CHAR
                       INDEX filelist           IS PRIMARY 
                             TT-FILECTR.

DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-DispDim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DispDim Procedure 
FUNCTION DispDim RETURNS DECIMAL
  ( INPUT ip-dim AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatNotes Procedure 
FUNCTION FormatNotes RETURNS CHARACTER
  ( INPUT v-text  AS CHAR,
    INPUT v-len   AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBinLocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBinLocs Procedure 
FUNCTION GetBinLocs RETURNS CHARACTER
  ( INPUT icItemNo  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetItemName Procedure 
FUNCTION GetItemName RETURNS CHARACTER
  (INPUT icItemNo AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLocBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLocBin Procedure 
FUNCTION GetLocBin RETURNS CHARACTER
  (INPUT icDieNo  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 22.81
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CalculateUpValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalculateUpValue Procedure 
PROCEDURE CalculateUpValue :
/*------------------------------------------------------------------------------
  Purpose     : Computes Up Value.
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  /* Reset the Up Value. */
  viNumUp = 0.

  /* Calculate the "Up" value. */
  IF job-mch.dept <> 'GL' THEN
  DO:

    /* For RS, Add up it's Blanks' Up Value. */
    IF job-mch.dept = 'RS' THEN
    DO:
      FOR  EACH b-eb NO-LOCK
          WHERE b-eb.company   = job-hdr.company
            AND b-eb.est-no    = job-hdr.est-no
            AND b-eb.form-no   = job-hdr.frm:
        viNumUp = viNumUP + b-eb.num-up.
      END.
    END.

    /* For the others, just capture the Blank's Up Value. */
    ELSE
    DO:
      FOR FIRST b-eb NO-LOCK
          WHERE b-eb.company   = b2-job-hdr.company
            AND b-eb.est-no    = b2-job-hdr.est-no
            AND b-eb.form-no   = b2-job-hdr.frm
            AND b-eb.blank-no  = b2-job-hdr.blank-no:
        viNumUp = b-eb.num-up.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Clean up routine.
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.
  RELEASE OBJECT chHyper            NO-ERROR.

  /* Delete pre-existing PDF File. */
  OS-DELETE value(v-dir + "CardBoards.pdf").

  /* Set the PDF Merging Utility. */
  ASSIGN CommandString = CurrDir + "\util\pdftk ".
  
  /* Add the PDF Filenames to be merged to the command string.  */
  FOR EACH tt-filelist :
    ASSIGN CommandString = CommandString + " " + tt-FileName .
  END.
  
  /* Indicate the new filename of combined PDF File. */
  ASSIGN CommandString = CommandString + " cat output " + v-dir + "CardBoards.pdf".
  
  /* Merge the PDF Files. */

  OS-COMMAND SILENT VALUE(CommandString).
  
  /* Delete the "old" PDF Files. */
  FOR EACH tt-filelist :
      OS-DELETE value(tt-FileName).
  END.
  
  chExcelApplication:ScreenUpdating = TRUE.

  /* Reset the Active Printer to the Original Printer. */
  IF CurActivePrinter <> '' THEN
    WshNetwork:SetDefaultPrinter(CurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:Quit() NO-ERROR.
  
  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateSheets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateSheets Procedure 
PROCEDURE CreateSheets :
/*------------------------------------------------------------------------------
  Purpose     : Creates the Sheet for the given Dept.
  Parameters  : icDept - Machine Department
  Notes       : 
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icDept    AS CHAR NO-UNDO.

  FOR  EACH job-hdr NO-LOCK
      WHERE job-hdr.company    = cocode
        AND job-hdr.job-no    >= substring (icBegJobNo,1,6)
        AND job-hdr.job-no    <= substring (icEndJobNo,1,6)
        AND job-hdr.job-no2   >= iiBegJobNo2
        AND job-hdr.job-no2   <= iiEndJobNo2
      BREAK
         BY job-hdr.job-no
         BY job-hdr.job-no2
         BY job-hdr.frm:

    IF FIRST-OF (job-hdr.frm) THEN
    DO:

      FOR FIRST job-mch NO-LOCK
          WHERE job-mch.company   = job-hdr.company
            AND job-mch.job       = job-hdr.job
            AND job-mch.job-no    = job-hdr.job-no
            AND job-mch.job-no2   = job-hdr.job-no2
            AND job-mch.frm       = job-hdr.frm
            AND job-mch.dept      = icDept 
                USE-INDEX line-idx:

        CASE icDept:

          WHEN 'RS' THEN
            ASSIGN 
              viSheetNo = job-hdr.frm
              viRS      = viRS + 1.

          WHEN 'PR' THEN
            ASSIGN
              viSheetNo = viRS + 1
              viPR      = viPR + 1.

          WHEN 'GL' THEN
          DO:
            FOR EACH b-job-mch NO-LOCK
               WHERE b-job-mch.company = job-mch.company
                 AND b-job-mch.job     = job-mch.job
                 AND b-job-mch.job-no  = job-mch.job-no
                 AND b-job-mch.job-no2 = job-mch.job-no2
                 AND b-job-mch.frm     = job-mch.frm
                 AND b-job-mch.dept    = icDept 
                     USE-INDEX line-idx:
              ASSIGN
                viSheetNo = viRS + viPR + 1
                viGL = viGL + 1.
              chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) NO-ERROR.
            END.
          END.

          WHEN 'DC' THEN
            ASSIGN
              viSheetNo = viRS + viPR + viGL + 1
              viDC = viDC + 1.

          WHEN 'WN' THEN
          DO:
            FOR EACH b-job-mch NO-LOCK
               WHERE b-job-mch.company = job-mch.company
                 AND b-job-mch.job     = job-mch.job
                 AND b-job-mch.job-no  = job-mch.job-no
                 AND b-job-mch.job-no2 = job-mch.job-no2
                 AND b-job-mch.frm     = job-mch.frm
                 AND b-job-mch.dept    = icDept 
                     USE-INDEX line-idx:
              ASSIGN
                viSheetNo = viRS + viPR + viGL + viDC + 1
                viWN = viWN + 1.
              chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) NO-ERROR.
            END.
          END.

          WHEN 'SW' THEN
            ASSIGN
              viSheetNo = viRS + viPR + viGL + viDC + viWN + 1
              viSW = viSW + 1.

        END CASE.

        IF NOT icDept = 'GL' AND
           NOT icDept = 'WN' THEN
          chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) NO-ERROR.

        viSheets = viRS + viPR + viGL + viDC + viWN + viSW.
      END.
    END.
  END.
  
  /* Delete the Master Sheet. */
  chWorkbook:WorkSheets(viSheets + 1):Activate         NO-ERROR.
  chWorkbook:WorkSheets(viSheets + 1):delete()         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSheet Procedure 
PROCEDURE DeleteSheet :
/*------------------------------------------------------------------------------
  Purpose     :   Deletes the unused Sheet for the specified Dept.
  Parameters  :   icDept - Machine Department
  Notes       :       
------------------------------------------------------------------------------*/
  
  /* Parameters */
  DEF INPUT PARAM icDept        AS CHAR NO-UNDO.

  CASE icDept:

    WHEN 'RS' THEN
    DO:
      chWorkbook:WorkSheets(1):Activate   NO-ERROR.
      chWorkbook:WorkSheets(1):delete()   NO-ERROR.
    END.

    WHEN 'PR' THEN
    DO:
      chWorkbook:WorkSheets(viRS + 1):Activate  NO-ERROR.
      chWorkbook:WorkSheets(viRS + 1):delete()  NO-ERROR.
    END.

    WHEN 'GL' THEN
    DO:
      chWorkbook:WorkSheets(viRS + viPR + 1):Activate   NO-ERROR.
      chWorkbook:WorkSheets(viRS + viPR + 1):delete()   NO-ERROR.
    END.

    WHEN 'DC' THEN
    DO:
      chWorkbook:WorkSheets(viRS + viPR + viGL + 1):Activate   NO-ERROR.
      chWorkbook:WorkSheets(viRS + viPR + viGL + 1):delete()   NO-ERROR.
    END.

    WHEN 'WN' THEN
    DO:
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + 1):Activate   NO-ERROR.
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + 1):delete()   NO-ERROR.
    END.

    WHEN 'SW' THEN
    DO:
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + viWN + 1):Activate   NO-ERROR.
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + viWN + 1):delete()   NO-ERROR.
    END.

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DieData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DieData Procedure 
PROCEDURE DieData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE vcJobNum  AS CHARACTER  NO-UNDO.

  FIND FIRST b2-job-hdr NO-LOCK 
       WHERE b2-job-hdr.company = job-hdr.company
         AND b2-job-hdr.job-no  = job-hdr.job-no
         AND b2-job-hdr.job-no2 = job-hdr.job-no2
         AND b2-job-hdr.frm     = job-hdr.frm NO-ERROR.
  FIND FIRST b-itemfg   OF b2-job-hdr NO-LOCK NO-ERROR.
  
  FIND FIRST b-prep NO-LOCK
       WHERE b-prep.company = b-itemfg.company
         AND b-prep.code    = b-itemfg.die-no NO-ERROR.
         
  IF AVAIL b-itemfg THEN DO:
    RUN SetCellValue ("C" + string (viLine + 3), b-itemfg.die-no)             NO-ERROR.
    RUN SetCellValue ("E" + string (viLine + 3), GetLocBin (b-itemfg.die-no)) NO-ERROR.
  END.

  IF AVAIL b-prep THEN DO:
      RUN SetCellValue ("F" + STRING (viLine + 3), b-prep.received-date)       NO-ERROR.
      RUN SetCellVAlue ("H" + STRING (viLine + 3), b-prep.no-of-impressions)   NO-ERROR.
      RUN SetCellValue ("J" + STRING (viLine + 3), b-prep.last-date)           NO-ERROR. 
      vcJobNum = b-prep.last-job-no + '-' + STRING (b-prep.last-job-no2, '99').
      RUN SetCellValue ("M" + STRING (viLIne + 3), vcJobNum)                   NO-ERROR.
  END. /*IF AVAIL b-prep THEN DO:*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FGItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FGItems Procedure 
PROCEDURE FGItems :
/*------------------------------------------------------------------------------
  Purpose     : Generetes the list of FG Items for the specified Dept.
  Parameters  : icDept - Machine Department
  Note        :       
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icDept  AS CHAR NO-UNDO.
  
  /* Variables */
  DEFINE VARIABLE vcPNum  AS CHAR NO-UNDO.
  DEFINE VARIABLE vcPNum-2 AS CHAR NO-UNDO.
  DEFINE VARIABLE viQty   AS INTE NO-UNDO.
  DEFINE VARIABLE viJobQty-2 AS INT NO-UNDO.
  DEF VAR viIndex AS INT NO-UNDO.

  /* Pocess all FG Items of a Job Form/Sheet. */
  FOR EACH b2-job-hdr NO-LOCK
     WHERE b2-job-hdr.company = job-hdr.company
       AND b2-job-hdr.job-no  = job-hdr.job-no
       AND b2-job-hdr.job-no2 = job-hdr.job-no2
       AND b2-job-hdr.frm     = job-hdr.frm:

    FIND FIRST b-itemfg OF b2-job-hdr NO-LOCK NO-ERROR.

    FIND FIRST b-eb NO-LOCK
         WHERE b-eb.company   = b2-job-hdr.company
           AND b-eb.est-no    = b2-job-hdr.est-no
           AND b-eb.form-no   = b2-job-hdr.frm
           AND b-eb.blank-no  = b2-job-hdr.blank-no NO-ERROR.

    IF AVAIL b-eb THEN
      ASSIGN 
        vcPNum = b-eb.part-no
        viQty  = b-eb.bl-qty.
    ELSE
      ASSIGN 
        vcPNum = ''
        viqty  = 0.

    RUN CalculateUpValue.
    RUN GetJobQty.

    /* By Dept: */
    CASE icDept:
      
      WHEN 'RS' THEN IF ilRS THEN 
      DO:
        IF b2-job-hdr.blank-no = 1 THEN
        DO: 
          RUN SetCellValue ("A7",   b-itemfg.i-no).
          RUN SetCellValue ("C7",   vcPNum).
          RUN SetCellValue ("D7",   b-itemfg.i-name).
          RUN SetCellValue ("G7",   b-itemfg.part-dscr1).
          RUN SetCellValue ("L7",   viNumUp).
          RUN SetCellValue ("M7",   b-job-hdr.blank-no).
/*           run SetCellValue ("D9",   viQty). */
/*           run SetCellValue ("D9",   b-job-hdr.qty). */
        END.
      END.
  
      WHEN 'PR' THEN IF ilPR THEN 
      DO: 
          viLine = b2-job-hdr.blank-no + 6.

          RUN SetCellValue ("A" + string (viLine),   b-itemfg.i-no).
          RUN SetCellValue ("C" + string (viLine),   vcPNum).
          RUN SetCellValue ("D" + string (viLine),   b-itemfg.i-name).
          RUN SetCellValue ("G" + string (viLine),   b-itemfg.part-dscr1).
          RUN SetCellValue ("K" + string (viLine),   viJobQty).
          RUN SetCellValue ("L" + string (viLine),   b2-job-hdr.qty).
          RUN SetCellValue ("M" + string (viLine),   viNumUp).
      END.
  
      WHEN 'DC' THEN IF ilDC THEN 
      DO:
          viLine = b2-job-hdr.blank-no + 6.
  
          RUN SetCellValue ("A" + string (viLine),   b-itemfg.i-no).
          RUN SetCellValue ("C" + string (viLine),   vcPNum).
          RUN SetCellValue ("D" + string (viLine),   b-itemfg.i-name).
          RUN SetCellValue ("G" + string (viLine),   b-itemfg.part-dscr1).
          RUN SetCellValue ("J" + string (viLine),   viJobQty).
          RUN SetCellValue ("K" + string (viLine),   b2-job-hdr.qty).
          RUN SetCellValue ("L" + string (viLine),   viNumUp).
          RUN SetDeptNotes (vcRecKey, 'WB', job-mch.frm, 2).
      END.

      WHEN 'GL' OR
      WHEN 'WN' OR 
      WHEN 'SW' THEN 
      DO: 
          RUN GetItemInfo(OUTPUT vcPNum-2,
                          OUTPUT viJobQty-2).
          RUN SetCellValue ("A8",   vcItemNo).
          RUN SetCellValue ("C8",   vcPNum-2).
          RUN SetCellValue ("D8",   vcItemNm).
          RUN SetCellValue ("G8",   vcItemDesc).
          RUN SetCellValue ("J8",   viJobQty-2 ).
          RUN SetCellValue ("K8",   b2-job-hdr.qty).
    
          FIND FIRST b-eb NO-LOCK
               WHERE b-eb.company   = job-hdr.company
                 AND b-eb.est-no    = job-hdr.est-no
                 AND b-eb.form-no   = job-mch.frm
                 AND b-eb.blank-no  = job-mch.blank-no
            NO-ERROR.
    
          FIND FIRST b-est-flm NO-LOCK
               WHERE b-est-flm.company  = job-hdr.company
                 AND b-est-flm.est-no   = job-hdr.est-no
                 AND b-est-flm.snum     = job-mch.frm
                 AND b-est-flm.bnum     = job-mch.blank-no
            NO-ERROR.
    
          FIND FIRST b-itemfg OF b2-job-hdr NO-LOCK NO-ERROR.
    
          FIND FIRST b-ef NO-LOCK
               WHERE b-ef.company   = b2-job-hdr.company
                 AND b-ef.est-no    = b2-job-hdr.est-no
                 AND b-ef.form-no   = b2-job-hdr.frm NO-ERROR.
    
          IF AVAIL b-eb THEN
          DO:
            IF icDept = 'WN' OR
               icDept = 'GL' OR
               icDept = 'SW' THEN
            DO:
            
              IF icDept = 'WN' THEN
              DO:
              
                IF AVAIL b-est-flm THEN DO: 
                    RUN SetCellValue ("B13",  GetItemName (b-est-flm.i-no)).
                    RUN SetItemInv(INPUT b-est-flm.company,
                                   INPUT b-est-flm.i-no, /*Item*/
                                   INPUT NO). /*film = no, case = yes*/
                END.
                IF AVAIL b-ef THEN
                  RUN SetCellValue ("B14",  b-ef.leaf-l[1] + 1).
              END.

              ELSE
                 IF icDept = 'GL' THEN
                 DO viIndex = 1 TO 4:
                    IF mach.dept[viIndex] EQ 'WN' THEN
                    DO:
                        IF AVAIL b-est-flm THEN DO:
                            RUN SetCellValue ("B13",  GetItemName (b-est-flm.i-no)).
                            RUN SetItemInv(INPUT b-est-flm.company,
                                      INPUT b-est-flm.i-no, /*Item*/
                                      INPUT NO). /*film = no, case = yes*/
                        END.
                       IF AVAIL b-ef THEN
                          RUN SetCellValue ("B14",  b-ef.leaf-l[1] + 1).

                       LEAVE.
                    END. 
                 END.
              
              IF AVAIL b-itemfg THEN
                  RUN SetCellValue ("K14",  b-itemfg.prod-notes).

              IF icDept <> 'SW' THEN DO:
                RUN SetCellValue ("G13",  GetItemName (b-eb.cas-no)).
                RUN SetItemInv(INPUT b-eb.company,
                               INPUT b-eb.cas-no, /*Item*/
                               INPUT YES). /*film = no, case = yes*/
                RUN SetCellValue("C10", b-eb.die-no).
              END.
              RUN SetCellValue ("G14",  b-eb.cas-cnt).
              RUN SetCellValue ("K15",  b-eb.cas-pal).
              RUN SetCellValue ("K13",  GetItemName (b-eb.tr-no)).
              RUN SetCellValue ("K16",  b-eb.tr-cnt).
            END.
          END.
      END.
  
    END CASE.
  END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillData Procedure 
PROCEDURE FillData :
/*------------------------------------------------------------------------------
  Purpose     : Populates the Sheet for the given Department.
  Parameters  : icDept - Machine Department
  Notes       :       
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icDept  AS CHAR NO-UNDO.

  /* Variables */
  DEF VARIABLE    viCount AS INTE NO-UNDO.

  RUN SetSheetName    (icDept).
  RUN SetSheetHeader  (icDept).
  RUN FGItems         (icDept).
  RUN RMItems         (icDept).
  
  IF job-mch.blank-no < 2 THEN
    ASSIGN  viOldLine   = viLine
            viOldLoop   = viLoop.
        
  IF icDept = 'GL' OR 
     icDept = 'WN' THEN DO viCount = 1 TO 4:
    IF CAN-DO ('GL,WN',mach.dept [viCount]) THEN DO:

      RUN SetDeptNotes    (vcRecKey, mach.dept [viCount], job-mch.frm, 1) NO-ERROR.
      vlWN2 = TRUE.
      RUN SetDeptNotes    (vcRecKey, mach.dept [viCount], job-mch.frm, 2) NO-ERROR.
    END.
  END.

  ELSE DO:
    RUN SetDeptNotes    (vcRecKey, icDept, job-mch.frm, 1) NO-ERROR.  
    RUN SetDeptNotes    ('',       icDept, job-mch.frm, 2) NO-ERROR.
  END.
  
  RELEASE OBJECT chWorkSheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetItemInfo Procedure 
PROCEDURE GetItemInfo :
/*------------------------------------------------------------------------------
  Purpose     : Extracts the Item Information.
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER vcPNum-2 AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAMETER vcJob-Qty-2 AS DEC NO-UNDO.

  FIND FIRST b2-job-hdr NO-LOCK 
       WHERE b2-job-hdr.company   = job-mch.company
         AND b2-job-hdr.job-no    = job-mch.job-no
         AND b2-job-hdr.job-no2   = job-mch.job-no2
         AND b2-job-hdr.frm       = job-mch.frm
         AND b2-job-hdr.blank-no  = job-mch.blank-no
             NO-ERROR.

  IF AVAIL b2-job-hdr THEN
  DO:
     FIND FIRST b-itemfg OF b2-job-hdr NO-LOCK NO-ERROR.
    
     FIND FIRST oe-ordl WHERE
          oe-ordl.company  EQ b2-job-hdr.company AND
          oe-ordl.i-no     EQ b2-job-hdr.i-no AND
          oe-ordl.ord-no   EQ b2-job-hdr.ord-no
          NO-LOCK NO-ERROR.
    
     IF AVAILABLE oe-ordl THEN
     DO:
        vcJob-Qty-2 = oe-ordl.qty.
        RELEASE oe-ordl.
     END.
     ELSE
        vcJob-Qty-2 = 0.
  END.

  IF AVAIL b-itemfg THEN
    ASSIGN 
      vcItemNo    = b-itemfg.i-no
      vcItemNm    = b-itemfg.i-name
      vcItemDesc  = b-itemfg.part-dscr1.
  ELSE
    ASSIGN
      vcItemNo    = ''
      vcItemNm    = ''
      vcItemDesc  = ''.

  FIND FIRST b-eb NO-LOCK
       WHERE b-eb.company   = b2-job-hdr.company
         AND b-eb.est-no    = b2-job-hdr.est-no
         AND b-eb.form-no   = b2-job-hdr.frm
         AND b-eb.blank-no  = b2-job-hdr.blank-no NO-ERROR.

    IF AVAIL b-eb THEN
      ASSIGN 
        vcPNum-2 = b-eb.part-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetJobQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetJobQty Procedure 
PROCEDURE GetJobQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND FIRST oe-ordl NO-LOCK 
       WHERE oe-ordl.company  EQ b2-job-hdr.company
         AND oe-ordl.i-no     EQ b2-job-hdr.i-no
         AND oe-ordl.ord-no   EQ b2-job-hdr.ord-no NO-ERROR.
    
  IF AVAILABLE oe-ordl THEN viJobQty = oe-ordl.qty.
                       ELSE viJobQty = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSheetQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSheetQty Procedure 
PROCEDURE GetSheetQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM iiSheetQty  AS DECI NO-UNDO.
  DEF INPUT PARAM icUOM       AS CHAR NO-UNDO.

  IF icUOM EQ "EA" THEN
    viSheetQty = iiSheetQty.
  ELSE  
    RUN sys/ref/convquom.p  (INPUT  icUOM, 
                             INPUT  "EA", 
                             INPUT  job-mat.basis-w,
                             INPUT  job-mat.len, 
                             INPUT  job-mat.wid, 
                             INPUT  job-mat.dep,
                             INPUT  iiSheetQty, 
                             OUTPUT viSheetQty).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose   :   Initializes Excel Environment
  Parameters:   None
  Notes     :   
------------------------------------------------------------------------------*/

  /* Capture the current active printer. */
  IF LvOutputSelection = "email" THEN
    ASSIGN 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
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
  
  /* Switch Printer to PDFCamp Printer. */
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter).

  /* Start a new session of Excel. */
  IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF NOT (VALID-HANDLE (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
/*   /* Set our current directory. */     */
/*   RUN UTIL/CurrDir.p (output CurrDir). */
  
  /* Set the Excel Template to be used. */
/*   ASSIGN chFile = CurrDir + "\Template\Indiana SheeterLS.xlt" no-error. */
  ASSIGN FILE-INFO:FILE-NAME = "Template\Indiana SheeterLS.xlt"
         chFile = FILE-INFO:FULL-PATHNAME.
         
  IF SEARCH (chFile) = ? THEN DO:
    MESSAGE 'Template File: ' "Template\Indiana SheeterLS.xlt" 
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
  END.
  /* Make Excel visible. */
  ASSIGN 
      chExcelApplication:VISIBLE = TRUE
      chExcelApplication:ScreenUpdating = FALSE.
  
  /* If we are going to E-Mail or Print, hide Excel. */
  IF LvOutputSelection = "Email"    OR 
     LvOutputSelection = "Printer"  THEN
    chExcelApplication:VISIBLE = FALSE.
  
  /* Clear tt-FileList. */
  EMPTY TEMP-TABLE tt-filelist.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InkData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InkData Procedure 
PROCEDURE InkData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Clear Ink Temp-Table. */
  EMPTY TEMP-TABLE tt-ink.
  
  viUnitNo = 0.

  /* Build Ink List. */
  FOR EACH b-job-mat NO-LOCK
     WHERE b-job-mat.company  = job-hdr.company
       AND b-job-mat.job      = job-hdr.job
       AND b-job-mat.frm      = job-hdr.frm,
      FIRST b-item NO-LOCK
     WHERE b-item.company     = b-job-mat.company
       AND b-item.i-no        = b-job-mat.i-no
       AND (b-item.mat-type   = 'I' OR
            b-item.mat-type   = 'V')
        BY b-job-mat.frm
        BY b-job-mat.blank-no
        BY b-item.i-no:
       
    
    FIND FIRST reftable
        WHERE reftable EQ "ce/v-est3.w Unit#"
          AND reftable.company EQ eb.company
          AND reftable.loc     EQ eb.est-no
          AND reftable.code    EQ STRING(eb.form-no,"9999999999")
          AND reftable.code2   EQ STRING(eb.blank-no,"9999999999")
        NO-LOCK NO-ERROR.

    FIND FIRST b-rt
        WHERE b-rt.reftable EQ "ce/v-est3.w Unit#1"
          AND b-rt.company  EQ eb.company
          AND b-rt.loc      EQ eb.est-no
          AND b-rt.code     EQ STRING(eb.form-no,"9999999999")
          AND b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
        NO-LOCK NO-ERROR.

    DO i = 1 TO 20:
      v-unit = IF i LE 12 AND AVAIL reftable THEN reftable.val[i]
               ELSE
               IF AVAIL b-rt                 THEN b-rt.val[i - 12]
                                             ELSE 0.

      IF eb.i-code2[i] EQ b-job-mat.i-no THEN DO:
        FIND FIRST tt-ink
            WHERE tt-ink.i-code   EQ eb.i-code2[i]
              AND tt-ink.form-no  EQ eb.form-no
              AND tt-ink.blank-no EQ eb.blank-no
              AND tt-ink.i-pass   EQ eb.i-ps2[i]
              AND tt-ink.i-unit   EQ v-unit
            NO-ERROR.

        IF NOT AVAIL tt-ink THEN DO:
          CREATE tt-ink.
          ASSIGN
           tt-ink.loc-bin  = GetBinLocs(b-job-mat.i-no)
           tt-ink.i-code   = eb.i-code2[i]
           tt-ink.q-onh    = b-item.q-onh
           tt-ink.form-no  = eb.form-no
           tt-ink.blank-no = eb.blank-no
           tt-ink.i-dscr   = eb.i-dscr2[i]
           tt-ink.i-pass   = eb.i-ps2[i]
           tt-ink.i-qty    = b-job-mat.qty
           tt-ink.i-unit   = v-unit
           tt-ink.i-name   = b-item.i-name
           tt-ink.rm-dscr  = b-item.i-dscr.
        END.
      END.
    END. /* loop i */

    FIND FIRST est 
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
        NO-LOCK NO-ERROR.

    FIND FIRST tt-ink
        WHERE tt-ink.i-code    EQ b-job-mat.i-no
          AND tt-ink.form-no   EQ b-job-mat.frm
          AND (tt-ink.blank-no EQ b-job-mat.blank-no OR
               (AVAIL est AND est.est-type EQ 4))
        NO-ERROR.

    IF NOT AVAIL tt-ink                              AND
       (b-job-mat.blank-no  EQ eb.blank-no OR
        (b-job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN DO:
      CREATE tt-ink.
      ASSIGN
       tt-ink.loc-bin  = GetBinLocs(b-job-mat.i-no)
       tt-ink.i-code   = b-job-mat.i-no
       tt-ink.q-onh    = b-item.q-onh
       tt-ink.form-no  = eb.form-no
       tt-ink.blank-no = eb.blank-no
       tt-ink.i-dscr   = b-item.est-dscr
       tt-ink.i-name   = b-item.i-name
       tt-ink.i-qty    = b-job-mat.qty
       tt-ink.rm-dscr  = b-item.i-dscr
       tt-ink.i-unit = 0 
       tt-ink.i-pass   = 1.
    END.
    
  /*     if avail tt-ink then tt-ink.i-qty = tt-ink.i-qty + b-job-mat.qty. */
  END.

  /* Start from the last line of the RM Items. */
  viLoop = 27 - (24 - viLine).

  FOR EACH tt-ink NO-LOCK
     WHERE tt-ink.i-code > ''
        BY tt-ink.i-unit:

    RUN SetCellValue ("A"  + string (viLoop),  tt-ink.rm-dscr).
    RUN SetCellValue ("C"  + string (viLoop),  tt-ink.i-name).
    RUN SetCellValue ("F"  + string (viLoop),  tt-ink.loc-bin).
    RUN SetCellValue ("I"  + string (viLoop),  tt-ink.q-onh).
    RUN SetCellValue ("J"  + string (viLoop),  tt-ink.i-unit).
    RUN SetCellValue ("K"  + string (viLoop),  STRING (tt-ink.i-qty, '->>,>>9.9999')).
    
    viLoop = viLoop + 1.
  END. /* each tt-ink */  
  
  ASSIGN viOldLoop = viLoop.

  /* If we did not use all the space for Inks, get rid of the extra lnes. */
  IF viLoop - (viLine + 3) < 9 THEN
  DO:
      vcRange = "A" + string (viLoop) + '..' + "M" + string (viLoop + 9 - (viLoop - (viLine + 3))).
      chWorkSheet:Range (vcRange):delete     NO-ERROR.
  END.

  RUN SetDeptNotes (vcRecKey, 'BL', job-mch.frm, 1).
  RUN SetDeptNotes (vcRecKey, 'VY', job-mch.frm, 1).
  RUN SetDeptNotes (vcRecKey, 'MC', job-mch.frm, 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose     : Main Loop for the Report Logic
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  /* Open our Excel Template. */  
  ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

  /*Force add-in to load that defines the bar code conversion formula*/
  chExcelApplication:Workbooks:OPEN("J:\Rcode\Template\BarCodeWizC128.xla") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    chExcelApplication:Workbooks:OPEN("C:\Program Files (x86)\BarCodeWiz Code 128 Fonts\Addins\BarCodeWizC128.xla") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    chExcelApplication:Workbooks:OPEN("C:\Program Files\BarCodeWiz Code 128 Fonts\Addins\BarCodeWizC128.xla") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  chExcelApplication:Workbooks:OPEN("C:\Program Files (x86)\BarCodeWiz Code 128 Fonts Trial\Addins\BarCodeWizC128.xla") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    chExcelApplication:Workbooks:OPEN("C:\Program Files\BarCodeWiz Code 128 Fonts Trial\Addins\BarCodeWizC128.xla") NO-ERROR.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = FALSE.

  /* Prepare the Sheets to be used. */
  RUN PrepareSheets.

  /* Process Selected Jobs. */
  RUN ProcessJobs.

  /* enable screen updating */
  chExcelApplication:ActiveWindow:VIEW = 3.
  chExcelApplication:ScreenUpdating = TRUE.
  /* Preview the Spreadsheet. */
/*   chWorkbook:PrintOut(1,999,1,YES,,NO,NO,,NO) no-error. */
/*                                                         */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepareSheets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepareSheets Procedure 
PROCEDURE PrepareSheets :
/*------------------------------------------------------------------------------
  Purpose   :   Prepare the sheets to be used by each Department.
  Parameters:   None
  Notes     :       
------------------------------------------------------------------------------*/

  /* Sheeter */
  IF ilRS 
    THEN RUN CreateSheets  ('RS').
    ELSE RUN DeleteSheet   ('RS').

  /* Printer */
  IF ilPR 
    THEN RUN CreateSheets  ('PR').
    ELSE RUN DeleteSheet   ('PR').

  /* Gluer */
  IF ilGL 
    THEN RUN CreateSheets  ('GL').
    ELSE RUN DeleteSheet   ('GL').

  /* Die Cut */
  IF ilDC 
    THEN RUN CreateSheets  ('DC').
    ELSE RUN DeleteSheet   ('DC').

  /* Window */
  IF ilGL 
    THEN RUN CreateSheets  ('WN').
    ELSE RUN DeleteSheet   ('WN').

  /* Shrink Wrap */
  IF ilSW 
    THEN RUN CreateSheets  ('SW').
    ELSE RUN DeleteSheet   ('SW').

  /* If no records exist, Quit. */
  IF viSheets = 0 THEN
  DO:
    RUN CleanUp.
    chExcelApplication:Quit() NO-ERROR.
    MESSAGE 'Not a single record exists to process.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'close' TO THIS-PROCEDURE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintNotes Procedure 
PROCEDURE PrintNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAM icDept AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAM iiType AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAM ip-notes-blank AS LOG NO-UNDO.
  DEFINE INPUT PARAM ip-last AS LOG NO-UNDO.

  IF vcDeptNotes > '' 
    THEN vcDeptNotes = FormatNotes (vcDeptNotes, 80).
    ELSE IF iiType = 2 THEN vcDeptNotes = ''.
    ELSE IF icDept = 'BL' OR 
            icDept = 'VY' OR 
            icDept = 'MC' THEN vcDeptNotes = ''.
    ELSE IF ip-last AND ip-notes-blank THEN vcDeptNotes = 'No Department Notes.'.
  CASE icDept:

    WHEN 'RS' THEN RUN SetCellValue ("A" + string (viLine + 15),  vcDeptNotes).
    WHEN 'PR' THEN RUN SetCellValue ("A" + string (viLoop +  7),  vcDeptNotes).
    WHEN 'DC' THEN RUN SetCellValue ("A" + string (viLine + 18),  vcDeptNotes).
    WHEN 'SW' THEN RUN SetCellValue ("A" + string (viLine + 12),  vcDeptNotes).
    WHEN 'GL' OR
    WHEN 'WN' THEN DO:
      IF iiType = 1 THEN 
        RUN SetCellValue ("A" + string (viLine + 12),  vcDeptNotes).
      ELSE DO:
        RUN SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
        vlWN2 = TRUE.
      END.
    END.
    
    WHEN 'QS' THEN RUN SetCellValue ("A" + string (viLine + 16),  vcDeptNotes).
    WHEN 'QP' THEN RUN SetCellValue ("A" + string (viLoop +  8),  vcDeptNotes).
    WHEN 'QD' THEN RUN SetCellValue ("A" + string (viLine + 19),  vcDeptNotes).
    WHEN 'QK' THEN RUN SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
    WHEN 'QW' OR
    WHEN 'QG' THEN DO:
      IF vlWN2 = TRUE THEN DO:
        RUN SetCellValue ("A" + string (viLine + 14),  vcDeptNotes).
        ASSIGN vlWN2  = FALSE.
/*                viLine = viLine + 1. */
      END.
      ELSE DO:
        RUN SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
      END.
    END.
    
    WHEN 'BL' THEN RUN SetCellValue ("C" + string (viLoop +  1),  vcDeptNotes).
    WHEN 'VY' THEN RUN SetCellValue ("G" + string (viLoop +  1),  vcDeptNotes).
    WHEN 'MC' THEN RUN SetCellValue ("J" + string (viLoop +  1),  vcDeptNotes).
    WHEN 'WB' THEN RUN SetCellValue ("M" + string (viLine     ),  vcDeptNotes).

   END CASE.

   ASSIGN   viLoop = viLoop + 1
            viLine = viLine + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessJobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessJobs Procedure 
PROCEDURE ProcessJobs :
/*------------------------------------------------------------------------------
  Purpose   :   Process all jobs within selected range.
  Parameters:   <None>
  Notes     :   <None>
------------------------------------------------------------------------------*/
  /* For each jobs within selected range: */
  FOR  EACH job-hdr NO-LOCK
      WHERE job-hdr.company    = cocode
        AND job-hdr.job-no    >= substring (icBegJobNo,1,6)
        AND job-hdr.job-no    <= substring (icEndJobNo,1,6)
        AND job-hdr.job-no2   >= iiBegJobNo2
        AND job-hdr.job-no2   <= iiEndJobNo2,
      FIRST eb NO-LOCK
      WHERE eb.company        = job-hdr.company
        AND eb.est-no         = job-hdr.est-no
        AND eb.form-no        = job-hdr.frm,
      FIRST ef NO-LOCK
      WHERE ef.company        = job-hdr.company
        AND ef.est-no         = job-hdr.est-no,
      FIRST cust NO-LOCK
      WHERE cust.company     = job-hdr.company
        AND cust.cust-no     = job-hdr.cust-no
/*       first oe-ord no-lock                      */
/*       where oe-ord.company    = job-hdr.company */
/*         and oe-ord.est-no     = job-hdr.est-no  */
      BREAK
         BY job-hdr.job-no
         BY job-hdr.job-no2
         BY job-hdr.frm:

    /* Group by Form : */
    IF FIRST-OF (job-hdr.frm) THEN
    DO:
      FIND FIRST job NO-LOCK
           WHERE job.company  = job-hdr.company
             AND job.job      = job-hdr.job NO-ERROR.

      IF AVAIL job THEN vcRecKey = job.rec_key.
                   ELSE vcRecKey = ?.

      /* Set the Job-No. */
      vcJobNo = job-hdr.job-no + '-' +  string (job-hdr.job-no2, '99') + '-' +
                                        string (job-hdr.frm, '99').

      /* Get the last Blank-No to determine # of Items. */
      FIND LAST b-job-hdr NO-LOCK
          WHERE b-job-hdr.company = job-hdr.company
            AND b-job-hdr.job-no  = job-hdr.job-no
            AND b-job-hdr.job-no2 = job-hdr.job-no2
            AND b-job-hdr.frm     = job-hdr.frm NO-ERROR.
  
      /* Go through all the machines of the selected Job. */
      FOR EACH job-mch NO-LOCK
         WHERE job-mch.company   = job-hdr.company
           AND job-mch.job       = job-hdr.job
           AND job-mch.job-no    = job-hdr.job-no
           AND job-mch.job-no2   = job-hdr.job-no2
           AND job-mch.frm       = job-hdr.frm
           USE-INDEX line-idx,
         FIRST mach NO-LOCK
               {sys/ref/machW.i}
           AND mach.m-code EQ job-mch.m-code
         BREAK
            BY job-mch.job-no
            BY job-mch.job-no2
            BY job-mch.frm
            BY job-mch.blank-no
            BY job-mch.dept:

        /* Get the description of Window item. */
        FIND FIRST est-flm NO-LOCK
             WHERE est-flm.company = job-hdr.company
               AND est-flm.est-no  = job-hdr.est-no NO-ERROR.
            
        /* Process the selected Departments. */
        CASE job-mch.dept:
  
          /* Sheeter */
          WHEN 'RS' THEN IF ilRS THEN RUN FillData ('RS').
  
          /* Printer */
          WHEN 'PR' THEN IF ilPR THEN RUN FillData ('PR').
  
          /* Gluer */
          WHEN 'GL' THEN IF ilGL THEN RUN FillData ('GL').

          /* Die Cutter */
          WHEN 'DC' THEN IF ilDC THEN RUN FillData ('DC').
  
          /* Window */
          WHEN 'WN' THEN IF ilGL THEN RUN FillData ('WN').
              
          /* Shrink Wrap */
          WHEN 'SW' THEN IF ilSW THEN RUN FillData ('SW').

        END CASE.
      END. /* each job-mch */
    END. /* first-of job-hdr.frm */
  END. /* each job-hdr */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RMItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RMItems Procedure 
PROCEDURE RMItems :
/*------------------------------------------------------------------------------
     Purpose:   Populates the RM Items for the Dept.
  Parameters:   None
       Notes:       
------------------------------------------------------------------------------*/
    
  /* Parameters */
  DEF INPUT PARAM icDept  AS CHAR NO-UNDO.

  IF job-mch.blank-no LT 2 THEN 
  DO:

    viLine = 13.
  
    IF icDept = 'RS' OR
       icDept = 'PR' OR
       icDept = 'DC' THEN
    DO:
    
/*       for each mat-act no-lock                                                          */
/*          where mat-act.job-no   = job-mch.job-no                                        */
/*            and mat-act.job-no2  = job-mch.job-no2                                       */
/*            and mat-act.s-num    = job-mch.frm                                           */
/*            and mat-act.b-num    = job-mch.blank-no,                                     */
/*                                                                                         */
/*          first item of mat-act                                                          */
/*          where item.mat-type = 'B',                                                     */
/*                                                                                         */
/*          first job-mat no-lock                                                          */
/*          where job-mat.company  = mat-act.company                                       */
/*            and job-mat.job-no   = mat-act.job-no                                        */
/*            and job-mat.job-no2  = mat-act.job-no2                                       */
/*            and job-mat.frm      = mat-act.s-num                                         */
/*            and job-mat.i-no     = mat-act.rm-i-no:                                      */
/*                                                                                         */
/*         if viLine < 25 then                                                             */
/*         do:                                                                             */
/*           run SetCellValue ("A" + string (viLine), mat-act.rm-i-no)   no-error.         */
/*           run SetCellValue ("C" + string (viLine), mat-act.loc-bin)   no-error.         */
/*           run SetCellValue ("D" + string (viLine), mat-act.tag)       no-error.         */
/*           run SetCellValue ("F" + string (viLine), item.i-dscr)       no-error.         */
/*           run SetCellValue ("H" + string (viLine), item.cal)          no-error.         */
/*           run SetCellValue ("I" + string (viLine), item.r-wid)        no-error.         */
/*           run SetCellValue ("J" + string (viLine), job-mat.len)       no-error.         */
/*                                                                                         */
/* /*           run GetSheetQty (mat-act.qty). */                                          */
/*                                                                                         */
/*           if icDept = 'RS'                                                              */
/*             then run SetCellValue ("L" + string (viLine), mat-act.qty * .99)  no-error. */
/*             else run SetCellValue ("K" + string (viLine), viSheetQty * .99)  no-error.  */
/*         end.                                                                            */
/*                                                                                         */
/*         viLine = viLine + 1.                                                            */
/*                                                                                         */
/*       end.                                                                              */
    
      FOR EACH rm-rctd NO-LOCK
         WHERE rm-rctd.company    = job-mch.company
           AND rm-rctd.rita-code  = 'I'
           AND rm-rctd.job-no     = job-mch.job-no
           AND rm-rctd.job-no2    = job-mch.job-no2
           AND rm-rctd.s-num      = job-mch.frm,
         FIRST item OF rm-rctd NO-LOCK
         WHERE item.mat-type = 'B',
         FIRST job-mat NO-LOCK
         WHERE job-mat.company    = job-hdr.company
           AND job-mat.job-no     = job-hdr.job-no
           AND job-mat.job-no2    = job-hdr.job-no2
           AND job-mat.frm        = job-hdr.frm
           AND job-mat.i-no       = item.i-no
            BY rm-rctd.s-num:
      
        IF viLine < 25 THEN
        DO: 

          RUN SetCellValue ("A" + string (viLine), rm-rctd.i-no)        NO-ERROR.
          RUN SetCellValue ("C" + string (viLine), rm-rctd.loc-bin)     NO-ERROR.
          RUN SetCellValue ("D" + string (viLine), rm-rctd.tag)         NO-ERROR.
          RUN SetCellValue ("F" + string (viLine), item.i-dscr)         NO-ERROR.
          RUN SetCellValue ("H" + string (viLine), item.cal)            NO-ERROR.
          RUN SetCellValue ("I" + string (viLine), DispDim ('W'))       NO-ERROR.
          RUN SetCellValue ("J" + string (viLine), DispDim ('L'))       NO-ERROR.
      
          RUN GetSheetQty (rm-rctd.qty,
                           rm-rctd.pur-uom).

          IF icDept = 'RS' 
            THEN RUN SetCellValue ("L" + string (viLine), viSheetQty * .99)  NO-ERROR.
            ELSE RUN SetCellValue ("K" + string (viLine), viSheetQty * .99)  NO-ERROR.
        END.
        
        viLine = viLine + 1.
      END.

      
      FOR EACH rm-rcpth NO-LOCK
         WHERE rm-rcpth.company   = job-mch.company
           AND rm-rcpth.job-no    = job-mch.job-no
           AND rm-rcpth.job-no2   = job-mch.job-no2,
          EACH rm-rdtlh OF rm-rcpth NO-LOCK,
         FIRST item OF rm-rcpth NO-LOCK
         WHERE item.mat-type = 'B',
         FIRST job-mat NO-LOCK
         WHERE job-mat.company  = job-hdr.company
           AND job-mat.job-no   = job-hdr.job-no
           AND job-mat.job-no2  = job-hdr.job-no2
           AND job-mat.frm      = job-hdr.frm
           AND job-mat.i-no     = item.i-no:
        IF viLine < 25 THEN
        DO: 

          RUN SetCellValue ("A" + string (viLine), rm-rcpth.i-no)       NO-ERROR.
          RUN SetCellValue ("C" + string (viLine), rm-rdtlh.loc-bin)    NO-ERROR.
          RUN SetCellValue ("D" + string (viLine), rm-rdtlh.tag)        NO-ERROR.
          RUN SetCellValue ("F" + string (viLine), item.i-dscr)         NO-ERROR.
          RUN SetCellValue ("H" + string (viLine), item.cal)            NO-ERROR.
          RUN SetCellValue ("I" + string (viLine), item.r-wid)          NO-ERROR.
          RUN SetCellValue ("J" + string (viLine), job-mat.len)         NO-ERROR.
      
          RUN GetSheetQty (rm-rdtlh.qty,
                           rm-rcpth.pur-uom).

          IF icDept = 'RS' 
            THEN RUN SetCellValue ("L" + string (viLine), viSheetQty * .99)  NO-ERROR.
            ELSE RUN SetCellValue ("K" + string (viLine), viSheetQty * .99)  NO-ERROR.
        END.
        
        viLine = viLine + 1.
      END.

      IF viLine > 13 AND 
         viLine < 25 THEN
      DO:
        vcRange = "A" + string (viLine) + '..' + "N24".
        chWorkSheet:Range (vcRange):delete     NO-ERROR.
      END.
      ELSE
        viLine = 25.

      IF job-mch.dept EQ 'PR' THEN  RUN InkData.
      IF job-mch.dept EQ 'DC' THEN  RUN DieData.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCellValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCellValue Procedure 
PROCEDURE SetCellValue :
/*------------------------------------------------------------------------------
  Purpose   : Positions to a cell and set its value.
  Parameters: icPosition - Cell Position
              icCellValue - Cell Value
  Notes     :       
------------------------------------------------------------------------------*/
  
  /* Parameters */
  DEF INPUT PARAM icPosition  AS CHAR NO-UNDO.
  DEF INPUT PARAM icCellValue AS CHAR NO-UNDO.

  /* Go to the Cell Position and Set its value. */
  chWorkSheet:Range(icPosition):value = icCellValue NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetDeptNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDeptNotes Procedure 
PROCEDURE SetDeptNotes :
/*------------------------------------------------------------------------------
    Purpose   : Captures Notes for a given Department.
    Parameters: icRecKey  - Job Rec Key
                icDept    - Dept Code
                iiForm    - Form
      Notes   :  
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icRecKey  AS CHAR NO-UNDO.
  DEF INPUT PARAM icDept    AS CHAR NO-UNDO.
  DEF INPUT PARAM iiForm    AS INTE NO-UNDO.
  DEF INPUT PARAM iiType    AS INTE NO-UNDO.

  DEFINE VARIABLE viEntry   AS INTE NO-UNDO.
  
  DEF VAR lv-notes-blank AS LOG NO-UNDO.

  ASSIGN vcDeptNotes  = "".

  IF iiType = 2 THEN
  DO:
    CASE icDept:
      WHEN 'RS' THEN icDept = 'QS'.
      WHEN 'PR' THEN icDept = 'QP'.
      WHEN 'DC' THEN icDept = 'QD'.
      WHEN 'GL' THEN icDept = 'QG'.
      WHEN 'WN' THEN icDept = 'QW'.
      WHEN 'SW' THEN icDept = 'QK'.
    END CASE.

    FIND FIRST b2-job-hdr NO-LOCK 
         WHERE b2-job-hdr.company = job-hdr.company
           AND b2-job-hdr.job-no  = job-hdr.job-no
           AND b2-job-hdr.job-no2 = job-hdr.job-no2
           AND b2-job-hdr.frm     = job-hdr.frm NO-ERROR.
    FIND FIRST b-itemfg OF b2-job-hdr NO-LOCK NO-ERROR.
    IF AVAIL b-itemfg THEN icRecKey = b-itemfg.rec_key.
  END.

  FOR EACH notes NO-LOCK
     WHERE notes.rec_key = icRecKey 
       AND CAN-DO(icDept,notes.note_code)
       AND notes.note_form_no = IF iiType = 1 THEN iiForm
                                              ELSE notes.note_form_no

        BY notes.note_form_no 
        BY notes.note_date 
        BY notes.note_time:
    
    lv-notes-blank = YES.

    IF (job-mch.blank-no > 1 AND NOT iiType = 2) OR
       (icDept = 'BL' OR 
        icDept = 'VY' OR 
        icDept = 'MC') THEN
      ASSIGN  viLine  = viOldLine
              viLoop  = viOldLoop.

    DO viEntry = 1 TO NUM-ENTRIES (notes.note_text, CHR(10)):
      vcDeptNotes = ENTRY (viEntry, notes.note_text, CHR(10)).
      
      IF vcDeptNotes NE "" THEN
         lv-notes-blank = NO.

      RUN PrintNotes (icDept, iiType, lv-notes-blank, viEntry EQ num-entries (notes.note_text, CHR(10))).
    END.
  END. /* FOR EACH notes */

  IF vcDeptNotes = '' THEN DO:
  
    FOR EACH notes NO-LOCK
       WHERE notes.rec_key = icRecKey 
         AND CAN-DO(icDept,notes.note_code)
         AND notes.note_form_no = 0
          BY notes.note_form_no 
          BY notes.note_date 
          BY notes.note_time:
      
      lv-notes-blank = YES.

      IF (job-mch.blank-no > 1 AND NOT iiType = 2) OR
         (icDept = 'BL' OR 
          icDept = 'VY' OR 
          icDept = 'MC') THEN
      ASSIGN  viLine  = viOldLine
              viLoop  = viOldLoop.

      DO viEntry = 1 TO NUM-ENTRIES (notes.note_text, CHR(10)):
        vcDeptNotes = ENTRY (viEntry, notes.note_text, CHR(10)).
        
        IF vcDeptNotes NE "" THEN
           lv-notes-blank = NO.

        RUN PrintNotes (icDept, iiType, lv-notes-blank, viEntry EQ num-entries (notes.note_text, CHR(10)) ).
      END.
    END. /* FOR EACH notes */
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetItemInv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetItemInv Procedure 
PROCEDURE SetItemInv :
/*------------------------------------------------------------------------------
  Purpose:     Builds the mini inventory table for film and cases
  Parameters:  company, item number, no = film / yes = case
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplCase AS LOGICAL NO-UNDO.

DEFINE BUFFER bf-item FOR ITEM.
DEFINE BUFFER bf-rm-bin FOR rm-bin.
DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.

DEFINE VARIABLE iLine AS INTEGER     NO-UNDO.

EMPTY TEMP-TABLE ttRmBin.

FIND FIRST bf-item 
    WHERE bf-item.company EQ ipcCompany
      AND bf-item.i-no EQ ipcINo
    NO-LOCK NO-ERROR.
IF AVAIL bf-item THEN DO:
    iLine = 0.
    FOR EACH bf-rm-bin
        WHERE bf-rm-bin.company EQ bf-item.company
          AND bf-rm-bin.i-no EQ bf-item.i-no
          AND bf-rm-bin.qty GT 0
        NO-LOCK:
        CREATE ttRmBin.
        BUFFER-COPY bf-rm-bin TO ttRmBin.
        RUN rm/GetRmBinAgeDate(INPUT ROWID(bf-rm-bin),
                               OUTPUT ttRmBin.rct-date).
    END.
    FOR EACH ttRmBin NO-LOCK
        BY ttRmBin.rct-date
        BY ttRmBin.rec_key:
        iLine = iLine + 1.
        IF iLine LE 6 THEN
            IF iplCase THEN DO:
/*                 RUN SetCellValue ("E" + STRING(iLine + 17), ttRmBin.loc). */
                RUN SetCellValue ("F" + STRING(iLine + 17), ttRmBin.loc-bin).
                RUN SetCellValue ("G" + STRING(iLine + 17), ttRmBin.tag).
                RUN SetCellValue ("H" + STRING(iLine + 17), STRING(ttRmBin.qty)).
            END.
            ELSE DO:
/*                 RUN SetCellValue ("A" + STRING(iLine + 17), ttRmBin.loc). */
                RUN SetCellValue ("A" + STRING(iLine + 17), ttRmBin.loc-bin).
                RUN SetCellValue ("B" + STRING(iLine + 17), ttRmBin.tag).
                RUN SetCellValue ("C" + STRING(iLine + 17), STRING(ttRmBin.qty)).
            END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSheetHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSheetHeader Procedure 
PROCEDURE SetSheetHeader :
/*------------------------------------------------------------------------------
  Purpose     : Sets the Sheet's Header Data.
  Parameters  : icDept - Machine Dept
  Notes       : None
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icDept AS CHAR NO-UNDO.

  /* Common data. */
  RUN SetCellValue ("B1",   job-mch.m-code).
  RUN SetCellValue ("N1",   job-mch.speed).
  RUN SetCellValue ("B2",   mach.m-dscr).
  RUN SetCellValue ("L1",   vcJobNo).
  RUN SetCellValue ("L2",   job-hdr.est-no).
  RUN SetCellValue ("C4",   TODAY).
  RUN SetCellValue ("H4",   cust.name).

  /* Blank No unique to GL/WN and SW Sheets */
  IF icDept = 'GL' OR
     icDept = 'WN' OR
     icDept = 'SW' THEN
  RUN SetCellValue ("L3",   job-mch.blank-no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSheetName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSheetName Procedure 
PROCEDURE SetSheetName :
/*------------------------------------------------------------------------------
  Purpose   :   Sets the Sheet Name of the Active Sheet.
  Parameters:   icDept - Machine Department
  Notes     :   None
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM icDept  AS CHAR NO-UNDO.

  /* Set the WorkSheet Name based on Dept. */ 
  CASE icDept:
    WHEN 'RS' THEN  viSheetNo = job-mch.frm.
    WHEN 'PR' THEN  viSheetNo = viRS + job-mch.frm.
    WHEN 'GL' THEN
    DO:  
      viGLSeq   = viGLSeq + 1.
      viSheetNo = viRS + viPR + viGLSeq.
    END.
    WHEN 'DC' THEN  viSheetNo = viRS + viPR + viGL + job-mch.frm.
    WHEN 'WN' THEN
    DO:  
      viWNSeq = viWNSeq + 1.
      viSheetNo = viRS + viPR + viGL + viDC + viWNSeq.
    END.
    WHEN 'SW' THEN  viSheetNo = viRS + viPR + viGL + viDC + viWN + job-mch.frm.
  END CASE.
  
  /* Go to the Sheet we need to work on. */
  chWorkSheet = chExcelApplication:Sheets:item (viSheetNo)  NO-ERROR.

  /* If it's GL or WN, combine the Dept, Form and Blank No for the Sheet Name. */
  IF icDept = 'GL' OR
     icDept = 'WN' THEN
    chWorkSheet:name  = icDept + '-' + string (job-mch.frm, '99') + '-' + string (job-mch.blank-no, '99') NO-ERROR.

  /* Otherwise, we just need the Dept and the Form. */
  ELSE
    chWorkSheet:name  = icDept + '-' + string (job-mch.frm, '99') NO-ERROR.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(viSheetNo):Activate NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-DispDim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DispDim Procedure 
FUNCTION DispDim RETURNS DECIMAL
  ( INPUT ip-dim AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR     ld-dim      AS DEC NO-UNDO.
  DEF VAR     v-wid-num   AS DEC NO-UNDO.
  DEF VAR     v-len-num   AS DEC NO-UNDO.

  DEF BUFFER  b-jm        FOR job-mat .

  IF AVAIL rm-rctd THEN DO:

     FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                          AND po-ordl.po-no     EQ int(rm-rctd.po-no)
                          AND po-ordl.i-no      EQ rm-rctd.i-no
                          AND po-ordl.job-no    EQ rm-rctd.job-no
                          AND po-ordl.job-no2   EQ rm-rctd.job-no2
                          AND po-ordl.item-type EQ YES
                          AND po-ordl.s-num     EQ rm-rctd.s-num
     NO-LOCK NO-ERROR.

     IF AVAIL po-ordl THEN
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
     ELSE DO:
        IF rm-rctd.job-no NE "" THEN
           FIND FIRST b-jm WHERE b-jm.company EQ cocode
                             AND b-jm.rm-i-no EQ rm-rctd.i-no
                             AND b-jm.job-no  EQ rm-rctd.job-no
                             AND b-jm.job-no2 EQ rm-rctd.job-no2
                             AND b-jm.frm     EQ rm-rctd.s-num
                             NO-LOCK NO-ERROR.
        IF AVAIL b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        ELSE DO:
           FIND FIRST ITEM WHERE item.company EQ cocode
                             AND item.i-no    EQ rm-rctd.i-no
                             NO-LOCK NO-ERROR.
           IF AVAIL item THEN
              IF item.r-wid EQ 0 THEN
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        END.
    END.
      
    IF ip-dim = "W" THEN    ld-dim = v-wid-num.
                    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
   
  END.
  
  RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatNotes Procedure 
FUNCTION FormatNotes RETURNS CHARACTER
  ( INPUT v-text  AS CHAR,
    INPUT v-len   AS INT):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /* Variables */
  DEF VAR vreturn AS CHAR NO-UNDO.
  DEF VAR ventry  AS CHAR NO-UNDO.

  ASSIGN v-text = REPLACE(v-text,CHR(10),"`")
         v-text = REPLACE(v-text,CHR(13)," ").
                   
  DO i = 1 TO NUM-ENTRIES(v-text,"`"):

     ASSIGN ventry = ENTRY(i,v-text,"`").

     DO WHILE TRUE:
     
        IF LENGTH(ventry) < v-len THEN DO:

           vreturn = vreturn + (IF vreturn <> "" THEN "`" 
                                                 ELSE "") 
                             + ventry.
           LEAVE.
        END.   
                        
        ASSIGN vreturn = vreturn + (IF vreturn <> "" THEN "`" 
                                                     ELSE "") 
                                 + SUBSTRING(ventry,1,v-len)

               ventry = SUBSTRING(ventry,v-len + 1).

     END. /* DO WHILE TRUE: */
  END. /* DO i = 1 TO NUM-ENTRIES(v-text,"`"): */

  RETURN vreturn.
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBinLocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetBinLocs Procedure 
FUNCTION GetBinLocs RETURNS CHARACTER
  ( INPUT icItemNo  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  ASSIGN 
      vcBinLocs = ''
      viCount   = 0.

  FOR EACH rm-bin NO-LOCK
     WHERE rm-bin.company = b-eb.company
       AND rm-bin.i-no    = icItemNo
       AND rm-bin.qty     > 0:

    ASSIGN 
      vcBinLocs = vcBinLocs + ', ' + rm-bin.loc-bin + ' ' + trim (STRING (rm-bin.qty, '->>>>>>>9'))
      vcBinLocs = LEFT-TRIM (vcBinLocs, ',')
      viCount   = viCount + 1.

    IF viCount GE 3 THEN LEAVE.
  END.
    
  RETURN vcBinLocs.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetItemName Procedure 
FUNCTION GetItemName RETURNS CHARACTER
  (INPUT icItemNo AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Converts an Item No to Item Name.
    Notes:  
------------------------------------------------------------------------------*/

  /* Lookup the Item based on Item No given. */
  FIND FIRST b-item NO-LOCK
       WHERE b-item.company = cocode
         AND b-item.i-no    = icItemNo NO-ERROR.

  /* If found return the Item Name, otherwise, return blank. */
  IF AVAIL b-item THEN RETURN b-item.i-name.
                  ELSE RETURN ''.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLocBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLocBin Procedure 
FUNCTION GetLocBin RETURNS CHARACTER
  (INPUT icDieNo  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the Loc Bin for the Die No.
    Notes:  
------------------------------------------------------------------------------*/

  /* Variables */
  DEF VAR vcLocBin  AS CHAR NO-UNDO.

  IF AVAIL b-itemfg THEN
    FIND FIRST prep NO-LOCK
         WHERE prep.company = b-itemfg.company
           AND prep.code    = b-itemfg.die-no NO-ERROR.

  IF AVAIL prep THEN  vcLocBin = prep.loc-bin.
                ELSE  vcLocBin = ''.

  RETURN vcLocBin.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

