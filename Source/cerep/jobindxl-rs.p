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
def input param icBegJobNo        as char no-undo.
def input param icEndJobNo        as char no-undo.
def input param iiBegJobNo2       as inte no-undo.
def input param iiEndJobNo2       as inte no-undo.
def input param ilRS              as log  no-undo.
def input param ilPR              as log  no-undo.
def input param ilDC              as log  no-undo.
def input param ilGL              as log  no-undo.
def input param ilSW              as log  no-undo.

/* Buffers */
def buffer b-job-hdr  for job-hdr.
def buffer b2-job-hdr for job-hdr.
def buffer b-job-mat  for job-mat.
def buffer b-eb       for eb.
def buffer b2-eb      for eb.
def buffer b-ef       for ef.
def buffer b-job-mch  for job-mch.
def buffer b-itemfg   for itemfg.
def buffer b-item     for item.
def buffer b-rt       for reftable.
def buffer b-est-flm  for est-flm.
DEF BUFFER b-prep     FOR prep.
DEF BUFFER b-reftable FOR reftable.

/* Includes */
{sys/inc/var.i shared}

/* Temp-Tables */
def temp-table tt-ink NO-UNDO
         field form-no   as inte
         field blank-no  as inte
         field i-pass    as inte
         field i-unit    as inte
         field i-code    as char
         field i-dscr    as char
         field i-name    as char
         field loc-bin   as char
         field q-onh     as inte
         field i-seq     as inte
         field i-qty     as deci format '->>,>>9.9999'
         field rm-dscr   AS CHAR 
         index i-code    i-code
         index i-unit    i-unit.

DEFINE TEMP-TABLE ttRmBin NO-UNDO LIKE rm-bin
    FIELD rct-date      LIKE rm-rcpth.trans-date
    .


/* Variables */
def var viLoop      as inte no-undo.
def var vcMachines  as char no-undo.
def var vcRMItem    as char no-undo.
def var vcJobNo     as char no-undo.
def var vlPage2     as log  no-undo init false.
def var vlSheet2    as log  no-undo init false.
def var viNumUp     as inte no-undo.
def var viSheets    as inte no-undo.
def var viRS        as inte no-undo.
def var viPR        as inte no-undo.
def var viDC        as inte no-undo.
def var viGL        as inte no-undo.
def var viWN        as inte no-undo.
def var viSW        as inte no-undo.
def var viSheetNo   as inte no-undo.
def var viLine      as inte no-undo.
def var viGLSeq     as inte no-undo.
def var viWNSeq     as inte no-undo.
def var vcItemNo    as char no-undo.
def var vcItemNm    as char no-undo.
def var vcItemDesc  as char no-undo.
def var viJobQty    as inte no-undo.
def var vcDeptNotes as char no-undo.
def var vcRecKey    as char no-undo.
def var viUnitNo    as inte no-undo.
def var vcBinLocs   as char no-undo.
def var viCount     as inte no-undo.
def var vcRange     as char no-undo.
def var vlWN2       as log  no-undo.
def var viSheetQty  as DECI no-undo.
def var viOldLine   as inte no-undo.
def var viOldLoop   as inte no-undo.
DEF VAR v-unit AS INT NO-UNDO.

/* VARIABLE FOR EXCEL OUTPUT */
DEFINE new SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE new SHARED VARIABLE CallingParameter     AS CHAR NO-UNDO.

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
DEFINE            VARIABLE LvCtr                as int          no-undo.
DEFINE            VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE            VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
define            variable CommandString        AS CHAR         NO-UNDO.
define            variable WshNetwork           as com-handle.
DEFINE            VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT no.

/* Build a Table to keep sequence of pdf files */
DEFINE new SHARED TEMP-TABLE tt-filelist NO-UNDO
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
  ( INPUT ip-dim AS char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FormatNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatNotes Procedure 
FUNCTION FormatNotes RETURNS character
  ( input v-text  as char,
    input v-len   as int) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBinLocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBinLocs Procedure 
FUNCTION GetBinLocs RETURNS CHARACTER
  ( input icItemNo  as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetItemName Procedure 
FUNCTION GetItemName RETURNS CHARACTER
  (input icItemNo as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLocBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetLocBin Procedure 
FUNCTION GetLocBin RETURNS CHARACTER
  (input icDieNo  as char)  FORWARD.

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

run InitializeExcel.
run MainLoop.
run Cleanup.

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
  if job-mch.dept <> 'GL' then
  do:

    /* For RS, Add up it's Blanks' Up Value. */
    if job-mch.dept = 'RS' then
    do:
      for  each b-eb no-lock
          where b-eb.company   = job-hdr.company
            and b-eb.est-no    = job-hdr.est-no
            and b-eb.form-no   = job-hdr.frm:
        viNumUp = viNumUP + b-eb.num-up.
      end.
    end.

    /* For the others, just capture the Blank's Up Value. */
    else
    do:
      for first b-eb no-lock
          where b-eb.company   = b2-job-hdr.company
            and b-eb.est-no    = b2-job-hdr.est-no
            and b-eb.form-no   = b2-job-hdr.frm
            and b-eb.blank-no  = b2-job-hdr.blank-no:
        viNumUp = b-eb.num-up.
      end.
    end.
  end.

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
  os-delete value(v-dir + "CardBoards.pdf").

  /* Set the PDF Merging Utility. */
  assign CommandString = CurrDir + "\util\pdftk ".
  
  /* Add the PDF Filenames to be merged to the command string.  */
  FOR EACH tt-filelist :
    assign CommandString = CommandString + " " + tt-FileName .
  END.
  
  /* Indicate the new filename of combined PDF File. */
  assign CommandString = CommandString + " cat output " + v-dir + "CardBoards.pdf".
  
  /* Merge the PDF Files. */

  os-command silent value(CommandString).
  
  /* Delete the "old" PDF Files. */
  FOR EACH tt-filelist :
      os-delete value(tt-FileName).
  END.
  
  chExcelApplication:ScreenUpdating = TRUE.

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
  def input param icDept    as char no-undo.

  for  each job-hdr no-lock
      where job-hdr.company    = cocode
        and job-hdr.job-no    >= substring (icBegJobNo,1,6)
        and job-hdr.job-no    <= substring (icEndJobNo,1,6)
        and job-hdr.job-no2   >= iiBegJobNo2
        and job-hdr.job-no2   <= iiEndJobNo2
      break
         by job-hdr.job-no
         by job-hdr.job-no2
         by job-hdr.frm:

    if first-of (job-hdr.frm) then
    do:

      for first job-mch no-lock
          where job-mch.company   = job-hdr.company
            and job-mch.job       = job-hdr.job
            and job-mch.job-no    = job-hdr.job-no
            and job-mch.job-no2   = job-hdr.job-no2
            and job-mch.frm       = job-hdr.frm
            and job-mch.dept      = icDept 
                use-index line-idx:

        case icDept:

          when 'RS' then
            assign 
              viSheetNo = job-hdr.frm
              viRS      = viRS + 1.

          when 'PR' then
            assign
              viSheetNo = viRS + 1
              viPR      = viPR + 1.

          when 'GL' then
          do:
            for each b-job-mch no-lock
               where b-job-mch.company = job-mch.company
                 and b-job-mch.job     = job-mch.job
                 and b-job-mch.job-no  = job-mch.job-no
                 and b-job-mch.job-no2 = job-mch.job-no2
                 and b-job-mch.frm     = job-mch.frm
                 and b-job-mch.dept    = icDept 
                     use-index line-idx:
              assign
                viSheetNo = viRS + viPR + 1
                viGL = viGL + 1.
              chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) no-error.
            end.
          end.

          when 'DC' then
            assign
              viSheetNo = viRS + viPR + viGL + 1
              viDC = viDC + 1.

          when 'WN' then
          do:
            for each b-job-mch no-lock
               where b-job-mch.company = job-mch.company
                 and b-job-mch.job     = job-mch.job
                 and b-job-mch.job-no  = job-mch.job-no
                 and b-job-mch.job-no2 = job-mch.job-no2
                 and b-job-mch.frm     = job-mch.frm
                 and b-job-mch.dept    = icDept 
                     use-index line-idx:
              assign
                viSheetNo = viRS + viPR + viGL + viDC + 1
                viWN = viWN + 1.
              chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) no-error.
            end.
          end.

          when 'SW' then
            assign
              viSheetNo = viRS + viPR + viGL + viDC + viWN + 1
              viSW = viSW + 1.

        end case.

        if not icDept = 'GL' and
           not icDept = 'WN' then
          chWorkbook:WorkSheets(viSheetNo):copy (chExcelApplication:Sheets:item(viSheetNo)) no-error.

        viSheets = viRS + viPR + viGL + viDC + viWN + viSW.
      end.
    end.
  end.
  
  /* Delete the Master Sheet. */
  chWorkbook:WorkSheets(viSheets + 1):Activate         no-error.
  chWorkbook:WorkSheets(viSheets + 1):delete()         no-error.

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
  def input param icDept        as char no-undo.

  case icDept:

    when 'RS' then
    do:
      chWorkbook:WorkSheets(1):Activate   no-error.
      chWorkbook:WorkSheets(1):delete()   no-error.
    end.

    when 'PR' then
    do:
      chWorkbook:WorkSheets(viRS + 1):Activate  no-error.
      chWorkbook:WorkSheets(viRS + 1):delete()  no-error.
    end.

    when 'GL' then
    do:
      chWorkbook:WorkSheets(viRS + viPR + 1):Activate   no-error.
      chWorkbook:WorkSheets(viRS + viPR + 1):delete()   no-error.
    end.

    when 'DC' then
    do:
      chWorkbook:WorkSheets(viRS + viPR + viGL + 1):Activate   no-error.
      chWorkbook:WorkSheets(viRS + viPR + viGL + 1):delete()   no-error.
    end.

    when 'WN' then
    do:
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + 1):Activate   no-error.
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + 1):delete()   no-error.
    end.

    when 'SW' then
    do:
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + viWN + 1):Activate   no-error.
      chWorkbook:WorkSheets(viRS + viPR + viGL + viDC + viWN + 1):delete()   no-error.
    end.

  end case.

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

  find first b2-job-hdr no-lock 
       where b2-job-hdr.company = job-hdr.company
         and b2-job-hdr.job-no  = job-hdr.job-no
         and b2-job-hdr.job-no2 = job-hdr.job-no2
         and b2-job-hdr.frm     = job-hdr.frm no-error.
  find first b-itemfg   of b2-job-hdr no-lock no-error.
  
  FIND FIRST b-prep NO-LOCK
       where b-prep.company = b-itemfg.company
         and b-prep.code    = b-itemfg.die-no NO-ERROR.
         
  if avail b-itemfg THEN do:
    run SetCellValue ("C" + string (viLine + 3), b-itemfg.die-no)             no-error.
    run SetCellValue ("E" + string (viLine + 3), GetLocBin (b-itemfg.die-no)) no-error.
  END.

  IF AVAIL b-prep THEN DO:

    FIND FIRST b-reftable NO-LOCK
         WHERE b-reftable.reftable EQ "PREPLASTJOB"  
           AND b-reftable.company  EQ b-prep.company 
           AND b-reftable.loc      EQ b-prep.loc
           AND b-reftable.code     EQ b-prep.CODE NO-ERROR.

    RUN SetCellValue ("F" + STRING (viLine + 3), b-prep.received-date)       NO-ERROR.
    RUN SetCellVAlue ("H" + STRING (viLine + 3), b-prep.no-of-impressions)   NO-ERROR.
    RUN SetCellValue ("J" + STRING (viLine + 3), b-prep.last-date)           NO-ERROR.
  end.

  IF AVAIL b-reftable THEN DO:
    vcJobNum = b-reftable.code2 + '-' + STRING (b-reftable.val[1], '99').
    RUN SetCellValue ("M" + STRING (viLIne + 3), vcJobNum)                   NO-ERROR.
  END.
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
  def input param icDept  as char no-undo.
  
  /* Variables */
  define variable vcPNum  as char no-undo.
  DEFINE VARIABLE vcPNum-2 AS CHAR NO-UNDO.
  define variable viQty   as inte no-undo.
  DEFINE VARIABLE viJobQty-2 AS INT NO-UNDO.
  DEF VAR viIndex AS INT NO-UNDO.

  /* Pocess all FG Items of a Job Form/Sheet. */
  for each b2-job-hdr no-lock
     where b2-job-hdr.company = job-hdr.company
       and b2-job-hdr.job-no  = job-hdr.job-no
       and b2-job-hdr.job-no2 = job-hdr.job-no2
       and b2-job-hdr.frm     = job-hdr.frm:

    find first b-itemfg of b2-job-hdr no-lock no-error.

    find first b-eb no-lock
         where b-eb.company   = b2-job-hdr.company
           and b-eb.est-no    = b2-job-hdr.est-no
           and b-eb.form-no   = b2-job-hdr.frm
           and b-eb.blank-no  = b2-job-hdr.blank-no no-error.

    if avail b-eb then
      assign 
        vcPNum = b-eb.part-no
        viQty  = b-eb.bl-qty.
    else
      assign 
        vcPNum = ''
        viqty  = 0.

    run CalculateUpValue.
    run GetJobQty.

    /* By Dept: */
    case icDept:
      
      when 'RS' then if ilRS then 
      do:
        if b2-job-hdr.blank-no = 1 then
        do: 
          run SetCellValue ("A7",   b-itemfg.i-no).
          run SetCellValue ("C7",   vcPNum).
          run SetCellValue ("D7",   b-itemfg.i-name).
          run SetCellValue ("G7",   b-itemfg.part-dscr1).
          run SetCellValue ("L7",   viNumUp).
          run SetCellValue ("M7",   b-job-hdr.blank-no).
/*           run SetCellValue ("D9",   viQty). */
/*           run SetCellValue ("D9",   b-job-hdr.qty). */
        end.
      end.
  
      when 'PR' then if ilPR then 
      do: 
          viLine = b2-job-hdr.blank-no + 6.

          run SetCellValue ("A" + string (viLine),   b-itemfg.i-no).
          run SetCellValue ("C" + string (viLine),   vcPNum).
          run SetCellValue ("D" + string (viLine),   b-itemfg.i-name).
          run SetCellValue ("G" + string (viLine),   b-itemfg.part-dscr1).
          run SetCellValue ("K" + string (viLine),   viJobQty).
          run SetCellValue ("L" + string (viLine),   b2-job-hdr.qty).
          run SetCellValue ("M" + string (viLine),   viNumUp).
      end.
  
      when 'DC' then if ilDC then 
      do:
          viLine = b2-job-hdr.blank-no + 6.
  
          run SetCellValue ("A" + string (viLine),   b-itemfg.i-no).
          run SetCellValue ("C" + string (viLine),   vcPNum).
          run SetCellValue ("D" + string (viLine),   b-itemfg.i-name).
          run SetCellValue ("G" + string (viLine),   b-itemfg.part-dscr1).
          run SetCellValue ("J" + string (viLine),   viJobQty).
          run SetCellValue ("K" + string (viLine),   b2-job-hdr.qty).
          run SetCellValue ("L" + string (viLine),   viNumUp).
          run SetDeptNotes (vcRecKey, 'WB', job-mch.frm, 2).
      end.

      when 'GL' or
      when 'WN' or 
      when 'SW' then 
      do: 
          run GetItemInfo(OUTPUT vcPNum-2,
                          OUTPUT viJobQty-2).
          run SetCellValue ("A8",   vcItemNo).
          run SetCellValue ("C8",   vcPNum-2).
          run SetCellValue ("D8",   vcItemNm).
          run SetCellValue ("G8",   vcItemDesc).
          run SetCellValue ("J8",   viJobQty-2 ).
          run SetCellValue ("K8",   b2-job-hdr.qty).
    
          find first b-eb no-lock
               where b-eb.company   = job-hdr.company
                 and b-eb.est-no    = job-hdr.est-no
                 and b-eb.form-no   = job-mch.frm
                 and b-eb.blank-no  = job-mch.blank-no
            no-error.
    
          find first b-est-flm no-lock
               where b-est-flm.company  = job-hdr.company
                 and b-est-flm.est-no   = job-hdr.est-no
                 and b-est-flm.snum     = job-mch.frm
                 and b-est-flm.bnum     = job-mch.blank-no
            no-error.
    
          find first b-itemfg of b2-job-hdr no-lock no-error.
    
          find first b-ef no-lock
               where b-ef.company   = b2-job-hdr.company
                 and b-ef.est-no    = b2-job-hdr.est-no
                 and b-ef.form-no   = b2-job-hdr.frm no-error.
    
          if avail b-eb then
          do:
            if icDept = 'WN' OR
               icDept = 'GL' or
               icDept = 'SW' then
            do:
            
              if icDept = 'WN' then
              do:
              
                IF AVAIL b-est-flm THEN do: 
                    run SetCellValue ("B13",  GetItemName (b-est-flm.i-no)).
                    RUN SetItemInv(INPUT b-est-flm.company,
                                   INPUT b-est-flm.i-no, /*Item*/
                                   INPUT NO). /*film = no, case = yes*/
                END.
                if avail b-ef then
                  run SetCellValue ("B14",  b-ef.leaf-l[1] + 1).
              end.

              ELSE
                 IF icDept = 'GL' THEN
                 DO viIndex = 1 TO 4:
                    IF mach.dept[viIndex] EQ 'WN' THEN
                    DO:
                        IF AVAIL b-est-flm THEN DO:
                            run SetCellValue ("B13",  GetItemName (b-est-flm.i-no)).
                            RUN SetItemInv(INPUT b-est-flm.company,
                                      INPUT b-est-flm.i-no, /*Item*/
                                      INPUT NO). /*film = no, case = yes*/
                        END.
                       if avail b-ef then
                          run SetCellValue ("B14",  b-ef.leaf-l[1] + 1).

                       LEAVE.
                    END. 
                 END.
              
              if avail b-itemfg then
                  run SetCellValue ("K14",  b-itemfg.prod-notes).

              if icDept <> 'SW' THEN DO:
                run SetCellValue ("G13",  GetItemName (b-eb.cas-no)).
                RUN SetItemInv(INPUT b-eb.company,
                               INPUT b-eb.cas-no, /*Item*/
                               INPUT YES). /*film = no, case = yes*/
                RUN SetCellValue("C10", b-eb.die-no).
              END.
              run SetCellValue ("G14",  b-eb.cas-cnt).
              run SetCellValue ("K15",  b-eb.cas-pal).
              run SetCellValue ("K13",  GetItemName (b-eb.tr-no)).
              run SetCellValue ("K16",  b-eb.tr-cnt).
            end.
          end.
      end.
  
    end case.
  end.  

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
  def input param icDept  as char no-undo.

  /* Variables */
  def variable    viCount as inte no-undo.

  run SetSheetName    (icDept).
  run SetSheetHeader  (icDept).
  run FGItems         (icDept).
  run RMItems         (icDept).
  
  if job-mch.blank-no < 2 then
    assign  viOldLine   = viLine
            viOldLoop   = viLoop.
        
  if icDept = 'GL' or 
     icDept = 'WN' then do viCount = 1 to 4:
    if can-do ('GL,WN',mach.dept [viCount]) then do:

      run SetDeptNotes    (vcRecKey, mach.dept [viCount], job-mch.frm, 1) no-error.
      vlWN2 = true.
      run SetDeptNotes    (vcRecKey, mach.dept [viCount], job-mch.frm, 2) no-error.
    end.
  end.

  else do:
    run SetDeptNotes    (vcRecKey, icDept, job-mch.frm, 1) no-error.  
    run SetDeptNotes    ('',       icDept, job-mch.frm, 2) no-error.
  end.
  
  release object chWorkSheet.

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

  find first b2-job-hdr no-lock 
       where b2-job-hdr.company   = job-mch.company
         and b2-job-hdr.job-no    = job-mch.job-no
         and b2-job-hdr.job-no2   = job-mch.job-no2
         and b2-job-hdr.frm       = job-mch.frm
         and b2-job-hdr.blank-no  = job-mch.blank-no
             no-error.

  if avail b2-job-hdr then
  DO:
     find first b-itemfg of b2-job-hdr no-lock no-error.
    
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
     else
        vcJob-Qty-2 = 0.
  END.

  if avail b-itemfg then
    assign 
      vcItemNo    = b-itemfg.i-no
      vcItemNm    = b-itemfg.i-name
      vcItemDesc  = b-itemfg.part-dscr1.
  else
    assign
      vcItemNo    = ''
      vcItemNm    = ''
      vcItemDesc  = ''.

  find first b-eb no-lock
       where b-eb.company   = b2-job-hdr.company
         and b-eb.est-no    = b2-job-hdr.est-no
         and b-eb.form-no   = b2-job-hdr.frm
         and b-eb.blank-no  = b2-job-hdr.blank-no no-error.

    if avail b-eb then
      assign 
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
                       else viJobQty = 0.

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

  def input param iiSheetQty  as DECI no-undo.
  DEF INPUT PARAM icUOM       AS CHAR NO-UNDO.

  if icUOM EQ "EA" then
    viSheetQty = iiSheetQty.
  else  
    run sys/ref/convquom.p  (input  icUOM, 
                             input  "EA", 
                             input  job-mat.basis-w,
                             input  job-mat.len, 
                             input  job-mat.wid, 
                             input  job-mat.dep,
                             input  iiSheetQty, 
                             output viSheetQty).

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
    assign 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
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
  if not (valid-handle (chExcelApplication)) THEN
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
/*   /* Set our current directory. */     */
/*   RUN UTIL/CurrDir.p (output CurrDir). */
  
  /* Set the Excel Template to be used. */
/*   ASSIGN chFile = CurrDir + "\Template\Indiana SheeterLS.xlt" no-error. */
    ASSIGN chFile = search ("Template\Indiana SheeterLS.xlt") no-error.
  
  if search (chFile) = ? then do:
    MESSAGE 'Template File: ' "Template\Indiana SheeterLS.xlt" 
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.
  /* Make Excel visible. */
  ASSIGN 
      chExcelApplication:VISIBLE = TRUE
      chExcelApplication:ScreenUpdating = FALSE.
  
  /* If we are going to E-Mail or Print, hide Excel. */
  IF LvOutputSelection = "Email"    or 
     LvOutputSelection = "Printer"  THEN
    chExcelApplication:VISIBLE = FALSE.
  
  /* Clear tt-FileList. */
  empty temp-table tt-filelist.

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
  empty temp-table tt-ink.
  
  viUnitNo = 0.

  /* Build Ink List. */
  for each b-job-mat no-lock
     where b-job-mat.company  = job-hdr.company
       and b-job-mat.job      = job-hdr.job
       and b-job-mat.frm      = job-hdr.frm,
      FIRST b-item no-lock
     where b-item.company     = b-job-mat.company
       and b-item.i-no        = b-job-mat.i-no
       and (b-item.mat-type   = 'I' or
            b-item.mat-type   = 'V')
        by b-job-mat.frm
        by b-job-mat.blank-no
        by b-item.i-no:
       
    
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

    do i = 1 to 20:
      v-unit = IF i LE 12 AND AVAIL reftable THEN reftable.val[i]
               ELSE
               IF AVAIL b-rt                 THEN b-rt.val[i - 12]
                                             ELSE 0.

      if eb.i-code2[i] eq b-job-mat.i-no then do:
        find first tt-ink
            where tt-ink.i-code   eq eb.i-code2[i]
              and tt-ink.form-no  eq eb.form-no
              and tt-ink.blank-no eq eb.blank-no
              AND tt-ink.i-pass   EQ eb.i-ps2[i]
              AND tt-ink.i-unit   EQ v-unit
            no-error.

        if not avail tt-ink then do:
          create tt-ink.
          assign
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
        end.
      end.
    end. /* loop i */

    FIND FIRST est 
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
        NO-LOCK NO-ERROR.

    find first tt-ink
        where tt-ink.i-code    eq b-job-mat.i-no
          and tt-ink.form-no   eq b-job-mat.frm
          and (tt-ink.blank-no eq b-job-mat.blank-no or
               (AVAIL est AND est.est-type eq 4))
        no-error.

    if not avail tt-ink                              and
       (b-job-mat.blank-no  eq eb.blank-no or
        (b-job-mat.blank-no eq 0 and eb.blank-no eq 1)) then do:
      create tt-ink.
      assign
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
    end.
    
  /*     if avail tt-ink then tt-ink.i-qty = tt-ink.i-qty + b-job-mat.qty. */
  end.

  /* Start from the last line of the RM Items. */
  viLoop = 27 - (24 - viLine).

  for each tt-ink no-lock
     where tt-ink.i-code > ''
        by tt-ink.i-unit:

    run SetCellValue ("A"  + string (viLoop),  tt-ink.rm-dscr).
    run SetCellValue ("C"  + string (viLoop),  tt-ink.i-name).
    run SetCellValue ("F"  + string (viLoop),  tt-ink.loc-bin).
    run SetCellValue ("I"  + string (viLoop),  tt-ink.q-onh).
    run SetCellValue ("J"  + string (viLoop),  tt-ink.i-unit).
    run SetCellValue ("K"  + string (viLoop),  string (tt-ink.i-qty, '->>,>>9.9999')).
    
    viLoop = viLoop + 1.
  end. /* each tt-ink */  
  
  ASSIGN viOldLoop = viLoop.

  /* If we did not use all the space for Inks, get rid of the extra lnes. */
  if viLoop - (viLine + 3) < 9 then
  do:
      vcRange = "A" + string (viLoop) + '..' + "M" + string (viLoop + 9 - (viLoop - (viLine + 3))).
      chWorkSheet:Range (vcRange):delete     no-error.
  end.

  run SetDeptNotes (vcRecKey, 'BL', job-mch.frm, 1).
  run SetDeptNotes (vcRecKey, 'VY', job-mch.frm, 1).
  run SetDeptNotes (vcRecKey, 'MC', job-mch.frm, 1).

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
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

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
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /* Prepare the Sheets to be used. */
  run PrepareSheets.

  /* Process Selected Jobs. */
  run ProcessJobs.

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
  if ilRS 
    then run CreateSheets  ('RS').
    else run DeleteSheet   ('RS').

  /* Printer */
  if ilPR 
    then run CreateSheets  ('PR').
    else run DeleteSheet   ('PR').

  /* Gluer */
  if ilGL 
    then run CreateSheets  ('GL').
    else run DeleteSheet   ('GL').

  /* Die Cut */
  if ilDC 
    then run CreateSheets  ('DC').
    else run DeleteSheet   ('DC').

  /* Window */
  if ilGL 
    then run CreateSheets  ('WN').
    else run DeleteSheet   ('WN').

  /* Shrink Wrap */
  if ilSW 
    then run CreateSheets  ('SW').
    else run DeleteSheet   ('SW').

  /* If no records exist, Quit. */
  if viSheets = 0 then
  do:
    run CleanUp.
    chExcelApplication:Quit() no-error.
    MESSAGE 'Not a single record exists to process.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'close' to this-procedure.
  end.

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
  
  define input param icDept as character no-undo.
  define input param iiType as integer   no-undo.
  DEFINE INPUT PARAM ip-notes-blank AS LOG NO-UNDO.
  DEFINE INPUT PARAM ip-last AS LOG NO-UNDO.

  if vcDeptNotes > '' 
    then vcDeptNotes = FormatNotes (vcDeptNotes, 80).
    else if iiType = 2 then vcDeptNotes = ''.
    else IF icDept = 'BL' or 
            icDept = 'VY' or 
            icDept = 'MC' then vcDeptNotes = ''.
    else if ip-last AND ip-notes-blank THEN vcDeptNotes = 'No Department Notes.'.
  case icDept:

    when 'RS' then run SetCellValue ("A" + string (viLine + 15),  vcDeptNotes).
    when 'PR' then run SetCellValue ("A" + string (viLoop +  7),  vcDeptNotes).
    when 'DC' then run SetCellValue ("A" + string (viLine + 18),  vcDeptNotes).
    when 'SW' then run SetCellValue ("A" + string (viLine + 12),  vcDeptNotes).
    when 'GL' OR
    when 'WN' then do:
      if iiType = 1 then 
        run SetCellValue ("A" + string (viLine + 12),  vcDeptNotes).
      else do:
        run SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
        vlWN2 = true.
      end.
    end.
    
    when 'QS' then run SetCellValue ("A" + string (viLine + 16),  vcDeptNotes).
    when 'QP' then run SetCellValue ("A" + string (viLoop +  8),  vcDeptNotes).
    when 'QD' then run SetCellValue ("A" + string (viLine + 19),  vcDeptNotes).
    when 'QK' then run SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
    when 'QW' OR
    when 'QG' then do:
      if vlWN2 = true then do:
        run SetCellValue ("A" + string (viLine + 14),  vcDeptNotes).
        assign vlWN2  = false.
/*                viLine = viLine + 1. */
      end.
      else do:
        run SetCellValue ("A" + string (viLine + 13),  vcDeptNotes).
      end.
    end.
    
    when 'BL' then run SetCellValue ("C" + string (viLoop +  1),  vcDeptNotes).
    when 'VY' then run SetCellValue ("G" + string (viLoop +  1),  vcDeptNotes).
    when 'MC' then run SetCellValue ("J" + string (viLoop +  1),  vcDeptNotes).
    when 'WB' then run SetCellValue ("M" + string (viLine     ),  vcDeptNotes).

   end case.

   assign   viLoop = viLoop + 1
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
  for  each job-hdr no-lock
      where job-hdr.company    = cocode
        and job-hdr.job-no    >= substring (icBegJobNo,1,6)
        and job-hdr.job-no    <= substring (icEndJobNo,1,6)
        and job-hdr.job-no2   >= iiBegJobNo2
        and job-hdr.job-no2   <= iiEndJobNo2,
      first eb no-lock
      where eb.company        = job-hdr.company
        and eb.est-no         = job-hdr.est-no
        and eb.form-no        = job-hdr.frm,
      first ef no-lock
      where ef.company        = job-hdr.company
        and ef.est-no         = job-hdr.est-no,
      first cust no-lock
      where cust.company     = job-hdr.company
        and cust.cust-no     = job-hdr.cust-no
/*       first oe-ord no-lock                      */
/*       where oe-ord.company    = job-hdr.company */
/*         and oe-ord.est-no     = job-hdr.est-no  */
      break
         by job-hdr.job-no
         by job-hdr.job-no2
         by job-hdr.frm:

    /* Group by Form : */
    if first-of (job-hdr.frm) then
    do:
      find first job no-lock
           where job.company  = job-hdr.company
             and job.job      = job-hdr.job no-error.

      if avail job then vcRecKey = job.rec_key.
                   else vcRecKey = ?.

      /* Set the Job-No. */
      vcJobNo = job-hdr.job-no + '-' +  string (job-hdr.job-no2, '99') + '-' +
                                        string (job-hdr.frm, '99').

      /* Get the last Blank-No to determine # of Items. */
      find last b-job-hdr no-lock
          where b-job-hdr.company = job-hdr.company
            and b-job-hdr.job-no  = job-hdr.job-no
            and b-job-hdr.job-no2 = job-hdr.job-no2
            and b-job-hdr.frm     = job-hdr.frm no-error.
  
      /* Go through all the machines of the selected Job. */
      for each job-mch no-lock
         where job-mch.company   = job-hdr.company
           and job-mch.job       = job-hdr.job
           and job-mch.job-no    = job-hdr.job-no
           and job-mch.job-no2   = job-hdr.job-no2
           and job-mch.frm       = job-hdr.frm
           use-index line-idx,
         first mach no-lock
               {sys/ref/mach.w}
           and mach.m-code eq job-mch.m-code
         break
            by job-mch.job-no
            by job-mch.job-no2
            by job-mch.frm
            by job-mch.blank-no
            by job-mch.dept:

        /* Get the description of Window item. */
        find first est-flm no-lock
             where est-flm.company = job-hdr.company
               and est-flm.est-no  = job-hdr.est-no no-error.
            
        /* Process the selected Departments. */
        case job-mch.dept:
  
          /* Sheeter */
          when 'RS' then if ilRS then run FillData ('RS').
  
          /* Printer */
          when 'PR' then if ilPR then run FillData ('PR').
  
          /* Gluer */
          when 'GL' then if ilGL then run FillData ('GL').

          /* Die Cutter */
          when 'DC' then if ilDC then run FillData ('DC').
  
          /* Window */
          when 'WN' then if ilGL then run FillData ('WN').
              
          /* Shrink Wrap */
          when 'SW' then if ilSW then run FillData ('SW').

        end case.
      end. /* each job-mch */
    end. /* first-of job-hdr.frm */
  end. /* each job-hdr */

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
  def input param icDept  as char no-undo.

  if job-mch.blank-no lt 2 then 
  do:

    viLine = 13.
  
    if icDept = 'RS' or
       icDept = 'PR' or
       icDept = 'DC' then
    do:
    
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
    
      for each rm-rctd no-lock
         where rm-rctd.company    = job-mch.company
           and rm-rctd.rita-code  = 'I'
           and rm-rctd.job-no     = job-mch.job-no
           and rm-rctd.job-no2    = job-mch.job-no2
           and rm-rctd.s-num      = job-mch.frm,
         first item of rm-rctd no-lock
         where item.mat-type = 'B',
         first job-mat no-lock
         where job-mat.company    = job-hdr.company
           and job-mat.job-no     = job-hdr.job-no
           and job-mat.job-no2    = job-hdr.job-no2
           and job-mat.frm        = job-hdr.frm
           and job-mat.i-no       = item.i-no
            by rm-rctd.s-num:
      
        if viLine < 25 then
        do: 

          run SetCellValue ("A" + string (viLine), rm-rctd.i-no)        no-error.
          run SetCellValue ("C" + string (viLine), rm-rctd.loc-bin)     no-error.
          run SetCellValue ("D" + string (viLine), rm-rctd.tag)         no-error.
          run SetCellValue ("F" + string (viLine), item.i-dscr)         no-error.
          run SetCellValue ("H" + string (viLine), item.cal)            no-error.
          run SetCellValue ("I" + string (viLine), DispDim ('W'))       no-error.
          run SetCellValue ("J" + string (viLine), DispDim ('L'))       no-error.
      
          run GetSheetQty (rm-rctd.qty,
                           rm-rctd.pur-uom).

          if icDept = 'RS' 
            then run SetCellValue ("L" + string (viLine), viSheetQty * .99)  no-error.
            else run SetCellValue ("K" + string (viLine), viSheetQty * .99)  no-error.
        end.
        
        viLine = viLine + 1.
      end.

      
      for each rm-rcpth no-lock
         where rm-rcpth.company   = job-mch.company
           and rm-rcpth.job-no    = job-mch.job-no
           and rm-rcpth.job-no2   = job-mch.job-no2,
          each rm-rdtlh of rm-rcpth no-lock,
         first item of rm-rcpth no-lock
         where item.mat-type = 'B',
         first job-mat no-lock
         where job-mat.company  = job-hdr.company
           and job-mat.job-no   = job-hdr.job-no
           and job-mat.job-no2  = job-hdr.job-no2
           and job-mat.frm      = job-hdr.frm
           and job-mat.i-no     = item.i-no:
        if viLine < 25 then
        do: 

          run SetCellValue ("A" + string (viLine), rm-rcpth.i-no)       no-error.
          run SetCellValue ("C" + string (viLine), rm-rdtlh.loc-bin)    no-error.
          run SetCellValue ("D" + string (viLine), rm-rdtlh.tag)        no-error.
          run SetCellValue ("F" + string (viLine), item.i-dscr)         no-error.
          run SetCellValue ("H" + string (viLine), item.cal)            no-error.
          run SetCellValue ("I" + string (viLine), item.r-wid)          no-error.
          run SetCellValue ("J" + string (viLine), job-mat.len)         no-error.
      
          run GetSheetQty (rm-rdtlh.qty,
                           rm-rcpth.pur-uom).

          if icDept = 'RS' 
            then run SetCellValue ("L" + string (viLine), viSheetQty * .99)  no-error.
            else run SetCellValue ("K" + string (viLine), viSheetQty * .99)  no-error.
        end.
        
        viLine = viLine + 1.
      end.

      if viLine > 13 and 
         viLine < 25 then
      do:
        vcRange = "A" + string (viLine) + '..' + "N24".
        chWorkSheet:Range (vcRange):delete     no-error.
      end.
      else
        viLine = 25.

      if job-mch.dept eq 'PR' then  run InkData.
      if job-mch.dept eq 'DC' then  run DieData.
    end.
  end.
  
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
  def input param icPosition  as char no-undo.
  def input param icCellValue as char no-undo.

  /* Go to the Cell Position and Set its value. */
  chWorkSheet:Range(icPosition):value = icCellValue no-error.

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
  def input param icRecKey  as char no-undo.
  def input param icDept    as char no-undo.
  def input param iiForm    as inte no-undo.
  def input param iiType    as inte no-undo.

  define variable viEntry   as inte no-undo.
  
  DEF VAR lv-notes-blank AS LOG NO-UNDO.

  ASSIGN vcDeptNotes  = "".

  if iiType = 2 then
  do:
    case icDept:
      when 'RS' then icDept = 'QS'.
      when 'PR' then icDept = 'QP'.
      when 'DC' then icDept = 'QD'.
      when 'GL' then icDept = 'QG'.
      when 'WN' then icDept = 'QW'.
      when 'SW' then icDept = 'QK'.
    end case.

    find first b2-job-hdr no-lock 
         where b2-job-hdr.company = job-hdr.company
           and b2-job-hdr.job-no  = job-hdr.job-no
           and b2-job-hdr.job-no2 = job-hdr.job-no2
           and b2-job-hdr.frm     = job-hdr.frm no-error.
    find first b-itemfg of b2-job-hdr no-lock no-error.
    if avail b-itemfg then icRecKey = b-itemfg.rec_key.
  end.

  FOR EACH notes NO-LOCK
     WHERE notes.rec_key = icRecKey 
       AND CAN-DO(icDept,notes.note_code)
       AND notes.note_form_no = if iiType = 1 then iiForm
                                              else notes.note_form_no

        BY notes.note_form_no 
        BY notes.note_date 
        BY notes.note_time:
    
    lv-notes-blank = YES.

    if (job-mch.blank-no > 1 and not iiType = 2) OR
       (icDept = 'BL' OR 
        icDept = 'VY' OR 
        icDept = 'MC') then
      assign  viLine  = viOldLine
              viLoop  = viOldLoop.

    do viEntry = 1 to num-entries (notes.note_text, chr(10)):
      vcDeptNotes = entry (viEntry, notes.note_text, chr(10)).
      
      IF vcDeptNotes NE "" THEN
         lv-notes-blank = NO.

      run PrintNotes (icDept, iiType, lv-notes-blank, viEntry EQ num-entries (notes.note_text, chr(10))).
    end.
  END. /* FOR EACH notes */

  if vcDeptNotes = '' then do:
  
    FOR EACH notes NO-LOCK
       WHERE notes.rec_key = icRecKey 
         AND CAN-DO(icDept,notes.note_code)
         AND notes.note_form_no = 0
          BY notes.note_form_no 
          BY notes.note_date 
          BY notes.note_time:
      
      lv-notes-blank = YES.

      if (job-mch.blank-no > 1 and not iiType = 2) OR
         (icDept = 'BL' OR 
          icDept = 'VY' OR 
          icDept = 'MC') then
      assign  viLine  = viOldLine
              viLoop  = viOldLoop.

      do viEntry = 1 to num-entries (notes.note_text, chr(10)):
        vcDeptNotes = entry (viEntry, notes.note_text, chr(10)).
        
        IF vcDeptNotes NE "" THEN
           lv-notes-blank = NO.

        run PrintNotes (icDept, iiType, lv-notes-blank, viEntry EQ num-entries (notes.note_text, chr(10)) ).
      end.
    END. /* FOR EACH notes */
  end.
  
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
  def input param icDept as char no-undo.

  /* Common data. */
  run SetCellValue ("B1",   job-mch.m-code).
  RUN SetCellValue ("N1",   job-mch.speed).
  run SetCellValue ("B2",   mach.m-dscr).
  run SetCellValue ("L1",   vcJobNo).
  run SetCellValue ("L2",   job-hdr.est-no).
  run SetCellValue ("C4",   today).
  run SetCellValue ("H4",   cust.name).

  /* Blank No unique to GL/WN and SW Sheets */
  if icDept = 'GL' or
     icDept = 'WN' or
     icDept = 'SW' then
  run SetCellValue ("L3",   job-mch.blank-no).

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
  def input param icDept  as char no-undo.

  /* Set the WorkSheet Name based on Dept. */ 
  case icDept:
    when 'RS' then  viSheetNo = job-mch.frm.
    when 'PR' then  viSheetNo = viRS + job-mch.frm.
    when 'GL' then
    do:  
      viGLSeq   = viGLSeq + 1.
      viSheetNo = viRS + viPR + viGLSeq.
    end.
    when 'DC' then  viSheetNo = viRS + viPR + viGL + job-mch.frm.
    when 'WN' then
    do:  
      viWNSeq = viWNSeq + 1.
      viSheetNo = viRS + viPR + viGL + viDC + viWNSeq.
    end.
    when 'SW' then  viSheetNo = viRS + viPR + viGL + viDC + viWN + job-mch.frm.
  end case.
  
  /* Go to the Sheet we need to work on. */
  chWorkSheet = chExcelApplication:Sheets:item (viSheetNo)  no-error.

  /* If it's GL or WN, combine the Dept, Form and Blank No for the Sheet Name. */
  if icDept = 'GL' or
     icDept = 'WN' then
    chWorkSheet:name  = icDept + '-' + string (job-mch.frm, '99') + '-' + string (job-mch.blank-no, '99') no-error.

  /* Otherwise, we just need the Dept and the Form. */
  else
    chWorkSheet:name  = icDept + '-' + string (job-mch.frm, '99') no-error.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(viSheetNo):Activate no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-DispDim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DispDim Procedure 
FUNCTION DispDim RETURNS DECIMAL
  ( INPUT ip-dim AS char ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR     ld-dim      AS DEC NO-UNDO.
  DEF VAR     v-wid-num   AS DEC NO-UNDO.
  DEF VAR     v-len-num   AS DEC NO-UNDO.

  DEF BUFFER  b-jm        FOR job-mat .

  IF AVAIL rm-rctd THEN DO:

     find first po-ordl where po-ordl.company   eq cocode
                          and po-ordl.po-no     eq int(rm-rctd.po-no)
                          and po-ordl.i-no      eq rm-rctd.i-no
                          and po-ordl.job-no    eq rm-rctd.job-no
                          and po-ordl.job-no2   eq rm-rctd.job-no2
                          and po-ordl.item-type eq yes
                          and po-ordl.s-num     eq rm-rctd.s-num
     no-lock no-error.

     if avail po-ordl then
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
     else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
    end.
      
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
FUNCTION FormatNotes RETURNS character
  ( input v-text  as char,
    input v-len   as int):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /* Variables */
  DEF VAR vreturn AS char no-undo.
  DEF VAR ventry  AS char no-undo.

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
  ( input icItemNo  as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  assign 
      vcBinLocs = ''
      viCount   = 0.

  for each rm-bin no-lock
     where rm-bin.company = b-eb.company
       and rm-bin.i-no    = icItemNo
       and rm-bin.qty     > 0:

    assign 
      vcBinLocs = vcBinLocs + ', ' + rm-bin.loc-bin + ' ' + trim (string (rm-bin.qty, '->>>>>>>9'))
      vcBinLocs = left-trim (vcBinLocs, ',')
      viCount   = viCount + 1.

    if viCount ge 3 then leave.
  end.
    
  return vcBinLocs.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetItemName Procedure 
FUNCTION GetItemName RETURNS CHARACTER
  (input icItemNo as char) :
/*------------------------------------------------------------------------------
  Purpose:  Converts an Item No to Item Name.
    Notes:  
------------------------------------------------------------------------------*/

  /* Lookup the Item based on Item No given. */
  find first b-item no-lock
       where b-item.company = cocode
         and b-item.i-no    = icItemNo no-error.

  /* If found return the Item Name, otherwise, return blank. */
  if avail b-item then return b-item.i-name.
                  else return ''.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLocBin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetLocBin Procedure 
FUNCTION GetLocBin RETURNS CHARACTER
  (input icDieNo  as char) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the Loc Bin for the Die No.
    Notes:  
------------------------------------------------------------------------------*/

  /* Variables */
  def var vcLocBin  as char no-undo.

  if avail b-itemfg then
    find first prep no-lock
         where prep.company = b-itemfg.company
           and prep.code    = b-itemfg.die-no no-error.

  if avail prep then  vcLocBin = prep.loc-bin.
                else  vcLocBin = ''.

  return vcLocBin.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

