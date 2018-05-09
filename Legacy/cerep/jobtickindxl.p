&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : cerep/jobtickindxl.p 
    
    Purpose     : Generates Schedule Board Cards Using Excel Template

    Syntax      : run cerep/jobtickindxl.p (

    Description : Generates Schedule Board Cards Using Excel Template

    Author(s)   : Dennis Dizon
    
    Created     : Mar 28, 2007
    
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
def input param ip-multi-faxout   as log  no-undo. /* fax multiple recipents or single */
def input param ip-lines-per-page as int  no-undo.
def input param icBegJobNo        as char no-undo.
def input param iiBegJobNo2       as inte no-undo.
def input param icEndJobNo        as char no-undo.
def input param iiEndJobNo2       as inte no-undo.
def input param icBegMach         as char no-undo.
def input param icEndMach         as char no-undo.
def input param icBegForm         as char no-undo.
def input param icEndForm         as char no-undo.
def input param icBegBlnk         as char no-undo.
def input param icEndBlnk         as char no-undo.

/* Buffers */
def buffer b-job-hdr  for job-hdr.
def buffer b2-job-hdr for job-hdr.
def buffer b-job-mat  for job-mat.
def buffer b-eb       for eb.
def buffer b2-eb      for eb.
def buffer b-itemfg   for itemfg.
def buffer b-item     for item.
def buffer b-rt       for reftable.

/* Includes */
{sys/inc/var.i shared}

def temp-table tt-ink
/*          field form-no   as inte */
/*          field blank-no  as inte */
/*          field i-pass    as inte */
         field i-unit    as inte
         field i-code    as char
         field i-dscr    as char
         field i-name    as char
         field loc-bin   as char
         field q-onh     as inte
         field i-seq     as inte
         field i-qty     as deci format '->>,>>9.9999'
         index i-code    i-code
         index i-unit    i-unit.


/* Variables */
def var viLoop        as inte no-undo.
def var vcMachines    as char no-undo.
def var vcRMItem      as char no-undo.
def var vcJobNo       as char no-undo.
def var vlPage2       as log  no-undo init false.
def var vlSheet2      as log  no-undo init false.
def var vlSheet3      as log  no-undo init false.
def var vlSheet4      as log  no-undo init false. 
def var vlSheet5      as log  no-undo init false. 
def var vlSheet6      as log  no-undo init false. 
def var viNumUp       as inte no-undo.            
def var vcItemNm      as char no-undo.
def var vcItemDesc    as char no-undo.
def var viPack        as inte no-undo.
def var vcCase        as CHAR no-undo.
def var vfWidth       as deci no-undo.
def var vfLength      as deci no-undo.
def var vcMillNum1    as char no-undo. 
def var vcMillNum2    as char no-undo. 
def var vcMillNum3    as char no-undo. 
def var vcMillNum4    as char no-undo. 
def var vlInkProcess  as log  no-undo. 
DEF VAR vlPoly        AS LOG  NO-UNDO.
def var viCount       as inte no-undo.
def var viUnitNo      as inte no-undo.

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
DEFINE            VARIABLE vcTemplateFile       AS CHARACTER    NO-UNDO.
DEFINE            VARIABLE vcBarCode            AS CHARACTER    NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE new SHARED TEMP-TABLE tt-filelist
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

&IF DEFINED(EXCLUDE-GetDueDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDueDate Procedure 
FUNCTION GetDueDate returns date
  () FORWARD.

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
         HEIGHT             = 21.48
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

run InitializeExcel.
run MainLoop.       
run CleanUp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CalcNumUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcNumUp Procedure 
PROCEDURE CalcNumUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  viNumUp = 0.
  
  if job-mch.dept <> 'GL' then
    for each b-eb fields(num-up) no-lock
        where b-eb.company   = job-hdr.company
          and b-eb.est-no    = job-hdr.est-no
          and b-eb.form-no   = job-hdr.frm:
        viNumUp = viNumUP + b-eb.num-up.
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
  
/*   /* Delete pre-existing Temporary files. */                                                     */
/*   os-delete value("c:\tmp\" + STRING(job-hdr.job-no) + ".xls").                                  */
/*   os-delete value("c:\tmp\asi.pdf").                                                             */
/*   os-delete value("c:\tmp\" + STRING(job-hdr.job-no) + ".pdf").                                  */
/*                                                                                                  */
/*   /* Printer */                                                                                  */
/*   IF LvOutputSelection = "PRINTER" THEN                                                          */
/*   DO:                                                                                            */
/*      IF LvFirstTimePrint = NO THEN                                                               */
/*      DO :                                                                                        */
/*        chExcelApplication:Dialogs(8):Show.                                                       */
/*        chWorkbook:Close(no) no-error.                                                            */
/*        ASSIGN LvFirstTimePrint = YES.                                                            */
/*      END.                                                                                        */
/*      ELSE DO :                                                                                   */
/*        chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().                                */
/*        chWorkbook:Close(no) no-error.                                                            */
/*      END.                                                                                        */
/*   END.                                                                                           */
/*                                                                                                  */
/*   /* E-mail */                                                                                   */
/*   IF LvOutputSelection = "Email" THEN                                                            */
/*   DO:                                                                                            */
/*     chExcelApplication:ActiveSheet:SaveAs("c:\tmp\" + STRING(job-hdr.job-no) + ".xls") no-error. */
/*     chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().                                   */
/*     chWorkbook:Close(no) no-error.                                                               */
/*     pause 3.                                                                                     */
/*     OS-DELETE VALUE("c:\tmp\" + STRING(job-hdr.job-no) + ".xls").                                */
/*     OS-RENAME c:\tmp\asi.pdf VALUE("c:\tmp\" + STRING(job-hdr.job-no) + ".pdf").                 */
/*     ASSIGN LvCtr = LvCtr + 1.                                                                    */
/*     CREATE tt-filelist.                                                                          */
/*     ASSIGN                                                                                       */
/*       tt-FileCtr  = LvCtr                                                                        */
/*       tt-FileName = "c:\tmp\" + STRING(job-hdr.job-no) + ".pdf".                                 */
/*   END.                                                                                           */
/*                                                                                                  */
/*   /* Protect the Excel File if output is Screen. */                                              */
/*   ELSE IF LvOutputSelection = "Screen" THEN                                                      */
/*     chExcelApplication:ActiveSheet:Protect("advance4me").                                        */

  /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook   NO-ERROR.
  RELEASE OBJECT chWorkSheet  NO-ERROR.
  RELEASE OBJECT chHyper      NO-ERROR.
  
  /* Delete pre-existing PDF File. */
  os-delete value(v-dir + "CardBoards.pdf").

  RUN UTIL/CurrDir.p (output CurrDir).

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

&IF DEFINED(EXCLUDE-DeleteExtraSheets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteExtraSheets Procedure 
PROCEDURE DeleteExtraSheets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if not vlSheet6 then run DeleteSheet (6).
  if not vlSheet5 then run DeleteSheet (5).
  if not vlSheet4 then run DeleteSheet (4).
  if not vlSheet3 then run DeleteSheet (3).
  if not vlSheet2 then run DeleteSheet (2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSheet Procedure 
PROCEDURE DeleteSheet :
/*------------------------------------------------------------------------------
  Purpose   : Deletes the given Sheet.
  Parameters: iiSheet - Sheet #.
  Notes     :       
------------------------------------------------------------------------------*/

  /* Parameters */
  def input param iiSheet   as inte no-undo.

  /* Delete given Sheet. */
  chWorkbook:WorkSheets(iiSheet):Activate         no-error.
  chWorkbook:WorkSheets(iiSheet):delete()         no-error.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_DC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_DC Procedure 
PROCEDURE Dept_DC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if job-mch.blank-no >= 2 then next.
  run SetCellValue ("A17",  job-mch.m-code).
  run SetCellValue ("M17",  vcJobNo).
  run SetCellValue ("A18",  mach.m-dscr).
  run SetCellValue ("M18",  string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("A20",  itemfg.i-name).
  run SetCellValue ("P20",  b-job-hdr.blank-no).
  run SetCellValue ("A21",  itemfg.part-dscr1).
  run SetCellValue ("P21",  viNumUp).
  run SetCellValue ("A22",  '#' + eb.part-no).
  run SetCellValue ("E24",  string (job-mch.run-qty, '->>>,>>9')).
  run SetCellValue ("A27",  vcMillNum1).
  run SetCellValue ("E27",  vcMillNum2).
  run SetCellValue ("I27",  vcMillNum3).
  run SetCellValue ("M27",  vcMillNum4).
  RUN SetCellValue ("A31",  vcBarCode).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_GL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_GL Procedure 
PROCEDURE Dept_GL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  run GetItemInfo.

  /* Get Customer Part No. */
  find first b-eb no-lock
       where b-eb.company   = job-hdr.company
         and b-eb.est-no    = job-hdr.est-no
         and b-eb.form-no   = job-mch.frm 
         and b-eb.blank-no  = job-mch.blank-no no-error.

  vcRMItem = ''.

  if avail b-eb then
  do:
    viPack = b-eb.cas-cnt.
    find first item no-lock
         where item.company = job-mch.company
           and item.i-no    = b-eb.adhesive no-error.
    if avail item then vcRMItem = item.i-name.
    FIND FIRST ITEM NO-LOCK
        WHERE ITEM.company = job-mch.company
            AND ITEM.i-no = b-eb.cas-no.
    if avail item then vcCase = item.i-name.
  end.

  else viPack = 0.

  case job-mch.blank-no:
    when 1 then
    do:
      run SetCellValue ("A33",  job-mch.m-code).
      run SetCellValue ("M33",  vcJobNo).
      run SetCellValue ("A34",  mach.m-dscr).
      run SetCellValue ("M34",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("A36",  vcItemNm).
      run SetCellValue ("M36",  '#' + b-eb.part-no).
      run SetCellValue ("A37",  vcItemDesc).
      run SetCellValue ("I39",  string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("A41",  vcRMItem).
      run SetCellValue ("N42",  string (viPack, '->>,>>9')).
      run SetCellValue ("G40",  vcCase).
      RUN SetCellValue ("A46",  vcBarCode).
    end.

    when 2 then
    do:
      vlPage2 = true.
      run SetCellValue ("A60",  job-mch.m-code).
      run SetCellValue ("M60",  vcJobNo).
      run SetCellValue ("A61",  mach.m-dscr).
      run SetCellValue ("M61",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("A63",  vcItemNm).
      run SetCellValue ("M63",  '#' + b-eb.part-no).
      run SetCellValue ("A64",  vcItemDesc).
      run SetCellValue ("I66",  string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("A68",  vcRMItem).
      run SetCellValue ("N69",  string (viPack, '->>,>>9')).
      run SetCellValue ("G67",  vcCase).
      RUN SetCellValue ("A73",  vcBarCode).
    end.

    when 3 then
    do:
      vlPage2 = true.
      run SetCellValue ("A74",  job-mch.m-code).
      run SetCellValue ("M74",  vcJobNo).
      run SetCellValue ("A75",  mach.m-dscr).
      run SetCellValue ("M75",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("A77",  vcItemNm).
      run SetCellValue ("M77",  '#' + b-eb.part-no).
      run SetCellValue ("A78",  vcItemDesc).
      run SetCellValue ("I80",  string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("A82",  vcRMItem).
      run SetCellValue ("N83",  string (viPack, '->>,>>9')).
      run SetCellValue ("G81",  vcCase).
      RUN SetCellValue ("A87",  vcBarCode).
    end.

    when 4 then
    do:
      vlPage2 = true.
      run SetCellValue ("A89",  job-mch.m-code).
      run SetCellValue ("M89",  vcJobNo).
      run SetCellValue ("A90",  mach.m-dscr).
      run SetCellValue ("M90",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("A92",  vcItemNm).
      run SetCellValue ("M92",  '#' + b-eb.part-no).
      run SetCellValue ("A93",  vcItemDesc).
      run SetCellValue ("I95",  string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("A97",  vcRMItem).
      run SetCellValue ("N98",  string (viPack, '->>,>>9')).
      run SetCellValue ("G96",  vcCase).
      RUN SetCellValue ("A102",  vcBarCode).
    end.
  end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_PR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_PR Procedure 
PROCEDURE Dept_PR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if job-mch.blank-no >= 2 then next.
  run SetCellValue ("S1",   job-mch.m-code).
  run SetCellValue ("AE1",  vcJobNo).
  run SetCellValue ("S2",   mach.m-dscr).
  run SetCellValue ("AE2",  string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("S4",   itemfg.i-name).
  run SetCellValue ("AH4",  b-job-hdr.blank-no).
  run SetCellValue ("S5",   itemfg.part-dscr1).
  run SetCellValue ("AH5",  viNumUp).
  run SetCellValue ("S6",   '#' + eb.part-no).
  run SetCellValue ("W8",   string (job-mch.run-qty, '->>>,>>9')).
  run SetCellValue ("AH9",  string (vlInkProcess, 'Yes/No')).
  run SetCellValue ("S11",  vcMillNum1).
  run SetCellValue ("W11",  vcMillNum2).
  run SetCellValue ("AA11", vcMillNum3).
  run SetCellValue ("AE11", vcMillNum4).
  run SetCellValue ("AE50", trim (string (job-mch.run-qty, '->>>,>>9'))).
  run SetCellValue ("AE59", GetLocBin(itemfg.die-no)).
  RUN SetCellValue ("S15",  vcBarCode).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_RS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_RS Procedure 
PROCEDURE Dept_RS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if job-mch.blank-no >= 2 then next.
            
  assign
    vfWidth   = 0
    vfLength  = 0.

  for each b-job-mat NO-LOCK
      where b-job-mat.company = job-mch.company
        and b-job-mat.job-no  = job-mch.job-no
        and b-job-mat.job-no2 = job-mch.job-no2
        and b-job-mat.frm     = job-mch.frm,
      first item no-lock
      where item.company      = b-job-mat.company
        and item.i-no         = b-job-mat.rm-i-no
        and item.mat-type     = 'B':
    assign
      vfWidth   = b-job-mat.wid
      vfLength  = b-job-mat.len.
  end.

  run SetCellValue ("A1",   job-mch.m-code).
  run SetCellValue ("M1",   vcJobNo).
  run SetCellValue ("A2",   mach.m-dscr).
  run SetCellValue ("M2",   string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("A4",   itemfg.i-name).
  run SetCellValue ("P4",   b-job-hdr.blank-no).
  run SetCellValue ("A5",   itemfg.part-dscr1).
  run SetCellValue ("P5",   viNumUp).
  run SetCellValue ("A6",   '#' + eb.part-no).
  run SetCellValue ("E8",   string (job-mch.run-qty, '->>>,>>9')).
  run SetCellValue ("H8",   string (vfWidth,  '->,>>9.9999')).
  run SetCellValue ("M8",   string (vfLength, '->,>>9.9999')).
  run SetCellValue ("A11",  vcMillNum1).
  run SetCellValue ("E11",  vcMillNum2).
  run SetCellValue ("I11",  vcMillNum3).
  run SetCellValue ("M11",  vcMillNum4).
  run SetCellValue ("M52",  string (job-mch.run-qty, '->>>,>>9')).  
  run SetCellValue ("M51",  viNumUp).
  RUN SetCellValue ("O6",   STRING(vlPoly,"Yes/No")).
  RUN SetCellValue ("A15",  vcBarCode).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_SW) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_SW Procedure 
PROCEDURE Dept_SW :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if job-mch.blank-no >= 2 then next.

  run GetItemInfo.

  /* Get Customer Part No. */
  find first b-eb no-lock
       where b-eb.company   = job-hdr.company
         and b-eb.est-no    = job-hdr.est-no
         and b-eb.form-no   = job-mch.frm 
         and b-eb.blank-no  = job-mch.blank-no no-error.

  vcRMItem = ''.

  if avail b-eb then
  do:
    viPack = b-eb.cas-cnt.
    find first item no-lock
         where item.company = job-mch.company
           and item.i-no    = b-eb.adhesive no-error.
    if avail item then vcRMItem = item.i-name.
  end.

  else viPack = 0.

  run SetCellValue ("S33",  job-mch.m-code).
  run SetCellValue ("AE33", vcJobNo).
  run SetCellValue ("S34",  mach.m-dscr).
  run SetCellValue ("AE34", string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("S36",  itemfg.i-name).
  run SetCellValue ("AH36", '#' + b-eb.part-no).
  run SetCellValue ("S37",  vcItemDesc).
  run SetCellValue ("Y39",  string (job-mch.run-qty, '->>>,>>9')).
  run SetCellValue ("AF42", string (viPack, '->>,>>9')).
  RUN SetCellValue ("S46",  vcBarCode).

/*   run SetCellValue ("S17",  job-mch.m-code).                       */
/*   run SetCellValue ("AE17", vcJobNo).                              */
/*   run SetCellValue ("S18",  mach.m-dscr).                          */
/*   run SetCellValue ("AE18", string (GetDueDate(), '99/99/9999')).  */
/*   run SetCellValue ("S20",  vcItemNm).                             */
/*   run SetCellValue ("AE20", '#' + b-eb.part-no).                   */
/*   run SetCellValue ("S21",  vcItemDesc).                           */
/*   run SetCellValue ("AA24", string (job-mch.run-qty, '->>>,>>9')). */
/*   run SetCellValue ("S26",  vcRMItem).                             */
/*   run SetCellValue ("AF27", string (viPack, '->>,>>9')).           */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dept_WN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dept_WN Procedure 
PROCEDURE Dept_WN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  run GetItemInfo.

  /* Get Customer's Part No. */
  find first b-eb no-lock
       where b-eb.company   = job-hdr.company
         and b-eb.est-no    = job-hdr.est-no
         and b-eb.form-no   = job-mch.frm 
         and b-eb.blank-no  = job-mch.blank-no no-error.

  if avail b-eb then do:
      viPack = b-eb.cas-cnt.
      FIND FIRST ITEM NO-LOCK
        WHERE ITEM.company = b-eb.company
            AND ITEM.i-no = b-eb.cas-no.
      IF AVAIL ITEM THEN vcCase = ITEM.i-name.
  END.
  else viPack = 0.

  vcRMItem = ''.

  if avail est-flm then
  do:
    find first item of est-flm no-lock no-error.
    if avail item then vcRMItem = item.i-name.
  end.

  case job-mch.blank-no:
    when 1 then
    do:
      run SetCellValue ("S17",  job-mch.m-code).
      run SetCellValue ("AE17", vcJobNo).
      run SetCellValue ("S18",  mach.m-dscr).
      run SetCellValue ("AE18", string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("S20",  vcItemNm).
      run SetCellValue ("AE20", '#' + b-eb.part-no).
      run SetCellValue ("S21",  vcItemDesc).
      run SetCellValue ("AA24", string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("S26",  vcRMItem).
      run SetCellValue ("AF27", string (viPack, '->>,>>9')).
      RUN SetCellValue ("Y25", vcCase).
      RUN SetCellValue ("S31",  vcBarCode).
    end.

    when 2 then
    do:
      vlPage2 = true.
      run SetCellValue ("S60",  job-mch.m-code).
      run SetCellValue ("AE60", vcJobNo).
      run SetCellValue ("S61",  mach.m-dscr).
      run SetCellValue ("AE61", string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("S63",  vcItemNm).
      run SetCellValue ("AE63", '#' + b-eb.part-no).
      run SetCellValue ("S64",  vcItemDesc).
      run SetCellValue ("AA66", string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("S68",  vcRMItem).
      run SetCellValue ("AF69", string (viPack, '->>,>>9')).
      RUN SetCellValue ("Y67", vcCase).
      RUN SetCellValue ("S73",  vcBarCode).
    end.

    when 3 then
    do:
      vlPage2 = true.
      run SetCellValue ("S74",  job-mch.m-code).
      run SetCellValue ("AE74",  vcJobNo).
      run SetCellValue ("S75",  mach.m-dscr).
      run SetCellValue ("AE75",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("S77",  vcItemNm).
      run SetCellValue ("AE77", '#' + b-eb.part-no).
      run SetCellValue ("S78",  vcItemDesc).
      run SetCellValue ("AA80", string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("S82",  vcRMItem).
      run SetCellValue ("AF83", string (viPack, '->>,>>9')).
      RUN SetCellValue ("Y81", vcCase).
      RUN SetCellValue ("S87",  vcBarCode).
    end.

    when 4 then
    do:
      vlPage2 = true.
      run SetCellValue ("S89",  job-mch.m-code).
      run SetCellValue ("AE89",  vcJobNo).
      run SetCellValue ("S90",  mach.m-dscr).
      run SetCellValue ("AE90",  string (GetDueDate(), '99/99/9999')).
      run SetCellValue ("S92",  vcItemNm).
      run SetCellValue ("AE92", '#' + b-eb.part-no).
      run SetCellValue ("S93",  vcItemDesc).
      run SetCellValue ("AA95", string (job-mch.run-qty, '->>>,>>9')).
      run SetCellValue ("S97",  vcRMItem).
      run SetCellValue ("AF98", string (viPack, '->>,>>9')).
      RUN SetCellValue ("Y96", vcCase).
      RUN SetCellValue ("S102",  vcBarCode).
    end.

  end case.

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

  find first b2-job-hdr no-lock 
       where b2-job-hdr.company   = job-mch.company
         and b2-job-hdr.job-no    = job-mch.job-no
         and b2-job-hdr.job-no2   = job-mch.job-no2
         and b2-job-hdr.frm       = job-mch.frm
         and b2-job-hdr.blank-no  = job-mch.blank-no
             no-error.

  if avail b2-job-hdr then
    find first b-itemfg of b2-job-hdr no-lock no-error.

  if avail b-itemfg then
    assign 
      vcItemNm    = b-itemfg.i-name
      vcItemDesc  = b-itemfg.part-dscr1.
  else
    assign
      vcItemNm    = ''
      vcItemDesc  = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetMillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMillData Procedure 
PROCEDURE GetMillData :
/*------------------------------------------------------------------------------
     Purpose:   Run the Dialog for Mill Data Input.
  Parameters:   None
       Notes:       
------------------------------------------------------------------------------*/
  
  /* Reset variables. */
  assign
    vcMillNum1    = ''
    vcMillNum2    = ''
    vcMillNum3    = ''
    vcMillNum4    = ''
    vlInkProcess  = no.

  /* Activate the dialog box to prompt for Mill Data. */
  run cerep/d-indiana.w  (input   job-hdr.job-no,
                          input   job-hdr.job-no2,
                          input   job-hdr.frm,
                          output  vcMillNum1,
                          output  vcMillNum2,  
                          output  vcMillNum3,  
                          output  vcMillNum4,  
                          output  vlInkProcess,
                          OUTPUT  vlPoly).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose:     Initializes Excel Environment
  Parameters:  <none>
  Notes:       
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
  
  vcTemplateFile = "Template\ScheduleBoards.xlt".

  FILE-INFO:FILE-NAME = vcTemplateFile.
  
  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.

  if chFile = ? then do:
    MESSAGE 'Your Excel Template: Template\ScheduleBoards.xlt cannot be found.'  skip
            'Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'close':u to this-procedure.
  end.
  
  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = false.
  
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
      each b-item no-lock
     where b-item.company     = b-job-mat.company
       and b-item.i-no        = b-job-mat.i-no
       and (b-item.mat-type   = 'I' or
            b-item.mat-type   = 'V')
        by b-job-mat.frm
        by b-job-mat.blank-no
        by b-item.i-no:
       
    find first tt-ink where tt-ink.i-code = b-job-mat.i-no no-lock no-error.

    if not avail tt-ink then
    do:
      create tt-ink.
      assign
/*         tt-ink.loc-bin  = GetBinLocs (b-job-mat.i-no) */
        tt-ink.i-code   = b-job-mat.i-no
/*         tt-ink.q-onh    = b-item.q-onh */
        tt-ink.i-dscr   = b-item.i-dscr
        tt-ink.i-name   = b-item.i-name.
    end.
    
    tt-ink.i-qty  = tt-ink.i-qty + b-job-mat.qty.
  end.

  for  each b2-eb no-lock 
      where b2-eb.company = job-hdr.company
        and b2-eb.est-no  = job-hdr.est-no
        and b2-eb.form-no = job-hdr.frm
         by b2-eb.form-no
         by b2-eb.blank-no:

    FIND FIRST reftable 
         WHERE reftable         EQ "ce/v-est3.w Unit#"
           AND reftable.company EQ b2-eb.company
           AND reftable.loc     EQ b2-eb.est-no
           AND reftable.code    EQ STRING(b2-eb.form-no,"9999999999")
           AND reftable.code2   EQ STRING(b2-eb.blank-no,"9999999999")
         NO-LOCK NO-ERROR.
  
    FIND FIRST b-rt
         WHERE b-rt.reftable EQ "ce/v-est3.w Unit#1"
           AND b-rt.company  EQ b2-eb.company
           AND b-rt.loc      EQ b2-eb.est-no
           AND b-rt.code     EQ STRING(b2-eb.form-no,"9999999999")
           AND b-rt.code2    EQ STRING(b2-eb.blank-no,"9999999999")
         NO-LOCK NO-ERROR.
  
    do viCount = 1 to 20:
  
      find first tt-ink exclusive-lock
           where tt-ink.i-code = b2-eb.i-code2 [viCount] no-error.
  
      if avail tt-ink and 
               tt-ink.i-unit = 0 then
        tt-ink.i-unit = IF viCount LE 12 AND AVAIL reftable 
                          then reftable.val [viCount]
                          else IF AVAIL b-rt  THEN b-rt.val [viCount - 12]
                                              ELSE 0.
    end.
  end.

  viLoop = 51.

  for each tt-ink no-lock
     where tt-ink.i-code > ''
        by tt-ink.i-unit:

    run SetCellValue ("T"  + string (viLoop),  tt-ink.i-dscr).
    run SetCellValue ("Y"  + string (viLoop),  tt-ink.i-name).
    run SetCellValue ("AI" + string (viLoop),  string (tt-ink.i-qty, '->>,>>9.9999')).
    
    viLoop = viLoop + 1.
  end. /* each tt-ink */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InkData-OLD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InkData-OLD Procedure 
PROCEDURE InkData-OLD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Clear Ink Temp-Table. */
  empty temp-table tt-ink.

  /* Build Ink List. */
  for first b-eb no-lock
      where b-eb.company        = job-hdr.company
        and b-eb.est-no         = job-hdr.est-no
        and b-eb.form-no        = job-hdr.frm
        and b-eb.blank-no       = job-hdr.blank-no:

    do viLoop = 1 to 6:
      find first b-job-mat no-lock
           where b-job-mat.company   = job-hdr.company
             and b-job-mat.job       = job-hdr.job
             and b-job-mat.frm       = job-hdr.frm
             and b-job-mat.blank-no  = job-hdr.blank-no 
             and b-job-mat.i-no      = b-eb.i-code2 [viLoop] no-error.
      
      create tt-ink.
      assign 
        tt-ink.i-code = eb.i-code2 [viLoop]
        tt-ink.i-dscr = eb.i-dscr2 [viLoop]
        tt-ink.i-qty  = if avail b-job-mat then b-job-mat.qty
                                           else 0.
    end.
    
    viLoop = 1.

    for each tt-ink no-lock:

      run SetCellValue ("T"  + string (50 + viLoop),  tt-ink.i-code).
      run SetCellValue ("Y"  + string (50 + viLoop),  tt-ink.i-dscr).
      run SetCellValue ("AI" + string (50 + viLoop),  string (tt-ink.i-qty, '->>,>>9.9999')).
      
      viLoop = viLoop + 1.
    end. /* each tt-ink */
  end. /* first b-eb */      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InkData_20070501) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InkData_20070501 Procedure 
PROCEDURE InkData_20070501 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Clear Ink Temp-Table. */
  empty temp-table tt-ink.

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
       
    find first tt-ink where tt-ink.i-code = b-job-mat.i-no no-lock no-error.

    if not avail tt-ink then
    do:
      create tt-ink.
      assign
        tt-ink.i-code   = b-job-mat.i-no
        tt-ink.i-dscr   = b-item.i-name.
    end.
    
    tt-ink.i-qty  = tt-ink.i-qty + b-job-mat.qty.
  end.

  viLoop = 1.

  for each tt-ink no-lock:

    run SetCellValue ("T"  + string (50 + viLoop),  tt-ink.i-code).
    run SetCellValue ("Y"  + string (50 + viLoop),  tt-ink.i-dscr).
    run SetCellValue ("AI" + string (50 + viLoop),  string (tt-ink.i-qty, '->>,>>9.9999')).
    
    viLoop = viLoop + 1.
  end. /* each tt-ink */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose:      Main Loop for the Report Logic
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false   no-error.

  /* Go through each jobs selected by the user. */
  for  each job-hdr no-lock
      where job-hdr.company   = cocode
        and job-hdr.job-no    ge icBegJobNo
        and job-hdr.job-no2   ge iiBegJobNo2
        and job-hdr.job-no    le icEndJobNo
        and job-hdr.job-no2   le iiEndJobNo2,
      first eb no-lock
      where eb.company        = job-hdr.company
        and eb.est-no         = job-hdr.est-no
        and eb.form-no        = job-hdr.frm
        and eb.blank-no       = job-hdr.blank-no,
      first ef no-lock
      where ef.company        = job-hdr.company
        and ef.est-no         = job-hdr.est-no,
      first itemfg of job-hdr no-lock
      break
         by job-hdr.job-no
         by job-hdr.job-no2
         by job-hdr.frm:

    if first-of (job-hdr.frm) then
    do:
      
      /* Get Mill Data. */
      run GetMillData.
    
      /* Combine Job-No + Job-No2 + Form. */
      assign 
        vcJobNo           = job-hdr.job-no + '-' +  string (job-hdr.job-no2, '99') + '-' +
                                                    string (job-hdr.frm, '99')
        vcBarCode         = job-hdr.job-no + "-"
                          + STRING(job-hdr.job-no2) + "."
                          + STRING(job-hdr.frm) + "."
                          + STRING(job-hdr.blank-no) + "."
                          + "1"
        vcMachines        = ''
        chWorkSheet       = chExcelApplication:Sheets:item(job-hdr.frm)
        chWorkSheet:name  = vcJobNo 
        vlPage2           = false
        no-error.
  
      /* Monitor Sheet 2, 3, and 4: */
      run MonitorSheets.
  
      /* Go to the Active Sheet. */
      chWorkbook:WorkSheets(job-hdr.frm):Activate no-error.
  
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
               {sys/ref/machW.i}
           and mach.m-code eq job-mch.m-code
         break 
            by job-mch.frm
            by job-mch.blank-no:
        
        /* Machine Filter */
        if not (icEndMach = '') and
           not (job-mch.m-code >= icBegMach   and
                job-mch.m-code <= icEndMach)  then next.

        /* Form Filter */
        if not (icEndForm = '') and
           not (job-mch.frm >= int (icBegForm)   and
                job-mch.frm <= int (icEndForm))  then next.

        /* Blank Filter */
        if not (icEndBlnk = '') and
           not (job-mch.blank-no >= int (icBegBlnk)   and
                job-mch.blank-no <= int (icEndBlnk))  then next.
  
        /* Get the description of Window item. */
        find first est-flm no-lock
             where est-flm.company eq job-hdr.company
               AND est-flm.est-no  eq job-hdr.est-no
               AND est-flm.snum    eq job-mch.frm
               AND est-flm.bnum    eq job-mch.blank-no no-error.
  
        /* Concatenate the machine names of the job. */
        if index (vcMachines, job-mch.m-code) = 0 then
          vcMachines  = vcMachines + string (job-mch.m-code) + '  '.
        
        run CalcNumUp.
  
        /* Process Each Dept. */
        case job-mch.dept:
          
          when 'RS' then run Dept_RS.
          when 'PR' then run Dept_PR.
          when 'DC' then run Dept_DC.
          when 'WN' then run Dept_WN.
          when 'GL' then run Dept_GL.
          when 'SW' then run Dept_SW.

        end case.

      end. /* each job-mch */
  
      /* Quality Team Notice */
      run QualityTeamNotice.
  
      /* Printer Advance Notice */
      run PrinterAdvanceNotice.      
  
      /* Ink Data */
      run InkData.
  
      /* If there's no Page 2, Delete Page 2. */
      if not vlPage2 then
      do:
        chWorkbook:WorkSheets(job-hdr.frm):Activate no-error.
        chWorkSheet:Range ("A60..AI110"):delete     no-error.
      end.
    end. /* first-of (job-hdr.frm) */
  end. /* each job-hdr */
  
  /* Delete unused Sheets. */
  run DeleteExtraSheets.  
  
  /* Go to the first Sheet. */
  chWorkbook:WorkSheets(1):Activate no-error.

  /* Make Excel visible. */
  chExcelApplication:VISIBLE = true.

  /* Preview the Spreadsheet. */
  chWorkbook:PrintOut(1,999,1,yes,,no,no)   no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MonitorSheets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MonitorSheets Procedure 
PROCEDURE MonitorSheets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if job-hdr.frm = 2  then vlSheet2 = true.
  if job-hdr.frm = 3  then vlSheet3 = true.
  if job-hdr.frm = 4  then vlSheet4 = true.
  if job-hdr.frm = 5  then vlSheet5 = true.
  if job-hdr.frm = 6  then vlSheet6 = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrinterAdvanceNotice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrinterAdvanceNotice Procedure 
PROCEDURE PrinterAdvanceNotice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  run SetCellValue ("AE48", vcJobNo).
  run SetCellValue ("S49",  '#' + eb.part-no).
  run SetCellValue ("AE49", string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("S50",  itemfg.i-name).
/*   run SetCellValue ("S50",  vcItemDesc). */
  run SetCellValue ("M50",  b-job-hdr.blank-no).
  run SetCellValue ("A51",  itemfg.part-dscr1).
  run SetCellValue ("X59",  itemfg.die-no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QualityTeamNotice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QualityTeamNotice Procedure 
PROCEDURE QualityTeamNotice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Quality Team Notice */
  run SetCellValue ("M48",  vcJobNo).
  run SetCellValue ("M49",  string (GetDueDate(), '99/99/9999')).
  run SetCellValue ("A50",  itemfg.i-name).
  run SetCellValue ("M50",  b-job-hdr.blank-no).
  run SetCellValue ("A51",  itemfg.part-dscr1).
  run SetCellValue ("A52",  '#' + eb.part-no).
  run SetCellValue ("A57",  trim (vcMachines)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCellValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCellValue Procedure 
PROCEDURE SetCellValue :
/*------------------------------------------------------------------------------
  Purpose:    Positions to a cell and set its value.
  
  Parameters: icPosition - Cell Position
              icCellValue - Cell Value
  Notes:       
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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetDueDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDueDate Procedure 
FUNCTION GetDueDate returns date
  ():
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  if job-mch.blank-no > 1 then
  do:
  
    find first b2-job-hdr no-lock
         where b2-job-hdr.company   = job-mch.company
           and b2-job-hdr.job-no    = job-mch.job-no
           and b2-job-hdr.job-no2   = job-mch.job-no2
           and b2-job-hdr.frm       = job-mch.frm
           and b2-job-hdr.blank-no  = job-mch.blank-no
               no-error.
  
    if avail b2-job-hdr then
      return b2-job-hdr.due-date.
    else
      return ?.
  end.

  else return job-hdr.due-date.

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

  if avail itemfg then
    find first prep no-lock
         where prep.company = itemfg.company
           and prep.code    = itemfg.die-no no-error.

  if avail prep then  vcLocBin = prep.loc-bin.
                else  vcLocBin = ''.

  return vcLocBin.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

