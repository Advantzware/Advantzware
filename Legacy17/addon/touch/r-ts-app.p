DEFINE INPUT PARAMETER IpEmployee AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER IpSuper AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER IpWeekending AS DATE NO-UNDO.

DEFINE SHARED VARIABLE LvWeekending AS DATE NO-UNDO.
DEFINE SHARED VARIABLE LvEmpNum AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE LvTSFolder AS CHAR NO-UNDO.
DEFINE VARIABLE LvEmpName AS CHAR NO-UNDO.
DEFINE VARIABLE LvNotes AS CHAR NO-UNDO.
DEFINE VARIABLE LvFileName AS CHAR NO-UNDO.

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
DEFINE            VARIABLE LvDate               AS DATE NO-UNDO. 
DEF VAR mypict AS COM-HANDLE.  

RUN UTIL/CurrDir.p (output CurrDir).

/* ASSIGN CurrDir = REPLACE(CurrDir, "/addon", ""). */
/* ASSIGN CurrDir = REPLACE(CurrDir, "\addon", ""). */

FIND FIRST TimeSheet NO-LOCK WHERE
    TimeSheet.employee = IpEmployee AND
    TimeSheet.WeekEnding = IpWeekEnding 
    NO-ERROR.

IF AVAILABLE TimeSheet AND SEARCH(Timesheet.FILE-NAME) <> ? THEN
DO:
  chFile = TimeSheet.FILE-NAME.
END.
ELSE RETURN ERROR.

  /* Start a new session of Excel. */
  if not (valid-handle (chExcelApplication)) THEN
  DO:
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
    chExcelApplication:VISIBLE = TRUE.
  END.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

  
  /* Open our Excel File. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  chExcelApplication:ActiveSheet:UnProtect("advance4me").
/*   MESSAGE CurrDir + "\Signature\" + IpSuper + ".jpg" VIEW-AS ALERT-BOX. */
  IF SEARCH(CurrDir + "\Signature\" + IpSuper + ".jpg") <> ? THEN
  DO:
    /* Supervisor's Signature */
    chExcelApplication:Goto("R28C3") NO-ERROR.
    /*chExcelApplication:ActiveSheet:Pictures:Insert(CurrDir + "\Signature\" + IpSuper + ".jpg"):Select.*/
    mypict = chExcelApplication:Range("C28"):Parent:Pictures:Insert(CurrDir + "\Signature\" + IpSuper + ".jpg").
    mypict:TOP = chExcelApplication:Range("C28"):TOP.
    mypict:LEFT = chExcelApplication:Range("C28"):LEFT.
    RELEASE OBJECT mypict.
    
    chExcelApplication:SELECTION:LOCKED = TRUE.
  END.
  ELSE DO :
      MESSAGE "No Signature File available for Supervisor" VIEW-AS ALERT-BOX ERROR.
      chWorkbook:Close(no) no-error.   
      chExcelApplication:Quit() no-error.
  
      RELEASE OBJECT chWorkbook NO-ERROR.
      RELEASE OBJECT chWorkSheet NO-ERROR.
      RELEASE OBJECT chHyper NO-ERROR.
      RETURN ERROR.
  END.

  
  chExcelApplication:Goto("R30C9") NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = TODAY.
  chExcelApplication:SELECTION:LOCKED = TRUE.

  ASSIGN v-cell = "r2c1".
  chExcelApplication:Goto(v-cell).
  


  chExcelApplication:ActiveSheet:Protect("advance4me").

  /* Create the record */
  MESSAGE "Please review the TimeSheet and update the information if needed before Approving TimeSheet" VIEW-AS ALERT-BOX.
  
  MESSAGE "Do you want to Approve the Time Sheet?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE "" UPDATE choice AS LOGICAL.

  IF choice = TRUE THEN
  DO:
    chWorkbook:SAVE.
    PAUSE 3.
    FIND CURRENT Timesheet EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
        TimeSheet.ApprovedBy = IpSuper
        TimeSheet.ApprovedOn = TODAY.
  END.
  FIND CURRENT Timesheet NO-LOCK NO-ERROR.

  
  chWorkbook:Close(no) no-error.   
  chExcelApplication:Quit() no-error.
  
  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chHyper NO-ERROR.
