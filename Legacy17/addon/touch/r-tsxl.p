DEFINE SHARED VARIABLE LvWeekending AS DATE NO-UNDO.
DEFINE SHARED VARIABLE LvEmpNum AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE LvTSFolder AS CHAR NO-UNDO.
DEFINE VARIABLE LvEmpName AS CHAR NO-UNDO.
DEFINE VARIABLE LvNotes AS CHAR NO-UNDO.
DEFINE VARIABLE LvFileName AS CHAR NO-UNDO.

DEFINE SHARED TEMP-TABLE tt-tsrep
    FIELD tt-date AS DATE
    FIELD tt-time-in1 AS CHAR 
    FIELD tt-time-out1 AS CHAR
    FIELD tt-time-in2 AS CHAR
    FIELD tt-time-out2 AS CHAR
    FIELD tt-wrk-hrs AS CHAR
    FIELD tt-ot-hrs AS CHAR
    FIELD tt-dt-hrs AS CHAR
INDEX pi-tsrep tt-date.

DEFINE SHARED TEMP-TABLE tt-note NO-UNDO
  FIELD employee LIKE emplogin.employee
  FIELD rec_key LIKE ASI.notes.rec_key
  FIELD note_date LIKE ASI.notes.note_date
  FIELD note_title LIKE ASI.notes.note_title
  FIELD note_src AS CHARACTER.

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

  FIND FIRST employee NO-LOCK WHERE employee.employee = LvEmpNum NO-ERROR.
  ASSIGN LvEmpName    = employee.first_name + " " + employee.last_name.


  RUN UTIL/CurrDir.p (output CurrDir).

/*   ASSIGN CurrDir = REPLACE(CurrDir, "/addon", ""). */
/*   ASSIGN CurrDir = REPLACE(CurrDir, "\addon", ""). */

  chFile = CurrDir + "\Template\TimeSheet.xlt".

  if chFile = ? then do:
    MESSAGE "Your Excel Template: " vcTemplateFile  skip
            "Please verify that the file exists."
      VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  end.
/*   MESSAGE chFile VIEW-AS ALERT-BOX. */

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


  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  /* Name */
  v-cell = "R3C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = LvEmpName.

  /* WeekEnding */
  v-cell = "R4C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = STRING(LvWeekEnding).

  ASSIGN inrowcount = 11 .
  DO LvDate = ( LvWeekEnding - 6 ) TO LvWeekEnding :
      ASSIGN inrowcount = inrowcount + 1.

      FIND FIRST tt-tsrep NO-LOCK WHERE tt-tsrep.tt-date = LvDate NO-ERROR.
      IF AVAILABLE tt-tsrep THEN
      DO:
          /* Time In1 */
          v-cell = "R" + string(inrowcount) + "C3".
          chExcelApplication:Goto(v-cell) NO-ERROR. 
          ASSIGN chExcelApplication:ActiveCell:Value = tt-time-in1.

          /* Time Out1 */
          v-cell = "R" + string(inrowcount) + "C4".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-time-out1.

          /* Time In2 */
          v-cell = "R" + string(inrowcount) + "C5".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-time-in2.

          /* Time Out2 */
          v-cell = "R" + string(inrowcount) + "C6".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-time-out2.

          /* Reg Hrs */
          v-cell = "R" + string(inrowcount) + "C7".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-wrk-hrs.

          /* OT Hrs */
          v-cell = "R" + string(inrowcount) + "C8".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-ot-hrs.

          /* DT Hrs */
          v-cell = "R" + string(inrowcount) + "C9".
          chExcelApplication:Goto(v-cell) NO-ERROR.
          ASSIGN chExcelApplication:ActiveCell:Value = tt-dt-hrs.
      END.
  END.

  for each tt-note BY tt-note.note_date:
      IF LvNotes = "" THEN
      ASSIGN LvNotes = string(tt-note.note_date) + " "  + tt-note.note_title.
      ELSE
      ASSIGN LvNotes = LvNotes + chr(10) + string(tt-note.note_date) + " "  + tt-note.note_title.
  END.

  /* Notes */
  v-cell = "R36C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = LvNotes.

  IF SEARCH(CurrDir + "\Signature\" + LvEmpNum + ".jpg") <> ? THEN
  DO:
    /* Employee Signature */
    chExcelApplication:Goto("R24C3") NO-ERROR.
    /*chExcelApplication:ActiveSheet:Pictures:Insert(CurrDir + "\Signature\" + LvEmpNum + ".jpg"):Select.*/
    mypict = chExcelApplication:Range("C24"):Parent:Pictures:Insert(CurrDir + "\Signature\" + LvEmpNum + ".jpg").
    mypict:TOP = chExcelApplication:Range("C24"):TOP.
    mypict:LEFT = chExcelApplication:Range("C24"):LEFT.
    RELEASE OBJECT mypict.
  END.
  /* Employee Signature */
  chExcelApplication:Goto("R26C9") NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = TODAY.

  ASSIGN v-cell = "r2c1".
  chExcelApplication:Goto(v-cell).
  chExcelApplication:ActiveSheet:Protect("advance4me").

  /* Create the record */
  MESSAGE "Please review the TimeSheet and update the information if needed before submitting TimeSheet" VIEW-AS ALERT-BOX.

  MESSAGE "Do you want to Submit the Time Sheet?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE "" UPDATE choice AS LOGICAL.

  IF choice = TRUE THEN
  DO:
    chExcelApplication:ActiveSheet:UnProtect("advance4me").
    chExcelApplication:Range("J12:K19"):SELECT.
    chExcelApplication:SELECTION:LOCKED = TRUE.
    chExcelApplication:ActiveSheet:Protect("advance4me").

    ASSIGN LvFileName = LvTSFolder + "\" + 
                        LvEmpNum + "-" + 
                        "WE-" + STRING(MONTH(LvWeekEnding)) + "-" +
                        STRING(DAY(LvWeekEnding)) + "-" +
                        STRING(YEAR(LvWeekEnding)) + "-" +
                        "SubmittedOn-" +
                        STRING(MONTH(today)) + "-" +
                        STRING(DAY(TODAY)) + "-" +
                        STRING(YEAR(today)) + "-" +
                        ".xls".


    chExcelApplication:ActiveSheet:SaveAs(LvFileName) no-error.
    IF SEARCH(LvFileName) = ? THEN
    DO:
      MESSAGE "Unable to create TimeSheet File. Please check the authorization to folder " LvTSFolder 
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    CREATE TimeSheet.
    assign
        TimeSheet.employee = LvEmpNum
        TimeSheet.WeekEnding = LvWeekEnding
        TimeSheet.file-name = LvFileName

        TimeSheet.SubmittedBy = LvEmpNum
        TimeSheet.SubmittedOn = TODAY.
  END.

  chWorkbook:Close(no) no-error.   
  chExcelApplication:Quit() no-error.

  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chHyper NO-ERROR.
