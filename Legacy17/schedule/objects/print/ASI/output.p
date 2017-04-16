/* output.p - custom output program for ASI */

DEFINE INPUT PARAMETER ipFileName AS CHARACTER NO-UNDO. /* report generated to */
DEFINE INPUT PARAMETER ipOutputTo AS CHARACTER NO-UNDO. /* output selection */
DEFINE INPUT PARAMETER ipPrintProgram AS CHARACTER NO-UNDO. /* report executed */
DEFINE INPUT PARAMETER ipPrintPrgmList AS CHARACTER NO-UNDO. /* from printPrgm var set in loadPro.p */
DEFINE INPUT PARAMETER ipFont AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipOrientation AS CHARACTER NO-UNDO.

DEFINE VARIABLE outputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE errorCode AS INTEGER NO-UNDO.
DEFINE VARIABLE subject AS CHARACTER NO-UNDO.
DEFINE VARIABLE body AS CHARACTER NO-UNDO.

ASSIGN
  subject = IF CAN-DO(ipPrintPrgmList,ipPrintProgram) THEN
            ENTRY(LOOKUP(ipPrintProgram,ipPrintPrgmList) + 1,ipPrintPrgmList)
            ELSE ipPrintProgram
  body = subject
  outputFile = 'c:\tmp\' + (IF ipOutputTo EQ 'Fax' THEN 'fax'
                       ELSE IF ipOutputTo EQ 'eMail' THEN 'att'
                       ELSE '') + STRING(TIME) + '.txt'.

IF NOT CAN-DO('Print,Screen',ipOutputTo) THEN
OS-COPY VALUE(ipFileName) VALUE(outputFile).

CASE ipOutputTo:
  WHEN 'eMail' THEN
  RUN custom/xpmail2.p ('ALL','sbPro.',outputFile,'',subject,body,OUTPUT errorCode).
  WHEN 'Fax' THEN
  RUN custom/asifax.p ('',outputFile,'',subject,body,OUTPUT errorCode).
  WHEN 'Print' THEN
  RUN custom/prntproc.p (ipFileName,ipFont,ipOrientation).
  WHEN 'Screen' THEN
  /* better to run notepad, gives all the same functionality w/o having to maintain
  RUN scr-rpt.w (ipFileName,subject,ipFont,ipOrientation). */
  OS-COMMAND NO-WAIT notepad.exe VALUE(ipFileName).
END CASE.
IF errorCode NE 0 THEN
MESSAGE 'ERROR:' errorCode VIEW-AS ALERT-BOX ERROR.
