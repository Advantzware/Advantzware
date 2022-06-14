/* output.p - custom output program for ASI */

DEFINE INPUT PARAMETER ipFileName AS CHARACTER NO-UNDO. /* report generated to */
DEFINE INPUT PARAMETER ipOutputTo AS CHARACTER NO-UNDO. /* output selection */
DEFINE INPUT PARAMETER ipPrintProgram AS CHARACTER NO-UNDO. /* report executed */
DEFINE INPUT PARAMETER ipPrintPrgmList AS CHARACTER NO-UNDO. /* from printPrgm var set in loadPro.p */
DEFINE INPUT PARAMETER ipFont AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipOrientation AS CHARACTER NO-UNDO.

/*DEFINE VARIABLE outputFile AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE errorCode AS INTEGER NO-UNDO.
DEFINE VARIABLE subject AS CHARACTER NO-UNDO.
DEFINE VARIABLE body AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.

ASSIGN
  subject = IF CAN-DO(ipPrintPrgmList,ipPrintProgram) THEN
            ENTRY(LOOKUP(ipPrintProgram,ipPrintPrgmList) + 1,ipPrintPrgmList)
            ELSE ipPrintProgram
  body    = subject
/*  outputFile = 'c:\tmp\' + (IF ipOutputTo EQ 'Fax' THEN 'fax'  */
/*                       ELSE IF ipOutputTo EQ 'eMail' THEN 'att'*/
/*                       ELSE '') + STRING(TIME) + '.txt'        */
          .

/*IF NOT CAN-DO('Print,Screen',ipOutputTo) THEN*/
/*OS-COPY VALUE(ipFileName) VALUE(outputFile). */

CASE ipOutputTo:
  WHEN 'eMail' THEN DO:
    RUN AOA/Recipients.w (INPUT-OUTPUT cRecipients).
    IF cRecipients NE "" THEN
    RUN spSendEmail (1, cRecipients, "", "", "", subject, body, ipFileName, OUTPUT lError, OUTPUT cMessage).
  END. // email
  WHEN 'Print' THEN
  RUN custom/prntproc.p (ipFileName,ipFont,ipOrientation).
  WHEN 'Screen' THEN
  /* better to run notepad, gives all the same functionality w/o having to maintain
  RUN scr-rpt.w (ipFileName,subject,ipFont,ipOrientation). */
&IF DEFINED(FWD-VERSION) > 0 &THEN
  open-mime-resource "text/plain" STRING("file:///" + ipFileName) FALSE.
&ELSE
  OS-COMMAND NO-WAIT notepad.exe VALUE(ipFileName).
&ENDIF
END CASE.
IF errorCode NE 0 THEN
MESSAGE 'ERROR:' errorCode VIEW-AS ALERT-BOX ERROR.
