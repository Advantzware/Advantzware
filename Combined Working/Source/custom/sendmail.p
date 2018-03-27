/* custom/sendmail.p   E-mail list file 
         Won't work with Outlook Express  10/02  YSK */
/*
DEF INPUT PARAMETER ip-listname AS cha NO-UNDO.
DEF INPUT PARAMETER ip-send-cust AS cha NO-UNDO.
*/

DEF INPUT PARAM cProfile AS CHAR NO-UNDO.
DEF INPUT PARAM iPriority AS INT NO-UNDO.
DEF INPUT PARAM cSubject AS CHAR NO-UNDO.
DEF INPUT PARAM cText AS CHAR NO-UNDO.
DEF INPUT PARAM cTo AS CHAR NO-UNDO.
DEF INPUT PARAM cFiles AS CHAR NO-UNDO.

DEF VAR chSession AS COM-HANDLE NO-UNDO.
DEF VAR chMessage AS COM-HANDLE NO-UNDO.
DEF VAR chRecip AS COM-HANDLE NO-UNDO.
DEF VAR chFiles AS COM-HANDLE NO-UNDO.

DEF VAR iLoop AS INT NO-UNDO.

CREATE "MAPI.SESSION" chsession.
MESSAGE "Step 1" VIEW-AS ALERT-BOX.

chSession:Login(cProfile).
MESSAGE "Step 2" VIEW-AS ALERT-BOX.

ASSIGN chMessage = chSession:Outbox:Messages:ADD()
       chMessage:Subject = cSubject
       chMessage:TEXT = cText
       chMessage:importance = iPriority.
MESSAGE "Step 3" VIEW-AS ALERT-BOX.
/* Send to */

DO iLoop = 1 TO NUM-ENTRIES(cTo):
   ASSIGN chRecip = chMessage:Recipients:ADD()
          chRecip:NAME = ENTRY(iLoop,cTo)
          chRecip:TYPE = 1.
   chRecip:Resolve.
END.

/*Attachments */
DO iLoop = 1 TO NUM-ENTRIES(cFiles):
   ASSIGN chMessage:TEXT = chMessage:TEXT + CHR(10)
          chFiles = chMessage:Attachments:ADD()
          chFiles:NAME = ENTRY(iLoop,cFiles)
          chFiles:SOURCE = ENTRY(iLoop,cFiles).
END.

/*send message */
chmessage:UPDATE(TRUE, TRUE).
chMessage:SEND(TRUE, FALSE).
chSession:Logoff.

RUN ReleaseObjects.

PROCEDURE ReleaseObjects:
    RELEASE OBJECT chRecip.
    RELEASE OBJECT chMessage.
    RELEASE OBJECT chSession.
    RELEASE OBJECT chFiles.
END.


