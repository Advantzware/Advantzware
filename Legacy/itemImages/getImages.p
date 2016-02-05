/* getImages.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFromDir AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipToDir AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipImageExt AS CHARACTER NO-UNDO.

DEFINE VARIABLE osFileName AS CHARACTER FORMAT 'X(30)' NO-UNDO.
DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
DEFINE VARIABLE itemNo AS CHARACTER NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF itemfg.

INPUT FROM OS-DIR(ipFromDir) NO-ECHO.
REPEAT:
  SET osFileName ^ attrList.
  IF attrList NE 'f' OR
     osFileName BEGINS '.' OR
     INDEX(osFileName,ipImageExt) EQ 0 THEN NEXT.
  ASSIGN
    itemNo = REPLACE(osFileName,ipFromDir,'')
    itemNo = REPLACE(itemNo,'\','')
    itemNo = REPLACE(itemNo,ipImageExt,'').
  FIND FIRST itemfg WHERE itemfg.company EQ ipCompany AND itemfg.i-no EQ itemNo
       EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE itemfg THEN DO:
    itemfg.box-image = ipToDir + '\' + osFileName.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    RELEASE itemfg.
    OS-RENAME VALUE(ipFromDir + '\' + osFileName)
              VALUE(ipToDir + '\' + osFileName).
    RUN logEntry ('Item: ' + itemNo).
  END.
  ELSE
     IF CAN-FIND(FIRST itemfg WHERE itemfg.company EQ ipCompany AND itemfg.i-no EQ itemNo) THEN
        RUN logEntry('Item ' + itemNo + ' is locked, will try again.').
  ELSE
     RUN logEntry('Item Not Found: ' + itemNo).
END.
INPUT CLOSE.

{itemImages/logEntry.i}
