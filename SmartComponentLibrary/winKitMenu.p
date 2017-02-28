DEF VAR cPrgrm AS CHAR NO-UNDO.
DEF VAR cMenu AS CHAR NO-UNDO.
DEF VAR lAdm AS LOG NO-UNDO.
DEF VAR cType AS CHAR NO-UNDO.

DEF TEMP-TABLE mnuItm NO-UNDO
  FIELD mnuItm AS CHAR
    INDEX mnuItem mnuItm.

INPUT FROM c:\tmp\windows.txt NO-ECHO.
REPEAT:
  IMPORT cPrgrm.
  CREATE mnuItm.
  ASSIGN
    cPrgrm = SUBSTR(cPrgrm,R-INDEX(cPrgrm,'\') + 1)
    mnuItm.mnuItm = cPrgrm
    .
END.
INPUT CLOSE.

INPUT FROM c:\tmp\menu.lst NO-ECHO.
OUTPUT TO c:\tmp\winKitMenu.txt.
REPEAT:
  IMPORT cPrgrm cMenu.
  FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ cPrgrm NO-ERROR.
  ASSIGN
    lAdm = CAN-FIND(FIRST mnuItm WHERE mnuItm.mnuItm EQ cPrgrm)
    cType = IF INDEX(cPrgrm,'.') EQ 0 THEN 'Menu'
       ELSE IF AVAIL(prgrms) AND prgrms.prgmname BEGINS 'r-' THEN 'Report'
       ELSE IF AVAIL(prgrms) AND prgrms.dir_group EQ 'listobjs' THEN 'Report'
       ELSE IF lAdm THEN 'Window'
       ELSE 'Unknown'
    .
  EXPORT cPrgrm cMenu lAdm cType.
END.
OUTPUT CLOSE.
INPUT CLOSE.
