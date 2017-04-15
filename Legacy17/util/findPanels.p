/* findpanelNames.p */

&SCOPED-DEFINE skipName address.,phone.

DEFINE VARIABLE wFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE baseName AS CHARACTER NO-UNDO LABEL 'Program' FORMAT 'x(10)'.
DEFINE VARIABLE codeLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE panelName AS CHARACTER NO-UNDO LABEL 'Panel?' FORMAT 'x(10)'.

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD panel-name AS CHAR FORMAT 'x(10)'
  FIELD base-name AS CHAR FORMAT 'x(10)'
  FIELD dir-name AS CHAR  FORMAT 'x(10)'
  FIELD code-line AS CHAR FORMAT 'x(20)'
    INDEX ttbl IS PRIMARY panel-name base-name.

DEFINE STREAM s1.

INPUT STREAM s1 FROM 'util/findPanels1.txt' NO-ECHO.
REPEAT:
  IMPORT STREAM s1 wFile.
  ASSIGN
    baseName = SUBSTR(wFile,R-INDEX(wFile,'\') + 1)
    baseName = REPLACE(baseName,'.w','.').
  INPUT FROM VALUE(wFile) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED codeLine.
    IF INDEX(codeLine,'init-object') EQ 0 THEN NEXT.
    IMPORT UNFORMATTED codeLine.
    IF INDEX(codeLine,'adm/objects') NE 0 OR
       INDEX(codeLine,'browsers') NE 0 OR
       INDEX(codeLine,'viewers') NE 0 OR
       INDEX(codeLine,'windows') NE 0 OR
       INDEX(codeLine,'viewerid') NE 0 THEN NEXT.
    ASSIGN
      codeLine = LEFT-TRIM(codeLine)
      codeLine = REPLACE(codeLine,'INPUT  ~'','')
      codeLine = REPLACE(codeLine,'~':U ,','')
      panelName = SUBSTR(codeLine,R-INDEX(codeLine,'/') + 1)
      panelName = REPLACE(panelName,'.w','.')
      panelName = REPLACE(panelName,'.r','.').
    IF baseName EQ panelName OR CAN-DO('{&skipName}',panelName) OR
       NOT CAN-FIND(prgrms WHERE prgrms.prgmname EQ panelName) THEN NEXT.
    FIND prgrms NO-LOCK WHERE prgrms.prgmname EQ panelName.
    CREATE ttbl.
    ASSIGN
      ttbl.panel-name = panelName
      ttbl.base-name = baseName
      ttbl.dir-name = prgrms.dir_group
      ttbl.code-line = codeLine.
  END.
  INPUT CLOSE.
END.
INPUT STREAM s1 CLOSE.

FOR EACH prgrms EXCLUSIVE-LOCK:
  prgrms.mfgroup = ''.
END.

FOR EACH ttbl NO-LOCK BREAK BY ttbl.panel-name:
  IF FIRST-OF(ttbl.panel-name) THEN DO:
    FIND prgrms EXCLUSIVE-LOCK WHERE prgrms.prgmname EQ ttbl.panel-name.
    prgrms.mfgroup = ttbl.base-name.
  END.
  ELSE
  prgrms.mfgroup = prgrms.mfgroup + ',' + ttbl.base-name.
END.
