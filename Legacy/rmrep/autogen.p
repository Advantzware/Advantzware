DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE tableName AS CHARACTER NO-UNDO INIT 'po-ord,po-ordl'.
DEFINE VARIABLE tagLabels AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttbl NO-UNDO
  FIELD fieldName AS CHAR
  FIELD fieldTable AS CHAR
  FIELD fieldLabel AS CHAR
  FIELD fieldExtent AS INT
  FIELD fieldExp AS CHAR.

OUTPUT TO 'rmrep\rmloadtg.i'.
PUT UNFORMATTED
  'DEFINE ~{1} SHARED WORKFILE w-po NO-UNDO' SKIP.
DO i = 1 TO NUM-ENTRIES(tableName):
  FIND _file NO-LOCK WHERE _file-name EQ ENTRY(i,tableName).
  FOR EACH _field OF _file NO-LOCK:
    IF CAN-FIND(FIRST ttbl WHERE fieldName EQ _field-name) THEN NEXT.
    PUT UNFORMATTED
      '  FIELD ' _field-name ' LIKE ' _file-name '.' _field-name SKIP.
    RUN createTtbl (_field._field-name,_file._file-name,_field._label,_field._extent,'').
  END.
END.
RUN createTtbl ('loc-bin','','Bin',0,'-').
RUN createTtbl ('company-name','','Company Name',0,'company.name').
RUN createTtbl ('partial','','Partial Qty',0,'-').
RUN createTtbl ('rcpt-qty','','Receipt Qty',0,'-').
RUN createTtbl ('tag-date','','Tag Date',0,'TODAY').
RUN createTtbl ('total-tags','','Total Tags',0,'-').
RUN createTtbl ('vend-name','','Vendor Name',0,'IF AVAIL vend THEN vend.name ELSE ~'~'').
PUT UNFORMATTED
  '  FIELD company-name LIKE company.name' SKIP
  '  FIELD loc-bin LIKE item.loc-bin' SKIP
  '  FIELD partial AS INT FORMAT ~'>>>,>>9~'' SKIP
  '  FIELD rcpt-qty AS INT FORMAT ~'>>>,>>9~'' SKIP
  '  FIELD tag-date AS DATE' SKIP
  '  FIELD total-tags AS INT' SKIP
  '  FIELD vend-name LIKE vend.name.' SKIP(1).

PUT UNFORMATTED '/* from-po' SKIP.
FOR EACH ttbl WHERE ttbl.fieldExp NE '-' BREAK BY fieldName:
  j = IF ttbl.fieldExtent EQ 0 THEN 1 ELSE ttbl.fieldExtent.
  DO i = 1 TO j:
    PUT UNFORMATTED 'w-po.' ttbl.fieldName.
    IF ttbl.fieldExtent NE 0 THEN
    PUT UNFORMATTED '[' i ']'.
    PUT UNFORMATTED ' = '.
    IF ttbl.fieldExp NE '' THEN
    PUT UNFORMATTED ttbl.fieldExp SKIP.
    ELSE
    DO:
      PUT UNFORMATTED ttbl.fieldTable '.' ttbl.fieldName.
      IF ttbl.fieldExtent NE 0 THEN
      PUT UNFORMATTED '[' i ']' SKIP.
      ELSE
      PUT UNFORMATTED SKIP.
    END.
  END. /* do i */
  tagLabels = tagLabels + ttbl.fieldLabel + ','.
END.
PUT UNFORMATTED '*/' SKIP.

PUT UNFORMATTED '/* tag labels' SKIP.
FOR EACH ttbl BREAK BY fieldName:
  j = IF ttbl.fieldExtent EQ 0 THEN 1 ELSE ttbl.fieldExtent.
  DO i = 1 TO j:
    PUT UNFORMATTED ttbl.fieldLabel.
    IF ttbl.fieldExtent NE 0 THEN
    PUT UNFORMATTED '[' i ']'.
    PUT UNFORMATTED ','.
  END. /* do i */
END.
PUT UNFORMATTED SKIP '*/' SKIP.

PUT UNFORMATTED '/* outputTagLine' SKIP.
FOR EACH ttbl BREAK BY fieldName:
  j = IF ttbl.fieldExtent EQ 0 THEN 1 ELSE ttbl.fieldExtent.
  DO i = 1 TO j:
    PUT UNFORMATTED '~'","~' w-po.' ttbl.fieldName.
    IF ttbl.fieldExtent NE 0 THEN
    PUT UNFORMATTED '[' i ']' SKIP.
    ELSE
    PUT UNFORMATTED SKIP.
  END. /* do i */
END.
PUT UNFORMATTED '*/' SKIP.

OUTPUT CLOSE.

PROCEDURE createTtbl:
  DEFINE INPUT PARAMETER ipFieldName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFieldTable AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFieldLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFieldExtent AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFieldExp AS CHARACTER NO-UNDO.

  CREATE ttbl.
  ASSIGN
    ttbl.fieldName = ipFieldName
    ttbl.fieldTable = ipFieldTable
    ttbl.fieldLabel = IF ipFieldLabel NE ? THEN ipFieldLabel
                      ELSE ipFieldName
    ttbl.fieldExtent = ipFieldExtent
    ttbl.fieldExp = ipFieldExp.
END PROCEDURE.
