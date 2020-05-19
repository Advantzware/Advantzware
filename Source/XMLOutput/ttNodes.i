/* ttNodes.i */

DEFINE {1} SHARED TEMP-TABLE ttNodes NO-UNDO
  FIELD order       AS INTEGER
  FIELD nodeName    AS CHARACTER
  FIELD nodeValue   AS CHARACTER
  FIELD parentName  AS CHARACTER
  FIELD level       AS INTEGER
  FIELD nodeType    AS CHARACTER
  FIELD parentOrder AS INTEGER
  INDEX order IS PRIMARY UNIQUE order.
