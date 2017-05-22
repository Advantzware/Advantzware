/* rfidDefs.i */

DEFINE WORKFILE rfidData NO-UNDO
  FIELD rfidTag      AS CHARACTER
  FIELD transType    AS CHARACTER
  FIELD fromLocation AS CHARACTER
  FIELD toLocation   AS CHARACTER
  FIELD loadNumber   AS CHARACTER
  FIELD transDate    AS DATE
  FIELD transTime    AS CHARACTER
  FIELD toWarehouse  AS CHARACTER
  .

CREATE rfidData.
