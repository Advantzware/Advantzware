DEFINE {1} SHARED TEMP-TABLE tt-stop NO-UNDO
  FIELD tran-type AS CHAR
  FIELD stop-no AS INT
  FIELD truck-no AS CHAR
  FIELD cust-no AS CHAR
  FIELD cust-name AS CHAR
  FIELD ship-no AS CHAR
  FIELD deliv-zone AS CHAR
  FIELD order-no AS INT
  FIELD rel-no AS INT
  FIELD bol-no AS INT
  FIELD no-units AS DEC
  FIELD ROWID AS ROWID
  INDEX tt-stop IS PRIMARY truck-no ASC stop-no ASC.
