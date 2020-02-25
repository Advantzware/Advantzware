DEFINE TEMP-TABLE tt-eb LIKE eb 
  FIELD selected AS LOGICAL
  FIELD row-id   AS ROWID 
  INDEX row-id row-id
  .
  