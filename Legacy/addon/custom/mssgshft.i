/* mssgshft.i */

FIND FIRST {&file} WHERE {&file}.company = ip-company {&where}
                     AND {&file}.shift = ip-startshift NO-LOCK NO-ERROR.
IF AVAILABLE {&file} THEN
DO WHILE TRUE:
  IF NOT CAN-DO(ip-startshift + ',' + ip-endshift,{&file}.shift) THEN
  DO:
    op-missingshift = {&file}.shift.
    RETURN.
  END.
  IF {&file}.shift = ip-endshift THEN
  LEAVE.
  FIND NEXT {&file} WHERE {&file}.company = ip-company {&where} NO-LOCK NO-ERROR.
  IF NOT AVAILABLE {&file} THEN
  FIND FIRST {&file} WHERE {&file}.company = ip-company {&where} NO-LOCK NO-ERROR.
END. /* do while */

IF CAN-FIND(FIRST {&file} WHERE {&file}.company = ip-company {&where}) THEN
RETURN.
