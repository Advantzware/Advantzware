/* employee.i */

IF CAN-FIND(FIRST rate OF employee) THEN
FOR EACH rate OF employee EXCLUSIVE-LOCK:
  DELETE rate.
END.
IF CAN-FIND(FIRST emplogin OF employee) THEN
FOR EACH emplogin OF employee EXCLUSIVE-LOCK:
  DELETE emplogin.
END.
IF CAN-FIND(FIRST empmach OF employee) THEN
FOR EACH empmach OF employee EXCLUSIVE-LOCK:
  DELETE empmach.
END.
