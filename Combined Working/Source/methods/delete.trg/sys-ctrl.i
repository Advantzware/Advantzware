/* sys-ctrl.i */

IF CAN-FIND(FIRST sys-ctrl-shipto OF sys-ctrl) THEN
FOR EACH sys-ctrl-shipto OF sys-ctrl EXCLUSIVE-LOCK:
  DELETE sys-ctrl-shipto.
END. /* each sys-ctrl-shipto */
