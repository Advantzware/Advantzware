/* flute.i */

IF CAN-FIND(FIRST stack-flute where stack-flute.company = flute.company and
                                    stack-flute.loc = flute.loc and
                                    stack-flute.code = flute.code) THEN
FOR EACH stack-flute where stack-flute.company = flute.company and
                           stack-flute.loc = flute.loc and
                           stack-flute.code = flute.code
                                    EXCLUSIVE-LOCK:
  DELETE stack-flute.
END.
