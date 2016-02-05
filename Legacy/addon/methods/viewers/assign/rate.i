/* rate.i */

DO WITH FRAME {&FRAME-NAME}:
IF rate.ratetype = 'Standard' AND
   CAN-FIND(FIRST buf-rate
       WHERE buf-rate.company = 'zzz'
         AND buf-rate.employee = 'zzzzz'
         AND buf-rate.ratetype NE 'Standard'
         AND buf-rate.shift = rate.shift:SCREEN-VALUE
         AND buf-rate.machine = rate.machine:SCREEN-VALUE) THEN
  DO:
    MESSAGE 'Add Default Rate Records for Shift: ' + rate.shift:SCREEN-VALUE +
           (IF rate.rate_usage:SCREEN-VALUE = 'NO' THEN
            '~& Machine: ' + rate.machine:SCREEN-VALUE ELSE '') + '?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE add-defaults AS LOGICAL.
    IF NOT add-defaults THEN
    RETURN.
    FOR EACH buf-rate NO-LOCK
        WHERE buf-rate.company = 'zzz'
          AND buf-rate.employee = 'zzzzz'
          AND buf-rate.ratetype NE 'Standard'
          AND buf-rate.shift = rate.shift:SCREEN-VALUE
          AND buf-rate.machine = rate.machine:SCREEN-VALUE:
      IF CAN-FIND(FIRST new-rate
             WHERE new-rate.company = rate.company
               AND new-rate.employee = rate.employee
               AND new-rate.ratetype = buf-rate.ratetype
               AND new-rate.shift = buf-rate.shift
               AND new-rate.machine = buf-rate.machine) THEN
      NEXT.
      CREATE new-rate.
      ASSIGN
        new-rate.company = rate.company
        new-rate.employee = rate.employee
        new-rate.shift = buf-rate.shift
        new-rate.machine = buf-rate.machine
        new-rate.ratetype = buf-rate.ratetype
        new-rate.rate = buf-rate.rate
        new-rate.rate_usage = buf-rate.rate_usage
        new-rate.factortype = buf-rate.factortype.
    END.
  END.
END.
