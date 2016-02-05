/* valid-log-fld.i */

  /* license check */
  DO WITH FRAME {&FRAME-NAME}:
    IF {&nameField} EQ 'POEXPORT' AND
       {&tableName}.log-fld:SCREEN-VALUE EQ 'yes' THEN DO:
      RUN util/chk-mod2.p ('POEXPORT','NK1','PO Export Module.') NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        {&tableName}.log-fld:SCREEN-VALUE = 'no'.
        APPLY 'ENTRY':U TO {&tableName}.log-fld.
        RETURN ERROR.
      END.
    END.
  END.
