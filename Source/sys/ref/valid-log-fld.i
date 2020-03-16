/* valid-log-fld.i */

  /* license check */
  DO WITH FRAME {&FRAME-NAME}:
    DEF VAR lAccess AS LOG NO-UNDO.
    IF {&nameField} EQ 'POEXPORT' AND
       {&tableName}.log-fld:SCREEN-VALUE EQ 'yes' THEN DO:
      RUN util/CheckModule.p ('ASI','POEXPORT', YES, OUTPUT lAccess) NO-ERROR.
      IF NOT lAccess THEN DO:
        {&tableName}.log-fld:SCREEN-VALUE = 'no'.
        APPLY 'ENTRY':U TO {&tableName}.log-fld.
        RETURN ERROR.
      END.
    END.
  END.
