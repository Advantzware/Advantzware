/* timeTriggers.i - rstark - 11/2/2019 */

&IF DEFINED(TIME-FIELDS) NE 0 &THEN
&IF "{1}" EQ "" &THEN
DEFINE VARIABLE iHourMax AS INTEGER NO-UNDO.
DEFINE VARIABLE iHourMin AS INTEGER NO-UNDO.

ASSIGN
    iHourMax = DYNAMIC-FUNCTION("sfHourMax")
    iHourMin = DYNAMIC-FUNCTION("sfHourMin")
    .
&ENDIF

&Scoped-define SELF-NAME {1}end_hour
ON LEAVE OF {1}end_hour IN FRAME {&FRAME-NAME} /* End Time */
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT iHourMin OR
                    INTEGER(SELF:SCREEN-VALUE) GT iHourMax.
    {methods/entryerr.i &error-message="Invalid Hour Entered"}
    {&methods/lValidateError.i NO}
END.

&Scoped-define SELF-NAME {1}end_minute
ON LEAVE OF {1}end_minute IN FRAME {&FRAME-NAME}
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR
                    INTEGER(SELF:SCREEN-VALUE) GT 59.
    {methods/entryerr.i &error-message="Invalid Minute Entered"}
    {&methods/lValidateError.i NO}
END.

&Scoped-define SELF-NAME {1}start_hour
ON LEAVE OF {1}start_hour IN FRAME {&FRAME-NAME}
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT iHourMin OR
                    INTEGER(SELF:SCREEN-VALUE) GT iHourMax.
    {methods/entryerr.i &error-message="Invalid Hour Entered"}
    {&methods/lValidateError.i NO}
END.

&Scoped-define SELF-NAME {1}start_minute
ON LEAVE OF {1}start_minute IN FRAME {&FRAME-NAME}
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR
                    INTEGER(SELF:SCREEN-VALUE) GT 59.
    {methods/entryerr.i &error-message="Invalid Minute Entered"}
    {&methods/lValidateError.i NO}
END.

&IF INDEX("{&TIME-FIELDS}","_second") NE 0 &THEN
&Scoped-define SELF-NAME {1}end_second
ON LEAVE OF {1}end_second IN FRAME {&FRAME-NAME}
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR
                    INTEGER(SELF:SCREEN-VALUE) GT 59.
    {methods/entryerr.i &error-message="Invalid Seconds Entered"}
    {&methods/lValidateError.i NO}
END.

&Scoped-define SELF-NAME {1}start_second
ON LEAVE OF {1}start_second IN FRAME {&FRAME-NAME}
DO:
    {&methods/lValidateError.i YES}
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR
                    INTEGER(SELF:SCREEN-VALUE) GT 59.
    {methods/entryerr.i &error-message="Invalid Seconds Entered"}
    {&methods/lValidateError.i NO}
END.
&ENDIF

&ENDIF
