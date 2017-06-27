{methods/lValidateError.i YES}
{est/valinks2.i {1} {2}}

RUN est/val-inks.p (FRAME {&FRAME-NAME}:HANDLE, INT("{2}"), INT("{1}")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
{methods/lValidateError.i NO}
