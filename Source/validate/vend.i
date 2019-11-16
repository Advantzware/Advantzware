/* vend.i Vendor validation */
IF LASTKEY = -1 THEN RETURN.
{&methods/lValidateError.i YES}
IF {1}:MODIFIED THEN DO:
   FIND FIRST vend WHERE vend.company = g_company AND                         
                         vend.vend-no = {1}:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL vend THEN DO:
         MESSAGE "Invalid vendor. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.
    IF AVAIL vend THEN {2} = vend.NAME.
    {&methods/lValidateError.i NO}
END.

