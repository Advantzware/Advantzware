/* vend-upd.i Vendor validation */

IF {1}:MODIFIED THEN DO:
   FIND FIRST vend WHERE vend.company = g_company AND                         
                         vend.vend-no = {1}:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL vend  THEN DO:
         MESSAGE "Invalid vendor. Try Help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO {1}.
         RETURN NO-APPLY.
    END.
    IF AVAIL vend THEN {2} = vend.NAME.
    
END.
