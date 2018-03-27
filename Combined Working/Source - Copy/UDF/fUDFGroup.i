/* fUDFGroup.i */

DEFINE VARIABLE cUDFGroup AS CHARACTER NO-UNDO.

FUNCTION fUDFGroup RETURNS CHARACTER (ipcUDFGroup AS CHARACTER):
    FIND FIRST mfgroup NO-LOCK
         WHERE LOOKUP(ipcUDFGroup,mfgroup.mfgroup_data,"|") NE 0
         NO-ERROR.
    RETURN IF NOT AVAILABLE mfgroup THEN ?
           ELSE ENTRY(1,mfgroup.mfgroup_data,"|").
END FUNCTION.

&IF "{1}" NE "" &THEN
cUDFGroup = fUDFGroup("{1}").
&ENDIF
