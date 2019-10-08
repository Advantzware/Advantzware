/* admViewersUsing.i */

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH.

{methods/defines/lValidateError.i}
DEFINE VARIABLE hProgram AS HANDLE NO-UNDO.

PROCEDURE pValidateError:
    DEFINE OUTPUT PARAMETER oplValidateError AS LOGICAL NO-UNDO.
    
    oplValidateError = lValidateError.
    
END PROCEDURE.    
