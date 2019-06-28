/* ********************  Decade Common SDO Procedures  ********************** */
DEF SHARED VAR xFocus# AS WIDGET-HANDLE NO-UNDO.

PROCEDURE chgQry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   DYNAMIC-FUNCTION('closeQuery':U ).
   DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.

PROCEDURE chgQrySort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcSort AS CHAR NO-UNDO.
    DYNAMIC-FUNCTION('setQuerySort':U IN THIS-PROCEDURE, ipcSort).
    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.

PROCEDURE chgQryWhere :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcWhere AS CHAR NO-UNDO.
    DYNAMIC-FUNCTION('setQueryWhere':U IN THIS-PROCEDURE, ipcWhere).
    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.

PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
  RUN SUPER.
  
    
END PROCEDURE.

PROCEDURE jumpToRowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER rRowid AS ROWID NO-UNDO.
    DYNAMIC-FUNCTION("fetchrowident":U IN TARGET-PROCEDURE, STRING(rRowid), "").
END PROCEDURE.

PROCEDURE refreshQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.

