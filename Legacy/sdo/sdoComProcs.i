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

PROCEDURE chgQry1 :
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

PROCEDURE moveIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER rRowid AS ROWID NO-UNDO.
    DYNAMIC-FUNCTION("fetchrowident":U IN TARGET-PROCEDURE, STRING(rRowid), "").
END PROCEDURE.

PROCEDURE zChgQry :
/*------------------------------------------------------------------------------
  Purpose:     Changes where-clause and (optional) sort-order of query
  Parameters:  ipQueryString
  Notes:       entry 1 of ipQueryString is new where clause (WHERE keyword not reqd)
               entry 2 (optional) is new sort order (BY keyword IS optional)
------------------------------------------------------------------------------*/ 
    DEF INPUT PARAMETER ipQueryString AS CHAR NO-UNDO.
    DEF VAR cQueryWhere AS CHAR NO-UNDO.
    DEF VAR cQuerySort AS CHAR NO-UNDO.
MESSAGE ipQueryString VIEW-AS ALERT-BOX.
    IF NUM-ENTRIES(ipQueryString, "|") > 1 THEN DO:
        ASSIGN
            cQueryWhere = ENTRY(1,ipQueryString, "|")
            cQuerySort = ENTRY(2,ipQueryString, "|").
        IF SUBSTRING(cQuerySort,1,2) <> "BY"
        AND cQuerySort <> "" THEN ASSIGN
            cQuerySort = "BY " + cQuerySort.
        DYNAMIC-FUNCTION('setQueryWhere':U IN THIS-PROCEDURE, cQueryWhere).
        DYNAMIC-FUNCTION('setQuerySort':U IN THIS-PROCEDURE, cQuerySort).
    END.
    ELSE DO:
        ASSIGN
            cQueryWhere = ipQueryString.
        DYNAMIC-FUNCTION('setQueryWhere':U IN THIS-PROCEDURE, cQueryWhere).
    END.

    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).

END PROCEDURE.

PROCEDURE zMoveIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipRowid AS ROWID NO-UNDO.
    DYNAMIC-FUNCTION("fetchrowident":U IN TARGET-PROCEDURE, STRING(ipRowid), "").
END PROCEDURE.

PROCEDURE zRefQry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.

