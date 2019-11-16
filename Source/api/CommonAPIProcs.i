/*------------------------------------------------------------------------
    File        : api/CommonAPIProcs.i
    Purpose     : This include file contains common functions and
                  procedure shared among Inbound and Outbound procedures

    Syntax      :

    Description : This include file contains common functions and
                  procedure shared among Inbound and Outbound procedures

    Author(s)   : Porandla Mithun
    Created     : Tue August 8th 10:01:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
  
PROCEDURE updateRequestData:
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE INPUT        PARAMETER ipcField         AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcValue         AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldValuePrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldValueSuffix AS CHARACTER NO-UNDO.        
    
    ASSIGN
        cFieldValuePrefix = "$"
        cFieldValueSuffix = "$"
        .
        
    IF ipcValue EQ ? THEN
        ipcValue = "".
    
    /* This will add an escape character (\) before a (") so JSON won't throw error */
    ASSIGN
        ipcValue = REPLACE(ipcValue, '\','\\')
        ipcValue = REPLACE(ipcValue, '"','\"')
        .
    
    ioplcRequestData = REPLACE(ioplcRequestData, cFieldValuePrefix + ipcField + cFieldValueSuffix, ipcValue).
END PROCEDURE.
