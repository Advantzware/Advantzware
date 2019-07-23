
/*------------------------------------------------------------------------
    File        : JSONProcs.p
    Purpose     : 

    Syntax      :

    Description : JSON Handler

    Author(s)   : Porandla Mithun
    Created     : Mon Jun 24 07:57:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

{api/inbound/ttRequest.i}

/* This is used for reading request JSON */
PROCEDURE ReadRequestData:
    DEFINE INPUT  PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRequest. 
    
    DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
    
    /* Remove curly braces from request JSON string at the start and end */
    ASSIGN 
        iplcRequestData = REPLACE(iplcRequestData,'~{','')
        iplcRequestData = REPLACE(iplcRequestData,'}','')
        oplSuccess = YES
        .

    temp-table-block:
    DO iCounter = 1 TO NUM-ENTRIES(iplcRequestData,',\"'):
       CREATE ttRequest.
       ASSIGN ttRequest.fieldOrder  = iCounter
              ttRequest.fieldName   = REPLACE(ENTRY(1,ENTRY(iCounter, iplcRequestData,',\"'),":"),'\"','')
              ttRequest.fieldValue  = REPLACE(ENTRY(2,ENTRY(iCounter, iplcRequestData,',\"'),":"),'\"','')
              ttRequest.fieldName   = REPLACE(ttRequest.fieldName,'"','')
              ttRequest.fieldValue  = REPLACE(ttRequest.fieldValue,'"','') no-error.
                            
       IF ERROR-STATUS:ERROR THEN DO:
           ASSIGN 
               oplSuccess = NO
               opcMessage = "Bad JSON Request"
               .
           LEAVE temp-table-block.
       END.
    END.
END PROCEDURE.


