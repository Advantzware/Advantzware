 
 /*------------------------------------------------------------------------
    File        : Attribute
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Mon Oct 18 22:31:02 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.Attribute: 
    DEFINE PUBLIC PROPERTY RequestDataType AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY RequestData AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC Attribute ( ):
        SUPER ().
        
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID UpdateRequestData( INPUT-OUTPUT iplcRequestData AS LONGCHAR, ipcAttributeName AS CHARACTER, ipcAttributeValue AS CHARACTER ):
        RUN Format_UpdateRequestData (
            INPUT-OUTPUT iplcRequestData,
            INPUT        ipcAttributeName,
            INPUT        ipcAttributeValue,
            INPUT        RequestDataType
            ). 
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ReplaceAttributes( ipBufferHandle AS HANDLE ):
        RETURN pReplaceAttributes(THIS-OBJECT:RequestData, ipBufferHandle:TABLE, ipBufferHandle).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ReplaceAttributes( iplcRequestData AS LONGCHAR, ipBufferHandle AS HANDLE ):
        RETURN pReplaceAttributes(iplcRequestData, ipBufferHandle:TABLE, ipBufferHandle).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ReplaceAttributes( iplcRequestData AS LONGCHAR, ipcSourceTable AS CHARACTER, ipBufferHandle AS HANDLE ):
        RETURN pReplaceAttributes(iplcRequestData, ipcSourceTable, ipBufferHandle).
    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LONGCHAR pReplaceAttributes( iplcRequestData AS LONGCHAR, ipcSourceTable AS CHARACTER, ipBufferHandle AS HANDLE ):
        DEFINE VARIABLE hdField        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldValue    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iExtent        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cAttributeName AS CHARACTER NO-UNDO.
        
        IF NOT VALID-HANDLE(ipBufferHandle) THEN
            RETURN iplcRequestData.
        
        FOR EACH attribute NO-LOCK
            WHERE attribute.dbTableName   EQ ipcSourceTable 
              AND attribute.attributeName NE ?
              AND attribute.dbFieldName   NE "":
            cFieldValue = "".
            
            hdField = ipBufferHandle:BUFFER-FIELD(attribute.dbFieldName) NO-ERROR.
                
            IF VALID-HANDLE (hdField) AND ipBufferHandle:AVAILABLE THEN
                cFieldValue = STRING(hdField:BUFFER-VALUE(attribute.dbFieldExtent)).
            
            UpdateRequestData (INPUT-OUTPUT iplcRequestData, attribute.attributeName, cFieldValue).                  
        END.        
        
        DO iIndex = 1 TO ipBufferHandle:NUM-FIELDS:
            cFieldValue = "".
            
            hdField = ipBufferHandle:BUFFER-FIELD(iIndex) NO-ERROR.

            IF VALID-HANDLE (hdField) THEN DO:
                cAttributeName = ipBufferHandle:NAME + "." + hdField:NAME.
                IF hdField:EXTENT GT 0 THEN DO:
                    DO iExtent = 1 TO hdField:EXTENT :
                        IF ipBufferHandle:AVAILABLE THEN
                            cFieldValue = STRING(hdField:BUFFER-VALUE(iExtent)).
                        
                        UpdateRequestData (INPUT-OUTPUT iplcRequestData, cAttributeName + "[" + STRING(iExtent) + "]", cFieldValue).
                    END.
                END.
                ELSE DO:
                    IF ipBufferHandle:AVAILABLE THEN
                        cFieldValue = STRING(hdField:BUFFER-VALUE()).
                    
                    UpdateRequestData (INPUT-OUTPUT iplcRequestData, cAttributeName, cFieldValue).                  
                END.
            END.
        END.
                
        RETURN iplcRequestData.
    END METHOD.    
END CLASS.