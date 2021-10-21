 
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
    DEFINE TEMP-TABLE ttAttribute NO-UNDO
        FIELD dbFieldName   AS CHARACTER
        FIELD dbFieldExtent AS INTEGER
        FIELD attributeName AS CHARACTER
        .

	DEFINE PUBLIC PROPERTY RequestData AS CHARACTER NO-UNDO 
	GET.
	SET. 

    DEFINE PUBLIC PROPERTY RequestDataType AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    DEFINE PUBLIC PROPERTY SourceTable AS CHARACTER NO-UNDO 
    GET.
    SET. 
        
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC Attribute ( ipcSourceTable AS CHARACTER ):
		SUPER ().
        
        SourceTable = ipcSourceTable.		
        
        LoadAttributes().
	END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadAttributes( ):
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        
        cFileName = "entities\" + SourceTable + ".json".
        
        IF SEARCH (cFileName) EQ ? THEN
            RETURN.

        TEMP-TABLE ttAttribute:READ-JSON ("FILE", SEARCH (cFileName)) NO-ERROR.
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ReplaceAttributes( iplcRequestData AS LONGCHAR, ipBufferHandle AS HANDLE ):
        RETURN pReplaceAttributes(iplcRequestData, ipBufferHandle).
    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ReplaceAttributes( ipBufferHandle AS HANDLE ):
        RETURN pReplaceAttributes(THIS-OBJECT:RequestData, ipBufferHandle).
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE LONGCHAR pReplaceAttributes( iplcRequestData AS LONGCHAR, ipBufferHandle AS HANDLE ):
        DEFINE VARIABLE hdField       AS HANDLE   NO-UNDO.
        
        IF NOT VALID-HANDLE(ipBufferHandle) THEN
            RETURN iplcRequestData.
        
        IF NOT ipBufferHandle:AVAILABLE THEN
            RETURN iplcRequestData.

        FOR EACH ttAttribute
            WHERE ttAttribute.attributeName NE ?:
            hdField = ipBufferHandle:BUFFER-FIELD(ttAttribute.dbFieldName) NO-ERROR.
            IF VALID-HANDLE (hdField) THEN
                RUN Format_UpdateRequestData (
                    INPUT-OUTPUT iplcRequestData,
                    INPUT        ttAttribute.attributeName,
                    INPUT        STRING(hdField:BUFFER-VALUE(ttAttribute.dbFieldExtent)),
                    INPUT        RequestDataType
                    ).                  
        END.
        
        RETURN iplcRequestData.
    END METHOD.
        
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetSourceTable( ipcSourceTable AS CHARACTER ):
		
		SourceTable = ipcSourceTable.

	END METHOD.


    METHOD PUBLIC VOID UpdateDelimiter (INPUT-OUTPUT ioplcRequestData AS LONGCHAR):
    /*------------------------------------------------------------------------------
     Purpose: Replaces and trims the delimiters of the request data for a given data type
     Notes:
    ------------------------------------------------------------------------------*/
        IF RequestDataType EQ "JSON" THEN
            ASSIGN
                ioplcRequestData = REPLACE(ioplcRequestData, "$comma$", ",")
                ioplcRequestData = TRIM(ioplcRequestData, ",")
                .
        ELSE
            ASSIGN
                ioplcRequestData = REPLACE(ioplcRequestData, "$linefeed$", "~n")
                ioplcRequestData = TRIM(ioplcRequestData, "~n")
                .
                
        /* Replaces $page$ with Form Feed character (PAGE keyword) */
        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData, "$formfeed$", CHR(12))
            ioplcRequestData = TRIM(ioplcRequestData, CHR(12))
            .    
    END METHOD.
END CLASS.