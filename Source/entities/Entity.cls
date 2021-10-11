 
 /*------------------------------------------------------------------------
    File        : Entity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Sun Aug 29 00:39:47 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.Entity IMPLEMENTS entities.IEntity INHERITS BusinessEntity: 
    
    DEFINE TEMP-TABLE ttAttribute NO-UNDO
        FIELD dbFieldName   AS CHARACTER
        FIELD attributeName AS CHARACTER
        .
	
	DEFINE VARIABLE RequestData     AS LONGCHAR  NO-UNDO.
	DEFINE VARIABLE RequestDataType AS CHARACTER NO-UNDO.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecordLimit AS HANDLE NO-UNDO
            GET.
            SET.
            
	CONSTRUCTOR PUBLIC Entity ( dsEntity AS HANDLE, srcEntity AS HANDLE ):
        DEFINE VARIABLE hDataSourceArray AS HANDLE    NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray   AS CHARACTER NO-UNDO EXTENT 1.

        SUPER (dsEntity).

        /* Data Source for each table in dataset. Should be in table order as defined 
           in DataSet */     
        hDataSourceArray[1] = srcEntity.
                
        /* Skip-list entry for each table in dataset. Should be in temp-table order 
           as defined in DataSet */
        /* Each skip-list entry is a comma-separated list of field names, to be
           ignored in create stmt */
            
        cSkipListArray[1] = "".
        
        THIS-OBJECT:ProDataSource = hDataSourceArray.
        THIS-OBJECT:SkipList = cSkipListArray.
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL Find( cFilter AS CHARACTER ):
        THIS-OBJECT:ProDataSet:GET-BUFFER-HANDLE(1):BATCH-SIZE = 1.
        
        SUPER:ReadData(cFilter).
        
        THIS-OBJECT:ProDataSet:GET-BUFFER-HANDLE(1):BATCH-SIZE = 0.
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL Find( cFilter AS CHARACTER, OUTPUT DATASET-HANDLE ProDataSet):
		THIS-OBJECT:ProDataSet:GET-BUFFER-HANDLE(1):BATCH-SIZE = 1.
		
		SUPER:ReadData(cFilter).
		
        THIS-OBJECT:ProDataSet:GET-BUFFER-HANDLE(1):BATCH-SIZE = 0.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadAttributes( ipcFileName AS CHARACTER ):
		
		IF SEARCH (ipcFileName) EQ ? THEN
		    RETURN.

        TEMP-TABLE ttAttribute:READ-JSON ("FILE", SEARCH (ipcFileName)) NO-ERROR.
	END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Read(INPUT filter AS CHARACTER, OUTPUT DATASET-HANDLE ProDataSet):
        SUPER:ReadData(filter).
    END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Read(INPUT filter AS CHARACTER ):
        SUPER:ReadData(filter).
    END METHOD.

          
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Create(INPUT-OUTPUT DATASET-HANDLE ProDataSet ):           
               
        SUPER:CreateData(DATASET-HANDLE ProDataSet).        
    END METHOD.    
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR ReplaceEntityAttributes( ipBufferHandle AS HANDLE ):
		DEFINE VARIABLE hdField       AS HANDLE   NO-UNDO.
		DEFINE VARIABLE lcRequestData AS LONGCHAR NO-UNDO.
		
		lcRequestData = THIS-OBJECT:RequestData.
		
		IF NOT VALID-HANDLE(ipBufferHandle) THEN
		    RETURN lcRequestData.
		
		IF NOT ipBufferHandle:AVAILABLE THEN
		    RETURN lcRequestData.
		
		FOR EACH ttAttribute
		    WHERE ttAttribute.attributeName NE ?:
		    hdField = ipBufferHandle:BUFFER-FIELD(ttAttribute.dbFieldName) NO-ERROR.
		    IF VALID-HANDLE (hdField) THEN
                RUN Format_UpdateRequestData (
                    INPUT-OUTPUT lcRequestData,
                    INPUT        ttAttribute.attributeName,
                    INPUT        STRING(hdField:BUFFER-VALUE),
                    INPUT        RequestDataType
                    ).  		        
		END.
		
		RETURN lcRequestData.
	END METHOD.

    METHOD PRIVATE VOID pUpdateDelimiter (INPUT-OUTPUT ioplcRequestData AS LONGCHAR):
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

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetRequestDataAndType( INPUT iplcRequestData AS LONGCHAR, ipcRequestDataType AS CHARACTER ):
		ASSIGN
		    RequestData     = iplcRequestData
		    RequestDataType = ipcRequestDataType
		    .
	END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Update(INPUT-OUTPUT DATASET-HANDLE ProDataSet):           
    
        SUPER:UpdateData(DATASET-HANDLE ProDataSet BY-REFERENCE).          
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Delete(INPUT-OUTPUT DATASET-HANDLE ProDataSet):                  
    
        SUPER:DeleteData(DATASET-HANDLE ProDataSet BY-REFERENCE).
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LONGCHAR UpdateRequestData( ):
		DEFINE VARIABLE hdQuery             AS HANDLE   NO-UNDO.
		DEFINE VARIABLE hdBuffer            AS HANDLE   NO-UNDO.
		DEFINE VARIABLE lcRequestData       AS LONGCHAR NO-UNDO.
		DEFINE VARIABLE lcConcatRequestData AS LONGCHAR NO-UNDO.
		
		hdBuffer = THIS-OBJECT:ProDataset:GET-TOP-BUFFER().
        IF NOT VALID-HANDLE (hdBuffer) THEN
            RETURN THIS-OBJECT:RequestData.
            
        CREATE QUERY hdQuery.
        
        hdQuery:ADD-BUFFER(hdBuffer).
        hdQuery:QUERY-PREPARE ("FOR EACH " + hdBuffer:NAME).
        hdQuery:QUERY-OPEN().
        
        lcRequestData = THIS-OBJECT:RequestData.
        
        REPEAT:
            hdQuery:GET-NEXT ().

            IF hdQuery:QUERY-OFF-END THEN
                LEAVE.
            
            lcConcatRequestData = lcConcatRequestData + ReplaceEntityAttributes( hdBuffer ).
        END.

        pUpdateDelimiter (INPUT-OUTPUT lcConcatRequestData).
        
        RETURN lcConcatRequestData.
        
        FINALLY:
            DELETE OBJECT hdQuery.	
        END FINALLY.
	END METHOD.

END CLASS.