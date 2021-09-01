 
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

        TEMP-TABLE ttAttribute:READ-JSON ("FILE", ipcFileName) NO-ERROR.
	END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Read(INPUT filter AS CHARACTER, OUTPUT DATASET-HANDLE ProDataSet ):
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

	METHOD PUBLIC VOID ReplaceEntityAttributes( INPUT-OUTPUT ioplcRequestData AS LONGCHAR, iplcRequestDataType AS CHARACTER, ipBufferHandle AS HANDLE ):
		DEFINE VARIABLE hdField AS HANDLE NO-UNDO.
		IF NOT VALID-HANDLE(ipBufferHandle) THEN
		    RETURN.
		
		IF NOT ipBufferHandle:AVAILABLE THEN
		    RETURN.
		
		FOR EACH ttAttribute:
		    hdField = ipBufferHandle:BUFFER-FIELD(ttAttribute.dbFieldName) NO-ERROR.
		    IF VALID-HANDLE (hdField) THEN
                RUN Format_UpdateRequestData (
                    INPUT-OUTPUT ioplcRequestData,
                    INPUT        ttAttribute.attributeName,
                    INPUT        STRING(hdField:BUFFER-VALUE),
                    INPUT        iplcRequestDataType
                    ).  		          
		END.
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
END CLASS.