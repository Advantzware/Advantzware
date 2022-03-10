 
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
    	
	DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
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
        
        oAttribute = NEW system.Attribute().
        
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
		RETURN oAttribute:ReplaceAttributes(ipBufferHandle).
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetRequestDataAndType( INPUT iplcRequestData AS LONGCHAR, ipcRequestDataType AS CHARACTER ):
		oAttribute:RequestData     = iplcRequestData.
		oAttribute:RequestDataType = ipcRequestDataType.
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
            RETURN oAttribute:RequestData.
            
        CREATE QUERY hdQuery.
        
        hdQuery:ADD-BUFFER(hdBuffer).
        hdQuery:QUERY-PREPARE ("FOR EACH " + hdBuffer:NAME).
        hdQuery:QUERY-OPEN().
        
        lcRequestData = oAttribute:RequestData.
        
        REPEAT:
            hdQuery:GET-NEXT ().

            IF hdQuery:QUERY-OFF-END THEN
                LEAVE.
            
            lcConcatRequestData = lcConcatRequestData + ReplaceEntityAttributes( hdBuffer ).
        END.

        RETURN lcConcatRequestData.
        
        FINALLY:
            DELETE OBJECT hdQuery.	
        END FINALLY.
	END METHOD.

END CLASS.