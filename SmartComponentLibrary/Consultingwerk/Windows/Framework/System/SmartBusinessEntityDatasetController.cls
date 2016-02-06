/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : SmartBusinessEntityDatasetController
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 19:02:05
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.System.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartBusinessEntityDatasetController 
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF : 

    DEFINE VARIABLE oChildTableAdapter AS SmartDatasetAdapter NO-UNDO . 

    { Consultingwerk/SmartFramework/System/dsSmartBusinessEntity.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller                                                                     
        Notes:   Interface IDatasetController member                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBusinessEntityDatasetController class                                                                      
        Notes:   Assigns the DatasetHandle property                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsSmartBusinessEntity:HANDLE .         
        
    END CONSTRUCTOR.
    
	/*------------------------------------------------------------------------------
	    Purpose: Creates the dependet tables for the BusinessEntity
	    Notes:
	    @param pcBusinessEntityName The BusinessEntity name
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CreateSmartBusinessEntityTable (pcBusinessEntityName AS CHARACTER):
		
		DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
        DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.
        
        DEFINE BUFFER eSmartBusinessEntityTable FOR eSmartBusinessEntityTable . 
        
		Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:FetchDataset ("":U, 
		                                                                        pcBusinessEntityName, 
		                                                                        OUTPUT DATASET-HANDLE hDataset).
		
		Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsSmartBusinessEntity:HANDLE, TRUE).
		
        DO iBuffer = 1 TO hDataset:NUM-BUFFERS ON ERROR UNDO, THROW:
            
            CREATE eSmartBusinessEntityTable.
            
            ASSIGN
                eSmartBusinessEntityTable.BusinessEntityGuid      = eSmartBusinessEntity.BusinessEntityGuid
                eSmartBusinessEntityTable.TableName               = hDataset:GET-BUFFER-HANDLE (iBuffer):NAME
                eSmartBusinessEntityTable.TableOrder              = iBuffer
                eSmartBusinessEntityTable.BusinessEntityTableGuid = ? /* allow more than a single record */
                .
        END.
		
		SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT):SubmitChanges ().
		
		IF VALID-OBJECT (oChildTableAdapter) THEN 
		    oChildTableAdapter:RetrieveData() . 
		
		FINALLY:
		    Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hDataset).
		END FINALLY.
	
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.                                                                      
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object detaching from the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):
        
    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.                                                                     
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object attaching to the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

        IF TYPE-OF (poConsumer, SmartDatasetAdapter) AND 
           CAST (poConsumer, SmartDatasetAdapter):EntityTable = "eSmartBusinessEntityTable":U THEN 
            
            oChildTableAdapter = CAST (poConsumer, SmartDatasetAdapter) . 

    END METHOD.

END CLASS.
