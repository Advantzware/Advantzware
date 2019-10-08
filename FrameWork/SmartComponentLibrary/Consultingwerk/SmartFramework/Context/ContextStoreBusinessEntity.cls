/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ContextStoreBusinessEntity
    Purpose     : Business Entity for SmartContextStore
    Syntax      : 
    Description : Web2 / REST Store of Context Dataset
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 25.02.2016 08:33:55
    Notes       : Maintained by the Service Consultingwerk.Framework.Server.ContextDatasetStore
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.*         FROM PROPATH .
USING Consultingwerk.SmartFramework.Context.* FROM PROPATH .
USING Consultingwerk.OERA.*                   FROM PROPATH .
USING Consultingwerk.Util.*                   FROM PROPATH . 
USING Progress.Lang.*                         FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Context.ContextStoreBusinessEntity", type="BusinessEntity") .

CLASS Consultingwerk.SmartFramework.Context.ContextStoreBusinessEntity 
    INHERITS SmartBusinessEntity
    USE-WIDGET-POOL: 

    { Consultingwerk/SmartFramework/Context/dsContextStore.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ContextStoreBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ContextStoreBusinessEntity ():
        SUPER (DATASET dsContextStore:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Context.ContextStoreDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update 
                 operations                                                                     
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():
      
        FOR EACH eSmartContextStore:
        
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartContextStore:HANDLE, 
                                                           "SessionID":U,
                                                           "Cannot store Context without a Session ID."{&TRAN}) .
        
            IF NOT BUFFER eSmartContextStore:ERROR OR 
               DATASET dsContextStore:ERROR THEN 
               
                ASSIGN eSmartContextStore.LastAccessed = NOW .
        END.

        FOR EACH eSmartContextStore WHERE ROW-STATE (eSmartContextStore) = ROW-CREATED:
        
            ASSIGN eSmartContextStore.Created      = NOW . 
        
        END.  
              
    END METHOD.

END CLASS.
