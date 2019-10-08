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
    File        : UiTranslationBusinessEntity
    Purpose     : Business Entity for UiTranslation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:40:10
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsUiTranslation.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UiTranslationBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationBusinessEntity ():
        SUPER (DATASET dsUiTranslation:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.UiTranslationDataAccess":U .

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
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the dataset with a single eSmartUiTranslation record
                 per object for which translations exist in the database  
        Notes:   
        @param dsUiTranslation INPUT-OUTPUT DATASET
        @param poParameter The Parameter Object for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslatableControls (INPUT-OUTPUT DATASET dsUiTranslation, 
                                                poParameter AS Consultingwerk.CharacterHolder):
        
        DEFINE VARIABLE oDataAccess AS UiTranslationDataAccess NO-UNDO .  
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter, "poParameter":U) .
        
        IF NOT VALID-OBJECT (THIS-OBJECT:DataAccessObject) THEN 
            THIS-OBJECT:InitializeDataAccessObject () .
        
        oDataAccess = CAST (THIS-OBJECT:DataAccessObject, 
                            UiTranslationDataAccess) .
        
        oDataAccess:GetTranslatableControls (poParameter:Value) .
        
    END METHOD.
    
END CLASS.
