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
    File        : LanguageBusinessEntity
    Purpose     : Business Entity for Language
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories
    Created     : 12.10.2012 11:03:51
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                  FROM PROPATH . 
USING Consultingwerk.SmartFramework.*        FROM PROPATH . 
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.LanguageBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsLanguage.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LanguageBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LanguageBusinessEntity ():
        SUPER (DATASET dsLanguage:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.LanguageDataAccess":U .

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

        FOR EACH eSmartLanguage ON ERROR UNDO, THROW:
            
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartLanguage:HANDLE, "LanguageIsoCode":U, "ISO code should not be empty."{&TRAN}).
            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartLanguage:HANDLE, "LanguageName":U, "Language Name should not be empty."{&TRAN}).
            
        END.
        
    END METHOD.

END CLASS.
