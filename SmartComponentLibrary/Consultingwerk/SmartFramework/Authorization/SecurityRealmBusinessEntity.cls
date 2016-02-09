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
    File        : SecurityRealmBusinessEntity
    Purpose     : Business Entity for SecurityRealm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 13:35:44
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH . 
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityRealmBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/Authorization/dsSecurityRealm.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SecurityRealmBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityRealmBusinessEntity ():
        SUPER (DATASET dsSecurityRealm:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.SecurityRealmDataAccess":U .

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

        FOR EACH eSmartSecurityRealm:
            Consultingwerk.OERA.Validate:MaxLength (BUFFER eSmartSecurityRealm:HANDLE, 
                                                    "SecurityRealmCode":U, 
                                                    15, 
                                                    "The realm code should not be longer than three characters."{&TRAN}) .

            Consultingwerk.OERA.Validate:IsNotNullOrEmpty (BUFFER eSmartSecurityRealm:HANDLE, 
                                                           "SecurityRealmService":U, 
                                                           "Security realm service needs to be specified."{&TRAN}) . 
        END.
        
    END METHOD.

END CLASS.
