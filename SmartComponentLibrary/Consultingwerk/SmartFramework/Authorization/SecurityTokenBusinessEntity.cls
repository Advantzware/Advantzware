/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SecurityTokenBusinessEntity
    Purpose     : Business Entity for SmartSecurityToken
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2014 20:51:22
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

@BusinessEntityTable (name="eSmartSecurityToken", MandatoryColumns="SecurityTokenCode", DefaultSort="BY eSmartSecurityToken.SecurityTokenCode") .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityTokenBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/Authorization/dsSecurityToken.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SecurityTokenBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityTokenBusinessEntity ():
        SUPER (DATASET dsSecurityToken:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authorization.SecurityTokenDataAccess":U .

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

END CLASS.
