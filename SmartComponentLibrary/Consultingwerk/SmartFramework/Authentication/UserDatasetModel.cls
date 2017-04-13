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
    File        : UserDatasetModel 
    Purpose     : Dataset Model for the UserBusinessEntity
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 21/12/2013 00:34:53,310+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.UserDatasetModel
    INHERITS UserDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDatasetModel class
        Notes:   
        @param pUserGuid The filter value for the eSmartUser.UserGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDatasetModel (pUserGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUser:Filter:UserGuid:EQ (pUserGuid) .

        THIS-OBJECT:SmartUser:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDatasetModel class
        Notes:   
        @param pUserName The filter value for the eSmartUser.UserName field
        @param pLoginCompanyGuid The filter value for the eSmartUser.LoginCompanyGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDatasetModel (pUserName AS CHARACTER, pLoginCompanyGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUser:Filter:UserName:EQ (pUserName) .
        THIS-OBJECT:SmartUser:Filter:LoginCompanyGuid:EQ (pLoginCompanyGuid) .

        THIS-OBJECT:SmartUser:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDatasetModel class
        Notes:   
        @param pUserSecurityId The filter value for the eSmartUser.UserSecurityId field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDatasetModel (pUserSecurityId AS INTEGER):
        SUPER ().

        THIS-OBJECT:SmartUser:Filter:UserSecurityId:EQ (pUserSecurityId) .

        THIS-OBJECT:SmartUser:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a UserDatasetModel filtered by a UserName
        Notes:   Alternative to the search construcors which are by UserGuid or 
                 UserName in combination to LoginCompanyGuid
        @param pcUserName The user name to search for
        @return A UserDatasetModel instance filtered by the user name (any login company) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC UserDatasetModel FromUsername (pcUserName AS CHARACTER):
        
        DEFINE VARIABLE oModel AS UserDatasetModel NO-UNDO . 
        
        oModel = NEW UserDatasetModel () . 
        
        oModel:SmartUser:Filter:UserName:EQ (pcUserName) .
        oModel:SmartUser:Filter:Run () .

        RETURN oModel .

    END METHOD.

END CLASS.
