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
    File        : SecurityToken
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17/04/2014 20:51:25,749+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityTokenDatasetModel
    INHERITS SecurityTokenDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityTokenDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityTokenDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityTokenDatasetModel class
        Notes:   
        @param pSecurityTokenGuid The filter value for the eSmartSecurityToken.SecurityTokenGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityTokenDatasetModel (pSecurityTokenGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartSecurityToken:Filter:SecurityTokenGuid:EQ (pSecurityTokenGuid) .

        THIS-OBJECT:SmartSecurityToken:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
