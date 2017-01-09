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
    File        : SecurityRealm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:40:27,837+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityRealmDatasetModel
    INHERITS SecurityRealmDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityRealmDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityRealmDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityRealmDatasetModel class
        Notes:   
        @param pSecurityRealmGuid The filter value for the eSmartSecurityRealm.SecurityRealmGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityRealmDatasetModel (pSecurityRealmGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartSecurityRealm:Filter:SecurityRealmGuid:EQ (pSecurityRealmGuid) .

        THIS-OBJECT:SmartSecurityRealm:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
