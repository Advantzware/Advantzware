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
    File        : UserGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:41:25,443+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.UserGroupDatasetModel
    INHERITS UserGroupDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserGroupDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserGroupDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserGroupDatasetModel class
        Notes:   
        @param pUserGroupGuid The filter value for the eSmartUserGroup.UserGroupGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserGroupDatasetModel (pUserGroupGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUserGroup:Filter:UserGroupGuid:EQ (pUserGroupGuid) .

        THIS-OBJECT:SmartUserGroup:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
