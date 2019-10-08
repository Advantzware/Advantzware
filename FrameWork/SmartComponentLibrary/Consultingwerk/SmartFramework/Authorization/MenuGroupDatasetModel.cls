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
    File        : MenuGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:38:52,357+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetModel
    INHERITS MenuGroupDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuGroupDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuGroupDatasetModel class
        Notes:   
        @param pMenuGroupGuid The filter value for the eSmartMenuGroup.MenuGroupGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupDatasetModel (pMenuGroupGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartMenuGroup:Filter:MenuGroupGuid:EQ (pMenuGroupGuid) .

        THIS-OBJECT:SmartMenuGroup:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
