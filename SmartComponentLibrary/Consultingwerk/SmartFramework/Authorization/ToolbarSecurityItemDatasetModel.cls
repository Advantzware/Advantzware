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
    File        : ToolbarSecurityItem
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20/04/2014 11:44:44,615+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemDatasetModel
    INHERITS ToolbarSecurityItemDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolbarSecurityItemDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolbarSecurityItemDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolbarSecurityItemDatasetModel class
        Notes:   
        @param pToolbarSecurityItemGuid The filter value for the eSmartToolbarSecurityItem.ToolbarSecurityItemGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolbarSecurityItemDatasetModel (pToolbarSecurityItemGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartToolbarSecurityItem:Filter:ToolbarSecurityItemGuid:EQ (pToolbarSecurityItemGuid) .

        THIS-OBJECT:SmartToolbarSecurityItem:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
