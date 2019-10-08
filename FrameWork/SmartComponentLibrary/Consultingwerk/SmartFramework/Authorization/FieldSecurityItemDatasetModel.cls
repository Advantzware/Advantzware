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
    File        : FieldSecurityItem
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 18/08/2014 10:56:43,940+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.FieldSecurityItemDatasetModel
    INHERITS FieldSecurityItemDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldSecurityItemDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldSecurityItemDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldSecurityItemDatasetModel class
        Notes:   
        @param pFieldSecurityItemGuid The filter value for the eSmartFieldSecurityItem.FieldSecurityItemGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldSecurityItemDatasetModel (pFieldSecurityItemGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartFieldSecurityItem:Filter:FieldSecurityItemGuid:EQ (pFieldSecurityItemGuid) .

        THIS-OBJECT:SmartFieldSecurityItem:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
