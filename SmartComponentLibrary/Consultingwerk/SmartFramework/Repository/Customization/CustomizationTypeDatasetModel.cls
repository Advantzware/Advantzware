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
    File        : CustomizationType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21/05/2014 11:03:29,916+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Customization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Customization.CustomizationTypeDatasetModel
    INHERITS CustomizationTypeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomizationTypeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomizationTypeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomizationTypeDatasetModel class
        Notes:   
        @param pCustomizationTypeGuid The filter value for the eSmartCustomizationType.CustomizationTypeGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomizationTypeDatasetModel (pCustomizationTypeGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartCustomizationType:Filter:CustomizationTypeGuid:EQ (pCustomizationTypeGuid) .

        THIS-OBJECT:SmartCustomizationType:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
