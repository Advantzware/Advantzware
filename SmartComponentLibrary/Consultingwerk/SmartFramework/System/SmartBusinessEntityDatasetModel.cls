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
    File        : SmartBusinessEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17/10/2013 19:01:56,380+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityDatasetModel
    INHERITS SmartBusinessEntityDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityDatasetModel class
        Notes:   
        @param pBusinessEntityGuid The filter value for the eSmartBusinessEntity.BusinessEntityGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityDatasetModel (pBusinessEntityGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartBusinessEntity:Filter:BusinessEntityGuid:EQ (pBusinessEntityGuid) .

        THIS-OBJECT:SmartBusinessEntity:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
