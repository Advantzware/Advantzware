/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ObjectType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/10/2015 01:55:48,692+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ObjectTypeDatasetModel
    INHERITS ObjectTypeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectTypeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectTypeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectTypeDatasetModel class
        Notes:   
        @param pObjectTypeGuid The filter value for the eSmartObjectType.ObjectTypeGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectTypeDatasetModel (pObjectTypeGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartObjectType:Filter:ObjectTypeGuid:EQ (pObjectTypeGuid) .

        THIS-OBJECT:SmartObjectType:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
