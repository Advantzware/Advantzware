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
    File        : AttributeGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 20/05/2014 17:41:24,644+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeGroupDatasetModel
    INHERITS AttributeGroupDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeGroupDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeGroupDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeGroupDatasetModel class
        Notes:   
        @param pAttributeGroupGuid The filter value for the eSmartAttributeGroup.AttributeGroupGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeGroupDatasetModel (pAttributeGroupGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartAttributeGroup:Filter:AttributeGroupGuid:EQ (pAttributeGroupGuid) .

        THIS-OBJECT:SmartAttributeGroup:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
