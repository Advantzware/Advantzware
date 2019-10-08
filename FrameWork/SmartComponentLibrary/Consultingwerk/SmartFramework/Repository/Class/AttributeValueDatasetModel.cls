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
    File        : AttributeValue
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20/10/2015 20:22:15,540+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeValueDatasetModel
    INHERITS AttributeValueDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeValueDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeValueDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeValueDatasetModel class
        Notes:   
        @param pAttributeValueGuid The filter value for the eSmartAttributeValue.AttributeValueGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeValueDatasetModel (pAttributeValueGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartAttributeValue:Filter:AttributeValueGuid:EQ (pAttributeValueGuid) .

        THIS-OBJECT:SmartAttributeValue:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
