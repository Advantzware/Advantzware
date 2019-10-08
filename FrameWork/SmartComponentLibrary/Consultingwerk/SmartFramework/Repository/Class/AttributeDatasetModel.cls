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
    File        : Attribute
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21/05/2014 11:42:05,054+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeDatasetModel
    INHERITS AttributeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeDatasetModel class
        Notes:   
        @param pAttributeGuid The filter value for the eSmartAttribute.AttributeGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeDatasetModel (pAttributeGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartAttribute:Filter:AttributeGuid:EQ (pAttributeGuid) .

        THIS-OBJECT:SmartAttribute:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
