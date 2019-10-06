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
    File        : ClassType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 04/09/2015 23:45:08,457+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ClassTypeDatasetModel
    INHERITS ClassTypeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassTypeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassTypeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassTypeDatasetModel class
        Notes:   
        @param pClassTypeGuid The filter value for the eSmartClassType.ClassTypeGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassTypeDatasetModel (pClassTypeGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartClassType:Filter:ClassTypeGuid:EQ (pClassTypeGuid) .

        THIS-OBJECT:SmartClassType:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
