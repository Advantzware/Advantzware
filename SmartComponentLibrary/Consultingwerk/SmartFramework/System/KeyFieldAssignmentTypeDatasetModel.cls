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
    File        : KeyFieldAssignmentType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/10/2015 10:27:28,599+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity", type="DatasetModel") .

CLASS Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeDatasetModel
    INHERITS KeyFieldAssignmentTypeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KeyFieldAssignmentTypeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KeyFieldAssignmentTypeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KeyFieldAssignmentTypeDatasetModel class
        Notes:   
        @param pKeyFieldAssignmentGUID The filter value for the eSmartKeyFieldAssignmentType.KeyFieldAssignmentGUID field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KeyFieldAssignmentTypeDatasetModel (pKeyFieldAssignmentGUID AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartKeyFieldAssignmentType:Filter:KeyFieldAssignmentGUID:EQ (pKeyFieldAssignmentGUID) .

        THIS-OBJECT:SmartKeyFieldAssignmentType:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
