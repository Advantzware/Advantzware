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
    File        : KeyFieldAssignmentType_Generated
    Purpose     : Auto Generated portion of the KeyFieldAssignmentType class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/10/2015 10:32:39,306+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsKeyFieldAssignmentType.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartKeyFieldAssignmentType table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartKeyFieldAssignmentType AS SmartKeyFieldAssignmentTypeTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KeyFieldAssignmentTypeDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KeyFieldAssignmentTypeDatasetModel_Generated ():
        SUPER (DATASET dsKeyFieldAssignmentType:HANDLE).

        THIS-OBJECT:SmartKeyFieldAssignmentType = NEW SmartKeyFieldAssignmentTypeTableModel (THIS-OBJECT, BUFFER eSmartKeyFieldAssignmentType:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartKeyFieldAssignmentType) .

    
    END CONSTRUCTOR.

  

END CLASS.
