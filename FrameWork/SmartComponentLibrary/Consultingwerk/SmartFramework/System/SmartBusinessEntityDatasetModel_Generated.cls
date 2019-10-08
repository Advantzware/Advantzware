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
    File        : SmartBusinessEntity_Generated
    Purpose     : Auto Generated portion of the SmartBusinessEntity class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/09/2015 14:11:07,382+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsSmartBusinessEntity.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartBusinessEntity table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartBusinessEntity AS SmartBusinessEntityTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartBusinessEntityTable table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartBusinessEntityTable AS SmartBusinessEntityTableTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityDatasetModel_Generated ():
        SUPER (DATASET dsSmartBusinessEntity:HANDLE).

        THIS-OBJECT:SmartBusinessEntity = NEW SmartBusinessEntityTableModel (THIS-OBJECT, BUFFER eSmartBusinessEntity:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartBusinessEntity) .

        THIS-OBJECT:SmartBusinessEntityTable = NEW SmartBusinessEntityTableTableModel (THIS-OBJECT, BUFFER eSmartBusinessEntityTable:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartBusinessEntityTable) .

    
    END CONSTRUCTOR.

  

END CLASS.
