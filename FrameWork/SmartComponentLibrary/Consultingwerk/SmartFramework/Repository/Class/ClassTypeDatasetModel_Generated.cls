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
    File        : ClassType_Generated
    Purpose     : Auto Generated portion of the ClassType class
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

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Class.ClassTypeDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Repository/Class/dsClassType.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartClassType table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartClassType AS SmartClassTypeTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassTypeDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassTypeDatasetModel_Generated ():
        SUPER (DATASET dsClassType:HANDLE).

        THIS-OBJECT:SmartClassType = NEW SmartClassTypeTableModel (THIS-OBJECT, BUFFER eSmartClassType:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartClassType) .

    
    END CONSTRUCTOR.

  

END CLASS.
