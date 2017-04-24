/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ModifiedState_Generated
    Purpose     : Auto Generated portion of the ModifiedState class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31/12/2016 00:59:18,675+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.ModifiedStateDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsModifiedState.i &ACCESS=PROTECTED }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartModifiedState table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartModifiedState AS SmartModifiedStateTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModifiedStateDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModifiedStateDatasetModel_Generated ():
        SUPER (DATASET dsModifiedState:HANDLE).

        THIS-OBJECT:SmartModifiedState = NEW SmartModifiedStateTableModel (THIS-OBJECT, BUFFER eSmartModifiedState:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartModifiedState) .

    
    END CONSTRUCTOR.

  

END CLASS.
