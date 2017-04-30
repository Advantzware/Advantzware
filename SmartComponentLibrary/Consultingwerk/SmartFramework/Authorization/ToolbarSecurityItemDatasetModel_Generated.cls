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
    File        : ToolbarSecurityItem_Generated
    Purpose     : Auto Generated portion of the ToolbarSecurityItem class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:20,173+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsToolbarSecurityItem.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartToolbarSecurityItem table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartToolbarSecurityItem AS SmartToolbarSecurityItemTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.ToolbarSecurityItemBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolbarSecurityItemDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolbarSecurityItemDatasetModel_Generated ():
        SUPER (DATASET dsToolbarSecurityItem:HANDLE).

        THIS-OBJECT:SmartToolbarSecurityItem = NEW SmartToolbarSecurityItemTableModel (THIS-OBJECT, BUFFER eSmartToolbarSecurityItem:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartToolbarSecurityItem) .

    
    END CONSTRUCTOR.

  

END CLASS.
