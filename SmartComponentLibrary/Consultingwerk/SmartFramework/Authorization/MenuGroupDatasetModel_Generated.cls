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
    File        : MenuGroup_Generated
    Purpose     : Auto Generated portion of the MenuGroup class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:19,056+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.MenuGroupDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the PossibleGroup table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PossibleGroup AS PossibleGroupTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartMenuGroup table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartMenuGroup AS SmartMenuGroupTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.MenuGroupBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuGroupDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuGroupDatasetModel_Generated ():
        SUPER (DATASET dsMenuGroup:HANDLE).

        THIS-OBJECT:PossibleGroup = NEW PossibleGroupTableModel (THIS-OBJECT, BUFFER ePossibleGroup:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:PossibleGroup) .

        THIS-OBJECT:SmartMenuGroup = NEW SmartMenuGroupTableModel (THIS-OBJECT, BUFFER eSmartMenuGroup:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartMenuGroup) .

    
    END CONSTRUCTOR.

  

END CLASS.
