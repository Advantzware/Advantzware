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
    File        : Menu_Generated
    Purpose     : Auto Generated portion of the Menu class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:24,067+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.MenuBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Menu.MenuDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Menu/dsMenu.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartFunction table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartFunction AS SmartFunctionTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartMenu table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartMenu AS SmartMenuTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuDatasetModel_Generated ():
        SUPER (DATASET dsMenu:HANDLE).

        THIS-OBJECT:SmartFunction = NEW SmartFunctionTableModel (THIS-OBJECT, BUFFER eSmartFunction:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartFunction) .

        THIS-OBJECT:SmartMenu = NEW SmartMenuTableModel (THIS-OBJECT, BUFFER eSmartMenu:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartMenu) .

    
    END CONSTRUCTOR.

  

END CLASS.
