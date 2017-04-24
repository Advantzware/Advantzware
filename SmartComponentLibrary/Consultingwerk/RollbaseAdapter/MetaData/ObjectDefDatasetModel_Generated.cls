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
    File        : ObjectDef_Generated
    Purpose     : Auto Generated portion of the ObjectDef class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/09/2014 11:58:35,271+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.ObjectDefDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDef.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the DataFieldDef table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataFieldDef AS DataFieldDefTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the DataFieldDefs table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataFieldDefs AS DataFieldDefsTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the DataObjectDef table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataObjectDef AS DataObjectDefTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ListItem table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListItem AS ListItemTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ListItems table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListItems AS ListItemsTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the RelationshipDef table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RelationshipDef AS RelationshipDefTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the RelationshipDefs table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RelationshipDefs AS RelationshipDefsTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.RollbaseAdapter.MetaData.ObjectDefBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectDefDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectDefDatasetModel_Generated ():
        SUPER (DATASET dsObjectDef:HANDLE).

        THIS-OBJECT:DataFieldDef = NEW DataFieldDefTableModel (THIS-OBJECT, BUFFER DataFieldDef:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:DataFieldDef) .

        THIS-OBJECT:DataFieldDefs = NEW DataFieldDefsTableModel (THIS-OBJECT, BUFFER DataFieldDefs:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:DataFieldDefs) .

        THIS-OBJECT:DataObjectDef = NEW DataObjectDefTableModel (THIS-OBJECT, BUFFER DataObjectDef:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:DataObjectDef) .

        THIS-OBJECT:ListItem = NEW ListItemTableModel (THIS-OBJECT, BUFFER ListItem:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:ListItem) .

        THIS-OBJECT:ListItems = NEW ListItemsTableModel (THIS-OBJECT, BUFFER ListItems:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:ListItems) .

        THIS-OBJECT:RelationshipDef = NEW RelationshipDefTableModel (THIS-OBJECT, BUFFER RelationshipDef:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:RelationshipDef) .

        THIS-OBJECT:RelationshipDefs = NEW RelationshipDefsTableModel (THIS-OBJECT, BUFFER RelationshipDefs:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:RelationshipDefs) .

    
    END CONSTRUCTOR.

  

END CLASS.
