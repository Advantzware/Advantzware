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
    File        : AttributeGroup_Generated
    Purpose     : Auto Generated portion of the AttributeGroup class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:25,555+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeGroupBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Repository.Class.AttributeGroupDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Repository/Class/dsAttributeGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartAttributeGroup table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartAttributeGroup AS SmartAttributeGroupTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Repository.Class.AttributeGroupBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttributeGroupDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttributeGroupDatasetModel_Generated ():
        SUPER (DATASET dsAttributeGroup:HANDLE).

        THIS-OBJECT:SmartAttributeGroup = NEW SmartAttributeGroupTableModel (THIS-OBJECT, BUFFER eSmartAttributeGroup:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartAttributeGroup) .

    
    END CONSTRUCTOR.

  

END CLASS.
