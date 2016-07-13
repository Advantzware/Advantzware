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
    File        : FieldSecurityItem_Generated
    Purpose     : Auto Generated portion of the FieldSecurityItem class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:18,583+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.FieldSecurityItemDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsFieldSecurityItem.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartFieldSecurityItem table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartFieldSecurityItem AS SmartFieldSecurityItemTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldSecurityItemDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldSecurityItemDatasetModel_Generated ():
        SUPER (DATASET dsFieldSecurityItem:HANDLE).

        THIS-OBJECT:SmartFieldSecurityItem = NEW SmartFieldSecurityItemTableModel (THIS-OBJECT, BUFFER eSmartFieldSecurityItem:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartFieldSecurityItem) .

    
    END CONSTRUCTOR.

  

END CLASS.
