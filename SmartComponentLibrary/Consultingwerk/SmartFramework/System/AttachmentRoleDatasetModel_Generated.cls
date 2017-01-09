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
    File        : AttachmentRole_Generated
    Purpose     : Auto Generated portion of the AttachmentRole class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:28,237+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.AttachmentRoleBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.AttachmentRoleDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsAttachmentRole.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartAttachmentRole table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartAttachmentRole AS SmartAttachmentRoleTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.AttachmentRoleBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttachmentRoleDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentRoleDatasetModel_Generated ():
        SUPER (DATASET dsAttachmentRole:HANDLE).

        THIS-OBJECT:SmartAttachmentRole = NEW SmartAttachmentRoleTableModel (THIS-OBJECT, BUFFER eSmartAttachmentRole:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartAttachmentRole) .

    
    END CONSTRUCTOR.

  

END CLASS.
