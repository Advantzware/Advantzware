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
    File        : SecurityToken_Generated
    Purpose     : Auto Generated portion of the SecurityToken class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:19,885+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.SecurityTokenBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityTokenDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsSecurityToken.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartSecurityToken table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartSecurityToken AS SmartSecurityTokenTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.SecurityTokenBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityTokenDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityTokenDatasetModel_Generated ():
        SUPER (DATASET dsSecurityToken:HANDLE).

        THIS-OBJECT:SmartSecurityToken = NEW SmartSecurityTokenTableModel (THIS-OBJECT, BUFFER eSmartSecurityToken:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartSecurityToken) .

    
    END CONSTRUCTOR.

  

END CLASS.
