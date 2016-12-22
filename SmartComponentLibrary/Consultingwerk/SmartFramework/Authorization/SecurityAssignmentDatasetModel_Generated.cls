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
    File        : SecurityAssignment_Generated
    Purpose     : Auto Generated portion of the SecurityAssignment class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:19,431+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityAssignmentDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsSecurityAssignment.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartSecurityAssignment table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartSecurityAssignment AS SmartSecurityAssignmentTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityAssignmentDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityAssignmentDatasetModel_Generated ():
        SUPER (DATASET dsSecurityAssignment:HANDLE).

        THIS-OBJECT:SmartSecurityAssignment = NEW SmartSecurityAssignmentTableModel (THIS-OBJECT, BUFFER eSmartSecurityAssignment:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartSecurityAssignment) .

    
    END CONSTRUCTOR.

  

END CLASS.
