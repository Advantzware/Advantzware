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
    File        : UserGroup_Generated
    Purpose     : Auto Generated portion of the UserGroup class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:20,417+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authorization.UserGroupDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authorization/dsUserGroup.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UserGroupPossibleAssignments table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserGroupPossibleAssignments AS UserGroupPossibleAssignmentsTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartUserGroup table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartUserGroup AS SmartUserGroupTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserGroupDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserGroupDatasetModel_Generated ():
        SUPER (DATASET dsUserGroup:HANDLE).

        THIS-OBJECT:UserGroupPossibleAssignments = NEW UserGroupPossibleAssignmentsTableModel (THIS-OBJECT, BUFFER ePossibleAssignments:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:UserGroupPossibleAssignments) .

        THIS-OBJECT:SmartUserGroup = NEW SmartUserGroupTableModel (THIS-OBJECT, BUFFER eSmartUserGroup:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartUserGroup) .

    
    END CONSTRUCTOR.

  

END CLASS.
