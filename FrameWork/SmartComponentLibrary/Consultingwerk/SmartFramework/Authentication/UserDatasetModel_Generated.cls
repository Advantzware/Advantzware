/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : User_Generated
    Purpose     : Auto Generated portion of the User class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 12/06/2015 01:49:00,511+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authentication.UserBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.Authentication.UserDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/Authentication/dsUser.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartUser table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartUser AS SmartUserTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDatasetModel_Generated ():
        SUPER (DATASET dsUser:HANDLE).

        THIS-OBJECT:SmartUser = NEW SmartUserTableModel (THIS-OBJECT, BUFFER eSmartUser:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartUser) .

    
    END CONSTRUCTOR.

  

END CLASS.
