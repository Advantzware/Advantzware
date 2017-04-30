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
    File        : LoginCompany_Generated
    Purpose     : Auto Generated portion of the LoginCompany class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06/03/2015 12:52:29,063+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity", type="DatasetModelGenerated") .

CLASS Consultingwerk.SmartFramework.System.LoginCompanyDatasetModel_Generated 
    INHERITS DatasetModel ABSTRACT: 

    { Consultingwerk/SmartFramework/System/dsLoginCompany.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartLoginCompany table model
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartLoginCompany AS SmartLoginCompanyTableModel NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity this model belongs to 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE OVERRIDE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoginCompanyDatasetModel_Generated class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LoginCompanyDatasetModel_Generated ():
        SUPER (DATASET dsLoginCompany:HANDLE).

        THIS-OBJECT:SmartLoginCompany = NEW SmartLoginCompanyTableModel (THIS-OBJECT, BUFFER eSmartLoginCompany:HANDLE) .

        THIS-OBJECT:TableModels:Add (THIS-OBJECT:SmartLoginCompany) .

    
    END CONSTRUCTOR.

  

END CLASS.
