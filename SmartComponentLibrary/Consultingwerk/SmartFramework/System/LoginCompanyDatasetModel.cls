/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LoginCompany
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:48:03,333+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.LoginCompanyDatasetModel
    INHERITS LoginCompanyDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoginCompanyDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LoginCompanyDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LoginCompanyDatasetModel class
        Notes:   
        @param pLoginCompanyGuid The filter value for the eSmartLoginCompany.LoginCompanyGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LoginCompanyDatasetModel (pLoginCompanyGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartLoginCompany:Filter:LoginCompanyGuid:EQ (pLoginCompanyGuid) .

        THIS-OBJECT:SmartLoginCompany:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
