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
    File        : create-user-comp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Feb 07 18:14:58 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.SmartFramework.System.* FROM PROPATH.

DEFINE VARIABLE oLoginCompany AS LoginCompanyDatasetModel NO-UNDO .

/* ***************************  Main Block  *************************** */

oLoginCompany = NEW LoginCompanyDatasetModel () .

FOR EACH company:

    oLoginCompany:SmartLoginCompany:Filter:LoginCompanyName:EQ (company.company):RUN () .

    IF NOT oLoginCompany:SmartLoginCompany:Available THEN DO:

        oLoginCompany:TrackingChanges = TRUE . 

        oLoginCompany:SmartLoginCompany:Create() .
    
        ASSIGN oLoginCompany:SmartLoginCompany:LoginCompanyName          = company.name 
               oLoginCompany:SmartLoginCompany:LoginCompanyShort         = SUBSTITUTE ("&1 - &2", company.name, company.company)
               oLoginCompany:SmartLoginCompany:LoginCompanyReferenceChar = company.company
            .        
    
        oLoginCompany:SaveChanges() .
    END.    
END.
