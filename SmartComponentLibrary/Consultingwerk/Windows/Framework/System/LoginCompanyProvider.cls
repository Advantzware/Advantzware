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
    File        : LoginCompanyProvider
    Purpose     : SmartFramework dependent ILoginCompanyProvider implementation
                  Retrieves login companies from the SmartDB
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 23:07:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH . 
USING Consultingwerk.OERA.*                           FROM PROPATH . 
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*       FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Threading.*                              FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.System.LoginCompanyProvider 
    IMPLEMENTS ILoginCompanyProvider: 

    {Consultingwerk/SmartFramework/ttLoginCompany.i &NO-BEFORE}  
    {Consultingwerk/SmartFramework/System/dsLoginCompany.i}
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttLoginCompany Temp-Table with the list of available 
                 login companies
        Notes:   
        @param ttLoginCompany OUTPUT Temp-Table with the list of available login companies
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetAvailableLoginCompanies (OUTPUT TABLE ttLoginCompany):
       
        EMPTY TEMP-TABLE ttLoginCompany . 
       
        FrameworkSettings:ServiceAdapter:RetrieveData ("":U, 
                                                       "Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U,
                                                       NEW FetchDataRequest ("eSmartLoginCompany":U),
                                                       OUTPUT DATASET dsLoginCompany) .
        
        FOR EACH eSmartLoginCompany:
        
            CREATE ttLoginCompany .
            BUFFER-COPY eSmartLoginCompany TO ttLoginCompany
                ASSIGN ttLoginCompany.LoginCompanyKey = eSmartLoginCompany.LoginCompanyGuid .
        END.

    END METHOD .

END CLASS.
