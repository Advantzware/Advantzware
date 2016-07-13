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
    File        : ILoginCompanyProvider
    Purpose     : Interface for services that provide the list of available login companies
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 22:59:51 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.ILoginCompanyProvider:  
  
    {Consultingwerk/SmartFramework/ttLoginCompany.i &NO-BEFORE}  
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttLoginCompany Temp-Table with the list of available 
                 login companies
        Notes:   
        @param ttLoginCompany OUTPUT Temp-Table with the list of available login companies
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetAvailableLoginCompanies (OUTPUT TABLE ttLoginCompany).

END INTERFACE.