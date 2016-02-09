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
    File        : IUserCredentials
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 07 01:51:33 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.IUserCredentials:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Company Key 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CompanyKey AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Company Name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CompanyName AS CHARACTER NO-UNDO 
    GET.
    SET. 
      
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Password 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Password AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the UserName
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UserName AS CHARACTER NO-UNDO 
	GET.
	SET. 

END INTERFACE.