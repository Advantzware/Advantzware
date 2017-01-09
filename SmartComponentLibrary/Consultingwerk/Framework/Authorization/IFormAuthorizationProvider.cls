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
    File        : IFormAuthorizationProvider
    Purpose     : Provides authorization information for launching Forms
    Syntax      : 
    Description : 2
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 08 22:51:45 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.Authorization.IFormAuthorizationProvider:  
  
	/*------------------------------------------------------------------------------
		Purpose: Provides authorization for launching Forms 																	  
		Notes:  											
		@param poRequestor The reference to the requesting object
		@param pcClassName The Form class name to launch
		@return Logical value indicating if the Form startup is restricted						  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsFormStartupRestricted (poRequestor AS Progress.Lang.Object,
                                                   pcClassName AS CHARACTER).

END INTERFACE.
