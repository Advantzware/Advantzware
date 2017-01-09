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
    File        : IToolbarAuthorizationProvider
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 08 23:25:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.Authorization.IToolbarAuthorizationProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Provides authorization for Toolbar Items in a Form
        Notes:   For SmartComponent Library Tools, it is optional to provide the at security key for the linked object (e.g. the SmartDataViewer)
        @param poRequestor The reference to the requesting object
        @param pcToolbarActionKey The key of the Toolbar item
        @param pcFormClassName The Form class name to that contains the Toolbar
        @param pcLinkedObjectSecurityKey The security key of the linked object
        @return Logical value indicating if the Form startup is restricted                        
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsToolbarActionRestricted (poRequestor AS Progress.Lang.Object,
	                                                 pcToolbarActionKey AS CHARACTER, 
	                                                 pcFormClassName AS CHARACTER,
	                                                 pcLinkedObjectSecurityKey AS CHARACTER).

END INTERFACE.
