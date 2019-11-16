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
    File        : IToolbarItemSecurityService
    Purpose     : Interface for classes providing toolbar item security information 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 18 21:33:01 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.Authorization.IToolbarItemSecurityService:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns if the given toolbar item is restricted
        Notes:   When the toolbar item is unknown on the current system, the security realm default will be returned
        @param pcObjectName The name of the container or SmartTableIOTarget 
        @param pcToolbarItemKey The name of the toolbar item 
        @return Logical value indicating if the given toolbar item is restricted for the current user, TRUE means the user does not have access to the toolbar item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolbarItemRestricted (pcObjectName AS CHARACTER,
                                                   pcToolbarItemKey AS CHARACTER).

END INTERFACE.
