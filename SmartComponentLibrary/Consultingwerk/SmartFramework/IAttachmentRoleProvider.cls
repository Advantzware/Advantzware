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
    File        : IAttachmentRoleProvider
    Purpose     : Interface for services that provide the list of available attachment roles
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 22:59:51 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.IAttachmentRoleProvider:  
  
    {Consultingwerk/SmartFramework/ttAttachmentRole.i &NO-BEFORE}  
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttAttachmentRole Temp-Table with the list of available 
                 attachment roles
        Notes:   
        @param ttAttachmentRole OUTPUT Temp-Table with the list of available attachment roles
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetAttachmentRoles (OUTPUT TABLE ttAttachmentRole).

    /*------------------------------------------------------------------------------
        Purpose: Returns the Count of Attachment Roles
        Notes:   
        @return The count of attachment roles
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC INTEGER GetCount ().

END INTERFACE.