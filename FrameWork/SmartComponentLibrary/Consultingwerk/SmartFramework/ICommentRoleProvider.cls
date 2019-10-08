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
    File        : ICommentRoleProvider
    Purpose     : Interface for services that provide the list of available comment roles
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 22:59:51 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.ICommentRoleProvider:  
  
    {Consultingwerk/SmartFramework/ttCommentRole.i &NO-BEFORE}  
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttCommentRole Temp-Table with the list of available 
                 comment roles
        Notes:   
        @param ttCommentRole OUTPUT Temp-Table with the list of available comment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetCommentRoles (OUTPUT TABLE ttCommentRole).

    /*------------------------------------------------------------------------------
        Purpose: Returns the Count of Comment Roles
        Notes:   
        @return The count of comment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER GetCount ().

END INTERFACE.