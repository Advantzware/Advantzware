/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IContextDatasetStore
    Purpose     : Interface for services that manage the persistent store
                  of the Context Dataset between REST requests 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 25 08:04:24 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Server.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .

INTERFACE Consultingwerk.Framework.Server.IContextDatasetStore:  
  
    /*------------------------------------------------------------------------------
        Purpose: Restores the Session Context 
        Notes:   
        @param phPrincipal The CLIENT-PRINCIPAL valid for the current request
        @return Logical value indicating if the session context was restored 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RestoreSessionContext (phPrincipal AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Stores the Session Context 
        Notes:   
        @param phPrincipal The CLIENT-PRINCIPAL valid for the current request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreSessionContext (phPrincipal AS HANDLE).

END INTERFACE.
