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
    File        : IFrameworkBootstrap
    Purpose     : Provides interarction with a framework bootstrap
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 04 23:14:45 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IFrameworkBootstrap:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the framework bootstrap has
                 already been executed                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IsInitialized AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Executes the framework bootstrap                                                                        
        Notes:   The IsInitialized method is supposed to return True after the 
                 bootstrap has been executed                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Initialize ().     

END INTERFACE.
