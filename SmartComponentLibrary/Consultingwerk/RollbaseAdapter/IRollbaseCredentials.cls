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
    File        : IRollbaseCredentials
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 08:53:15 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.RollbaseAdapter.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH.

INTERFACE Consultingwerk.RollbaseAdapter.IRollbaseCredentials:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Password for the Rollbase integration
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Password AS CHARACTER NO-UNDO 
    GET.
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the UserName for the Rollbase integration 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UserName AS CHARACTER NO-UNDO 
	GET.

END INTERFACE.