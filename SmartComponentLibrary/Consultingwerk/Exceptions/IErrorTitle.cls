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
    File        : IErrorTitle
    Purpose     : Interface for Errors that support a Title property to 
                  be used for error dialogs
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 20 00:15:09 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH.

INTERFACE Consultingwerk.Exceptions.IErrorTitle:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Title to be used in the Error Dialog or Message Box 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO 
	GET.

END INTERFACE.