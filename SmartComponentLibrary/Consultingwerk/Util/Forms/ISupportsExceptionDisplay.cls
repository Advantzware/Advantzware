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
    File        : ISupportsErrorDisplay
    Purpose     : Optional interface to be implemented by Error Message Form
                  objects. Supports receiving the reference to the error object
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 10 00:04:44 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Util.Forms.ISupportsExceptionDisplay:  
  
    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the reference to the error object                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Exception AS Progress.Lang.Error NO-UNDO 
    GET.
    SET. 

END INTERFACE.