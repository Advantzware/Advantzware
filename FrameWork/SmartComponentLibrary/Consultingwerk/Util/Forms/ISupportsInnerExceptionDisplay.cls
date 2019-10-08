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
    File        : ISupportsInnerErrorDisplay
    Purpose     : Optional interface to be implemented by Error Message Form
                  objects. Supports displaying of the inner error (the 
                  original error)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 11 00:04:44 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Util.Forms.ISupportsInnerExceptionDisplay:  
  
    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the reference to the inner error object                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY InnerException AS Progress.Lang.Error NO-UNDO 
    GET.
    SET. 

END INTERFACE.