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
    File        : IErrorHandler
    Purpose     : Defines a methods required for handling messages, 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 31 20:13:43 CEST 2011
    Notes       : See Consultingwerk.Util.ErrorHelper, 
                  Consultingwerk.FrameworkSettings.ListIErrorHandler
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.IErrorHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Handles an Error object                                                                        
        Notes:   Logical return value indicates if the error has been completely
                 Handled, that is when this method returns TRUE, the ErrorHelper
                 will not display the error anymore, retrun false when the 
                 ErrorHelper should still display the error message dialog. 
        @param poError The reference to the Error to handle
        @return Logical value indicating if the error has been completely handled                                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleError (poError AS Progress.Lang.Error).

END INTERFACE.