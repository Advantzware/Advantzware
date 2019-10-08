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
    File        : IDateInputValidator
    Purpose     : Validates a Date value (used by the InputPromptHelper
                  routines)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 18 00:00:17 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.IDateInputValidator:  
  
    /*------------------------------------------------------------------------------
        Purpose: Validates a DATE value                                                                       
        Notes:                                                          
        @param pdaValue The DATE value to validate
        @return TRUE when the validation is passed, otherwise FALSE              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateDateValue (pdaValue AS DATE).

END INTERFACE.