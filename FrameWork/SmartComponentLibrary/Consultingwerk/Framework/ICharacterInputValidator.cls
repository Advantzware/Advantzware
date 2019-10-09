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
    File        : ICharacterInputValidator
    Purpose     : Validates a character value (used by the InputPromptHelper
                  routines)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 18 00:00:17 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.ICharacterInputValidator:  
  
    /*------------------------------------------------------------------------------
        Purpose: Validates a CHARACTER value                                                                       
        Notes:                                                          
        @param pcValue The CHARACTER value to validate
        @return TRUE when the validation is passed, otherwise FALSE              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateCharacterValue (pcValue AS CHARACTER).

END INTERFACE.