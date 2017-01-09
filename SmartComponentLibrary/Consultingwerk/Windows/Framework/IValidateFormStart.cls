/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IValidateFormStart
    Purpose     : Interface to be implemented by Form's launched from the 
                  Consultingwerk.Windows.Framework.LaunchFormCallParameter
                  class in case the Form should be queried if it's o.k. to 
                  start the Form right now
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 13 14:58:14 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH.

INTERFACE Consultingwerk.Windows.Framework.IValidateFormStart:  
  
    /*------------------------------------------------------------------------------
        Purpose: Validates if the Form is o.k. to be started (Shown)
        Notes:   The implementing method may return FALSE or THROW an error to prevent
                 the Form instance from being shown. When an error is showen, the
                 ErrorHelper is used to show the error to the user 
        @param poContext The context of the menu function that launched the current object
        @return Logical value indicating if the Form may be started 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ValidateFormStart (poContext AS Progress.Lang.Object).

END INTERFACE.