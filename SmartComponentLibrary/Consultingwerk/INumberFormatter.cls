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
    File        : INumberFormatter
    Purpose     : Interface for classes that support formatting numbers
                  as human readable strings  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 21:15:03 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.INumberFormatter:  
  
    /*------------------------------------------------------------------------------
        Purpose: Formats a number as a string
        Notes:   
        @param piNumber The number to format
        @return The string representing the number in a human readable form
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER FormatNumber (piNumber AS INTEGER).

END INTERFACE.