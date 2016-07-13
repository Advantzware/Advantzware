/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IOutputStream
    Purpose     : Interface for classes that can handle stream output 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 07 22:08:32 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.IOutputStream:  
  
	/*------------------------------------------------------------------------------
		Purpose: Writes text to the output destination  																	  
		Notes:  										
		@param pcText The text for the output							  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Write (pcText AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination and terminates it with a 
                 line break                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteLine (pcText AS CHARACTER).

END INTERFACE.