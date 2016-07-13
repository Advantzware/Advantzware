/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : InvalidApplicationNameException
    Purpose     : Exception thrown by the Win32:GetApplicationCommandLine
                  method when the given application name could not be 
                  found in the Windows registry
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 23 22:54:40 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Windows.API.Exceptions.InvalidApplicationNameException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the application that caused this exception
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ApplicationName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvalidApplicationNameException class
        Notes:   
        @param pcErrorString The error message
        @param piMessageNumber The error message number
        @param pcApplicationName The name of the application that caused this issue
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC InvalidApplicationNameException (pcErrorString AS CHARACTER, 
	                                                    piMessageNumber AS INTEGER,
	                                                    pcApplicationName AS CHARACTER):
		
		SUPER (INPUT pcErrorString, INPUT piMessageNumber).
		
		ASSIGN THIS-OBJECT:ApplicationName = pcApplicationName .
		
	END CONSTRUCTOR.

END CLASS.
