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
    File        : DebugRunner
    Purpose     : Debug version of the SmartUnit Test runner. Allows 
                  execution of Unit Tests from within PDSOE - allowing 
                  to debug unit tests
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 21:29:57 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartUnit.TestRunner.* FROM PROPATH.
USING Progress.Lang.*                       FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartUnit.TestRunner.DebugRunner
    INHERITS TestRunner: 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the DebugRunner class
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DebugRunner ():
		SUPER ().
		
		ASSIGN 
		  THIS-OBJECT:LogMessages  = FALSE 
		  THIS-OBJECT:DialogOutput = TRUE .
		
	END CONSTRUCTOR.

END CLASS.
