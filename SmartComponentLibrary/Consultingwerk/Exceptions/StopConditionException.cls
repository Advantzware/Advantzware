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
    File        : StopConditionException
    Purpose     : Exception thrown when a STOP condition has been catched
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner, Consultingwerk Ltd.
    Created     : Wed May 01 19:23:58 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.StopConditionException
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StopConditionException class
        Notes:                          
        @param pcErrorString The error message associated with this StopConditionException object
        @param piMessageNumber The error message number associated with this StopConditionException object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC StopConditionException (pcErrorString   AS CHARACTER, 
	                                           piMessageNumber AS INTEGER):
		SUPER (INPUT pcErrorString, INPUT piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StopConditionException class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC StopConditionException ():
		SUPER ("A stop condition has occured."{&TRAN}, 0 ).
		
	END CONSTRUCTOR.

END CLASS.
