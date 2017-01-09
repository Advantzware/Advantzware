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
    File        : UnableToInvokeMethodException
    Purpose     : Exception thrown when an invokable method can not be 
                  invoked
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 23 21:25:58 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*              FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.UnableToInvokeMethodException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException (poInnerException AS Progress.Lang.Error, 
	                                                  pcErrorString AS CHARACTER):
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException (poInnerException AS Progress.Lang.Error, 
	                                                  pcErrorString AS CHARACTER, 
	                                                  piMessageNumber AS INTEGER):
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException (poInnerException AS Progress.Lang.Error):
		SUPER (poInnerException).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException (pcErrorString AS CHARACTER):
		SUPER (pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException (pcErrorString AS CHARACTER, 
	                                                  piMessageNumber AS INTEGER):
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToInvokeMethodException ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
