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
    File        : IgnoreAtServiceInterface
    Purpose     : Base class for Exceptions which will be caught and ignored 
                  by the Service Interface and thus not returned to the
                  caller.  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Mar 06 08:42:38 CET 2015
    Notes       : This is helpful for errors that should cancel all processing
                  on the server but not return this as an error to the 
                  caller
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.IgnoreAtServiceInterface 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface (poInnerException AS Progress.Lang.Error, 
	                                             pcErrorString AS CHARACTER):
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface (poInnerException AS Progress.Lang.Error, 
	                                             pcErrorString AS CHARACTER, 
	                                             piMessageNumber AS INTEGER):
	                                                 
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface (poInnerException AS Progress.Lang.Error):
	    
		SUPER (INPUT poInnerException).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface (pcErrorString AS CHARACTER):
	    
		SUPER (INPUT pcErrorString).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface (pcErrorString AS CHARACTER, 
	                                             piMessageNumber AS INTEGER):
	                                                 
		SUPER (INPUT pcErrorString, INPUT piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the IgnoreAtServiceInterface class
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC IgnoreAtServiceInterface ():
		
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
