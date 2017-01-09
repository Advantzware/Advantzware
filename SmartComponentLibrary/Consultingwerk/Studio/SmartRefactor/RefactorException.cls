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
    File        : RefactorException
    Purpose     : Except
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 13 04:48:32 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.Studio.SmartRefactor.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Studio.SmartRefactor.RefactorException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException (poInnerException AS Progress.Lang.Error, 
	                                      pcErrorString AS CHARACTER):
	                                          
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException (poInnerException AS Progress.Lang.Error, 
	                                      pcErrorString AS CHARACTER, 
	                                      piMessageNumber AS INTEGER):
	                                          
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException (poInnerException AS Progress.Lang.Error):
		
		SUPER (poInnerException).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException (pcErrorString AS CHARACTER):
	    
		SUPER (pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException (pcErrorString AS CHARACTER, 
	                                      piMessageNumber AS INTEGER):
	    
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RefactorException
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RefactorException ():
		
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
