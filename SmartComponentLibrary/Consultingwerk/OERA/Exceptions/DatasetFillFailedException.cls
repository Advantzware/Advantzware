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
    File        : DatasetFillFailedException
    Purpose     : Error raised by the DataAccess class when the Dataset
                  FILL() method returned FALSE and the ThrowErrorsOnFillFailed
                  property is set to TRUE
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 26 12:23:39 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.DatasetFillFailedException    
    INHERITS FillFailedException
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException (poInnerException AS Progress.Lang.Error, 
	                                               pcErrorString AS CHARACTER):
	                                            
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException (poInnerException AS Progress.Lang.Error, 
        	                                       pcErrorString AS CHARACTER, 
        	                                       piMessageNumber AS INTEGER):
	                                            
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException (poInnerException AS Progress.Lang.Error):
	    
		SUPER (poInnerException).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException (pcErrorString AS CHARACTER):
	    
		SUPER (pcErrorString).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException (pcErrorString AS CHARACTER, 
	                                               piMessageNumber AS INTEGER):
	    
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatasetFillFailedException class
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatasetFillFailedException ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
