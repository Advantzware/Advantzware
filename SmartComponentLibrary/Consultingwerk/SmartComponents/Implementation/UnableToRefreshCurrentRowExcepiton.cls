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
    File        : UnableToRefreshCurrentRowExcepiton
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 26 09:24:45 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.UnableToRefreshCurrentRowExcepiton 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowExceptionclass
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowExceptionobject
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton (poInnerException AS Progress.Lang.Error, 
	                                                       pcErrorString AS CHARACTER):
		SUPER (poInnerException, pcErrorString).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
        @param piMessageNumber The error message number associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton (poInnerException AS Progress.Lang.Error, 
	                                                       pcErrorString AS CHARACTER, 
	                                                       piMessageNumber AS INTEGER):
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/		
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton (poInnerException AS Progress.Lang.Error):
		SUPER (poInnerException).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton (pcErrorString AS CHARACTER):
		SUPER (pcErrorString).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
        @param piMessageNumber The error message number associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/		
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton (pcErrorString AS CHARACTER, 
	                                                       piMessageNumber AS INTEGER):
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowExcepiton ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
