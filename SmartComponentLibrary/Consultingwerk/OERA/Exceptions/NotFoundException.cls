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
    File        : NotFoundException
    Purpose     : Exception thrown by the DataAccess:FindRecord methods
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 20 22:15:57 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.NotFoundException 
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotFoundException class
        Notes:                          
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
        @param pcTitle The title for the Exception
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NotFoundException (pcErrorString AS CHARACTER, 
	                                      piMessageNumber AS INTEGER, 
	                                      pcTitle AS CHARACTER):
	                                          
		SUPER (pcErrorString, piMessageNumber, pcTitle).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotFoundException class
        Notes:                          
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotFoundException (pcErrorString AS CHARACTER, 
                                          piMessageNumber AS INTEGER):
        
        SUPER (pcErrorString, piMessageNumber, "No record is available."{&TRAN}).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NotFoundException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotFoundException ():
        
        SUPER ("No record is available."{&TRAN}, 
               0, 
               "No record is available."{&TRAN}).
        
    END CONSTRUCTOR.

END CLASS.
