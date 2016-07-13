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
    File        : CancelException
    Purpose     : An Error object signalling that the called operation
                  has been cancelled (exit or returned).
    Syntax      : 
    Description : Useful for routines that otherwise would require a 
                  logical return value or output parameter.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 19 23:21:28 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.Exception FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Exceptions.CancelException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException (poInnerException AS Progress.Lang.ProError, 
                                        pcErrorString AS CHARACTER):
                                            
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException (poInnerException AS Progress.Lang.ProError, 
                                        pcErrorString AS CHARACTER, 
                                        piMessageNumber AS INTEGER):
                                            
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException (poInnerException AS Progress.Lang.ProError):
        
        SUPER (INPUT poInnerException).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException (pcErrorString AS CHARACTER):
        
        SUPER (pcErrorString).
        
    END CONSTRUCTOR.

        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException (pcErrorString AS CHARACTER, 
                                        piMessageNumber AS INTEGER):
        
        SUPER (INPUT pcErrorString, INPUT piMessageNumber).
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CancelException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelException ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
