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
    File        : ValidationException
    Purpose     : Error raised for validation errors received from the 
                  backend
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 03 15:46:24 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Implementation.ValidationException 
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the RecordIdentifier associated with 
                 this error 
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecordIdentifier AS RecordIdentifier NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ValidationException class
        Notes:  
        @param pcErrorString The error message associated with this ExceptionWithTitle object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValidationException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString, "Validation Error"{&TRAN}).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ValidationException class
        Notes:         
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValidationException (pcErrorString AS CHARACTER, 
                                            piMessageNumber AS INTEGER):
                                                
        SUPER (pcErrorString, 
               piMessageNumber, 
               "Validation Error"{&TRAN}).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ValidationException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValidationException ():
        SUPER ("Validation Error"{&TRAN}).
        
    END CONSTRUCTOR.

END CLASS.
