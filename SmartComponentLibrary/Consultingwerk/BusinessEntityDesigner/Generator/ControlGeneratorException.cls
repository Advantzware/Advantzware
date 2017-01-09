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
    File        : ControlGeneratorException
    Purpose     : Exception thrown by the ContainerControlGenerator for 
                  issues during code generation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 24 17:03:31 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.ControlGeneratorException 
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}:  

    &SCOPED-DEFINE Title "Error during code generation"{&TRAN}

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ControlGeneratorException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException (poInnerException AS Progress.Lang.Error, 
                                                  pcErrorString AS CHARACTER):
                                                      
        SUPER (poInnerException, pcErrorString, {&Title}).
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ControlGeneratorException object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException (poInnerException AS Progress.Lang.Error, 
                                                  pcErrorString AS CHARACTER, 
                                                  piMessageNumber AS INTEGER):
                                                      
        SUPER (poInnerException, pcErrorString, piMessageNumber, {&Title}).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException (poInnerException AS Progress.Lang.Error):
                                                      
        SUPER (poInnerException, {&Title}).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
        @param pcErrorString The error message associated with this ControlGeneratorException object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException (pcErrorString AS CHARACTER, 
                                                  piMessageNumber AS INTEGER):
                                                      
        SUPER (pcErrorString, piMessageNumber, {&Title}).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
        @param pcErrorString The error message associated with this ControlGeneratorException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException (pcErrorString AS CHARACTER):
                                                      
        SUPER (pcErrorString, {&Title}).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ControlGeneratorException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorException ():
        
        SUPER ({&Title}).
        
    END CONSTRUCTOR.

END CLASS.
