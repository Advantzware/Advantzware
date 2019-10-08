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
    File        : InvokeBusinessTaskException
    Purpose     : Exception thrown when a Business Task cannot be loaded
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Mon Jun 15 03:56:29 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.InvokeBusinessTaskException 
    INHERITS ServiceInterfaceException 
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
        @param pcErrorString The error message associated with this InvokeBusinessTaskException object
        @param piMessageNumber The error message number associated with this InvokeBusinessTaskException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException (pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
        @param pcErrorString The error message associated with this InvokeBusinessTaskException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this InvokeBusinessTaskException object
        @param piMessageNumber The error message number associated with this InvokeBusinessTaskException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException (poInnerException AS Error,
                                                    pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvokeBusinessTaskException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this InvokeBusinessTaskException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvokeBusinessTaskException (poInnerException AS Error,
                                                    pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException,
               pcErrorString).
        
    END CONSTRUCTOR.                                      

END CLASS.
