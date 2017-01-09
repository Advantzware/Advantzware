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
    File        : ServiceInterfaceException
    Purpose     : Exception thrown by the Service Interface in case a 
                  Business Entity or Business Task cannot be started
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 15 03:45:41 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.ServiceInterfaceException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Business Service associated with the 
                 error 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessService AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param piMessageNumber The error message number associated with this ServiceInterfaceException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException (pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
        @param pcErrorString The error message associated with this ServiceInterfaceException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param piMessageNumber The error message number associated with this ServiceInterfaceException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ServiceInterfaceException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ServiceInterfaceException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceInterfaceException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException,
               pcErrorString).
        
    END CONSTRUCTOR.                                      

END CLASS.
