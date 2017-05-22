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
    File        : AuthenticationException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 29 22:52:11 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.Exceptions.AuthenticationException 
    INHERITS Exception
    IMPLEMENTS IErrorTitle
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Error message title
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO INITIAL "Authentication Error"{&TRAN}
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
        @param pcErrorString The error message associated with this AuthenticationException object
        @param piMessageNumber The error message number associated with this AuthenticationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException (pcErrorString AS CHARACTER,
                                                piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
        @param pcErrorString The error message associated with this AuthenticationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).
    
    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this AuthenticationException object
        @param piMessageNumber The error message number associated with this AuthenticationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException (poInnerException AS Error,
                                                pcErrorString AS CHARACTER,
                                                piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
                
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AuthenticationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this AuthenticationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthenticationException (poInnerException AS Error,
                                                pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.    

END CLASS.
