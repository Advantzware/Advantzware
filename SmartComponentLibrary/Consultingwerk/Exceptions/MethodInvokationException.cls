/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : MethodInvokationException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 16 13:54:47 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.MethodInvokationException 
    INHERITS Exception 
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Method Name associated with this exception 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param pcErrorString The error message associated with this MethodInvokationException object
        @param piMessageNumber The error message number associated with this MethodInvokationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param pcErrorString The error message associated with this MethodInvokationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this MethodInvokationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this MethodInvokationException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param pcErrorString The error message associated with this MethodInvokationException object
        @param piMessageNumber The error message number associated with this MethodInvokationException object
        @param pcMethodName The method name associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER,
                                                  pcMethodName AS CHARACTER):        
    
        SUPER (pcErrorString, piMessageNumber).

        THIS-OBJECT:MethodName = pcMethodName .
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param pcErrorString The error message associated with this MethodInvokationException object
        @param pcMethodName The method name associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (pcErrorString AS CHARACTER,
                                                  pcMethodName AS CHARACTER):        
        
        SUPER (pcErrorString).

        THIS-OBJECT:MethodName = pcMethodName .

    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this MethodInvokationException object
        @param pcMethodName The method name associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER,
                                                  piMessageNumber AS INTEGER,
                                                  pcMethodName AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
        THIS-OBJECT:MethodName = pcMethodName .

    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MethodInvokationException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this MethodInvokationException object
        @param pcMethodName The method name associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodInvokationException (poInnerException AS Error,
                                                  pcErrorString AS CHARACTER,
                                                  pcMethodName AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString).
        
        THIS-OBJECT:MethodName = pcMethodName .
        
    END CONSTRUCTOR.                                      

END CLASS.
