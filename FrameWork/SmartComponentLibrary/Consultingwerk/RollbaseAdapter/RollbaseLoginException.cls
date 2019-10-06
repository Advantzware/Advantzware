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
    File        : RollbaseLoginException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 11:55:18 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.RollbaseAdapter.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.RollbaseAdapter.RollbaseLoginException 
    INHERITS RollbaseException
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException (pcErrorString AS CHARACTER,
                                               piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
                                              
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException (pcErrorString AS CHARACTER):        
        
        SUPER (pcErrorString).
    
    END CONSTRUCTOR.                                      

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException (poInnerException AS Error):
    
        SUPER (poInnerException).        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException (poInnerException AS Error,
                                               pcErrorString AS CHARACTER,
                                               piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.
                                       
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RollbaseLoginException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseLoginException (poInnerException AS Error,
                                              pcErrorString AS CHARACTER):        
        
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.                                      

END CLASS.
