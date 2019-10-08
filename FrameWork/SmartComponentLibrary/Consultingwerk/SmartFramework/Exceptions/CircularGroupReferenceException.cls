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
    File        : CircularGroupReferenceException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 25 23:52:38 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Exceptions.CircularGroupReferenceException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException (poInnerException AS Progress.Lang.Error, 
                                                        pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException (poInnerException AS Progress.Lang.Error, 
                                                        pcErrorMessage AS CHARACTER, 
                                                        piMessageNumber AS INTEGER):
                                                     
        SUPER (poInnerException, pcErrorMessage, piMessageNumber).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).
        
    END CONSTRUCTOR.
    
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException (pcErrorMessage AS CHARACTER, 
                                                        piMessageNumber AS INTEGER):
                                                     
        SUPER (pcErrorMessage, piMessageNumber).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CircularGroupReferenceException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CircularGroupReferenceException ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
