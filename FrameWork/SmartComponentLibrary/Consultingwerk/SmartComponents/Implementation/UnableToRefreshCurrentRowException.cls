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
    File        : UnableToRefreshCurrentRowException
    Purpose     : Exception thrown from the SmartBusinessEntityAdapter:RefetchCurrentRow
                  method when the current row is modified
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 26 09:24:45 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.UnableToRefreshCurrentRowException 
    INHERITS UnableToRefreshCurrentRowExcepiton: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowExceptionclass
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowExceptionobject
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException (poInnerException AS Progress.Lang.Error, 
                                                           pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
        @param piMessageNumber The error message number associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException (poInnerException AS Progress.Lang.Error, 
                                                           pcErrorString AS CHARACTER, 
                                                           piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
        @param pcErrorString The error message associated with this UnableToRefreshCurrentRowException object
        @param piMessageNumber The error message number associated with this UnableToRefreshCurrentRowException object
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException (pcErrorString AS CHARACTER, 
                                                           piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnableToRefreshCurrentRowException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToRefreshCurrentRowException ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
