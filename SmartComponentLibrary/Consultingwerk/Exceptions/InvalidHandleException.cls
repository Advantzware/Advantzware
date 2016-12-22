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
    File        : InvalidHandleException
    Purpose     : An error object thrown to signal an invalid widget handle
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 28 13:09:05 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.InvalidHandleException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the optional description of the invalid handle                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HandleDescription AS CHARACTER NO-UNDO INIT ?
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Default constructor of the InvalidHandleException class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidHandleException ():
        
        SUPER ("Invalid handle."{&TRAN}, 0) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidHandleException class                                                                      
        Notes:                                                                        
        @param pcDescription The Description of the invalid handle to be used in the error message
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidHandleException (pcDescription AS CHARACTER):
        
        SUPER (SUBSTITUTE ("Invalid &1 handle."{&TRAN}, pcDescription), 0) .
        
        THIS-OBJECT:HandleDescription = pcDescription .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Default constructor of the InvalidHandleException class                                                                      
        Notes:                                                                        
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidHandleException (poInnerException AS Error):
        
        SUPER (poInnerException, 
               "Invalid handle."{&TRAN}, 0) .
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidHandleException class                                                                      
        Notes:                                                                        
        @param poInnerException The reference to the original error
        @param pcDescription The Description of the invalid handle to be used in the error message
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidHandleException (poInnerException AS Error,
                                               pcDescription AS CHARACTER):
        
        SUPER (poInnerException, 
               SUBSTITUTE ("Invalid &1 handle."{&TRAN}, pcDescription), 
               0) .
        
        THIS-OBJECT:HandleDescription = pcDescription .
        
    END CONSTRUCTOR.

END CLASS.
