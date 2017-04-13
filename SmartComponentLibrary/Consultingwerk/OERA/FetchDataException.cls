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
    File        : FetchDataException
    Purpose     : Exception thrown from the DataAccess:FetchData method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 11:55:18 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.Exception.


USING Consultingwerk.OERA.Query.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.FetchDataException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the QueryString send from the client when the error was caused
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClientQueryString AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the SourceDefaultQuery used when the error was caused
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SourceDefaultQueryString AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidSourceMappingException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcClientQueryString The QueryString send from the client when the error was caused
        @param pcSourceDefaultQueryString The SourceDefaultQuery used when the error was caused
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FetchDataException (poInnerException AS Error,
                                           pcErrorString AS CHARACTER,
                                           piMessageNumber AS INTEGER,
                                           pcClientQueryString AS CHARACTER,
                                           pcSourceDefaultQueryString AS CHARACTER):        
    
        SUPER (poInnerException, pcErrorString, piMessageNumber).
    
        ASSIGN THIS-OBJECT:ClientQueryString        = pcClientQueryString 
               THIS-OBJECT:SourceDefaultQueryString = pcSourceDefaultQueryString. 
    
    END CONSTRUCTOR.
                                              
END CLASS.
