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
    File        : InsertToQueryException
    Purpose     : Exception thrown from the QueryString:InsertToQuery method
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

CLASS Consultingwerk.OERA.Query.InsertToQueryException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the PrepareString that was used when the error was caused
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrepareString AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidSourceMappingException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcPrepareString The PrepareString that was used when the error was caused
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InsertToQueryException (poInnerException AS Error,
                                               pcErrorString AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcPrepareString AS CHARACTER):        
    
        SUPER (poInnerException, pcErrorString, piMessageNumber).
    
        ASSIGN THIS-OBJECT:PrepareString = pcPrepareString . 
    
    END CONSTRUCTOR.
                                              
END CLASS.
