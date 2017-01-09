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
    File        : InvalidSourceMappingException
    Purpose     : 
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

CLASS Consultingwerk.OERA.Query.InvalidSourceMappingException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the Column Name that is responsible for raising this error
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidSourceMappingException class
        Notes:                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidSourceMappingException ():
    
        SUPER ().        
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidSourceMappingException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidSourceMappingException (pcErrorString AS CHARACTER,
                                                      piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidSourceMappingException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcColumnName The Column Name for which this Exception is raised
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidSourceMappingException (pcErrorString AS CHARACTER,
                                                      piMessageNumber AS INTEGER,
                                                      pcColumnName AS CHARACTER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
        THIS-OBJECT:ColumnName = pcColumnName . 
    
    END CONSTRUCTOR.
                                              
END CLASS.
