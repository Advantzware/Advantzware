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
    File        : InvalidParameterValueException
    Purpose     : An error object thrown to signal invalid parameter values 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwer Ltd.
    Created     : Sun Feb 17 09:48:56 CET 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.InvalidParameterValueException 
    INHERITS Exception 
    {&SERIALIZABLE}: 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the invalid parameter
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParameterName AS CHARACTER 
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the value of the invalid parameter
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParameterValue AS CHARACTER 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the class name that caused the InvalidParameterException
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER 
    GET.
    PRIVATE SET.    
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidParameterException class
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidParameterValueException ():

        SUPER ("The parameter value is invalid."{&TRAN}, 0).
    
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidParameterException class
        Notes:                                                                        
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidParameterValueException (poInnerException AS Error):

        SUPER (poInnerException,
               "The parameter value is invalid."{&TRAN}, 
               0).
    
    END CONSTRUCTOR.

    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidParameterException class
        Notes:                                                                        
        @param pcParameterName The name of the invalid parameter
        @param pcValue The invalid parameter name
        @param pcClassName The name of the class that caused the InvalidParameterException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidParameterValueException (pcParameterName AS CHARACTER,
                                                       pcValue         AS CHARACTER,
                                                       pcClassName     AS CHARACTER ):

        SUPER (SUBSTITUTE("The parameter value '&2' for &1 in &3 is invalid."{&TRAN}, 
                          pcParameterName, pcValue, pcClassName), 0).
           
        ASSIGN ParameterName  = pcParameterName
               ParameterValue = pcValue 
               ClassName      = pcClassName .           
           
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidParameterException class
        Notes:                                                                        
        @param poInnerException The reference to the original error
        @param pcParameterName The name of the invalid parameter
        @param pcValue The invalid parameter name
        @param pcClassName The name of the class that caused the InvalidParameterException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidParameterValueException (poInnerException AS Error,
                                                       pcParameterName  AS CHARACTER,
                                                       pcValue          AS CHARACTER,
                                                       pcClassName      AS CHARACTER ):

        SUPER (poInnerException,
               SUBSTITUTE("The parameter value '&2' for &1 in &3 is invalid."{&TRAN}, 
                          pcParameterName, pcValue, pcClassName), 0).
           
        ASSIGN ParameterName  = pcParameterName
               ParameterValue = pcValue 
               ClassName      = pcClassName .           
           
    END CONSTRUCTOR.

END CLASS.
