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
    File        : NamedQueryParameterMissingException
    Purpose     : Exception thrown from business entities when parameters to
                  a named query are missing 
    Syntax      : 
    Description : Exception to be thrown by actual business logic
    Author(s)   : Mike Fechner 
    Created     : Sun Sep 13 18:06:08 CEST 2015
    Notes       : SCL-1018
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.NamedQueryParameterMissingException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the parameter that raised this exception 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParameterName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the query that raised this exception 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NamedQueryParameterMissingException class
        Notes:   
        @param pcQueryName The named query name associated with this exception
        @param pcParameterName The named query parameter associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NamedQueryParameterMissingException (pcQueryName AS CHARACTER,
                                                            pcParameterName AS CHARACTER):
                                                       
        SUPER (SUBSTITUTE ("Parameter ~"&2~" missing for named query ~"&1~".":U, pcQueryName, pcParameterName), 0).
        
        THIS-OBJECT:QueryName = pcQueryName . 
        
    END CONSTRUCTOR.

END CLASS.
