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
    File        : InvalidNamedQueryException
    Purpose     : Exception thrown from business entities when an invalid named
                  query name was passed in 
    Syntax      : 
    Description : Exception to be thrown by actual business logic
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 13 18:02:01 CEST 2015
    Notes       : SCL-1018
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH  .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.InvalidNamedQueryException 
    INHERITS Exception
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the query that raised this exception 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InvalidNamedQueryException class
        Notes:   
        @param pcQueryName The named query name associated with this exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidNamedQueryException (pcQueryName AS CHARACTER):

        SUPER (SUBSTITUTE ("Invalid named query: ~"&1~".":U, pcQueryName), 0).
        
        THIS-OBJECT:QueryName = pcQueryName . 
        
    END CONSTRUCTOR.

END CLASS.
