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
    File        : QueryOffEndException
    Purpose     : Exception thrown when attempting to navigate a query 
                  further than it's end 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 25 19:05:26 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.QueryOffEndException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query String associated with the error 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of records that was attempted to navigate to 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SkipRecords AS INTEGER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the QueryOffEndException class
        Notes:   Thrown from the DataAccess:SkipRecords method when navigating a Query 
                 using the Skip option
        @param pcErrorString The error message string
        @param piErrorNumber The error message number
        @param pcQueryString the query string associated with the error
        @param piSkipRecords The record number that was attempted to navigate to 
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC QueryOffEndException (pcErrorString AS CHARACTER,
	                                         piErrorNumber AS INTEGER,
	                                         pcQueryString AS CHARACTER,
	                                         piSkipRecords AS INTEGER):
	                                         
		SUPER (pcErrorString,
		       piErrorNumber) .
		       
        ASSIGN THIS-OBJECT:QueryString = pcQueryString 
               THIS-OBJECT:SkipRecords = piSkipRecords . 		       
		       
	END CONSTRUCTOR.

END CLASS.
