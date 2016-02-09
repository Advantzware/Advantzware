/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : InvalidTableNameException
    Purpose     : Exception thrown by DatabaseHelper:UnabbreviatedTableName
                  when an invalid table name has been passed
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 08 10:09:12 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.Exceptions.InvalidTableNameException 
    INHERITS Exception {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the TableName that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the InvalidTableNameException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcTableName The name of the table that caused the exception
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC InvalidTableNameException (pcErrorMessage AS CHARACTER,
	                                              pcTableName AS CHARACTER):

		SUPER (SUBSTITUTE (pcErrorMessage, pcTableName), 0).
	
	   ASSIGN THIS-OBJECT:TableName = pcTableName . 
		
	END CONSTRUCTOR.
		
END CLASS.
