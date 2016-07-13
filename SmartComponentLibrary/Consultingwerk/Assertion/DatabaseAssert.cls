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
    File        : DatabaseAssert
    Purpose     : Provides database related validation routines
    Syntax      : 
    Description : Routines in this class typically raise an error condition
                  when the condition is not met.
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Nov 30 20:05:43 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*  FROM PROPATH .  
USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Assertion.DatabaseAssert: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation.                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE DatabaseAssert ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies that the given database is connected
        Notes:   
        Throws:  Consultingwerk.Exceptions.DatabaseNotConnectedException
        @param pcDatabaseName The name of the database to verify
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID IsConnected (pcDatabaseName AS CHARACTER):
		
		IF NOT CONNECTED (pcDatabaseName) THEN 
		    UNDO, THROW NEW DatabaseNotConnectedException (pcDatabaseName) .

	END METHOD .

END CLASS.
