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
    File        : DatabaseNotConnectedException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 30 20:08:07 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.DatabaseNotConnectedException 
    INHERITS Exception
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatabaseNotConnectedException class
        Notes:                                                                   
        @param pcDatabaseName The database name to use in the error message     
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DatabaseNotConnectedException (pcDatabaseName AS CHARACTER):
	    
		SUPER (SUBSTITUTE ("Database ~"&1~" is not connected.", pcDatabaseName), 0).
		
	END CONSTRUCTOR.

END CLASS.
