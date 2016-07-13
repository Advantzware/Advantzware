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
    File        : Codepages
    Purpose     : Code page names constants
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 13 05:53:58 CET 2015
    Notes       : This list may not be complete. Customers should report
                  missing code page names so that we can add them to the 
                  constants 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

CLASS Consultingwerk.Codepages:
    
    DEFINE PUBLIC STATIC PROPERTY ISO8859-1  AS CHARACTER INIT "ISO8859-1":U  GET .   
    DEFINE PUBLIC STATIC PROPERTY ISO8859-2  AS CHARACTER INIT "ISO8859-2":U  GET .   
    DEFINE PUBLIC STATIC PROPERTY ISO8859-15 AS CHARACTER INIT "ISO8859-15":U GET .   

    DEFINE PUBLIC STATIC PROPERTY IBM850     AS CHARACTER INIT "IBM850":U     GET .   
    DEFINE PUBLIC STATIC PROPERTY IBM851     AS CHARACTER INIT "IBM851":U     GET .   
    DEFINE PUBLIC STATIC PROPERTY IBM852     AS CHARACTER INIT "IBM852":U     GET .   

    DEFINE PUBLIC STATIC PROPERTY UTF-8      AS CHARACTER INIT "UTF-8":U      GET .   

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the Codepages class
	    Notes:   Disallow instance creation
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE Codepages ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
