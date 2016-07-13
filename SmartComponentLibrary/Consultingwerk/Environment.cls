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
    File        : Environment
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 23:02:13 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Environment: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the NewLine character on the current platform
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY NewLine AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Environment class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC Environment ():
		
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        /* Mike Fechner, Consultingwerk Ltd. 21.05.2014
           10.2B Windows compiled code running on the AppServer will raise this
           error here when accessing System.Environmet:NewLine
           Error attempting to push run time parameters onto the stack. (984) */
        IF PROVERSION = "10.2B":U AND SESSION:REMOTE OR 
           PROVERSION BEGINS "11.5":U AND SESSION:CLIENT-TYPE = ClientType:MULTI-SESSION-AGENT THEN 
            ASSIGN Consultingwerk.Environment:NewLine = "~n":U .
        ELSE 
            ASSIGN Consultingwerk.Environment:NewLine = System.Environment:NewLine .
&ELSE
        ASSIGN Consultingwerk.Environment:NewLine = "~n":U .  
&ENDIF		
		
	END CONSTRUCTOR.

END CLASS.
