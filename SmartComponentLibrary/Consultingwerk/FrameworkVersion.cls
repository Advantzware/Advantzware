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
    File        : FrameworkVersion
    Purpose     : Returns the Framework Version (build number)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 23 12:36:51 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.FrameworkVersion: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Copyright String
        Notes:   
        @return The copyright string 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetCopyRights ():
		
		RETURN "(c)2008-2016 Consultingwerk Ltd. - All rights reserved.":U .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Framework Version String 
        Notes: 
        @return The framework version string  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetVersionString ():

		RETURN TRIM (ENTRY (1, "{Consultingwerk/frameworkversion.i}":U, CHR(10)), CHR(13)) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Framework Version String 
        Notes:   Override to ToString in Progress.Lang.Object 
        @return The framework version string  
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC CHARACTER ToString ():
		
		RETURN THIS-OBJECT:GetVersionString() .

	END METHOD.

END CLASS.
