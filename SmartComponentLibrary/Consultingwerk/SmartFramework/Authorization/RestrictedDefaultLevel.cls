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
    File        : RestrictedDefaultLevel
    Purpose     : Constants with default level of security restriction
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 18 18:24:44 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH . 
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.RestrictedDefaultLevel: 

    /*------------------------------------------------------------------------------
        Purpose: The default is not restricted, defined by the realm
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NOTRESTRICTED-BY-REALM AS CHARACTER INITIAL 'NOTRESTRICTED-BY-REALM':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The default is restricted, defined by the realm
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RESTRICTED-BY-REALM AS CHARACTER INITIAL 'RESTRICTED-BY-REALM':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The default is not restricted, defined for all users and all groups
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NOTRESTRICTED-FOR-ALL AS CHARACTER INITIAL 'NOTRESTRICTED-FOR-ALL':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The default is restricted, defined for all users and all groups
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RESTRICTED-FOR-ALL AS CHARACTER INITIAL 'RESTRICTED-FOR-ALL':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The default is not restricted, defined by a group the user/group is member of
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NOTRESTRICTED-BY-GROUP AS CHARACTER INITIAL 'NOTRESTRICTED-BY-GROUP':U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The default is restricted, defined by a group the user/group is member of
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY RESTRICTED-BY-GROUP AS CHARACTER INITIAL 'RESTRICTED-BY-GROUP':U NO-UNDO 
    GET.

	/*------------------------------------------------------------------------------
	    Purpose: Private constructor for the RestrictedDefaultLevel class
	    Notes:   Disallow instance creation
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE RestrictedDefaultLevel ():
		SUPER ().
		
	END CONSTRUCTOR.

END CLASS.
