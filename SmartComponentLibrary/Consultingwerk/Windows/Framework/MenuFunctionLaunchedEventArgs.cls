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
    File        : MenuFunctionLaunchedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 08 23:05:41 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                   FROM PROPATH .
USING Consultingwerk.Assertion.*         FROM PROPATH .
USING Consultingwerk.Windows.Framework.* FROM PROPATH . 
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.MenuFunctionLaunchedEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the context of the launched menu function   
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Context AS Progress.Lang.Object NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the launched object instance   
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LaunchedInstance AS Progress.Lang.Object NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the launched menu function   
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MenuFunctionKey AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the title of the launched menu function   
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuFunctionTitle AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuFunctionLaunchedEventArgs class   
        Notes:   
        @param pcMenuFunctionKey The reference to the key of the launched menu function associated with this event
        @param pcMenuFunctionTitle The reference to the title of the launched menu function associated with this event
        @param poContext The reference to the context provided to launch the function associated with this event
        @param poLaunchedInstance The reference to the launched instance associated with the event
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MenuFunctionLaunchedEventArgs (pcMenuFunctionKey AS CHARACTER,
	                                                  pcMenuFunctionTitle AS CHARACTER, 
	                                                  poContext AS Progress.Lang.Object,
	                                                  poLaunchedInstance AS Progress.Lang.Object):
		SUPER ().
		
		Assert:NotNullOrEmpty (pcMenuFunctionKey) .
		
		ASSIGN THIS-OBJECT:MenuFunctionKey   = pcMenuFunctionKey
               THIS-OBJECT:MenuFunctionTitle = pcMenuFunctionTitle
		       THIS-OBJECT:Context           = poContext 
		       THIS-OBJECT:LaunchedInstance  = poLaunchedInstance 
		       .
		
	END CONSTRUCTOR.

END CLASS.
