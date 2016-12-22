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
    File        : IFunctionCallCacheService
    Purpose     : Interface for caching classes that keep the information of
                  all IFunctionCallParameter classes
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Jan 14 16:27:08 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

{Consultingwerk/products.i}

INTERFACE Consultingwerk.SmartFramework.IFunctionCallCacheService:  
  
    /*------------------------------------------------------------------------------
        Purpose: ValueList of FunctionCallParameters
        Notes:   
    ------------------------------------------------------------------------------*/
    &IF DEFINED (DotNetAccessible) NE 0 &THEN
	DEFINE PUBLIC PROPERTY FunctionCallTypes AS Infragistics.Win.ValueList NO-UNDO 
	GET.
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Populates the ValueList properties of this class
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeService ().

END INTERFACE.