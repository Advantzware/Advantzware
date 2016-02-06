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
    File        : ICallParameterControlFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Dec 04 19:50:38 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Services.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH.

INTERFACE Consultingwerk.Windows.Framework.Menu.ICallParameterControlFactory:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns an instance of a CallParameterControl based on the type of
                 the CallParameter
        Notes:   
        @param poCallParameterType The class referenceof the CallParameter for which a UserControl should be provided 
        @return The specialized CallParameterControl instance
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC System.Windows.Forms.UserControl GetCallParameterControl (poCallParameterType AS Progress.Lang.Class).

END INTERFACE.