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
    File        : ILoginForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 15 23:36:15 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

{Consultingwerk/products.i}

INTERFACE Consultingwerk.Framework.Session.ILoginForm:  

    &IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Shows the form as a modal dialog box.
        @return One of the System.Windows.Forms.DialogResult values.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.DialogResult ShowDialog () .   
  
    /*------------------------------------------------------------------------------
        Purpose: Shows the form as a modal dialog box with the specified owner.                                                                      
        Notes:   
        @param poOwner Any object that implements IWin32Window that represents the top-level window that will own the modal dialog box.
        @return One of the System.Windows.Forms.DialogResult values.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.DialogResult ShowDialog (poOwner AS System.Windows.Forms.IWin32Window) .   
  
    &ENDIF
END INTERFACE.