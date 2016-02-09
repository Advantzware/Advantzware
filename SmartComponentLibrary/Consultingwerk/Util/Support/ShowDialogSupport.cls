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
    File        : ShowDialogSupport
    Purpose     : Helper class that can show GUI for .NET Dialog Forms
    Syntax      : 
    Description : Basically a workaround, when WAIT-FOR oForm:ShowDialog ()
                  is not supported (no -IOEverywhere 1 on 10.2B) or when
                  it may crash
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 18 12:11:59 CEST 2012
    Notes       : Used by the MessageFormHelper and ErrorHelper classes
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.Support.IShowDialogSupport.

USING Consultingwerk.Util.Support.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Util.Support.ShowDialogSupport 
    IMPLEMENTS IShowDialogSupport: 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Shows a .NET Form or ABL GUI for .NET hybrid Form as a Dialog 
        Notes:   
        @param poForm The .NET Form or ABL GUI for .NET hybrid Form to show as a Dialog                                                              
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC VOID ShowDialog (poForm AS System.Windows.Forms.Form):
        
        Consultingwerk.Support.DialogSupport:ShowDialog (poForm) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a .NET Form or ABL GUI for .NET hybrid Form as a Dialog 
        Notes:   
        @param poForm The .NET Common Dialog Form to show as a Dialog
        @return The CHARACTER representation of the DialogResult value (ToString())                                                              
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC CHARACTER ShowDialog (poForm AS System.Windows.Forms.CommonDialog):
        
        RETURN Consultingwerk.Support.DialogSupport:ShowDialog (poForm) .
        
    END METHOD .
&ENDIF
END CLASS.
