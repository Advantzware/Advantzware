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
    File        : IFormWithTabbedMdiManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 18 23:57:00 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

{ Consultingwerk/products.i }

INTERFACE Consultingwerk.Framework.IFormWithUltraTabbedMdiManager:  
&IF DEFINED (DotNetAccessible) NE 0 &THEN     

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraTabbedMdiManager                            
        Notes:                                                                        
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY TabbedMdiManager AS Infragistics.Win.UltraWinTabbedMdi.UltraTabbedMdiManager NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Assigns the Text to an MdiChildForm                                                                    
        Notes:                                       
        @param poForm The reference to the MdiChild Form
        @param pcText The Text to assign to the MdiChild Form                                 
    ------------------------------------------------------------------------------*/      
    METHOD PUBLIC VOID SetMdiTabTextForForm (poForm AS System.Windows.Forms.Form, 
                                             pcText AS CHARACTER) .
    
    /*------------------------------------------------------------------------------
        Purpose: Assigns the Tooltip Text to an MdiChildForm                                                                    
        Notes:                                       
        @param poForm The reference to the MdiChild Form
        @param pcToolTip The Tooltip Text to assign to the MdiChild Form                                 
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC VOID SetMdiTabToolTipForForm (poForm AS System.Windows.Forms.Form, 
                                                pcToolTip AS CHARACTER) .
&ENDIF
 END INTERFACE.
 