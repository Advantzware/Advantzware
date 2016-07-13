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
    File        : FormLauncher
    Purpose     : Dynamically starts a GUI for .NET form from the list of
                  available Forms
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 08 16:13:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*        FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .  

CLASS Consultingwerk.Windows.FormLauncher: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FormLauncher ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog listing all available Form's
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LaunchForm ():
        
        DEFINE VARIABLE oForm         AS Consultingwerk.Windows.Forms.DynamicFormLauncherForm NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                    NO-UNDO .
        DEFINE VARIABLE cFormType     AS CHARACTER                                            NO-UNDO .
        
        oForm = NEW Consultingwerk.Windows.Forms.DynamicFormLauncherForm () .
        
        IF VALID-OBJECT (FrameworkSettings:MdiContainer) THEN 
            WAIT-FOR oForm:ShowDialog (FrameworkSettings:MdiContainer) SET oDialogResult . 
        ELSE 
            WAIT-FOR oForm:ShowDialog () SET oDialogResult .     
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult}
        
        ASSIGN cFormType = oForm:ClassName .
        
        IF cFormType > "":U THEN 
             LaunchForm (cFormType, 
                         oForm:MdiChild) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Launches the given Form
        Notes:
        @param pcFormType The name of the Form type to launch (class name)
        @param plMdiChild Start as MdiChild (if an MdiParent is present)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LaunchForm (pcFormType AS CHARACTER,
                                          plMdiChild AS LOGICAL):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO . 

        oForm = DYNAMIC-NEW (pcFormType) () .
        
        IF plMdiChild AND VALID-OBJECT (FrameworkSettings:MdiContainer) THEN 
            oForm:MdiParent = FrameworkSettings:MdiContainer .

        oForm:Show () .

    END METHOD.

END CLASS.
