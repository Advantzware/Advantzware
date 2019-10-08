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
    File        : MicrosoftWidgetFactory
    Purpose     : Widget factory for core .NET WinForm Controls
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:19:16 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .  
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Microsoft.MicrosoftWidgetFactory: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the IWidget reference for a given .NET Control
        Notes:   Returns invalid reference when the widget is not known. The Widget:FromControl
                 method is expected to throw an error in that case when no factory did
                 resolve the type
        @param poControl The .NET Control to return the Widget reference for
        @return The reference to the IWidget representing the .NET Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IWidgetFacade FromControl (poControl AS System.Windows.Forms.Control):
        
/*        IF TYPE-OF (poControl, UltraTextEditor) THEN                              */
/*            RETURN NEW UltraTextEditorWidget (CAST (poControl, UltraTextEditor)) .*/

        RETURN ? . 

    END METHOD.

END CLASS.
