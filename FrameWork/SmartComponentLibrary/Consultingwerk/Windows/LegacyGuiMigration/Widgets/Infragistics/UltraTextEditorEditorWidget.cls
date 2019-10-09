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
    File        : UltraTextEditorEditorWidget
    Purpose     : Represents a multi-line text-editor
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:22:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraTextEditorEditorWidget 
    INHERITS UltraTextEditorWidget: 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the UltraTextEditorEditorWidget class
       Notes:
       @param poEditor The reference to the UltraTextEditor this instance should represent
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraTextEditorEditorWidget (poEditor AS UltraTextEditor):
        SUPER (poEditor).
        
        THIS-OBJECT:TYPE = "EDITOR":U . 
        
    END CONSTRUCTOR.
    
END CLASS.
