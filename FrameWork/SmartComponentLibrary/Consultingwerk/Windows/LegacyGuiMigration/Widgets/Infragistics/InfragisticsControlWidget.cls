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
    File        : InfragisticsControlWidget
    Purpose     : Abstract base class for Infragistics Widget classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 06 16:42:11 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                                       FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.*                                               FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.InfragisticsControlWidget
    INHERITS WidgetFacade 
    ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InfragisticsControlWidget class
        Notes:   
        @param poControl The UltraControlBase reference of the target Control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED InfragisticsControlWidget (poControl AS UltraControlBase):
        SUPER (poControl).
        
        ObjectAssert:IsValid (poControl, "poControl":U) .
        
        IF TYPE-OF (poControl, UltraCheckEditor) THEN 
            CAST (poControl, UltraCheckEditor):CheckedChanged:Subscribe (ValueChangedHandler) .
        ELSE IF TYPE-OF (poControl, TextEditorControlBase) THEN 
            CAST (poControl, TextEditorControlBase):ValueChanged:Subscribe (ValueChangedHandler) . 
        ELSE IF TYPE-OF (poControl, UltraNumericEditorBase) THEN 
            CAST (poControl, UltraNumericEditorBase):ValueChanged:Subscribe (ValueChangedHandler) . 
        ELSE IF TYPE-OF (poControl, UltraOptionSet) THEN 
            CAST (poControl, UltraOptionSet):ValueChanged:Subscribe (ValueChangedHandler) . 

        ELSE 
            MESSAGE "Unable to subscribe to ValueChanged event for: "{&TRAN} + poControl:GetType():FullName 
                VIEW-AS ALERT-BOX WARNING .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the .NET Control 
        Notes:   Sets the MODIFIFED Attribute to TRUE
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ValueChangedHandler (sender AS System.Object,
                                               e AS System.EventArgs):
        
        THIS-OBJECT:MODIFIED = TRUE .

    END METHOD .

END CLASS.
