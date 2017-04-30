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
    File        : InfragisticsWidgetFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:19:16 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*                                            FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .  
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.Misc.*                                          FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .
USING Progress.Util.*                                                  FROM ASSEMBLY .

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.InfragisticsWidgetFactory: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the IWidget reference for a given .NET Control
        Notes:   Returns invalid reference when the widget is not known. The Widget:FromControl
                 method is expected to throw an error in that case when no factory did
                 resolve the type
        @param poControl The .NET Control to return the Widget reference for
        @return The reference to the IWidget representing the .NET Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IWidgetFacade FromControl (poControl AS System.Windows.Forms.Control):
        
        IF TYPE-OF (poControl, UltraTextEditor) THEN DO:
            IF CAST (poControl, UltraTextEditor):MultiLine THEN 
                RETURN NEW UltraTextEditorEditorWidget (CAST (poControl, UltraTextEditor)) .
            ELSE 
                RETURN NEW UltraTextEditorWidget (CAST (poControl, UltraTextEditor)) .
        END.
        
        ELSE IF TYPE-OF (poControl, UltraNumericEditor) THEN DO:
            
            IF EnumHelper:AreEqual (CAST (poControl, UltraNumericEditor):NumericType,
                                    NumericType:Integer) THEN 
                RETURN NEW UltraNumericEditorIntegerWidget (CAST (poControl, UltraNumericEditor)) .
            ELSE                                                                 
                RETURN NEW UltraNumericEditorDecimalWidget (CAST (poControl, UltraNumericEditor)) .
        END.
        
        ELSE IF TYPE-OF (poControl, UltraNumericEditor) THEN DO:
            
            IF BindingSourceHelper:BindingDataType (poControl) = "DATETIME":U THEN 
                RETURN NEW UltraDateTimeEditorWidget (CAST (poControl, UltraDateTimeEditor)) .
            ELSE 
                RETURN NEW UltraDateEditorWidget (CAST (poControl, UltraDateTimeEditor)) .
        END . 
        
        ELSE IF TYPE-OF (poControl, UltraCheckEditor) THEN 
            RETURN NEW UltraCheckEditorWidget (CAST (poControl, UltraCheckEditor)) .

        ELSE IF TYPE-OF (poControl, UltraOptionSet) THEN 
            RETURN NEW UltraOptionSetWidget (CAST (poControl, UltraOptionSet)) .

        ELSE IF TYPE-OF (poControl, UltraComboEditor) THEN 
            RETURN NEW UltraComboEditorWidget (CAST (poControl, UltraComboEditor)) .

        ELSE IF TYPE-OF (poControl, UltraButton) THEN 
            RETURN NEW UltraButtonWidget (CAST (poControl, UltraButton)) .

        RETURN ? . 

    END METHOD.

END CLASS.
