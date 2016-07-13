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
    File        : ComboBoxHelper
    Purpose     : Supports migration from COMBO-BOX Widgets to Infragistics
                  UltraComboEditor or ComboBoxTool (UltraToolbarsManager)
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 13 14:46:07 CET 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Util.ComboBoxHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ComboBoxHelper (  ):
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Converts LIST-ITEMS or LIST-ITEM-PAIRS of an ABL Combo-Box Widget
                 In ValueListItems                                                                        
        Notes:   
        @param phWidget The handle of an ABL Combo-Box widget             
        @param poValueList The reference to an Infragistics ValueList instance                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID FillValueList (phWidget AS HANDLE, 
                                             poValueList AS Infragistics.Win.ValueList):
        
        DEFINE VARIABLE cListItems     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cListItemPairs AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
                
        IF NOT VALID-HANDLE (phWidget) THEN 
            RETURN .         
                    
        poValueList:ValueListItems:Clear() .
        
        ASSIGN cListItems     = phWidget:LIST-ITEMS 
               cListItemPairs = phWidget:LIST-ITEM-PAIRS NO-ERROR . 
               
        IF cListItems > "":U THEN DO:
            DO i = 1 TO NUM-ENTRIES (cListItems, phwidget:DELIMITER):
                poValueList:ValueListItems:Add (ENTRY(i, cListItems, phwidget:DELIMITER), 
                                                ENTRY(i, cListItems, phwidget:DELIMITER)) . 
            END.        
        END.
        ELSE DO:
            DO i = 1 TO NUM-ENTRIES (cListItemPairs, phwidget:DELIMITER) - 1 BY 2 :
                poValueList:ValueListItems:Add (ENTRY(i + 1, cListItemPairs, phwidget:DELIMITER), 
                                                ENTRY(i, cListItemPairs, phwidget:DELIMITER)) . 
            END.        
        END.               
            
        RETURN.

    END METHOD.
&ENDIF
END CLASS.
