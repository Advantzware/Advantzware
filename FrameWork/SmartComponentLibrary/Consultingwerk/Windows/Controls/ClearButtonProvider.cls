/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ClearButtonProvider
    Purpose     : Provides a Clear Button to UltraTextEditor and TextBox
                  Controls
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 06 21:59:52 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*         FROM PROPATH . 
USING Consultingwerk.Util.*              FROM PROPATH . 
USING Consultingwerk.Windows.Controls.*  FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.* FROM ASSEMBLY . 
USING Progress.Lang.*                    FROM PROPATH .
USING System.Drawing.*                   FROM ASSEMBLY .
USING System.Windows.Forms.*             FROM ASSEMBLY .   

CLASS Consultingwerk.Windows.Controls.ClearButtonProvider: 

    DEFINE VARIABLE oUltraTextEditor AS UltraTextEditor NO-UNDO .
    DEFINE VARIABLE oTextBox         AS TextBox         NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClearButtonProvider class
        Notes:   
        @param poUltraTextEditor The reference to the UltraTextEditor Control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClearButtonProvider (poUltraTextEditor AS UltraTextEditor):
        SUPER ().
    
        DEFINE VARIABLE oEditorButton AS EditorButton NO-UNDO . 
        DEFINE VARIABLE oForm         AS Form         NO-UNDO .
    
        ObjectAssert:IsValid (poUltraTextEditor, "poUltraTextEditor":U) .

        oEditorButton = NEW EditorButton ("clear":U) .
        oEditorButton:Text = "":U . 
        oEditorButton:Appearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/delete_bw_16.png":U) .
    
        poUltraTextEditor:ButtonsRight:Add (oEditorButton) .

        ASSIGN oUltraTextEditor = poUltraTextEditor 
               oForm            = poUltraTextEditor:FindForm () .

        poUltraTextEditor:EditorButtonClick:Subscribe (EditorButtonClickHandler) .
        oForm:FormClosed:Subscribe (FormClosedHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClearButtonProvider class
        Notes:   
        @param poTextBox The reference to the TextBox Control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClearButtonProvider (poTextBox AS TextBox):
        SUPER ().
    
        DEFINE VARIABLE oButton AS Button NO-UNDO . 
        DEFINE VARIABLE oForm   AS Form   NO-UNDO .
    
        ObjectAssert:IsValid (poTextBox, "poUltraTextEditor":U) .

        oButton          = NEW Button () .
        oButton:Name     = SUBSTITUTE ("clear_&1":U, poTextBox:Name) . 
        oButton:Text     = "":U . 
        oButton:TabStop  = FALSE . 
        oButton:Image    = ImageHelper:Load ("Consultingwerk/Windows/Images/delete_bw_16.png":U) .
        oButton:Location = NEW System.Drawing.Point (poTextBox:Left + poTextBox:Width - poTextBox:Height,
                                                     poTextBox:Top) .
        oButton:Size     = NEW System.Drawing.Size (poTextBox:Height, poTextBox:Height) .
        poTextBox:Width  = poTextBox:Width - poTextBox:Height .  
    
        poTextBox:Parent:Controls:Add (oButton) .

        ASSIGN oTextBox = poTextBox 
               oForm    = poTextBox:FindForm () .

        oButton:Click:Subscribe (ButtonClickHandler) .
        oForm:FormClosed:Subscribe (FormClosedHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the Button (TextBox)
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ButtonClickHandler (sender AS System.Object,
                                            e AS System.EventArgs):
        
        oTextBox:Text = "":U .
        oTextBox:Focus () . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the UltraTextEditor
                 Control 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID EditorButtonClickHandler (sender AS System.Object,
                                                  e AS EditorButtonEventArgs):
        
        IF e:Button:Key = "clear":U THEN DO: 
            oUltraTextEditor:Text = "":U .
            oUltraTextEditor:Focus () . 
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the FormClosed event of the managed Form                                                                        
        Notes:   Deletes this instance
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormClosedHandler (sender AS System.Object,
                                             e AS System.Windows.Forms.FormClosedEventArgs):
        
        DELETE OBJECT THIS-OBJECT .

    END METHOD.

END CLASS.
