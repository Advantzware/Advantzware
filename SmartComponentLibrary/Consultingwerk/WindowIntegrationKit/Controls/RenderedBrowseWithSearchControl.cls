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
    File        : RenderedBrowseWithSearchControl
    Purpose     : A variant of the RenderedBrowseControl with support for 
                  the UltraGrid FilterUI as a filter row
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 06 11:19:13 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*         FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Infragistics.Win.*                             FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseWithSearchControl 
    INHERITS RenderedBrowseControl: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the user pressed Enter or Return in a Filter Cell
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ApplyFilter DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Returns a dictionary of current filter values 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FilterValues AS CharacterDictionary NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RenderedBrowseWithSearchControl class                                                                      
        Notes:               
        @param poContext An instance of a Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseContext that contains the settings for this RenderedBrowseControl instance
    ------------------------------------------------------------------------------*/    
    CONSTRUCTOR PUBLIC RenderedBrowseWithSearchControl (poContext AS RenderedBrowseContext):
        SUPER (INPUT poContext).
        
        THIS-OBJECT:FilterValues = NEW CharacterDictionary ("":U, ",":U, "":U, CHR(1)) . 
        
        THIS-OBJECT:DisplayLayout:Override:AllowRowFiltering = DefaultableBoolean:True .
        THIS-OBJECT:DisplayLayout:Override:FilterUIType = FilterUIType:FilterRow .    
        THIS-OBJECT:DisplayLayout:Override:FilterEvaluationTrigger = FilterEvaluationTrigger:OnEnterKey . 
        THIS-OBJECT:DisplayLayout:Override:FilterOperandStyle = FilterOperandStyle:UseColumnEditor.
        THIS-OBJECT:DisplayLayout:Override:FilterOperatorLocation = FilterOperatorLocation:Hidden . 
        THIS-OBJECT:DisplayLayout:Override:FilterClearButtonLocation = FilterClearButtonLocation:Row .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Populates the current Filter Dictionary
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FillFilterDictionary ():
        
        DEFINE VARIABLE oFilterRow  AS UltraGridFilterRow  NO-UNDO . 
        DEFINE VARIABLE oFilterCell AS UltraGridFilterCell NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cKey        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER           NO-UNDO .
        
        THIS-OBJECT:FilterValues:Clear () .
        
        oFilterRow = THIS-OBJECT:Rows:FilterRow .

        IF THIS-OBJECT:ActiveCell:IsFilterRowCell AND 
           THIS-OBJECT:ActiveCell:IsInEditMode THEN 
            THIS-OBJECT:PerformAction (UltraGridAction:ExitEditMode) .                    

        {Consultingwerk/foreach.i UltraGridColumn oColumn in this-object:DisplayLayout:Bands[0]:Columns}
    
            ASSIGN cKey        = oColumn:Key 
                   oFilterCell = CAST(oFilterRow:Cells[cKey], UltraGridFilterCell) 
                   cValue      = STRING (UNBOX (oFilterCell:Value)). 
            
            IF cValue > "":U THEN 
                THIS-OBJECT:FilterValues:Add (cKey, cValue) .
         END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ApplyFilter event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnApplyFilter (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .

        THIS-OBJECT:ApplyFilter:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Key Down Event
        Notes:   Handles the Return or Enter key in a Filter cell
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (e AS System.Windows.Forms.KeyEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return) OR
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) THEN DO:

            IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND 
               THIS-OBJECT:ActiveCell:IsFilterRowCell THEN DO:
                
                THIS-OBJECT:FillFilterDictionary () .                
                THIS-OBJECT:OnApplyFilter (System.EventArgs:Empty) .
                   
                e:Handled = TRUE .    
            END.
        END.
    
        SUPER:OnKeyDown (e) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseClick event 
        Notes:   
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseClick (e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oUIElement AS Infragistics.Win.UIElement NO-UNDO .

        oUIElement = THIS-OBJECT:DisplayLayout:UIElement:ElementFromPoint(e:Location) . 
    
        DO WHILE VALID-OBJECT (oUIElement):
            IF TYPE-OF (oUIElement, Infragistics.Win.UltraWinGrid.FilterClearButtonUIElement) THEN DO:

                {Consultingwerk/foreach.i UltraGridCell oCell in this-object:Rows:FilterRow:Cells}
                    oCell:Value = ? /*"":U*/ . 
                END.
                
                THIS-OBJECT:FilterValues:Clear () .
                OnApplyFilter (System.EventArgs:Empty) .
                
                RETURN . 
            END. 
            
            oUIElement = oUIElement:Parent .
        END.

        SUPER:OnMouseClick (e) .
        
        CATCH err AS Progress.Lang.Error:
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
        
    END METHOD .
    
END CLASS.
