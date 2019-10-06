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
    File        : UltraGridColumnChooserService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 13 06:30:19 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.*        FROM PROPATH .
USING Consultingwerk.Windows.Forms.*  FROM PROPATH . 
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .
USING System.Windows.Forms.*          FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.UltraGridColumnChooserService 
    IMPLEMENTS IUltraGridColumnChooserService: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ColumnChooser was closed
        Notes: 
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ColumnChooserClosed SIGNATURE VOID 
           (INPUT sender AS Progress.Lang.Object, 
            INPUT e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the current UltraGridColumnChooserForm instance
        Notes:                                      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnChooserForm AS IUltraGridColumnChooserForm NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the current Grid
        Notes:                                      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentGrid AS UltraGridBase NO-UNDO 
    GET():
        IF VALID-OBJECT (THIS-OBJECT:ColumnChooserForm) THEN 
            RETURN THIS-OBJECT:ColumnChooserForm:ColumnChooser:SourceGrid . 
        ELSE 
            RETURN ?  .
     END GET.
    PRIVATE SET (INPUT arg AS UltraGridBase):
        IF VALID-OBJECT (THIS-OBJECT:ColumnChooserForm) THEN 
            THIS-OBJECT:ColumnChooserForm:ColumnChooser:SourceGrid = arg .
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Closes the ColumnChooser 
        Notes:                                      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentGridForm AS Form NO-UNDO 
    GET .
    PRIVATE SET . 
                 
    /*------------------------------------------------------------------------------
        Purpose: Closes the ColumnChooser 
        Notes:                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseColumnChooser ():

        IF VALID-OBJECT (THIS-OBJECT:ColumnChooserForm) THEN 
            CAST(CAST (THIS-OBJECT:ColumnChooserForm, 
                       Progress.Lang.Object),
                 System.Windows.Forms.Form):Close () .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosed event of the ColumnChooserForm                                                                       
        Notes:                                                                  
        @param sender The reference object that raised the event
        @param e The FormClosedEventArgs with the data for this event      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ColumnChooserFormClosedHandler (sender AS System.Object, 
                                                       e AS System.Windows.Forms.FormClosedEventArgs):
        
        THIS-OBJECT:OnColumnChooserClosed (Consultingwerk.EventArgs:Empty) .
        THIS-OBJECT:ColumnChooserForm = ? . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates an instance of the ColumnChooserForm                                                                      
        Notes:   Overriabled method, to allow customization of the actual ColumnChooserForm                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateColumnChooserForm ():
        
        THIS-OBJECT:ColumnChooserForm = NEW UltraGridColumnChooserForm () .

        CAST(CAST (THIS-OBJECT:ColumnChooserForm, 
                   Progress.Lang.Object),
             System.Windows.Forms.Form):FormClosed:Subscribe (ColumnChooserFormClosedHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosing event of the current grid container                                                                        
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CurrentGridFormClosingHandler (sender AS System.Object,
                                                         e AS System.Windows.Forms.FormClosingEventArgs):
        
        IF NOT e:Cancel THEN 
            THIS-OBJECT:CloseColumnChooser() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the ColumnChooser for an UltraGrid instance
        Notes:                                      
        @param poGrid The reference to the UltraGridBase instance to invoke the ColumnChooser for                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeColumnChooser (poGrid AS UltraGridBase):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .
        
        IF NOT VALID-OBJECT (THIS-OBJECT:ColumnChooserForm) THEN 
             THIS-OBJECT:CreateColumnChooserForm () .
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (THIS-OBJECT:ColumnChooserForm, "ColumnChooserForm":U) . 
        
        THIS-OBJECT:CurrentGrid = poGrid . 

        oForm = CAST(CAST (THIS-OBJECT:ColumnChooserForm, 
                           Progress.Lang.Object),
                     System.Windows.Forms.Form) .

        oForm:Show () .
        oForm:BringToFront () . 
        oForm:Focus () . 

        IF VALID-OBJECT (THIS-OBJECT:CurrentGridForm) THEN 
            THIS-OBJECT:CurrentGridForm:FormClosing:UNSUBSCRIBE (CurrentGridFormClosingHandler) .

        THIS-OBJECT:CurrentGridForm = poGrid:FindForm() .
        
        IF VALID-OBJECT (THIS-OBJECT:CurrentGridForm) THEN 
            THIS-OBJECT:CurrentGridForm:FormClosing:SUBSCRIBE (CurrentGridFormClosingHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ColumnChooserClosed event                                                                       
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for this event                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnColumnChooserClosed (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 

        THIS-OBJECT:ColumnChooserClosed:Publish (THIS-OBJECT, e) . 

    END METHOD.

END CLASS.
