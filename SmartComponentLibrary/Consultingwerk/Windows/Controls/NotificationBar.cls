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
    File        : NotificationBar
    Purpose     : Custom Control that visualizes a notification that is 
                  shown without modality at the top of a Form / Container
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 06 16:46:31 CEST 2013
    Notes       : Supports MouseClick event and flashing
                  Inspired by http://www.codeproject.com/Articles/26627/Notification-Bar
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*             FROM PROPATH . 
USING Consultingwerk.Windows.Controls.* FROM PROPATH .  
USING Progress.Lang.*                   FROM PROPATH .
USING System.*                          FROM ASSEMBLY .
USING System.Collections.Generic.*      FROM ASSEMBLY .
USING System.Text.*                     FROM ASSEMBLY .
USING System.Drawing.*                  FROM ASSEMBLY .
USING System.Windows.Forms.*            FROM ASSEMBLY .
USING System.Media.*                    FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Controls.NotificationBar 
    INHERITS Control: 

    DEFINE VARIABLE oFlashTimer         AS Timer   NO-UNDO .  
    DEFINE VARIABLE oCloseButtonSize    AS Size    NO-UNDO . 
    DEFINE VARIABLE iCloseButtonPadding AS INTEGER NO-UNDO INIT 6 .
    DEFINE VARIABLE lMouseInBounds      AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lControlHighlighted AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE iTickCount          AS INTEGER NO-UNDO INIT 0 .
    DEFINE VARIABLE iFlashCount         AS INTEGER NO-UNDO INIT 0 .
    DEFINE VARIABLE iFlashTo            AS INTEGER NO-UNDO INIT 0 .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the Image shown on the Left of the 
                 NotificationBar 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Image AS System.Drawing.Image NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if a sound should be played when the Control is shown 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PlaySoundWhenShown AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotificationBar class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NotificationBar ():
		SUPER ().
		
		ASSIGN oFlashTimer           = NEW Timer () 
		       oFlashTimer:Interval  = 750 
		       oCloseButtonSize      = NEW Size (20, 20)  
               THIS-OBJECT:BackColor = SystemColors:Info 
               THIS-OBJECT:Dock      = DockStyle:Top .
               .

        oFlashTimer:Tick:Subscribe (FlashTimerTickHandler) .

        THIS-OBJECT:SetStyle (ControlStyles:AllPaintingInWmPaint, TRUE) .
        THIS-OBJECT:SetStyle (ControlStyles:ResizeRedraw, TRUE) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Draws the Close Button 
        Notes:   
        @param e The PaintEventArgs of the Current Paint interaction
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DrawCloseButton (e AS PaintEventArgs):

        DEFINE VARIABLE oCloseButtonColor AS Color NO-UNDO . 
        DEFINE VARIABLE oLinePen          AS Pen   NO-UNDO . 
        DEFINE VARIABLE oLine1Start       AS Point NO-UNDO .
        DEFINE VARIABLE oLine1End         AS Point NO-UNDO .
        DEFINE VARIABLE oLine2Start       AS Point NO-UNDO .
        DEFINE VARIABLE oLine2End         AS Point NO-UNDO .
        
        oCloseButtonColor = System.Drawing.Color:Black.
    
        IF lMouseInBounds THEN 
            oCloseButtonColor = System.Drawing.Color:White .

        oLinePen = NEW Pen (oCloseButtonColor, 2) .
        
        ASSIGN oLine1Start = NEW Point((THIS-OBJECT:Width - (oCloseButtonSize:Width - iCloseButtonPadding)), iCloseButtonPadding) 
               oLine1End   = NEW Point((THIS-OBJECT:Width - iCloseButtonPadding), (oCloseButtonSize:Height - iCloseButtonPadding)) 
               oLine2Start = NEW Point((THIS-OBJECT:Width - iCloseButtonPadding), iCloseButtonPadding)
               oLine2End   = NEW Point((THIS-OBJECT:Width - (oCloseButtonSize:Width - iCloseButtonPadding)), (oCloseButtonSize:Height - iCloseButtonPadding)) .

        e:Graphics:DrawLine (oLinePen, oLine1Start, oLine1End) .
        e:Graphics:DrawLine (oLinePen, oLine2Start, oLine2End) .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Draws the Controls text 
        Notes:   
        @param e The PaintEventArgs of the Current Paint interaction
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DrawText (e AS PaintEventArgs):
		
        DEFINE VARIABLE iLeftPadding  AS INTEGER   NO-UNDO INIT 1 .
        DEFINE VARIABLE oTextSize     AS Size      NO-UNDO .
        DEFINE VARIABLE iMaxTextWidth AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iLineHeight   AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iNumLines     AS INTEGER   NO-UNDO INIT 1 .
        DEFINE VARIABLE oTextRect     AS Rectangle NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:Image) THEN DO: 
            
            iLeftPadding = THIS-OBJECT:Image:Width + 4 .
            
            e:Graphics:DrawImage (THIS-OBJECT:Image, NEW Point (2, 5)) .
        END.

        ASSIGN oTextSize = TextRenderer:MeasureText (e:Graphics, THIS-OBJECT:Text, THIS-OBJECT:Font) 
                
               iMaxTextWidth = (THIS-OBJECT:Width - (oCloseButtonSize:Width + (iCloseButtonPadding * 2)))
               
               iLineHeight   = oTextSize:Height + 2 .
    
        IF oTextSize:Width > iMaxTextWidth THEN 
            iNumLines = oTextSize:Width / iMaxTextWidth + 1 .

        ASSIGN oTextRect = NEW Rectangle(0, 0, 0, 0) 
               oTextRect:Width  = THIS-OBJECT:Width - (oCloseButtonSize:Width + iCloseButtonPadding) - iLeftPadding
               oTextRect:Height = (iNumLines * iLineHeight)
               oTextRect:X = iLeftPadding
               oTextRect:Y = 5 .

        THIS-OBJECT:Height = (iNumLines * iLineHeight) + 10 .

        TextRenderer:DrawText (e:Graphics, 
                               THIS-OBJECT:Text, 
                               THIS-OBJECT:Font, 
                               oTextRect, 
                               THIS-OBJECT:ForeColor, 
                               CAST (EnumHelperExt:Or (TextFormatFlags:WordBreak, TextFormatFlags:Left, TextFormatFlags:Top),
                                     TextFormatFlags)) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Flashes the Control for the given number of times and the given interval
                 for each flashing
        Notes:   
        @param piInterval The interval in milliseconds used for flashing
        @param piNumberOfTimes The number of times the Control is supposed to flash
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Flash (piInterval AS INTEGER,
	                          piNumberOfTimes AS INTEGER):
		
		ASSIGN iFlashTo   = piNumberOfTimes
		       iTickCount = 0
		       
		       oFlashTimer:Interval = piInterval . 
		       
		oFlashTimer:Start () .          

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Flashes the Control for the given number of times and default interval
                 for each flashing (750 milliseconds)
        Notes:   
        @param piNumberOfTimes The number of times the Control is supposed to flash
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Flash (piNumberOfTimes AS INTEGER):

        THIS-OBJECT:Flash (750, piNumberOfTimes) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Flashes the Control onces with for the given number of milliseconds
        Notes:   
        @param piInterval The interval in milliseconds used for flashing
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FlashOnce (piInterval AS INTEGER):

        THIS-OBJECT:Flash (piInterval, 1) .

    END METHOD . 


    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the FlashTimer 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event  
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FlashTimerTickHandler (sender AS System.Object,
	                                             e AS System.EventArgs):

        IF lControlHighlighted THEN DO:

            ASSIGN THIS-OBJECT:BackColor = SystemColors:Info 
                   lControlHighlighted   = FALSE 
                   iFlashCount           = iFlashCount + 1 .

            IF iFlashCount = iFlashTo THEN DO:
                oFlashTimer:Stop () .
                iFlashCount = 0 . 
            END.
        END.
        ELSE DO:
            THIS-OBJECT:BackColor = SystemColors:Highlight .
            lControlHighlighted = TRUE .
        END.
        
        ASSIGN iTickCount = iTickCount + 1 . 
        
        THIS-OBJECT:Invalidate () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseClick event
        Notes:   
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseClick (e AS MouseEventArgs):

        IF e:X >= (THIS-OBJECT:Width - oCloseButtonSize:Width + iCloseButtonPadding) AND e:Y <= 12 THEN 
            THIS-OBJECT:Hide () .
        ELSE 
            SUPER:OnMouseClick (e) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseEnter event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseEnter (e AS System.EventArgs):

        THIS-OBJECT:BackColor = SystemColors:Highlight .
        
        lMouseInBounds = TRUE .

        SUPER:OnMouseEnter (e) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseLeave event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseLeave (e AS System.EventArgs):

        IF lControlHighlighted THEN 
            THIS-OBJECT:BackColor = SystemColors:Highlight .
        ELSE 
            THIS-OBJECT:BackColor = SystemColors:Info .
        
        lMouseInBounds = FALSE .

        SUPER:OnMouseLeave (e) .
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the paint event
        Notes:   
        @param e The PaintEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnPaint (e AS PaintEventArgs):

        THIS-OBJECT:DrawText (e) .
        THIS-OBJECT:DrawCloseButton (e) .
		
		SUPER:OnPaint (e) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TextChanged event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnTextChanged (e AS System.EventArgs):
		
		SUPER:OnTextChanged (e) .
		
		THIS-OBJECT:Invalidate () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the VisibleChanged event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnVisibleChanged (e AS System.EventArgs):
		
		IF THIS-OBJECT:Visible AND THIS-OBJECT:PlaySoundWhenShown THEN 
		    
		    SystemSounds:Beep:Play () . 
		
        SUPER:OnVisibleChanged(e) .

	END METHOD .

END CLASS.
