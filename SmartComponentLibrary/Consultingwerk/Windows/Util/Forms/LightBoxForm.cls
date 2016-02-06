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
    File        : LightBoxForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 08 13:04:09 CEST 2012
    Notes       : This Form purposely does not inherit from the Consultingwerk
                  BaseForm class - as it's only technically a Form (other 
                  Container Controls do not support semi-transparency).
                  None of the BaseForm features, such as storing the 
                  Windows Position in the Registry is required here.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY . 
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.Form               FROM ASSEMBLY .
USING System.Drawing.*                    FROM ASSEMBLY .
USING System.Windows.Forms.Control        FROM ASSEMBLY .

&scoped-define RibbonOffset 7

CLASS Consultingwerk.Windows.Util.Forms.LightboxForm INHERITS Form: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of pixles from the top that the LightboxForm
                 is using to provide access to Controls at the top of the overlaid 
                 Form 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CustomOffset AS INTEGER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Overlaid Form
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OverlaidForm AS Form NO-UNDO 
    GET. 
    PRIVATE SET.          

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Overlaid Form
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OverlaidFormHasRibbon AS LOGICAL NO-UNDO INIT FALSE 
    GET. 
    PRIVATE SET.   
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LightboxForm class.
        Notes:
        @param poOverlaidForm The reference to the Form which is overlaid by the Lightbox    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LightboxForm (poOverlaidForm AS Form):
        
        THIS-OBJECT (poOverlaidForm, 0) .
        
    END CONSTRUCTOR .        
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LightboxForm class.
        Notes:
        @param poOverlaidForm The reference to the Form which is overlaid by the Lightbox
        @param piCustomOffset The number of pixles from the top that the LightboxForm is using to provide access to Controls at the top of the overlaid Form 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LightboxForm (poOverlaidForm AS Form, 
                                     piCustomOffset AS INTEGER):
        
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager NO-UNDO . 
        
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:OverlaidForm  = poOverlaidForm
               THIS-OBJECT:Owner         = OverlaidForm 
               THIS-OBJECT:CustomOffset  = piCustomOffset .
        
        IF VALID-OBJECT (poOverlaidForm) THEN DO:
            oToolbarsManager = UltraToolbarsManager:FromForm (poOverlaidForm) .
        
            IF VALID-OBJECT (oToolbarsManager) THEN 
                THIS-OBJECT:OverlaidFormHasRibbon = oToolbarsManager:Ribbon:Visible . 
        END.
        
        ResizeMe().
        MoveMe().
        
        /* Subscribe to events of the overlaid Form */
        OverlaidForm:Resize:Subscribe(THIS-OBJECT:OverlaidForm_ResizeEnd).
        OverlaidForm:ResizeEnd:Subscribe(THIS-OBJECT:OverlaidForm_ResizeEnd).
        OverlaidForm:Move:Subscribe(THIS-OBJECT:OverlaidForm_Move).
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LightboxForm class.
        Notes:   This Constructor is typically used for Testing purposes only
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LightboxForm ():
        
        SUPER().
        InitializeComponent().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.    
    
    /*------------------------------------------------------------------------------
        Purpose: Resizes the LightBox Form to match the size the overlaid Form
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ResizeMe():
        
        THIS-OBJECT:Size = NEW System.Drawing.Size (OverlaidForm:ClientRectangle:Size:Width, 
                                                    OverlaidForm:ClientRectangle:Size:Height - (IF THIS-OBJECT:OverlaidFormHasRibbon THEN {&RibbonOffset} ELSE 0) - THIS-OBJECT:CustomOffset).
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ResizeEnd event of the overlaid form
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OverlaidForm_ResizeEnd (sender AS System.Object, 
                                               e AS System.EventArgs):
    
        THIS-OBJECT:ResizeMe().
    
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Moves the LightBox Form to match the location of the overlaid Form
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID MoveMe():
        DEFINE VARIABLE oScreenRectangle AS Rectangle NO-UNDO. 
        
        oScreenRectangle = OverlaidForm:RectangleToScreen (OverlaidForm:ClientRectangle).
        
        THIS-OBJECT:Location = NEW Point (oScreenRectangle:X, 
                                          oScreenRectangle:Y + (IF THIS-OBJECT:OverlaidFormHasRibbon THEN {&RibbonOffset} ELSE 0) + THIS-OBJECT:CustomOffset).
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Move event of the overlaid form
        Notes:   
        @param poSender The reference to the object that raised the event
        @param poArgs The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OverlaidForm_Move (poSender AS System.Object,
                                          poArgs AS System.EventArgs):

        THIS-OBJECT:MoveMe().

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes the UI of the LightBox Form
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* LightboxForm */
        /*  */
        THIS-OBJECT:BackColor = System.Drawing.Color:Black.
        THIS-OBJECT:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Zoom.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
        THIS-OBJECT:ControlBox  = FALSE. 
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:None.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "LightboxForm":U.
        THIS-OBJECT:Opacity = 0.5.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Hide.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:Manual.
        THIS-OBJECT:Text = "Lightbox Form":U.
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Destructor of the FullWindowDragManager class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LightboxForm():

        IF VALID-OBJECT (THIS-OBJECT:OverlaidForm) THEN DO:
            THIS-OBJECT:OverlaidForm:Resize:Unsubscribe(OverlaidForm_ResizeEnd).
            THIS-OBJECT:OverlaidForm:ResizeEnd:Unsubscribe(OverlaidForm_ResizeEnd).
            THIS-OBJECT:OverlaidForm:Move:Unsubscribe(OverlaidForm_Move).
        END.
        
        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
