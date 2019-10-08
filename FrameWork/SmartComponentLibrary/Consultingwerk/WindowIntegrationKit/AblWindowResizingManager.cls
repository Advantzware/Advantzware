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
    File        : AblWindowResizingManager
    Purpose     : Provides Anchoring to ABL widgets relatively to an 
                  Anchor Point. Widgets outside that anchor will be resized 
                  or moved depending on which side of the Widget is kept
                  within the anchor or outside
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 22 13:33:01 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                      FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.AblWindowResizingManager: 

    DEFINE PROTECTED TEMP-TABLE ttFrame NO-UNDO 
        FIELD Instance AS Progress.Lang.Object
        FIELD FrameHandle AS HANDLE 
        FIELD AnchorX AS INTEGER 
        FIELD AnchorY AS INTEGER 
        INDEX FrameHandle IS PRIMARY UNIQUE 
            Instance FrameHandle  .

    /*------------------------------------------------------------------------------
        Purpose: Returns the current height in pixels of the managed ABL Window widget
        Notes:   Used to determine the delta during resizing operations                                                                       
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentWindowHeightPixels AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the current width in pixels of the managed ABL Window widget
        Notes:   Used to determine the delta during resizing operations                                                                       
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentWindowWidthPixels AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if nested frames should be ignored during resizing the 
                 parent frame
        Notes:   Used when nested Frames are resize by themselves                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IgnoreNestedFrames AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of Pixels from the top that Widgets 
                 contained within (widget:Y <= IgnorePixelsFromTop) should 
                 ignored while resizing                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IgnorePixelsFromTop AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the managed window                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WindowHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AblWindowResizingManager class                                                                       
        Notes:   
        @param phWindow The handle of the Window widget to manage                                                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AblWindowResizingManager (phWindow AS HANDLE):
        SUPER ().
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow WidgetTypeEnum:Window} .
        
        ASSIGN THIS-OBJECT:WindowHandle              = phWindow 
               THIS-OBJECT:CurrentWindowWidthPixels  = phWindow:WIDTH-PIXELS
               THIS-OBJECT:CurrentWindowHeightPixels = phWindow:HEIGHT-PIXELS . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Registers a frame for resizing                                                                        
        Notes:   Parameters include the location of an anchor point.
                 Widgets with the top left corner outside an rectangle defined by 
                 this anchor and the top left corner of the window will be moved to
                 the right or bottom during resizing operations. 
        @param phFrame The frame handle to register for resiging
        @param piAnchorX The X coordinate of the anchor
        @param piAnchorY The Y coordinate of the anchor                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeFrameForResizing (phFrame AS HANDLE,
                                                   piAnchorX AS INTEGER, 
                                                   piAnchorY AS INTEGER):
        
        CREATE ttFrame.
        ASSIGN ttFrame.Instance    = THIS-OBJECT 
               ttFrame.FrameHandle = phFrame 
               ttFrame.AnchorX     = piAnchorX
               ttFrame.AnchorY     = piAnchorY .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resizes the contents of the ABL window widgets to the current
                 dimensions of the Window                                                                       
        Notes:   Typically invoked during the WINDOW-RESIZED trigger of the ABL 
                 window widget                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResizeWindow ():

        DEFINE VARIABLE iDeltaWidth  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iDeltaHeight AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hWidget      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hFrame       AS HANDLE NO-UNDO.

        ASSIGN iDeltaWidth  = THIS-OBJECT:WindowHandle:WIDTH-PIXELS - THIS-OBJECT:CurrentWindowWidthPixels
               iDeltaHeight = THIS-OBJECT:WindowHandle:HEIGHT-PIXELS - THIS-OBJECT:CurrentWindowHeightPixels . 

        FOR EACH ttFrame WHERE ttFrame.Instance = THIS-OBJECT ON ERROR UNDO, THROW:
            
            ASSIGN hFrame = ttFrame.FrameHandle .
            
            IF iDeltaWidth > 0 THEN 
                ASSIGN hFrame:WIDTH-PIXELS = hFrame:WIDTH-PIXELS + iDeltaWidth .
            IF iDeltaHeight > 0 THEN 
                ASSIGN hFrame:HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS + iDeltaHeight .
            
            ASSIGN hWidget = hFrame:FIRST-CHILD:FIRST-CHILD . 
            
            DO WHILE VALID-HANDLE (hWidget):
                
                IF hWidget:TYPE = WidgetTypeEnum:Frame AND THIS-OBJECT:IgnoreNestedFrames THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT .     
                END.
                
                IF hWidget:Y + ttFrame.FrameHandle:Y < THIS-OBJECT:IgnorePixelsFromTop THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT .     
                END.
                
                /* move widgets outside of the anchor */
                IF hWidget:X >= ttFrame.AnchorX THEN 
                    hWidget:X = hWidget:X + iDeltaWidth .
                ELSE
                    /* resize */
                    IF hWidget:X < ttFrame.AnchorX AND hWidget:X + hWidget:WIDTH-PIXELS >= ttFrame.AnchorX AND
                       CAN-SET (hWidget, "WIDTH-PIXELS":U) THEN
                        hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + iDeltaWidth .
                    
                IF hWidget:Y >= ttFrame.AnchorY THEN 
                    hWidget:Y = hWidget:Y + iDeltaHeight .
                ELSE
                    /* resize */
                    IF hWidget:Y < ttFrame.AnchorY AND hWidget:Y + hWidget:HEIGHT-PIXELS >= ttFrame.AnchorY AND
                       CAN-SET (hWidget, "HEIGHT-PIXELS":U) THEN
                        hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + iDeltaHeight .
            
                hWidget = hWidget:NEXT-SIBLING . 
            END.
            
            FINALLY:
                ASSIGN ttFrame.AnchorX = ttFrame.AnchorX + iDeltaWidth 
                       ttFrame.AnchorY = ttFrame.AnchorY + iDeltaHeight .
                
                IF iDeltaWidth < 0 THEN 
                    ASSIGN hFrame:WIDTH-PIXELS = hFrame:WIDTH-PIXELS + iDeltaWidth NO-ERROR .
                IF iDeltaHeight < 0 THEN 
                    ASSIGN hFrame:HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS + iDeltaHeight NO-ERROR .
                    
                ASSIGN hFrame:SCROLLABLE = FALSE . 
                    
            END FINALLY.
        END.

        FINALLY:
            ASSIGN THIS-OBJECT:CurrentWindowWidthPixels  = THIS-OBJECT:WindowHandle:WIDTH-PIXELS 
                   THIS-OBJECT:CurrentWindowHeightPixels = THIS-OBJECT:WindowHandle:HEIGHT-PIXELS .
        END FINALLY.

    END METHOD.

END CLASS.
