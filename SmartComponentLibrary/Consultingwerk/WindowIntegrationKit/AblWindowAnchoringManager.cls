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
    File        : AblWindowAnchoringManager
    Purpose     : Provides .NET like Anchoring for ABL Widgets in a Window
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 25 09:17:55 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .
USING System.Windows.Forms.*                FROM ASSEMBLY . 

CLASS Consultingwerk.WindowIntegrationKit.AblWindowAnchoringManager: 

    DEFINE PROTECTED VARIABLE oWidgetAnchorStyles AS 
        "System.Collections.Generic.Dictionary<System.String,System.Windows.Forms.AnchorStyles>" NO-UNDO. 

    DEFINE PROTECTED VARIABLE oWidgetOriginalSizes AS 
        "System.Collections.Generic.Dictionary<System.String,System.String>" NO-UNDO. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the orignal height of the ABL Window in pixels  
        Notes:                                 
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY OriginalWindowHeightPixel AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the original width of the ABL Window in pixels  
        Notes:                                 
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY OriginalWindowWidthPixel AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose:  Returns the handle of the ABL window
        Notes:                                 
    ------------------------------------------------------------------------------*/      
    DEFINE PUBLIC PROPERTY Window AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AblWindowAnchoringManager class                                                                      
        Notes:                                                      
        @param phWindow The handle of the ABL window widget                  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AblWindowAnchoringManager (phWindow AS HANDLE):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Window = phWindow  

               THIS-OBJECT:OriginalWindowHeightPixel = phWindow:HEIGHT-PIXELS 
               THIS-OBJECT:OriginalWindowWidthPixel  = phWindow:WIDTH-PIXELS 
               . 

        ASSIGN oWidgetAnchorStyles  = NEW "System.Collections.Generic.Dictionary<System.String,System.Windows.Forms.AnchorStyles>" () 
               oWidgetOriginalSizes = NEW "System.Collections.Generic.Dictionary<System.String,System.String>" () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the AblWindowAnchoringManager instance for 
                 the given Window
        Notes:  
        @param phWindow The Window to return the AblWindowAnchoringManager for
        @return The reference of the AblWindowAnchoringManager instance for the given window                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC AblWindowAnchoringManager FromWindow (phWindow AS HANDLE):
        
        DEFINE VARIABLE oForm       AS Progress.Lang.Object      NO-UNDO .
        DEFINE VARIABLE oController AS AblWindowAnchoringManager NO-UNDO . 
        
        oForm = SESSION:FIRST-OBJECT .  
        
        DO WHILE VALID-OBJECT (oForm):
            IF TYPE-OF (oForm, AblWindowAnchoringManager) THEN DO:
            
                oController = CAST (oForm, AblWindowAnchoringManager) .                
            
                IF oController:Window = phWindow THEN 
                    RETURN oController . 
            END . 
            
            oForm = oForm:Next-Sibling .
        END.
        
        RETURN ? .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes Resizing in for the Window by registering the original
                 size of all Anchored Widgets                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeResizing ():

        DEFINE VARIABLE hWidget       AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hParent       AS HANDLE  NO-UNDO.
        
        DEFINE VARIABLE iParentWidth  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iParentHeight AS INTEGER NO-UNDO.
        
        oWidgetOriginalSizes:Clear () .
        
        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.AnchorStyles>"" oPair in oWidgetAnchorStyles}

            ASSIGN hWidget = WIDGET-HANDLE (oPair:Key) NO-ERROR . 
            
            IF NOT VALID-HANDLE (hWidget) THEN 
                NEXT .  
            
            ASSIGN hParent = hWidget:PARENT . 
            
            IF hParent:TYPE = "FIELD-GROUP":U THEN 
                ASSIGN hParent = hParent:PARENT . 
    
            IF hParent:TYPE = "WINDOW":U THEN 
                ASSIGN iParentWidth  = THIS-OBJECT:OriginalWindowWidthPixel 
                       iParentHeight = THIS-OBJECT:OriginalWindowHeightPixel .
            ELSE
                ASSIGN iParentWidth  = hParent:WIDTH-PIXELS 
                       iParentHeight = hParent:HEIGHT-PIXELS .
            
            oWidgetOriginalSizes:Add (oPair:Key,
                                      SUBSTITUTE ("&1,&2,&3,&4":U,
                                      hWidget:Y /* Top */,
                                      hWidget:X /* Left */,
                                      iParentHeight - hWidget:HEIGHT-PIXELS - hWidget:Y /* Bottom */,
                                      iParentWidth - hWidget:WIDTH-PIXELS - hWidget:X) /* Right */) .
        END.
    
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Handles the resize event of the Window                                                                     
        Notes:   
        @param  phFrame is a frame the will be resized to the size of the visible client area of the window                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResizeWindow (phFrame AS HANDLE):

        DEFINE VARIABLE iWindowWidth  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iWindowHeight AS INTEGER NO-UNDO.
        
        DEFINE VARIABLE iFrameWidth   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iFrameHeight  AS INTEGER NO-UNDO.
        
        DEFINE VARIABLE lAnchorTop    AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lAnchorBottom AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lAnchorLeft   AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lAnchorRight  AS LOGICAL NO-UNDO.
        
        DEFINE VARIABLE iFromTop      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iFromBottom   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iFromLeft     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iFromRight    AS INTEGER NO-UNDO.
        
        DEFINE VARIABLE iParentWidth AS DECIMAL NO-UNDO.
        DEFINE VARIABLE iParentHeight AS DECIMAL NO-UNDO.
        
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        ASSIGN iWindowWidth  = THIS-OBJECT:Window:WIDTH-PIXELS 
               iWindowHeight = THIS-OBJECT:Window:HEIGHT-PIXELS .
        
        IF VALID-HANDLE (phFrame) THEN DO:
            ASSIGN iFrameWidth  = MAX (iWindowWidth, THIS-OBJECT:OriginalWindowWidthPixel)
                   iFrameHeight = MAX (iWindowHeight, THIS-OBJECT:OriginalWindowHeightPixel) .      
            
            ASSIGN phFrame:X = 0 
                   phFrame:Y = 0 
                   
                   phFrame:SCROLLABLE = TRUE 
                   
                   phFrame:VIRTUAL-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS  
                   phFrame:VIRTUAL-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS .
        END.

        /* Mike Fechner / Consultingwerk Ltd. 24.05.2011
           All other resizable frames follow the main frame */
        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.AnchorStyles>"" oPair1 in oWidgetAnchorStyles}
            ASSIGN hWidget = WIDGET-HANDLE (oPair1:Key) NO-ERROR . 
            
            IF NOT VALID-HANDLE (hWidget) THEN 
                NEXT .
                
            IF hWidget:TYPE <> "FRAME":U OR hWidget = phFrame THEN 
                NEXT .                         
            
            ASSIGN hWidget:X = 0 
                   hWidget:Y = 0 
                   
                   hWidget:SCROLLABLE = TRUE 
                   
                   hWidget:VIRTUAL-HEIGHT-PIXELS = phFrame:VIRTUAL-HEIGHT-PIXELS   
                   hWidget:VIRTUAL-WIDTH-PIXELS  = phFrame:VIRTUAL-WIDTH-PIXELS .
        END.

        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.AnchorStyles>"" oPair2 in oWidgetAnchorStyles}
            ASSIGN hWidget = WIDGET-HANDLE (oPair2:Key) NO-ERROR . 
            
            IF NOT VALID-HANDLE (hWidget) THEN 
                NEXT .  

            ASSIGN lAnchorTop    = Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (oPair2:Value, 
                                                                                                    System.Windows.Forms.AnchorStyles:Top), 
                                                                      System.Windows.Forms.AnchorStyles:Top) 
                   lAnchorLeft   = Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (oPair2:Value, 
                                                                                                    System.Windows.Forms.AnchorStyles:Left), 
                                                                      System.Windows.Forms.AnchorStyles:Left)
                   lAnchorBottom = Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (oPair2:Value, 
                                                                                                    System.Windows.Forms.AnchorStyles:Bottom), 
                                                                      System.Windows.Forms.AnchorStyles:Bottom)
                   lAnchorRight  = Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (oPair2:Value, 
                                                                                                    System.Windows.Forms.AnchorStyles:Right), 
                                                                      System.Windows.Forms.AnchorStyles:Right)
            
                   iFromTop    = INTEGER (ENTRY (1, oWidgetOriginalSizes[oPair2:Key])) .
                   iFromLeft   = INTEGER (ENTRY (2, oWidgetOriginalSizes[oPair2:Key])) .
                   iFromBottom = INTEGER (ENTRY (3, oWidgetOriginalSizes[oPair2:Key])) .
                   iFromRight  = INTEGER (ENTRY (4, oWidgetOriginalSizes[oPair2:Key])) .
            
            IF hWidget:PARENT:TYPE = "FIELD-GROUP":U THEN 
                ASSIGN iParentWidth  = iFrameWidth
                       iParentHeight = iFrameHeight . 
            ELSE 
                ASSIGN iParentWidth  = hWidget:PARENT:WIDTH-PIXELS
                       iParentHeight = hWidget:PARENT:HEIGHT-PIXELS. 

            IF hWidget:PARENT:TYPE = "WINDOW":U THEN 
                ASSIGN iParentWidth  = MAX (iWindowWidth,  THIS-OBJECT:OriginalWindowWidthPixel)
                       iParentHeight = MAX (iWindowHeight, THIS-OBJECT:OriginalWindowHeightPixel) . 
            
            /* Resize in Width */
            IF lAnchorLeft AND lAnchorRight THEN
                ASSIGN hWidget:WIDTH-PIXELS = iParentWidth - iFromRight - hWidget:X.   
            
            /* Move to right */
            IF NOT lAnchorLeft AND lAnchorRight THEN
                ASSIGN hWidget:X = iParentWidth - (iFromRight + hWidget:WIDTH-PIXELS) .   
            
            /* Resize in Height */
            IF lAnchorTop AND lAnchorBottom THEN
                ASSIGN hWidget:HEIGHT-PIXELS = iParentHeight - iFromBottom - hWidget:Y.   
            
            /* Move to Bottom */
            IF NOT lAnchorTop AND lAnchorBottom THEN
                ASSIGN hWidget:Y = iParentHeight - (iFromBottom + hWidget:HEIGHT-PIXELS) .   
        END.

        IF VALID-HANDLE (phFrame) THEN DO:
            ASSIGN phFrame:WIDTH-PIXELS  = iFrameWidth
                   phFrame:HEIGHT-PIXELS = iFrameHeight
                   
                   phFrame:VIRTUAL-WIDTH-PIXELS = phFrame:WIDTH-PIXELS  
                   phFrame:VIRTUAL-HEIGHT-PIXELS = phFrame:HEIGHT-PIXELS  
                    
                   phFrame:SCROLLABLE = FALSE 
                   .
        END.

        /* Mike Fechner / Consultingwerk Ltd. 24.05.2011
           All other resizable frames follow the main frame */
        {Consultingwerk/foreach.i ""System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.AnchorStyles>"" oPair3 in oWidgetAnchorStyles}
            ASSIGN hWidget = WIDGET-HANDLE (oPair3:Key) NO-ERROR . 
            
            IF NOT VALID-HANDLE (hWidget) THEN 
                NEXT .
                
            IF hWidget:TYPE = "BROWSE":U THEN 
                APPLY "END-RESIZE":U TO hWidget .     
                
            IF hWidget:TYPE <> "FRAME":U OR hWidget = phFrame THEN 
                NEXT .                         

            ASSIGN hWidget:WIDTH-PIXELS  = phFrame:WIDTH-PIXELS  
                   hWidget:HEIGHT-PIXELS = phFrame:HEIGHT-PIXELS 
                   
                   hWidget:VIRTUAL-WIDTH-PIXELS = phFrame:VIRTUAL-WIDTH-PIXELS   
                   hWidget:VIRTUAL-HEIGHT-PIXELS = phFrame:VIRTUAL-HEIGHT-PIXELS   
                    
                   hWidget:SCROLLABLE = FALSE 
                   .
        END.
        
        CATCH err AS Progress.Lang.Error :
            MESSAGE err:GetMessage (1) SKIP (1) 
                    err:CallStack.    
        END CATCH.

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Sets Anchoring for an ABL Widget                                                                      
        Notes:                                   
        @param phWidget The widget handle
        @param poAnchorStyles The combined AnchorStyles 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetWidgetAnchorStyles (phWidget AS HANDLE, 
                                              poAnchorStyles AS System.Windows.Forms.AnchorStyles):
        
        DEFINE VARIABLE cWidget AS CHARACTER NO-UNDO.
        
        ASSIGN cWidget = STRING (phWidget) . 
        
        IF NOT VALID-HANDLE (phWidget) THEN
            UNDO, THROW NEW InvalidHandleException ("Widget":U) .
        
        IF oWidgetAnchorStyles:ContainsKey (cWidget) THEN 
            oWidgetAnchorStyles[cWidget] = poAnchorStyles . 
        ELSE 
            oWidgetAnchorStyles:Add (cWidget, poAnchorStyles) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Anchoring for an ABL Widget                                                                      
        Notes:                                   
        @param phWidget The widget handle
        @param poAnchorStyles1 The first AnchorStyles component                                     
        @param poAnchorStyles2 The second AnchorStyles component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetWidgetAnchorStyles (phWidget AS HANDLE, 
                                              poAnchorStyles1 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles2 AS System.Windows.Forms.AnchorStyles):

        DEFINE VARIABLE anchor AS System.Windows.Forms.AnchorStyles NO-UNDO .
        
        anchor = CAST(Progress.Util.EnumHelper:Or(poAnchorStyles1, poAnchorStyles2), 
                      System.Windows.Forms.AnchorStyles).
                      
        SetWidgetAnchorStyles (phWidget, anchor) .                      

    END.

    /*------------------------------------------------------------------------------
        Purpose: Sets Anchoring for an ABL Widget                                                                      
        Notes:                                   
        @param phWidget The widget handle
        @param poAnchorStyles1 The first AnchorStyles component                                     
        @param poAnchorStyles2 The second AnchorStyles component
        @param poAnchorStyles3 The third AnchorStyles component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetWidgetAnchorStyles (phWidget AS HANDLE, 
                                              poAnchorStyles1 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles2 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles3 AS System.Windows.Forms.AnchorStyles):

        DEFINE VARIABLE anchor AS System.Windows.Forms.AnchorStyles NO-UNDO .
        
        anchor = CAST(Progress.Util.EnumHelper:Or(poAnchorStyles1, poAnchorStyles2), 
                      System.Windows.Forms.AnchorStyles).

        anchor = CAST(Progress.Util.EnumHelper:Or(anchor, poAnchorStyles3), 
                      System.Windows.Forms.AnchorStyles).
                      
        SetWidgetAnchorStyles (phWidget, anchor) .                      
    END.

    /*------------------------------------------------------------------------------
        Purpose: Sets Anchoring for an ABL Widget                                                                      
        Notes:                                   
        @param phWidget The widget handle
        @param poAnchorStyles1 The first AnchorStyles component                                     
        @param poAnchorStyles2 The second AnchorStyles component
        @param poAnchorStyles3 The third AnchorStyles component
        @param poAnchorStyles4 The fourth AnchorStyles component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetWidgetAnchorStyles (phWidget AS HANDLE, 
                                              poAnchorStyles1 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles2 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles3 AS System.Windows.Forms.AnchorStyles,
                                              poAnchorStyles4 AS System.Windows.Forms.AnchorStyles):

        DEFINE VARIABLE anchor AS System.Windows.Forms.AnchorStyles NO-UNDO .
        
        anchor = CAST(Progress.Util.EnumHelper:Or(poAnchorStyles1, poAnchorStyles2), 
                      System.Windows.Forms.AnchorStyles).
                      
        anchor = CAST(Progress.Util.EnumHelper:Or(anchor, poAnchorStyles3), 
                      System.Windows.Forms.AnchorStyles).

        anchor = CAST(Progress.Util.EnumHelper:Or(anchor, poAnchorStyles4), 
                      System.Windows.Forms.AnchorStyles).
                      
        SetWidgetAnchorStyles (phWidget, anchor) .                      
    END.

END CLASS.
