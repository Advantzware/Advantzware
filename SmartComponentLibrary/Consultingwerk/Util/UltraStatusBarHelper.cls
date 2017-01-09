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
    File        : UltraStatusBarHelper
    Purpose     : Supports Merging of UltraStatusBar Controls
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : Useful when merging the Statusbar of an MDI Child into 
                  the MDI Parent
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 17 18:06:13 CEST 2011
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.*                FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinStatusBar.* FROM ASSEMBLY  .
&ENDIF
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Util.UltraStatusBarHelper: 
    
    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED UltraStatusBarHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Merges the UltraStatusPanel's of the Source (MDI Child) UltraStatusBar 
                 into the target (MDI Panel) UltraStatusBar, the Source's status panels 
                 are placed at the location of the placeholder status panel of the 
                 target.                                                                         
        Notes:   UltraStatusBar Panels that do not yet have a Key assigned will receive 
                 a GUID as the key for later recognition during the Synchronize           
        @param poTarget The reference to the target UltraStatusBar instance
        @param poSource The reference to the source UltraStatusBar instance
        @param pcPlaceholderKey The key of the placeholder status bar panel 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Merge (poTarget AS UltraStatusBar, 
                                     poSource AS UltraStatusBar, 
                                     pcPlaceholderKey AS CHARACTER):

        DEFINE VARIABLE oNewPanel AS UltraStatusPanel NO-UNDO . 
        DEFINE VARIABLE iPosition AS INTEGER          NO-UNDO.
        
        IF NOT poTarget:Panels:Exists (pcPlaceholderKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to merge status bars when target has no panel ~"&1~".", pcPlaceholderKey), 0) . 

        ASSIGN iPosition = poTarget:Panels [pcPlaceholderKey]:Index .

        {Consultingwerk/foreach.i UltraStatusPanel oPanel in poSource:Panels}
            oNewPanel = NEW UltraStatusPanel () .
            
            IF oPanel:Key > "":U THEN . 
                ELSE oPanel:Key = GUID .
             
            oNewPanel:Key = oPanel:Key .

            poTarget:Panels:Insert(iPosition, oNewPanel) .
            
            ASSIGN iPosition = iPosition + 1 . 
        END.

        IF poTarget:Panels:Exists (pcPlaceholderKey) THEN 
            poTarget:Panels [pcPlaceholderKey]:Visible = FALSE .

        UltraStatusBarHelper:Synchronize (poTarget, poSource) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Merges the UltraStatusPanel's of the Source (MDI Child) UltraStatusBar 
                 into the target (MDI Panel) UltraStatusBar, the Source's status panels 
                 are placed at the location of the placeholder status panel of the 
                 target.                                                                      
        Notes:   UltraStatusBar Panels that do not yet have a Key assigned will receive 
                 a GUID as the key for later recognition during the Synchronize        
                 Uses "placeholder" as the default key of the placeholder UltraStatusPanel
        @param poTarget The reference to the target UltraStatusBar instance
        @param poSource The reference to the source UltraStatusBar instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Merge (poTarget AS UltraStatusBar, 
                                     poSource AS UltraStatusBar):
        
        UltraStatusBarHelper:Merge (poTarget, poSource, "placeholder":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the UltraStatusPanel's of the Source (MDI Child) UltraStatusBar 
                 from the target (MDI Panel) UltraStatusBar and makes the placeholder 
                 status panel visible again.                                                                      
        Notes:                                                        
        @param poTarget The reference to the target UltraStatusBar instance
        @param poSource The reference to the source UltraStatusBar instance
        @param pcPlaceholderKey The key of the placeholder status bar panel 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Remove (poTarget AS UltraStatusBar, 
                                      poSource AS UltraStatusBar, 
                                      pcPlaceholderKey AS CHARACTER):
        
        {Consultingwerk/foreach.i UltraStatusPanel oPanel in poSource:Panels}

            IF poTarget:Panels:Exists (oPanel:Key) THEN 
                poTarget:Panels:Remove (poTarget:Panels [oPanel:Key]) .
        END.

        IF poTarget:Panels:Exists (pcPlaceholderKey) THEN 
            poTarget:Panels [pcPlaceholderKey]:Visible = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the UltraStatusPanel's of the Source (MDI Child) UltraStatusBar 
                 from the target (MDI Panel) UltraStatusBar and makes the placeholder 
                 status panel visible again.                                                                      
        Notes:   Uses "placeholder" as the default key of the placeholder UltraStatusPanel                                                                     
        @param poTarget The reference to the target UltraStatusBar instance
        @param poSource The reference to the source UltraStatusBar instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Remove (poTarget AS UltraStatusBar, 
                                      poSource AS UltraStatusBar):
        
        UltraStatusBarHelper:Remove (poTarget, poSource, "placeholder":U) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Controls whether an individual Panel within the target status bar   
                 should be visible or not. 
        Notes:   Uses "placeholder" as the default key of the placeholder UltraStatusPanel
        @param poStatusBar The reference to the UltraStatusBar Control that contains the panel
        @param plShow Logical flag indicating if the panel should be shown (true) or hidden (false)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowPanel (poStatusBar AS UltraStatusBar, 
                                         plShow      AS LOGICAL):
                                             
        UltraStatusBarHelper:ShowPanel (poStatusBar, "placeholder":U, plShow) .                                              

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Controls whether an individual Panel within the target status bar   
                 should be visible or not. 
        Notes:   Uses "placeholder" as the default key of the placeholder UltraStatusPanel       
        @param poStatusBar The reference to the UltraStatusBar Control that contains the panel
        @param pcPanelKey The key of the panel to show or hide
        @param plShow Logical flag indicating if the panel should be shown (true) or hidden (false)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowPanel (poStatusBar AS UltraStatusBar, 
                                         pcPanelKey  AS CHARACTER,
                                         plShow      AS LOGICAL):
                                             
        IF poStatusBar:Panels:Exists (pcPanelKey) THEN 
            poStatusBar:Panels [pcPanelKey]:Visible = plShow .
            
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Synchronizes the UltraStatusPanel's of the Source (MDI Child) UltraStatusBar 
                 with the target (MDI Panel) UltraStatusBar. Uses the Key of the 
                 panels to identify matches                                                       
        Notes:                             
        @param poTarget The reference to the target UltraStatusBar instance
        @param poSource The reference to the source UltraStatusBar instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Synchronize (poTarget AS UltraStatusBar, 
                                           poSource AS UltraStatusBar):
        
        DEFINE VARIABLE oTargetPanel AS UltraStatusPanel NO-UNDO . 
        
        poTarget:SuspendLayout() .
        
        {Consultingwerk/foreach.i UltraStatusPanel oSourcePanel in poSource:Panels}

            IF NOT poTarget:Panels:Exists (oSourcePanel:Key) THEN 
                NEXT .             

            oTargetPanel = poTarget:Panels [oSourcePanel:Key] .

            ASSIGN 
                oTargetPanel:BorderStyle = oSourcePanel:BorderStyle  
                oTargetPanel:CharacterPositionFormat = oSourcePanel:CharacterPositionFormat 
                oTargetPanel:Checked = oSourcePanel:Checked 
                oTargetPanel:CursorPositionFormat = oSourcePanel:CursorPositionFormat 
                oTargetPanel:DateTimeFormat = oSourcePanel:DateTimeFormat
                oTargetPanel:Enabled = oSourcePanel:Enabled 
                oTargetPanel:MinWidth = oSourcePanel:MinWidth
                oTargetPanel:SizingMode = oSourcePanel:SizingMode 
                oTargetPanel:Style = oSourcePanel:Style 
                oTargetPanel:Tag = oSourcePanel:Tag 
                oTargetPanel:Text = oSourcePanel:Text 
                oTargetPanel:ToolTipText = oSourcePanel:ToolTipText
                oTargetPanel:Visible = oSourcePanel:Visible 
                oTargetPanel:Width = oSourcePanel:Width
                oTargetPanel:WrapText = oSourcePanel:WrapText .
            
            ASSIGN 
                oTargetPanel:KeyStateInfo:DisplayStyle = oSourcePanel:KeyStateInfo:DisplayStyle
                oTargetPanel:KeyStateInfo:Key = oSourcePanel:KeyStateInfo:Key
                oTargetPanel:KeyStateInfo:OffText = oSourcePanel:KeyStateInfo:OffText
                oTargetPanel:KeyStateInfo:OnText = oSourcePanel:KeyStateInfo:OnText
                oTargetPanel:KeyStateInfo:Tag = oSourcePanel:KeyStateInfo:Tag .

            ASSIGN 
                oTargetPanel:MarqueeInfo:Delay = oSourcePanel:MarqueeInfo:Delay
                oTargetPanel:MarqueeInfo:IsActive = oSourcePanel:MarqueeInfo:IsActive
                oTargetPanel:MarqueeInfo:MarqueeDirection = oSourcePanel:MarqueeInfo:MarqueeDirection
                oTargetPanel:MarqueeInfo:MarqueeScrollAmount = oSourcePanel:MarqueeInfo:MarqueeScrollAmount
                oTargetPanel:MarqueeInfo:MarqueeStyle = oSourcePanel:MarqueeInfo:MarqueeStyle .

            ASSIGN 
                oTargetPanel:ProgressBarInfo:Label = oSourcePanel:ProgressBarInfo:Label
                oTargetPanel:ProgressBarInfo:Maximum = oSourcePanel:ProgressBarInfo:Maximum
                oTargetPanel:ProgressBarInfo:Minimum = oSourcePanel:ProgressBarInfo:Minimum
                oTargetPanel:ProgressBarInfo:PercentFormat = oSourcePanel:ProgressBarInfo:PercentFormat
                oTargetPanel:ProgressBarInfo:SegmentWidth = oSourcePanel:ProgressBarInfo:SegmentWidth
                oTargetPanel:ProgressBarInfo:ShowLabel = oSourcePanel:ProgressBarInfo:ShowLabel
                oTargetPanel:ProgressBarInfo:Style = oSourcePanel:ProgressBarInfo:Style
                oTargetPanel:ProgressBarInfo:Value = oSourcePanel:ProgressBarInfo:Value .
        END.

        poTarget:ResumeLayout (TRUE) .

    END METHOD.
&ENDIF
    
END CLASS.
