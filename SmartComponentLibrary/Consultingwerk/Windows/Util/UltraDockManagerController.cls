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
    File        : UltraDockManagerController
    Purpose     : Provides additional Control for the UltraDockManager
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 03 14:43:03 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.*           FROM PROPATH . 
USING Consultingwerk.Windows.Util.*   FROM PROPATH .  
USING Infragistics.Win.UltraWinDock.* FROM ASSEMBLY . 
USING Progress.Lang.*                 FROM PROPATH .
USING System.Windows.Forms.*          FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.UltraDockManagerController: 

    DEFINE VARIABLE oEnforceDockedPanes AS "System.Collections.Generic.List<DockableControlPane>":U NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the default system sound "Hand" should be played 
                 when cancelling actions on the UltraDockManager 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BeepWhenCancellingDrag AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraDockManager instance this Controller
                 is managing
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DockManager AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraDockManagerController class                                                                        
        Notes:                                                  
        @param poDockManager The reference to the UltraDockManager controlled by the new instance                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraDockManagerController (poDockManager AS UltraDockManager):
        SUPER ().
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poDockManager, 
                                                       "UltraDockManager Reference"{&TRAN}) .
        
        THIS-OBJECT:DockManager = poDockManager .
        
        poDockManager:BeforeDockChangeStart:Subscribe (BeforeDockChangeStartHandler) .
        poDockManager:BeforeToggleDockState:Subscribe (BeforeToggleDockStateHandler) .
        
        /* Mike Fechner, Consultingwerk Ltd. 03.08.2012
           :U here crashes the compiler */
        ASSIGN oEnforceDockedPanes = NEW "System.Collections.Generic.List<DockableControlPane>" () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeDockChangeStart event  
        Notes:   
        @param sender The object that has raised the BeforeDockChangeStart event 
        @param e The CancelablePaneEventArgs instance with the data for this event  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BeforeDockChangeStartHandler (sender AS System.Object, 
                                                        e AS CancelablePaneEventArgs):
        
        IF TYPE-OF (e:Pane, DockableControlPane) THEN DO:
            IF oEnforceDockedPanes:Contains (CAST (e:Pane, DockableControlPane)) THEN DO:
                e:Cancel = TRUE . 
                 
                IF THIS-OBJECT:BeepWhenCancellingDrag THEN  
                    System.Media.SystemSounds:Hand:Play() .  
                 
                RETURN . 
            END.
        END.
        ELSE IF TYPE-OF (e:Pane, DockableGroupPane) THEN DO:
            {Consultingwerk/foreach.i DockableControlPane oPane in oEnforceDockedPanes}    
                
                IF THIS-OBJECT:ContainsPane (CAST (e:Pane, DockableGroupPane),
                                             oPane) THEN DO: 
                    e:Cancel = TRUE . 
                    
                    IF THIS-OBJECT:BeepWhenCancellingDrag THEN  
                        System.Media.SystemSounds:Hand:Play() .  
                    
                    RETURN .                      
                END.            
            END.
        END.             

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeToggleDockState event  
        Notes:   
        @param sender The object that has raised the BeforeToggleDockState event 
        @param e The CancelablePaneEventArgs instance with the data for this event  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BeforeToggleDockStateHandler (sender AS System.Object, 
                                                        e AS CancelablePaneEventArgs):
        
        IF TYPE-OF (e:Pane, DockableControlPane) THEN DO:
            IF oEnforceDockedPanes:Contains (CAST (e:Pane, DockableControlPane)) THEN DO:
                e:Cancel = TRUE . 
                 
                IF THIS-OBJECT:BeepWhenCancellingDrag THEN  
                    System.Media.SystemSounds:Hand:Play() .  
                 
                RETURN . 
            END.
        END.
        ELSE IF TYPE-OF (e:Pane, DockableGroupPane) THEN DO:
            {Consultingwerk/foreach.i DockableControlPane oPane in oEnforceDockedPanes}    
                
                IF THIS-OBJECT:ContainsPane (CAST (e:Pane, DockableGroupPane),
                                             oPane) THEN DO: 
                    e:Cancel = TRUE . 
                    
                    IF THIS-OBJECT:BeepWhenCancellingDrag THEN  
                        System.Media.SystemSounds:Hand:Play() .  
                    
                    RETURN .                      
                END.            
            END.
        END.   
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Recursively searches for a child pane in the parent 
        Notes:   
        @param poParentPane The DockableGroupPane or DockAreaPane to look in 
        @param poChildPane The childpane to look for
        @return Logical value indicating if the pane is contained in the parent pane
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ContainsPane (poParentPane AS DockableGroupPane,
                                        poChildPane AS DockableControlPane):

        {Consultingwerk/foreach.i DockablePaneBase oPane in poParentPane:Panes}

            IF oPane = poChildPane THEN 
                RETURN TRUE . 
                
            IF TYPE-OF (oPane, DockableGroupPane) THEN 
                IF THIS-OBJECT:ContainsPane (CAST (oPane, DockableGroupPane),
                                             poChildPane) THEN 
                    RETURN TRUE .                                                
        END .

        RETURN FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enforces that the Dockable Pane of the given Control remains docked in the main form 
        Notes:   
        @param poControl The Control who's DockableControlPane should always remain docked in the main form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnforcePaneIsDocked (poControl AS Control):

        DEFINE VARIABLE oPane AS DockableControlPane NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poControl, 
                                                       "Control":U) .

        ASSIGN oPane = THIS-OBJECT:DockManager:PaneFromControl (poControl) . 
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (oPane, 
                                                       "DockableControlPane of Control":U) .

        oEnforceDockedPanes:Add (oPane) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enforces that a DockableControlPane remains docked in the main form 
        Notes:   
        @param poPane The DockableControlPane that should always remain docked in the main form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnforcePaneIsDocked (poPane AS DockableControlPane):
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poPane, 
                                                       "DockableControlPane":U) .
                                                       
        oEnforceDockedPanes:Add (poPane) .                                                       
                                                       
    END METHOD .

END CLASS.
