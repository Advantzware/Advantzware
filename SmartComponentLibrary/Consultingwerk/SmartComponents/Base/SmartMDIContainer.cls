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
    File        : SmartMDIContainer
    Purpose     : Base class for MdiContainer inheriting from the 
                  SmartWindowForm class 
    Syntax      : 
    Description : Provides management of the MdiContainer Title based on the 
                  active MdiChild
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 08 09:45:12 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.IMDIContainer FROM PROPATH .
USING Consultingwerk.Forms.BaseForm          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*  FROM PROPATH . 
USING Progress.Lang.*                        FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartMDIContainer 
    INHERITS SmartWindowForm 
    IMPLEMENTS IMDIContainer: 

    DEFINE PRIVATE VARIABLE cBaseText      AS CHARACTER NO-UNDO .    
    DEFINE PRIVATE VARIABLE lUpdatingTitle AS LOGICAL   NO-UNDO INIT FALSE .     
    
    /* Mike Fechner, Consultingwerk Ltd. 22.12.2009
       10.2B: Use of generic List<Form> instead of temp-table */    
    DEFINE VARIABLE oFormsList AS "System.Collections.Generic.List<System.Windows.Forms.Form>" NO-UNDO .  
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMDIContainer class                                                                         
        Notes:    
    ------------------------------------------------------------------------------*/ 
    CONSTRUCTOR PUBLIC SmartMDIContainer ():
        SUPER ().
        
        oFormsList = NEW "System.Collections.Generic.List<System.Windows.Forms.Form>" () . 
        
        THIS-OBJECT:MdiChildActivate:Subscribe (MdiChildActivateHandler) .
        THIS-OBJECT:TextChanged:Subscribe (TextChangedHandler) .
        
        cBaseText = THIS-OBJECT:Text .        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activate MdiChildActivate and TextChanged event handlers. This
                 is required to circumvent issues (frozen app) that may occur while 
                 changing the MdiContainer title (Text property) in the middle of 
                 the process of embedding (before the embedded window has been 
                 fully realized as an MDI child).                                                                         
        Notes:   Future releases of OpenEdge (bug fixes) may enable us to remove 
                 this method. It's not intended for public use!                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ActivateWindowTitleEventHandler ():
        
        THIS-OBJECT:MdiChildActivate:Subscribe (MdiChildActivateHandler) .
        THIS-OBJECT:TextChanged:Subscribe (TextChangedHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deactivate MdiChildActivate and TextChanged event handlers. This
                 is required to circumvent issues (frozen app) that may occur while 
                 changing the MdiContainer title (Text property) in the middle of 
                 the process of embedding (before the embedded window has been 
                 fully realized as an MDI child).                                                                         
        Notes:   Future releases of OpenEdge (bug fixes) may enable us to remove 
                 this method. It's not intended for public use! 
    ------------------------------------------------------------------------------*/ 
    METHOD PUBLIC VOID DeactivateWindowTitleEventHandler ():
        
        THIS-OBJECT:MdiChildActivate:Unsubscribe (MdiChildActivateHandler) .
        THIS-OBJECT:TextChanged:Unsubscribe (TextChangedHandler) .

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form. 
        Notes:   Event handler method for the MdiChildActivate event
                 with unknown values for both parameters. 
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                       
    ------------------------------------------------------------------------------*/        
    METHOD PRIVATE VOID MdiChildActivateHandler (sender AS System.Object, 
                                                 e AS System.EventArgs):
        
        DEFINE VARIABLE child AS System.Windows.Forms.Form NO-UNDO . 
        
        UpdateTitleWithMdiChild () .
        
        IF VALID-OBJECT(THIS-OBJECT:ActiveMdiChild) THEN DO:
            /* Add FormClosed event handler to reset window title after close of 
               last window */
            child = THIS-OBJECT:ActiveMdiChild . 
        
            IF NOT oFormsList:Contains (child) THEN DO: 
                oFormsList:Add (child) .
                
                THIS-OBJECT:ActiveMdiChild:FormClosed:Subscribe(MdiChildFormClosedHandler).
                THIS-OBJECT:ActiveMdiChild:TextChanged:Subscribe(MdiChildFormTextChangedHandler).
            END.
        END.        
    END.

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form. 
        Notes:   Invokes the private MdiChildActiveHandler event handler method
                 with unknown values for both parameters.                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID MdiChildActivateHandler ():
        
        MdiChildActivateHandler (?, ?).

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosed event of the MdiChild forms                                                                        
        Notes:
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID MdiChildFormClosedHandler (sender AS System.Object, 
                                                     e AS System.Windows.Forms.FormClosedEventArgs):
        
        DEFINE VARIABLE child AS System.Windows.Forms.Form NO-UNDO . 

        ASSIGN child = CAST(sender, System.Windows.Forms.Form) NO-ERROR . 
        
        IF oFormsList:Contains (child) THEN 
            oFormsList:Remove (child) .
        
        FINALLY:
            UpdateTitleWithMdiChild () .                
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form. 
        Notes:   Invoked when the Text property of the MDI Container is changed 
                 programmatically.         
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID TextChangedHandler (sender AS CLASS System.Object, 
                                            e AS CLASS System.EventArgs):
        IF lUpdatingTitle THEN RETURN . /* avoid recursion */
    
        ASSIGN cBaseText = THIS-OBJECT:TEXT .    
        
        UpdateTitleWithMdiChild () .
    END METHOD. 
    
    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form. 
        Notes:   Invoked when the Text property of the MDI Child is changed 
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID MdiChildFormTextChangedHandler (sender AS System.Object, 
                                                        e AS System.EventArgs):
        UpdateTitleWithMdiChild () .
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form. 
        Notes:                                                                           
    ------------------------------------------------------------------------------*/            
    METHOD PRIVATE VOID UpdateTitleWithMdiChild ():
        ASSIGN lUpdatingTitle = TRUE . 
        
        THIS-OBJECT:TextChanged:Unsubscribe(TextChangedHandler) .
        
        IF VALID-OBJECT(THIS-OBJECT:ActiveMdiChild) THEN     
            THIS-OBJECT:Text = cBaseText + " <":U + THIS-OBJECT:ActiveMdiChild:Text + ">":U .
        ELSE
            THIS-OBJECT:Text = cBaseText .

        THIS-OBJECT:TextChanged:Subscribe(TextChangedHandler) .

        FINALLY:
            ASSIGN lUpdatingTitle = FALSE . 
        END.
        
    END METHOD. 

END CLASS.
