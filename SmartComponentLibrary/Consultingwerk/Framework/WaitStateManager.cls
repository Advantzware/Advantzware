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
    File        : WaitStateManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 13 15:49:44 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.* FROM PROPATH .                 
USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Framework.WaitStateManager 
    IMPLEMENTS IWaitStateManager, IRaisesWaitStateChangedEvent: 

    /*------------------------------------------------------------------------------
        Purpose: Raised whenever the WaitState changes                                                                       
        Notes:                                   
        @param sender The reference to the object that raised the event
        @param e The WaitStateChangedEventArgs with the data for this event                                      
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT WaitStateChanged SIGNATURE VOID (sender AS Progress.Lang.Object, 
                                                         e AS WaitStateChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState ():

        THIS-OBJECT:ClearWaitState ("":U) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication for a specific message                                                                      
        Notes:                                                                      
        @param pcMessage The message to clear the wait state for
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState (pcMessage AS CHARACTER).
 
        SESSION:SET-WAIT-STATE ("":U) .

        OnWaitStateChanged (NEW WaitStateChangedEventArgs (WaitStateActionEnum:ClearWaitState,
                                                           pcMessage)) .
 
    END METHOD.
 
     /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication for a specific message from a specific
                 caller                                                                      
        Notes:   This is not supported by this class.
                 ClearWaitState (pcMessage) will call ClearWaitState ()                                                                       
        @param pcMessage The message to clear the wait-state for
        @param poCaller The reference to the caller                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState (pcMessage AS CHARACTER, 
                                       poCaller AS Progress.Lang.Object).
 
        THIS-OBJECT:ClearWaitState (pcMessage) .

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: Raises the WaitStateChanged event
        Notes:   
        @param e The WaitStateChangedEventArgs instance with the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnWaitStateChanged (e AS WaitStateChangedEventArgs):
        
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "WaitStateChanged":U) .

        THIS-OBJECT:WaitStateChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Indicate a general purpose wait-state to the user                                                                          
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState ():
        
        THIS-OBJECT:SetWaitState ("":U) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Indicate a wait-state with a message to the user.  This wait-state
                 will be bouund to a specific caller                                                                                          
        Notes:   The pcMessage is generally a message code, which will be interpreted
                 by classes implementing the IWaitStateManager interface 
                 This is not supported by this class.
                 SetWaitState (pcMessage) will call SetWaitState ()
        @param pcMessage The message related to the wait state
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState (pcMessage AS CHARACTER):
        
        SESSION:SET-WAIT-STATE ("GENERAL":U) .

        OnWaitStateChanged (NEW WaitStateChangedEventArgs (WaitStateActionEnum:SetWaitState,
                                                           pcMessage)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Indicate a wait-state with a message to the user.                                                                        
        Notes:   The pcMessage is generally a message code, which will be interpreted
                 by classes implementing the IWaitStateManager interface 
                 This is not supported by this class.
                 SetWaitState (pcMessage, poCaller) will call SetWaitState (pcMessage)       
        @param pcMessage The message related to the wait state
        @param poCaller The reference to the caller (currently unused)                                                                              
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState (pcMessage AS CHARACTER,
                                     poCaller AS Progress.Lang.Object):
        
        THIS-OBJECT:SetWaitState (pcMessage) .

    END METHOD.
    
END CLASS.
