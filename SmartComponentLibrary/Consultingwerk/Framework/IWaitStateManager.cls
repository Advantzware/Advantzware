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
    File        : IWaitStateManager
    Purpose     : Interface, defining methods dealing with wait states
                  i.e. while retrieving data from a back end business 
                  logic
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 13 15:39:43 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

INTERFACE Consultingwerk.Framework.IWaitStateManager:  

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState ().

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication for a specific message                                                                      
        Notes:                                  
        @param pcMessage The message to clear the wait-state for
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState (pcMessage AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication for a specific message from a specific 
                 caller                                                                      
        Notes:          
        @param pcMessage The message to clear the wait-state for
        @param poCaller The reference to the caller                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID ClearWaitState (pcMessage AS CHARACTER, 
                                       poCaller AS Progress.Lang.Object).

     /*------------------------------------------------------------------------------
        Purpose: Indicate a general purpose wait-state to the user                                                                          
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState ().

    /*------------------------------------------------------------------------------
        Purpose: Indicate a wait-state with a message to the user.                                                                        
        Notes:   The pcMessage is generally a message code, which will be interpreted
                 by classes implementing the IWaitStateManager interface   
        @param pcMessage The message related to the wait state
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState (pcMessage AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Indicate a wait-state with a message to the user. This wait-state
                 will be bouund to a specific caller                                                                        
        Notes:   The pcMessage is generally a message code, which will be interpreted
                 by classes implementing the IWaitStateManager interface      
        @param pcMessage The message related to the wait state
        @param poCaller The reference to the caller (currently unused)                                                                              
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetWaitState (pcMessage AS CHARACTER, 
                                     poCaller AS Progress.Lang.Object).

 END INTERFACE.
 