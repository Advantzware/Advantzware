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
    File        : ICancelProvider
    Purpose     : 
    Syntax      : 
    Description : Provides ability to control cancelling of a running job
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 23 11:02:01 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.ICancelProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user (?) presses cancel                                                                      
        Notes:    
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                                    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT Cancel SIGNATURE VOID 
           (sender AS Progress.Lang.Object,
            e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the CancelActive property is changed                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                                    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CancelActiveChanged SIGNATURE VOID 
           (sender AS Progress.Lang.Object,
            e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets or sets if the Cancel function is active or not.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CancelActive AS LOGICAL NO-UNDO  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Performs the cancel function                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformCancel ().

END INTERFACE.
