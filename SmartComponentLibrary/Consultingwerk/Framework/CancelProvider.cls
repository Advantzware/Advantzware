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
    File        : CancelProvider
    Purpose     : 
    Syntax      : 
    Description : Provides ability to control cancelling of a running job 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 23 13:24:53 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING Consultingwerk.Framework.* .

CLASS Consultingwerk.Framework.CancelProvider IMPLEMENTS ICancelProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user (?) presses cancel                                                                      
        Notes: 
        @param sender The reference to the object that raised the event                                               
        @param e The Consultingwerk.EventArgs with the data for this event                                                                                    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT Cancel SIGNATURE VOID (sender AS Progress.Lang.Object,
                                               e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the CancelActive property is changed                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event                                               
        @param e The Consultingwerk.EventArgs with the data for this event                                                                                    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CancelActiveChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets or sets if the Cancel function is active or not.                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CancelActive AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET (arg AS LOGICAL):
        THIS-OBJECT:CancelActive = arg . 
        
        OnCancelActiveChanged (Consultingwerk.EventArgs:Empty) .
    END. 

    /*------------------------------------------------------------------------------
        Purpose: Raises the Cancel event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs with the data for this event                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCancel (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 

        THIS-OBJECT:Cancel:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CancelActive event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs with the data for this event                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCancelActiveChanged (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 

        THIS-OBJECT:CancelActiveChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the cancel function                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformCancel ():
        
        OnCancel (Consultingwerk.EventArgs:Empty) .
        
    END METHOD . 

END CLASS.
