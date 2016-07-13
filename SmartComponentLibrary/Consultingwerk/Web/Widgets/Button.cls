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
    File        : Button
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 26 23:45:54 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.*         FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Button 
    INHERITS Control
    IMPLEMENTS IPerformInput, IRaiseEvents: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the user clicks on the button 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT Click SIGNATURE VOID (sender AS Progress.Lang.Object, 
                                              e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ButtonType
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ButtonType AS ButtonTypeEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if a Click event is pending 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY ClickEventPending AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Label of this Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Button class                                                                      
        Notes:                                  
        @param pcName The name of the Button Control                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Button (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
        THIS-OBJECT:ButtonType = ButtonTypeEnum:Submit .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Click event
        Notes:   
        @param e The Consultingwerk.EventArgs instance for the Click event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnClick (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:Click:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs Input from the Web Browser for this Control
        Notes:   
        @return The current input value of the control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        
        ASSIGN cValue = WebUtilities:GetField (THIS-OBJECT:Name) .
        
        IF cValue > "":U THEN 
            THIS-OBJECT:ClickEventPending = TRUE . 
        
        RETURN cValue .        
        
    END METHOD . 
            
    /*------------------------------------------------------------------------------
        Purpose: Performs the output for the Button                                                               
        Notes:                                  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<input type="&6" name="&5" style="top: &2px; left: &3px; width: &4px" value="&1"></button>':U,
                                        THIS-OBJECT:HtmlEncode (THIS-OBJECT:Label),
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width,
                                        THIS-OBJECT:Name,
                                        LC(THIS-OBJECT:ButtonType:ToString ())), 
                                        TRUE) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises Events from a Web Control
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RaiseEvents ():
        
        IF THIS-OBJECT:ClickEventPending THEN 
            THIS-OBJECT:OnClick (Consultingwerk.EventArgs:Empty) .
            
        FINALLY:
            THIS-OBJECT:ClickEventPending = FALSE .     
        END FINALLY.            

    END METHOD .

END CLASS.
