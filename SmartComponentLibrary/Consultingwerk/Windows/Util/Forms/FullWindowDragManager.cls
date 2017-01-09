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
    File        : FullWindowDragManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 03 17:57:26 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*        FROM PROPATH .
USING System.Windows.Forms.* FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.Forms.FullWindowDragManager: 

    DEFINE PRIVATE VARIABLE oControl AS Control NO-UNDO .
    DEFINE PRIVATE VARIABLE oForm    AS Form    NO-UNDO .  
    
    DEFINE VARIABLE lTrackingMovement  AS LOGICAL              NO-UNDO INIT FALSE .
    DEFINE VARIABLE oLastMouseLocation AS System.Drawing.Point NO-UNDO.     
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the FullWindowDragManager class.
        Notes:   Subscribes MouseDown, MouseMove and MouseUp events of the managed
                 control
        @param poManagedControl The reference to the Control which will be managed by this instance                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FullWindowDragManager (poManagedControl AS Control):
        SUPER ().
        
        ASSIGN oControl = poManagedControl
               oForm    = poManagedControl:FindForm () .
        
        oForm:FormClosed:Subscribe (FormClosedHandler) .    
            
        oControl:MouseDown:Subscribe (MouseDownHandler) .    
        oControl:MouseUp:Subscribe (MouseUpHandler) .   
            
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Handles the FormClosed event of the managed Form                                                                        
        Notes:   Deletes this instance
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormClosedHandler (sender AS System.Object,
                                             e AS System.Windows.Forms.FormClosedEventArgs):
        
        DELETE OBJECT THIS-OBJECT .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Start tracking of Mouse coordinates for MouseMove event handler                                                                      
        Notes:
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MouseDownHandler (sender AS System.Object, 
                                          e AS System.Windows.Forms.MouseEventArgs):
        
        IF Progress.Util.EnumHelper:AreNotEqual (e:Button,    
                                                 System.Windows.Forms.MouseButtons:Left) THEN RETURN . 
        
        ASSIGN lTrackingMovement  = TRUE
               oLastMouseLocation = oControl:PointToScreen(e:Location) . 

        oControl:MouseMove:Subscribe (MouseMoveHandler) .   

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Move the Form when mouse button is down                                                                       
        Notes:
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MouseMoveHandler (sender AS System.Object, 
                                          e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oLocation AS System.Drawing.Point NO-UNDO . 
        
        IF NOT lTrackingMovement OR NOT VALID-OBJECT (oLastMouseLocation) THEN 
            RETURN . 
            
        ASSIGN oLocation          = oControl:PointToScreen (e:Location) 
               oForm:Top          = oForm:Top + oLocation:Y - oLastMouseLocation:Y
               oForm:Left         = oForm:Left + oLocation:X - oLastMouseLocation:X
                    
               oLastMouseLocation = NEW System.Drawing.Point (oLocation:X, oLocation:Y) .   

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops tracking mouse movement for MouseMove event handler                                                                        
        Notes:
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MouseUpHandler (sender AS System.Object, 
                                        e AS System.Windows.Forms.MouseEventArgs):
        
        ASSIGN lTrackingMovement  = FALSE 
               oLastMouseLocation = ? . 

        oControl:MouseMove:Unsubscribe (MouseMoveHandler) .   

    END METHOD.

END CLASS.