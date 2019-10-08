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
    File        : FieldCreatedEventArgs
    Purpose     : Event argument class for the FieldCreated event of the 
                  SmartViewerDesignerControl
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 14 09:29:16 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Design.SmartComponents.FieldCreatedEventArgs
    INHERITS Consultingwerk.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Control                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Control AS System.Windows.Forms.Control NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Label Control                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Label AS System.Windows.Forms.Control NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldCreatedEventArgs class                                                                        
        Notes:                                                  
        @param poControl The reference to the Control for this event                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldCreatedEventArgs (poControl AS System.Windows.Forms.Control):
        SUPER ().

        ASSIGN THIS-OBJECT:Control = poControl .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldCreatedEventArgs class                                                                        
        Notes:                                                  
        @param poControl The reference to the Control for this event
        @param poLabel The reference to the Label Control for this event                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldCreatedEventArgs (poControl AS System.Windows.Forms.Control,
                                              poLabel AS System.Windows.Forms.Control):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Control = poControl
               THIS-OBJECT:Label   = poLabel  . 
        
    END CONSTRUCTOR.

END CLASS.

