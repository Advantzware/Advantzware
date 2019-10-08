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
    File        : AssignSettingFromFormEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 19 22:44:27 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.CancelableEventArgs.

USING Consultingwerk.Windows.Forms.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.AssignSettingFromFormEventArgs INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Form that has been created 
                 by an event subscriber
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Settings Instance
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SettingsInstance AS Progress.Lang.Object NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AssignSettingFromFormEventArgs class
        Notes:   
        @param poForm The Form to assign the values from
        @param oSettings The Settings instance to Assign to
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AssignSettingFromFormEventArgs (poForm AS System.Windows.Forms.Form,
                                                       oSettings AS Progress.Lang.Object):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Form = poForm 
               THIS-OBJECT:SettingsInstance = oSettings .
        
    END CONSTRUCTOR.

END CLASS.
