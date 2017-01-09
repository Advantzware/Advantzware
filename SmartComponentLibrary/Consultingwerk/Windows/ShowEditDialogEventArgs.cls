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
    File        : ShowEditDialogEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 19 22:31:37 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.CancelableEventArgs FROM PROPATH .
USING Consultingwerk.Windows.*           FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Windows.ShowEditDialogEventArgs INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the Form that has been created 
                 by an event subscriber
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO 
    GET.
    SET. 

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
        @param oSettingsInstance The Settings instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ShowEditDialogEventArgs (oSettingsInstance AS Progress.Lang.Object):
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
