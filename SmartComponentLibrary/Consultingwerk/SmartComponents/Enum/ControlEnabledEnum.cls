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
    File        : ControlEnabledEnum
    Purpose     : Defines states under which controls in a SmartViewerControl
                  are Enabled
    Syntax      : 
    Description : Used for SmartViewerControl:SetControlEnabled
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 14 15:47:05 CET 2009
    Notes       : Possible values: Add, Always, Newver, Update
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Enum.ControlEnabledEnum   : 

    /*------------------------------------------------------------------------------
        Purpose: The Control will be enabled during Add and Copy operations only
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Add AS CHARACTER NO-UNDO INIT "Add":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The Control will be enabled always regardless of the status
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Always AS CHARACTER NO-UNDO INIT "Always":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The Control will be enabled never regardless of the status
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Never AS CHARACTER NO-UNDO INIT "Never":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The Control will be enabled during Add, Copy and Update operations
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Update AS CHARACTER NO-UNDO INIT "Update":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The Control will be enabled during Update, buth not Add and Copy 
                 operations
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY UpdateOnly AS CHARACTER NO-UNDO INIT "UpdateOnly":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ControlEnabledEnum class
        Notes:   Disable instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ControlEnabledEnum ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
