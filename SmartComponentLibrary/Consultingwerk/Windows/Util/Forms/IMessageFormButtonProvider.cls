/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IMessageFormButtonProvider
    Purpose     : Interface for services that can provide custom Buttons
                  for the MessageForm  
    Syntax      : Allows adding buttons to report message or retrieve system 
                  information etc.
    Description : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+custom+buttons+on+the+MessageForm
                  for further instructions 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 06 14:27:31 CET 2014
    Notes       : The MessageForm assigns Location and Anchor. All other 
                  properties are to be assigned by the service implementation
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.*     FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.Windows.Util.Forms.IMessageFormButtonProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Buttons to be added to the dialog
        Notes:   Buttons should have a meaningful name
        @param poMessage The Message that is currently being processed
        @return The List of the System.Windows.Forms.Button objects to be added to the MessageForm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Button>" GetButtonsForMessage (poMessage AS Message).

    /*------------------------------------------------------------------------------
        Purpose: Handles the click event of a custom button
        Notes:   
        @param poButton The reference to the Button that was clicked
        @param poMessage The reference to the Message that is currently shown
        @return Logical value indicating if the Message Message Form should be closed (True = Close)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleButtonClick (poButton AS System.Windows.Forms.Button,
                                             poMessage AS Message).

END INTERFACE.
