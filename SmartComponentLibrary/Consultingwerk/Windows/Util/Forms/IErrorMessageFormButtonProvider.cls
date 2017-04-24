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
    File        : IErrorMessageFormButtonProvider
    Purpose     : Interface for services that can provide custom Buttons
                  for the ErrorMessageForm  
    Syntax      : Allows adding buttons to report errors etc.
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 10 14:27:31 CET 2014
    Notes       : The ErrorMessageForm assigns Location and Anchor. All other 
                  properties are to be assigned by the service implementation
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.Windows.Util.Forms.IErrorMessageFormButtonProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Buttons to be added to the dialog
        Notes:   Buttons should have a meaningful name
        @param poError The error that is currently being processed
        @return The List the System.Windows.Forms.Button objects to be added to the ErrorMessageForm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Button>" GetButtonsForError (poError AS Error).

    /*------------------------------------------------------------------------------
        Purpose: Handles the click event of a custom button
        Notes:   
        @param poButton The reference to the Button that was clicked
        @param poError The reference to the error that is currently shown
        @return Logical value indicating if the Error Message Form should be closed (True = Close)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleButtonClick (poButton AS System.Windows.Forms.Button,
                                             poError AS Error).

END INTERFACE.
