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
    File        : SampleErrorMessageFormButtonProvider
    Purpose     : Sample service that provides custom Buttons for the 
                  ErrorMessageForm  
    Syntax      : Allows adding buttons to report errors etc.
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 10 14:27:31 CET 2014
    Notes       : The ErrorMessageForm assigns Location and Anchor. All other 
                  properties are to be assigned by the service implementation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH . 
USING Consultingwerk.Util.*               FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Util.Forms.SampleErrorMessageFormButtonProvider 
    INHERITS ErrorMessageFormButtonProvider  
    IMPLEMENTS IErrorMessageFormButtonProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Buttons to be added to the dialog
        Notes:   Buttons should have a meaningful name
        @param poError The error that is currently being processed
        @return The List the System.Windows.Forms.Button objects to be added to the ErrorMessageForm
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC OVERRIDE "System.Collections.Generic.List<System.Windows.Forms.Button>" GetButtonsForError (poError AS Error):
		
		DEFINE VARIABLE oList   AS "System.Collections.Generic.List<System.Windows.Forms.Button>" NO-UNDO . 
		DEFINE VARIABLE oButton AS System.Windows.Forms.Button                                    NO-UNDO .                                   
		
		oList = SUPER:GetButtonsForError (poError) .
		
		IF FrameworkSettings:DebugMode THEN DO:
		    oButton = NEW System.Windows.Forms.Button () .
		    oButton:Name = "Kill":U .
		    oButton:Text = "Kill session"{&TRAN} . 

		    oList:Add (oButton) .
		END.

        oButton = NEW System.Windows.Forms.Button () .
        oButton:Name = "Copy":U .
        oButton:Text = "Copy"{&TRAN} . 
		
		oList:Add (oButton) .
		
		RETURN oList . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the click event of a custom button
        Notes:   
        @param poButton The reference to the Button that was clicked
        @param poError The reference to the error that is currently shown
        @return Logical value indicating if the Error Message Form should be closed (True = Close)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LOGICAL HandleButtonClick (poButton AS System.Windows.Forms.Button,
                                                      poError AS Error):
                           
        SUPER:HandleButtonClick (poButton, poError) .                           
                                             		
		CASE poButton:Name:
            WHEN "Kill":U THEN DO:
                IF MessageFormHelper:AskQuestion ("Are you sure?"{&TRAN}) = DialogResultEnum:DialogResultYes THEN 
                    System.Diagnostics.Process:GetCurrentProcess():Kill () .
            END.     
                
            WHEN "Copy":U THEN
                CLIPBOARD:VALUE = ErrorHelper:FormattedErrorMessagesExt (poError) .
        END CASE.

	END METHOD .

END CLASS.
