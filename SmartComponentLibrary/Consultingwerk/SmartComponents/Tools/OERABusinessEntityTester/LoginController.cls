/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LoginController
    Purpose     : Helper Class for the Business Entity Tester and
                  Business Entity Browser that manages the "Authenticate" button
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 02 18:37:07 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Session.* FROM PROPATH.
USING Consultingwerk.Assertion.*                                      FROM PROPATH .
USING Consultingwerk.Framework.*                                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                             FROM ASSEMBLY.
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.LoginController:

    DEFINE PRIVATE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

    /**
     * Purpose: Constructor for the LoginController class
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager
     * @param poForm The form the login dialog should be parented to
     */
    CONSTRUCTOR PUBLIC LoginController (poToolbarsManager AS UltraToolbarsManager,
                                        poForm AS System.Windows.Forms.Form):

        DEFINE VARIABLE oAuthenticationService AS IAuthenticationService NO-UNDO .

        ObjectAssert:IsValid (poToolbarsManager, "poToolbarsManager":U) .
        ObjectAssert:IsValid (poForm, "poForm":U) .

        ASSIGN
            oAuthenticationService = {Consultingwerk/get-service.i Consultingwerk.Framework.IAuthenticationService}
            oForm                  = poForm .

        IF VALID-OBJECT (oAuthenticationService) THEN
            poToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .
        ELSE DO:
            IF poToolbarsManager:Tools:Exists ("Authenticate":U) THEN
                poToolbarsManager:Tools ["Authenticate":U]:SharedProps:Enabled = FALSE .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Authenticates the user using the current IAuthenticationService
     * Notes:
     *
     */
    METHOD PROTECTED VOID Authenticate():

        DEFINE VARIABLE lLoginOk AS LOGICAL NO-UNDO.

        SessionManager:Login (oForm, OUTPUT lLoginOk) .

    END METHOD.

    /**
     * Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
     * Notes:
     * @param sender The reference to the System.Object that raised the event
     * @param e The ToolClikcEventArgs with the data for this event
     */
    METHOD PRIVATE VOID ToolClickHandler (sender AS System.Object,
                                          e AS ToolClickEventArgs):

        IF e:Tool:Key = "Authenticate":U THEN
            THIS-OBJECT:Authenticate () .

    END METHOD.


END CLASS.
