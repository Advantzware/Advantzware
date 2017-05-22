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
    File        : MDIContainer
    Purpose     : Base class for .NET MDI Container Forms
    Syntax      :
    Description : Manages the Text (Title) of the MDI Container form in a
                  way that it does also contain the Text (Title) of the
                  current active MDI Child
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 08 09:45:12 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .
USING Consultingwerk.Framework.IMDIContainer FROM PROPATH .
USING Consultingwerk.Forms.BaseForm FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Forms.MDIContainer
    INHERITS BaseForm
    IMPLEMENTS IMDIContainer:

    DEFINE PRIVATE VARIABLE cBaseText      AS CHARACTER NO-UNDO .
    DEFINE PRIVATE VARIABLE lUpdatingTitle AS LOGICAL   NO-UNDO INIT FALSE .

    /* Mike Fechner, Consultingwerk Ltd. 22.12.2009
       10.2B: Use of generic List<Form> instead of temp-table */
    DEFINE VARIABLE oFormsList AS "System.Collections.Generic.List<System.Windows.Forms.Form>":U NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MDIContainer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MDIContainer ():
        SUPER ().

        oFormsList = NEW "System.Collections.Generic.List<System.Windows.Forms.Form>":U () .

        THIS-OBJECT:MdiChildActivate:Subscribe (MdiChildActivateHandler) NO-ERROR .
        THIS-OBJECT:TextChanged:Subscribe (TextChangedHandler) NO-ERROR .

        cBaseText = THIS-OBJECT:Text .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activate MdiChildActivate and TextChanged event handlers. This
                 is required to circumvent issues (frozen app) that may occur while
                 changing the MdiContainer title (Text property) in the middle of
                 the process of embedding (before the embedded window has been
                 fully realized as an MDI child).
        Notes:   Future releases of OpenEdge (bug fixes) may enable us to remove
                 this method. It's not intended for public use!
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateWindowTitleEventHandler ():

        THIS-OBJECT:MdiChildActivate:Subscribe (MdiChildActivateHandler) NO-ERROR .
        THIS-OBJECT:TextChanged:Subscribe (TextChangedHandler) NO-ERROR .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deactivate MdiChildActivate and TextChanged event handlers. This
                 is required to circumvent issues (frozen app) that may occur while
                 changing the MdiContainer title (Text property) in the middle of
                 the process of embedding (before the embedded window has been
                 fully realized as an MDI child).
        Notes:   Future releases of OpenEdge (bug fixes) may enable us to remove
                 this method. It's not intended for public use!
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeactivateWindowTitleEventHandler ():

        THIS-OBJECT:MdiChildActivate:Unsubscribe (MdiChildActivateHandler) NO-ERROR .
        THIS-OBJECT:TextChanged:Unsubscribe (TextChangedHandler) NO-ERROR .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form.
        Notes:   Event handler for the Activate event of the MdiChilds
        @param sender System.Object that raised the Activate event
        @param e Default .NET event argument instance
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MdiChildActivateHandler (sender AS System.Object,
                                                 e AS System.EventArgs):

        DEFINE VARIABLE child AS System.Windows.Forms.Form NO-UNDO .

        UpdateTitleWithMdiChild () .

        IF VALID-OBJECT(THIS-OBJECT:ActiveMdiChild) THEN DO:
            /* Add FormClosed event handler to reset window title after close of
               last window */
            child = THIS-OBJECT:ActiveMdiChild .

            IF NOT oFormsList:Contains (child) THEN DO:
                oFormsList:Add (child) .

                THIS-OBJECT:ActiveMdiChild:FormClosed:Subscribe(MdiChildFormClosedHandler) NO-ERROR .
                THIS-OBJECT:ActiveMdiChild:TextChanged:Subscribe(MdiChildFormTextChangedHandler) NO-ERROR .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form.
        Notes:   Invokes the private MdiChildActiveHandler event handler method
                 with unknown values for both parameters.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MdiChildActivateHandler ():

        MdiChildActivateHandler (?, ?).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the MdiChild from the list of Forms
        Notes:   Event handler for the Closed event of the MdiChilds
        @param sender System.Object that raised the Closed event
        @param e .NET FormClosedEventArgs event argument instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID MdiChildFormClosedHandler (sender AS System.Object,
                                                     e AS System.Windows.Forms.FormClosedEventArgs):

        DEFINE VARIABLE child AS System.Windows.Forms.Form NO-UNDO .

        ASSIGN child = CAST(sender, System.Windows.Forms.Form) NO-ERROR .

        IF oFormsList:Contains (child) THEN
            oFormsList:Remove (child) .

        FINALLY:
            UpdateTitleWithMdiChild () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form.
        Notes:   Invoked when the Text property of the MDI Container is changed.
        @param sender System.Object that raised the TextChanged event
        @param e Default .NET EventArgs event argument instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TextChangedHandler (sender AS System.Object,
                                              e AS System.EventArgs):
        IF lUpdatingTitle THEN
            RETURN . /* avoid recursion */

        ASSIGN cBaseText = THIS-OBJECT:TEXT .

        UpdateTitleWithMdiChild () .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refresh MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form.
        Notes:   Invoked when the Text property of the MDI Child is changed .
        @param sender System.Object that raised the TextChanged event
        @param e Default .NET EventArgs event argument instance
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MdiChildFormTextChangedHandler (sender AS System.Object,
                                                        e AS System.EventArgs):

        UpdateTitleWithMdiChild () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Udpate MdiContainer Text (title bar) with Text from the current
                 active MdiChild Form and the original text of the Mdi Container.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateTitleWithMdiChild ():

        ASSIGN lUpdatingTitle = TRUE .

        THIS-OBJECT:TextChanged:Unsubscribe(TextChangedHandler) NO-ERROR .

        IF VALID-OBJECT(THIS-OBJECT:ActiveMdiChild) THEN
            THIS-OBJECT:Text = cBaseText + " <":U + THIS-OBJECT:ActiveMdiChild:Text + ">":U .
        ELSE
            THIS-OBJECT:Text = cBaseText .

        THIS-OBJECT:TextChanged:Subscribe(TextChangedHandler) NO-ERROR .

        FINALLY:
            ASSIGN lUpdatingTitle = FALSE .
        END FINALLY .

    END METHOD.

END CLASS.
