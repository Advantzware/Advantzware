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
    File        : TextInputPrompt
    Purpose     : Prompt for a Character value
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 10 12:03:43 CEST 2008
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Util.Forms.* .
USING Progress.Lang.* .
USING System.Windows.Forms.* .

CLASS Consultingwerk.Windows.Util.Forms.TextInputPrompt
    INHERITS TextInputPromptDesigner:

    DEFINE VARIABLE oFullWindowDragManager AS FullWindowDragManager NO-UNDO EXTENT 2 .

    /*------------------------------------------------------------------------------
        Purpose: Constrcutor of the TextInputPromt class.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TextInputPrompt ():
        SUPER ().

        THIS-OBJECT:edText:TextChanged:Subscribe (EditorTextChanged) .
        THIS-OBJECT:Shown:Subscribe (FormShown) .

        ASSIGN oFullWindowDragManager[1] = NEW FullWindowDragManager (THIS-OBJECT)
               oFullWindowDragManager[2] = NEW FullWindowDragManager (THIS-OBJECT:ultraGroupBox1) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the last Response
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Response AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the last DialogResult
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Result AS DialogResult NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the Editor control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID EditorTextChanged (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE iLength AS INTEGER NO-UNDO.

        iLength = LENGTH(THIS-OBJECT:edText:Text) .

        IF iLength > 0 THEN buOk:ENABLED = TRUE .
        ELSE buOk:ENABLED = FALSE .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handler for the Shown event of the Form
        Notes:   Set's focus into the text editor edText
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID FormShown (sender AS System.Object, e AS System.EventArgs):

        THIS-OBJECT:edText:Focus () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prompts the user for a character value
        Notes:
        @param pcTitle The Dialog Title
        @param pcDescription The Description of the prompt
        @param pcDefault The default value
        @param poParent The dialog parent form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Input (pcTitle AS CHARACTER,
                                     pcDescription AS CHARACTER,
                                     pcDefault AS CHARACTER,
                                     poParent AS System.Windows.Forms.Form):

        DEFINE VARIABLE frmInput AS TextInputPrompt NO-UNDO .

        frmInput = NEW TextInputPrompt () .
        frmInput:DeleteOnFormClosed = FALSE .

        IF VALID-OBJECT(poParent) THEN DO:
            frmInput:PARENT = poParent .
            frmInput:StartPosition = FormStartPosition:CenterParent .
        END.
        ELSE frmInput:StartPosition = FormStartPosition:CenterScreen .

        frmInput:TEXT               = pcTitle .
        frmInput:fiTitle:Text       = pcTitle  .
        frmInput:fiDescription:Text = pcDescription .
        frmInput:edText:Text        = pcDefault .

        WAIT-FOR frmInput:ShowDialog() .

        TextInputPrompt:Result = frmInput:DialogResult .
        TextInputPrompt:Response = frmInput:edText:Text .

        DELETE OBJECT frmInput .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prompts the user for a character value
        Notes:
        @param pcTitle The Dialog Title
        @param pcDescription The Description of the prompt
        @param poParent The dialog parent form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Input (pcTitle AS CHARACTER,
                                     pcDescription AS CHARACTER,
                                     poParent AS System.Windows.Forms.Form):

       TextInputPrompt:Input (pcTitle, pcDescription, "":U, poParent) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prompts the user for a character value
        Notes:
        @param pcTitle The Dialog Title
        @param pcDescription The Description of the prompt
        @param pcDefault The default value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Input (pcTitle AS CHARACTER,
                                     pcDescription AS CHARACTER,
                                     pcDefault AS CHARACTER):

       TextInputPrompt:Input (pcTitle, pcDescription, pcDefault, ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prompts the user for a character value
        Notes:
        @param pcTitle The Dialog Title
        @param pcDescription The Description of the prompt
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Input (pcTitle AS CHARACTER, pcDescription AS CHARACTER):

       TextInputPrompt:Input (pcTitle, pcDescription, "":U, ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destrcutor of the TextInputPromt class.
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC TextInputPrompt ():

        /* Mike Fechner, Consultingwerk Ltd. 05.08.2014
           SCL-392: Workaround to issue in OE11.4 with calling into
                    GarbageCollectorHelper:DeleteObject(Object[])  */
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[1]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[2]) .

    END DESTRUCTOR.

END CLASS.
