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
    File        : InputPromptHelper
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 17 20:35:38 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                    FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Windows.Forms.*              FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.InputPromptHelper:

    /**
     * Purpose: Constructor for the InputPromptHelper class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED InputPromptHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        RETURN InputPromptHelper:PromptForCharacterValue (pcTitle,
                                                          pcDescription,
                                                          ?,
                                                          CharacterPromptTypeEnum:TextBox,
                                                          INPUT-OUTPUT pcValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValidator The ICharacterInputValidator instance to validate the value entered by the user
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          poValidator AS ICharacterInputValidator,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        RETURN InputPromptHelper:PromptForCharacterValue (pcTitle,
                                                          pcDescription,
                                                          poValidator,
                                                          CharacterPromptTypeEnum:TextBox,
                                                          INPUT-OUTPUT pcValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poPromptType The prompt type (e.g. single line, editor, file name
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          poPromptType AS CharacterPromptTypeEnum,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        RETURN InputPromptHelper:PromptForCharacterValue (pcTitle,
                                                          pcDescription,
                                                          ?,
                                                          poPromptType,
                                                          INPUT-OUTPUT pcValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValidator The ICharacterInputValidator instance to validate the value entered by the user
     * @param poPromptType The prompt type (e.g. single line, editor, file name
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          poValidator AS ICharacterInputValidator,
                                                          poPromptType AS CharacterPromptTypeEnum,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        DEFINE VARIABLE oForm         AS CharacterInputPromptForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult             NO-UNDO .

        oForm = NEW CharacterInputPromptForm () .

        ASSIGN oForm:Title          = pcTitle
               oForm:Description    = pcDescription
               oForm:PromptType     = poPromptType
               oForm:InputValidator = poValidator
               oForm:CharacterValue = pcValue .

        InputPromptHelper:ShowDialog (oForm, OUTPUT oDialogResult) .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult FALSE} .

        ASSIGN pcValue = oForm:CharacterValue .

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog using
     *           a combo-box of possible values
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValueList A ListNameValuePair with the possible choices
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          poValueList AS ListNameValuePair,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        RETURN PromptForCharacterValue (pcTitle,
                                        pcDescription,
                                        poValueList,
                                        ?,
                                        INPUT-OUTPUT pcValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a CHARACTER Value using a modal dialog using
     *           a combo-box of possible values
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValueList A ListNameValuePair with the possible choices
     * @param poValidator The ICharacterInputValidator instance to validate the value entered by the user
     * @param pcValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForCharacterValue (pcTitle AS CHARACTER,
                                                          pcDescription AS CHARACTER,
                                                          poValueList AS ListNameValuePair,
                                                          poValidator AS ICharacterInputValidator,
                                                          INPUT-OUTPUT pcValue AS CHARACTER):

        DEFINE VARIABLE oForm         AS CharacterInputPromptForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult             NO-UNDO .

        oForm = NEW CharacterInputPromptForm () .

        ASSIGN oForm:Title          = pcTitle
               oForm:Description    = pcDescription
               oForm:ValueList      = poValueList
               oForm:CharacterValue = pcValue .

        InputPromptHelper:ShowDialog (oForm, OUTPUT oDialogResult) .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult FALSE} .

        ASSIGN pcValue = oForm:CharacterValue .

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a Integer Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param piValue INPUT-OUTPUT The Integer value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForIntegerValue (pcTitle AS CHARACTER,
                                                        pcDescription AS CHARACTER,
                                                        INPUT-OUTPUT piValue AS INTEGER):

        RETURN InputPromptHelper:PromptForIntegerValue (pcTitle,
                                                        pcDescription,
                                                        ?,
                                                        NumericPromptTypeEnum:TextBox,
                                                        ?,
                                                        ?,
                                                        INPUT-OUTPUT piValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a Integer Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValidator The IIntegerInputValidator instance to validate the value entered by the user
     * @param piValue INPUT-OUTPUT The Integer value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForIntegerValue (pcTitle AS CHARACTER,
                                                        pcDescription AS CHARACTER,
                                                        poValidator AS IIntegerInputValidator,
                                                        INPUT-OUTPUT piValue AS INTEGER):

        RETURN InputPromptHelper:PromptForIntegerValue (pcTitle,
                                                        pcDescription,
                                                        poValidator,
                                                        NumericPromptTypeEnum:TextBox,
                                                        ?,
                                                        ?,
                                                        INPUT-OUTPUT piValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a Integer Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poPromptType The prompt type (e.g. single line, editor, file name
     * @param piValue INPUT-OUTPUT The Integer value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForIntegerValue (pcTitle AS CHARACTER,
                                                        pcDescription AS CHARACTER,
                                                        poPromptType AS NumericPromptTypeEnum,
                                                        INPUT-OUTPUT piValue AS INTEGER):

        RETURN InputPromptHelper:PromptForIntegerValue (pcTitle,
                                                        pcDescription,
                                                        ?,
                                                        poPromptType,
                                                        ?,
                                                        ?,
                                                        INPUT-OUTPUT piValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a Integer Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValidator The ICharacterInputValidator instance to validate the value entered by the user
     * @param poPromptType The prompt type (e.g. single line, editor, file name
     * @param piValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForIntegerValue (pcTitle AS CHARACTER,
                                                        pcDescription AS CHARACTER,
                                                        poValidator AS IIntegerInputValidator,
                                                        poPromptType AS NumericPromptTypeEnum,
                                                        INPUT-OUTPUT piValue AS INTEGER):

        RETURN InputPromptHelper:PromptForIntegerValue (pcTitle,
                                                        pcDescription,
                                                        poValidator,
                                                        poPromptType,
                                                        ?,
                                                        ?,
                                                        INPUT-OUTPUT piValue) .

    END METHOD.

    /**
     * Purpose:  Prompts the user for a Integer Value using a modal dialog
     * Notes:
     * @param pcTitle The title for the input prompt
     * @param pcDescription The description shown in the Input prompt
     * @param poValidator The ICharacterInputValidator instance to validate the value entered by the user
     * @param poPromptType The prompt type (e.g. single line, editor, file name
     * @param piMinimumValue The minimum value that is acceptable
     * @param piMaximumValue The maximum value that is acceptable
     * @param piValue INPUT-OUTPUT The Character value entered
     * @return Logical value indicating if the user has choosen Ok or Cancel
     */
    METHOD PUBLIC STATIC LOGICAL PromptForIntegerValue (pcTitle AS CHARACTER,
                                                        pcDescription AS CHARACTER,
                                                        poValidator AS IIntegerInputValidator,
                                                        poPromptType AS NumericPromptTypeEnum,
                                                        piMinimumValue AS INTEGER,
                                                        piMaximumValue AS INTEGER,
                                                        INPUT-OUTPUT piValue AS INTEGER):

        DEFINE VARIABLE oForm         AS NumericInputPromptForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult           NO-UNDO .

        oForm = NEW NumericInputPromptForm () .

        ASSIGN oForm:Title                 = pcTitle
               oForm:Description           = pcDescription
               oForm:IntegerInputValidator = poValidator
               oForm:PromptType            = poPromptType
               oForm:MaximumValue          = piMaximumValue
               oForm:MinimumValue          = piMinimumValue
               oForm:NumericType           = NumericTypeEnum:Integer
               oForm:IntegerValue          = piValue .

        InputPromptHelper:ShowDialog (oForm, OUTPUT oDialogResult) .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult FALSE} .

        ASSIGN piValue = oForm:IntegerValue .

        RETURN TRUE .


    END METHOD.

    /**
     * Purpose: Invokes the dialog form using WAIT-FOR :ShowDialog ()
     * Notes:   Seperate method that returns a void value to allow compilation when
     *          -IOEverywhere 1 is not set. However for the runtime -IOEverywhere 1
     *          is retured
     * @param poForm The dialog form to show
     * @param poDialogResult OUTPUT The DialogResult of the current Form
     */
    METHOD PROTECTED STATIC VOID ShowDialog (poForm AS System.Windows.Forms.Form,
                                             OUTPUT poDialogResult AS System.Windows.Forms.DialogResult):

        WAIT-FOR poForm:ShowDialog () SET poDialogResult .

    END METHOD.

&ENDIF
END CLASS.
