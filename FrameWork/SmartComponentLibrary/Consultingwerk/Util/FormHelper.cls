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
    File        : FormHelper
    Purpose     :
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description : Methods which helps to Handle Forms
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 23 14:39:56 CEST 2008
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Forms.*     FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Progress.Util.*            FROM ASSEMBLY .
USING Progress.Windows.*         FROM ASSEMBLY .
USING System.Windows.Forms.*     FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.FormHelper   :

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor.
        Notes:   There's no need to create instances of the helper classes
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED FormHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN

    /*------------------------------------------------------------------------------
        Purpose: Activates a Form by un-Minimizing and Activating it
        Notes:
        @param poForm The Progress.Windows.Form to Activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ActivateForm (poForm AS Progress.Windows.Form):

        IF TYPE-OF (poForm, BaseForm) THEN
            IF EnumHelper:AreEqual (CAST (poForm, BaseForm):PreviousWindowState,
                                    FormWindowState:Minimized) THEN
                poForm:WindowState = CAST (poForm, BaseForm):PreviousWindowState .

&IF PROVERSION EQ "11.0" OR PROVERSION = "10.2B" &THEN
        ELSE DO:
            IF EnumHelper:AreEqual (poForm:WindowState,
                                    FormWindowState:Minimized) THEN
                poForm:WindowState = poForm:previousState .
        END.
&ENDIF

        poForm:Activate() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds the active child control in a Form
        Notes:
        @param poForm The reference to the Form
        @return The reference to the active Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Control FindActiveChildControl (poForm AS System.Windows.Forms.Form):

        DEFINE VARIABLE oControl AS Control NO-UNDO .

        oControl = poForm:ActiveControl .

        DO WHILE VALID-OBJECT (oControl) AND TYPE-OF (oControl, ContainerControl):

            IF VALID-OBJECT (CAST (oControl, ContainerControl):ActiveControl) THEN
                oControl = CAST (oControl, ContainerControl):ActiveControl .
            ELSE
                LEAVE .
        END.

        RETURN oControl.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Seeks the Control hierarchie up for a Parent of the given type
        Notes:
        @param poControl The reference to the Control to return the Parent for
        @param poType The reference to the type of the parent control to return
        @return The reference to the parent control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Control FindParentOfType (poControl AS System.Windows.Forms.Control,
                                                                        poType AS System.Type):

        DEFINE VARIABLE oParent AS System.Windows.Forms.Control NO-UNDO .

        ASSIGN oParent = poControl:Parent .

        DO WHILE VALID-OBJECT (oParent):
            IF poType:IsInstanceOfType (oParent) THEN
              RETURN oParent .

            ASSIGN oParent = oParent:Parent.
        END.

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a running instance of a Form Class in the SESSION:FIRST-OBJECT
                 chain of objects
        Notes:   Used only as a workaround, when iterating the SESSION:FIRST-FORM
                 chain of Progress.Windows.Form objects fails with error 15580.
        @param poClass The Form's class reference (Progress.Lang.Class instance)
        @return The reference to the running Form
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE STATIC Progress.Windows.Form FindRunningFormInObjects (poClass AS Progress.Lang.Class):

        DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO .

        ASSIGN oObject = SESSION:FIRST-OBJECT .

        DO WHILE VALID-OBJECT (oObject):
            IF TYPE-OF (oObject, Progress.Windows.Form) THEN DO:

                IF oObject:GetClass():IsA (poClass) THEN
                    RETURN CAST (oObject, Progress.Windows.Form) .
            END.

            oObject = oObject:NEXT-SIBLING .
        END.

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error:
            /* return any error, return ? */
            RETURN ? .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a running instance of a Form Class in the SESSION:FIRST-FORM
                 chain of Progress.Windows.Form objects
        Notes:
        @param pcFormType The Form's fully qualified class name
        @return The reference to the running Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Progress.Windows.Form FindRunningFormInstance (pcFormType AS CHARACTER):

        DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO .

        oClass = Progress.Lang.Class:GetClass (pcFormType) NO-ERROR .

        IF NOT VALID-OBJECT (oClass) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid class reference for &1."{&TRAN}, pcFormType),
                                      0) .

        RETURN FormHelper:FindRunningFormInstance (oClass) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a running instance of a Form Class in the SESSION:FIRST-FORM
                 chain of Progress.Windows.Form objects
        Notes:
        @param poClass The Form's class reference (Progress.Lang.Class instance)
        @return The reference to the running Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Progress.Windows.Form FindRunningFormInstance (poClass AS Progress.Lang.Class):

        DEFINE VARIABLE oIForm AS Progress.Windows.IForm NO-UNDO .
        DEFINE VARIABLE oForm  AS Progress.Windows.Form NO-UNDO .

        IF NOT VALID-OBJECT (poClass) THEN
            UNDO, THROW NEW AppError ("Invalid class reference for FindRunningFormInstance."{&TRAN}, 0) .

        ASSIGN oIForm = SESSION:FIRST-FORM .

        DO WHILE VALID-OBJECT (oIForm) ON ERROR UNDO, THROW:
            IF TYPE-OF (oIForm, Progress.Windows.Form) THEN DO:
                oForm = CAST (oIForm, Progress.Windows.Form) .

                IF oForm:GetClass():IsA (poClass) THEN
                    RETURN oForm .
            END.

&IF DEFINED (RetryFormLaunchingOnProgressLangSysError) NE 0 &THEN
            DO ON ERROR UNDO, THROW:
&ENDIF
                oIForm = oIForm:NextForm .

&IF DEFINED (RetryFormLaunchingOnProgressLangSysError) NE 0 &THEN
                /* Mike Fechner / Consultingwerk Ltd.

                   Workaround for error System.InvalidOperationException:
                   Attempt to use an object that has been explicitly deleted (15580)

                   that may be raised after a Form could initially not be started due
                   to OE11.1, OE11.2, OE11.3 issues with mix of hybrid classes and ABL
                   static members or overridden On... .NET event methods. When those
                   errors have been raised it appears that the ABL class representation
                   as a .NET Type is in an inconsistent state (until launched again)
                   and the oForm = oForm:NextForm raises the error even though the
                   object passed the DO WHILE VALID-OBJECT ....  */
                CATCH err AS Progress.Lang.Error:
                    /* Mike Fechner, Consultingwerk Ltd. 15.02.2014
                       Error may also be raised without error number, but 15580 in the error text */
                    IF err:GetMessageNum (1) = 15580 OR err:GetMessage(1) MATCHES "* (15580)*":U THEN DO:
                        RETURN FormHelper:FindRunningFormInObjects (poClass) .
                    END.
                    ELSE
                        UNDO, THROW err .
                END CATCH.
            END.
&ENDIF
        END.

        RETURN ? .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Getting the Screen Object of a Point indentificated by
                 piTop and piLeft.
        Notes:
        @param piTop The top coordinate of the point
        @param piLeft The left coordinate of the point
        @return The reference to the Screen the point is located on
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Windows.Forms.Screen GetScreenOfPoint (piTop AS INTEGER,
                                                                       piLeft AS INTEGER):

        DEFINE VARIABLE oScreenOfPoint AS System.Windows.Forms.Screen NO-UNDO.

        DEFINE VARIABLE i              AS INTEGER                     NO-UNDO INITIAL 1.
        DEFINE VARIABLE iUpperBound    AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE lDisplayFound  AS LOGICAL                     NO-UNDO INITIAL FALSE .

        DEFINE VARIABLE oScreens       AS System.Windows.Forms.Screen NO-UNDO EXTENT .

        /* Gets an array of all the screens connected to the system. */
        ASSIGN oScreens       = System.Windows.Forms.Screen:AllScreens
               iUpperBound    = EXTENT(oScreens)
               oScreenOfPoint = ?.

        REPEAT WHILE i <= iUpperBound AND
                NOT lDisplayFound ON ERROR UNDO, THROW:

            /* Marko Rüterbories, Consultingwerk Ltd. 03.01.2011
               Bug 2091: FormHelper:IsFormOnValidDisplay does not work with Windows 7
                         left/right docked Forms
               Changed the comparisons to test for less or equal and greater or equal */
            IF ((oScreens[i]:Bounds:Y <= piTop) AND
                   (oScreens[i]:Bounds:Y + oScreens[i]:Bounds:Height >= piTop)) THEN
            DO:
                IF ((oScreens[i]:Bounds:X <= piLeft) AND
                   (oScreens[i]:Bounds:X + oScreens[i]:Bounds:Width >= piLeft)) THEN
                DO:
                    /* The top left Point is on the Screen */
                    ASSIGN lDisplayFound  = TRUE
                           oScreenOfPoint = oScreens[i] .
                    LEAVE.
                END.
            END.

            ASSIGN i = i + 1.
        END.

        RETURN oScreenOfPoint.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines if a Form is active
        Notes:   For MDI Child Forms, this is only True, when the Form and the MDI
                 Container Form are Active
        @param poForm The reference to the Form
        @return Logical value indicating if the Form is active
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsActive (poForm AS System.Windows.Forms.Form):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poForm """Form"":U"} .

        IF VALID-OBJECT (poForm:MdiParent) THEN DO:

            IF System.Windows.Forms.Form:ActiveForm = poForm:MdiParent AND
               poForm:MdiParent:ActiveMdiChild      = poForm THEN

                RETURN TRUE .
            ELSE
                RETURN FALSE .
        END.
        ELSE
            RETURN System.Windows.Forms.Form:ActiveForm = poForm .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Test whether the given Point indentificated by piTop and
                 piLeft is on a valid Display.
        Notes:
        @param piTop The top coordinate of the point
        @param piLeft The left coordinate of the point
        @return Logical value indicating if the Form is on a valid display
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsFormOnValidDisplay (piTop AS INTEGER,
                                                       piLeft AS INTEGER):

        IF VALID-OBJECT (GetScreenOfPoint(piTop, piLeft)) THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Test whether the given Point indentificated by piTop and
                 piLeft is on a valid Display. If these Points are on a valid
                 Display but the right side is not on a valid one. The Form
                 is repositioned by setting new values for piTop and piLeft.
        Notes: If these Points are on a valid Display but the right side is not on a
               valid one. The Form is repositioned by setting new values for piTop and piLeft.
        @param piTop The top coordinate of the point, INPUT-OUTPUT as it may require to move the Form when it's to large
        @param piLeft The left coordinate of the point, INPUT-OUTPUT as it may require to move the Form when it's to large
        @param piWidth The width of the Form
        @param piHeight The height of the Form
        @return Logical value indicating if the Form is on a valid display
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsFormOnValidDisplay (INPUT-OUTPUT piTop    AS INTEGER,
                                                       INPUT-OUTPUT piLeft   AS INTEGER,
                                                       INPUT piWidth  AS INTEGER,
                                                       INPUT piHeight AS INTEGER):

        DEFINE VARIABLE iRight   AS INTEGER NO-UNDO.
        DEFINE VARIABLE iBottom  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iNewTop  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iNewLeft AS INTEGER NO-UNDO.

        DEFINE VARIABLE oScreen AS System.Windows.Forms.Screen  NO-UNDO.

        ASSIGN oScreen = GetScreenOfPoint(piTop, piLeft).

        IF VALID-OBJECT (oScreen) THEN DO:

            ASSIGN iRight   = piLeft + piWidth
                   iBottom  = piTop + piHeight
                   iNewTop  = piTop
                   iNewLeft = piLeft .

            /*If all parts of the Form are on valid displays nothing is to be done.*/
            IF ((GetScreenOfPoint(piTop, iRight) = ?) AND (GetScreenOfPoint(iBottom, iRight) = ?)) THEN DO:
                IF (oScreen:Bounds:Y + oScreen:Bounds:Height < iBottom) THEN
                DO:
                    ASSIGN iNewTop = oScreen:Bounds:Y + oScreen:Bounds:Height - piHeight .
                END.

                IF (oScreen:Bounds:X + oScreen:Bounds:Width < iRight) THEN
                DO:
                    ASSIGN iNewLeft = oScreen:Bounds:X + oScreen:Bounds:Width - piWidth .
                END.

                /*If the Form does not fit onto the display on which the TopLeft       */
                /*point is located nothing is to be done.                              */
                IF GetScreenOfPoint(iNewTop, iNewLeft) = oScreen THEN DO:
                    ASSIGN piTop  = iNewTop
                           piLeft = iNewLeft.
                END.
            END.

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows a Form as a dialog
        Notes:   Useful to circumvent missing -IOEverywhere 1 parameters during compile
                 time
        @param poForm The System.Windows.Forms.Form to show
        @param poDialogResult OUTPUT The DialogResult of the Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowDialog (poForm AS System.Windows.Forms.Form,
                                          OUTPUT poDialogResult AS DialogResult):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poForm} .

        WAIT-FOR poForm:ShowDialog () SET poDialogResult .

    END METHOD .
&ENDIF

END CLASS.
