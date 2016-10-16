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
    File        : WidgetHelper
    Purpose     :
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner, Consultingwerk Ltd.
    Created     : Tue Dec 15 06:22:09 CET 2009
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*              FROM PROPATH .
USING Consultingwerk.Assertion.*    FROM PROPATH .
USING Consultingwerk.Util.*         FROM PROPATH .
USING Consultingwerk.Util.Enum.*    FROM PROPATH .
USING Progress.Lang.*               FROM PROPATH .

CLASS Consultingwerk.Util.WidgetHelper:

    {Consultingwerk/Util/TempTables/ttWidgetsByLocation.i}

&IF DEFINED (DotNetAccessible) NE 0 AND DEFINED (Infragistics) NE 0 &THEN
    DEFINE STATIC PRIVATE VARIABLE oActivePopupControlContainer AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO .

&IF DEFINED (WinKit) NE 0 &THEN
    DEFINE PRIVATE STATIC VARIABLE oReplacedWidgetCustomizer AS Consultingwerk.WindowIntegrationKit.IReplacedWidgetCustomizer NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oSetBackgroundColorFilter AS Consultingwerk.WindowIntegrationKit.ISetBackgroundColorFilter NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oClickHandler             AS Consultingwerk.WindowIntegrationKit.IButtonClickHandler       NO-UNDO .
&ENDIF
&ENDIF

    /**
     * Purpose: Protected default constructor.
     * Notes:   There's no need to create instances of the helper classes
     */
    CONSTRUCTOR PROTECTED WidgetHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the static instance of the WidgetHelper
     * Notes:
     */
    CONSTRUCTOR STATIC WidgetHelper ():

&IF DEFINED (WinKit) NE 0 &THEN
        oReplacedWidgetCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IReplacedWidgetCustomizer} .
        oSetBackgroundColorFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.ISetBackgroundColorFilter} .
        oClickHandler = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IButtonClickHandler} .
&ENDIF

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the absolute position of a Widget in a Window
     * Notes:   Traverses nested frames (i.e ADM1, ADM2)
     * @param phWidget The handle of the Widget
     * @return The Consultingwerk.Point that represents the Widget position
     */
    METHOD PUBLIC STATIC Consultingwerk.Point AbsolutePositionOnWindow (phWidget AS HANDLE):

        DEFINE VARIABLE iX AS INTEGER NO-UNDO.
        DEFINE VARIABLE iY AS INTEGER NO-UNDO.

        WidgetHelper:AbsolutePositionOnWindow (phWidget, OUTPUT iX, OUTPUT iY) .

        RETURN NEW Consultingwerk.Point (iX, iY) .

    END METHOD .

    /**
     * Purpose: Returns the absolute position of a Widget in a Window
     * Notes:   Traverses nested frames (i.e ADM1, ADM2)
     * @param phWidget The handle of the Widget
     * @param pX OUTPUT The absolute X position of the widget
     * @param pY OUTPUT The absolute Y position of the widget
     */
    METHOD PUBLIC STATIC VOID AbsolutePositionOnWindow (phWidget AS HANDLE,
                                                        OUTPUT pX AS INTEGER,
                                                        OUTPUT pY AS INTEGER):

        DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

        ASSIGN hContainer = phWidget:PARENT

               pX = phWidget:X
               pY = phWidget:Y .

        DO WHILE VALID-HANDLE (hContainer):
            IF hContainer:TYPE = "WINDOW":U THEN LEAVE .

            ASSIGN pX = pX + hContainer:X
                   pY = pY + hContainer:Y .

            /* Mike Fechner, Consultingwerk Ltd. 11.11.2010
               Add PIXELS for Frame Title */
            IF hContainer:TYPE = "FRAME":U AND hContainer:TITLE > "":U THEN
                ASSIGN pY = pY + SESSION:PIXELS-PER-ROW - 1.

            ASSIGN hContainer = hContainer:PARENT .
        END.

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 AND DEFINED (Infragistics) NE 0 &THEN
    /**
     * Purpose: Overlays widgets of the given type with .NET Controls of the
     *          given type (i.e. BUTTON Widget -> UltraButton Control).
     * Notes:
     * @param phParentFieldGroup The field group of the frame the contains DATA FILL-IN widgets
     * @param poParentControl The .NET Control that should become the parent of the popup buttons (parent of the WindowContainer)
     * @param piRowOffset The offset in pixels from the top for placing the popup buttons (see EmbeddedWindowForm for details)
     * @param piColOffset The offset in pixels from the left for placing the popup buttons (see EmbeddedWindowForm for details)
     */
    METHOD PUBLIC STATIC VOID AddCalendarPopups (phParentFieldGroup AS HANDLE,
                                                 poParentControl    AS System.Windows.Forms.Control,
                                                 piRowOffset        AS INTEGER,
                                                 piColOffset        AS INTEGER):

        DEFINE VARIABLE hWidget     AS HANDLE                                               NO-UNDO.
        DEFINE VARIABLE oButton     AS Infragistics.Win.Misc.UltraButton                    NO-UNDO .

        DEFINE VARIABLE deX AS INTEGER NO-UNDO.
        DEFINE VARIABLE deY AS INTEGER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 10.06.2011
           Allow passing of FRAME */
        IF phParentFieldGroup:TYPE = "FRAME":U THEN
            ASSIGN phParentFieldGroup = phParentFieldGroup:FIRST-CHILD .

        IF NOT VALID-HANDLE (phParentFieldGroup) THEN
            RETURN .

        Consultingwerk.Util.WidgetHelper:AbsolutePositionOnWindow (phParentFieldGroup,
                                                                   OUTPUT deX,
                                                                   OUTPUT deY) .

        ASSIGN hWidget = phParentFieldGroup:FIRST-CHILD  .

        DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:

            IF hWidget:TYPE = "FRAME":U THEN DO ON ERROR UNDO, THROW:
                /* Mike Fechner, Consultingwerk Ltd. 10.06.2011
                   Handle nested frames */
                IF VALID-HANDLE (hWidget:FIRST-CHILD) THEN
                    Consultingwerk.Util.WidgetHelper:AddCalendarPopups (hWidget:FIRST-CHILD,
                                                                        poParentControl,
                                                                        piRowOffset,
                                                                        piColOffset) .
                NEXT.
            END.

            IF hWidget:TYPE <> "FILL-IN":U OR hWidget:DATA-TYPE <> "DATE":U OR hWidget:READ-ONLY THEN
                NEXT .

            oButton = NEW Infragistics.Win.Misc.UltraButton () .

            oButton:Tag = STRING (hWidget) .

            oButton:SIZE = NEW System.Drawing.Size (21, hWidget:HEIGHT-PIXELS) .
            oButton:Location = NEW System.Drawing.Point (hWidget:X + deX - piColOffset + hWidget:WIDTH-PIXELS,
                                                          hWidget:Y + deY - piRowOffset) .
            oButton:NAME = "CalendarPopup":U + hWidget:NAME .
            oButton:ENABLED = hWidget:SENSITIVE .
            oButton:VISIBLE = NOT hWidget:HIDDEN .

            oButton:TEXT = "...":U .

            oButton:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton .

            oButton:Click:Subscribe (Consultingwerk.Util.WidgetHelper:DatePopupClickHandler) .

            poParentControl:Controls:Add (oButton) .

            oButton:BringToFront() .

            FINALLY:
                IF VALID-HANDLE (hWidget) THEN
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
            END FINALLY.
        END.

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns true when a widget of the provided type existis in the frame
     * Notes:   Does not recursively process nested frames
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @param pcWidgetTypeList The List of WIDGET types to search for
     * @return True when a Widget was found, otherwise False
     */
    METHOD PUBLIC STATIC LOGICAL CanFindWidgetOfType (phParent AS HANDLE,
                                                      pcWidgetTypeList AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        IF phParent:TYPE <> "FIELD-GROUP":U THEN
            ASSIGN phParent = WidgetHelper:FindChildWidgetOfType (phParent, "FIELD-GROUP":U) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF LOOKUP (hWidget:TYPE, pcWidgetTypeList) > 0 THEN
              RETURN TRUE .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Returns true when a widget existis below the given pixel (row)
     * Notes:   Useful for knowing if a Browse widget is the bottom-most widget or not
     *          Does not recursively process nested frames
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @param piPixels The minumum number of pixels form the top that a widget is search at
     * @return True when a Widget was found, otherwise False
     */
    METHOD PUBLIC STATIC LOGICAL CanFindWidgetsBelowHeight (phParent AS HANDLE,
                                                            piPixels AS INTEGER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        IF phParent:TYPE <> "FIELD-GROUP":U THEN
            ASSIGN phParent = WidgetHelper:FindChildWidgetOfType (phParent, "FIELD-GROUP":U) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:Y > piPixels THEN
              RETURN TRUE .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN FALSE .
    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 AND DEFINED (Infragistics) NE 0 &THEN
    /**
     * Purpose: Event handler for the Click event of UltraButton Controls created
     *          by ReplaceWidget
     * Notes:
     * @param sender The System.Object that raised this event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE STATIC VOID ClickHandler (sender AS System.Object,
                                             e AS System.EventArgs):

        DEFINE VARIABLE hWidget AS WIDGET-HANDLE NO-UNDO.

        ASSIGN hWidget = WIDGET-HANDLE (ENTRY (1, UNBOX(CAST (sender, System.Windows.Forms.Control):Tag))) .

        IF VALID-HANDLE (hWidget) THEN DO:

&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oClickHandler) THEN
                IF oClickHandler:HandleClickEvent(CAST (sender, System.Windows.Forms.Control),
                                                  hWidget) = TRUE THEN
                    RETURN .
&ENDIF

            IF hWidget:NO-FOCUS OR ENTRY (2, UNBOX(CAST (sender, System.Windows.Forms.Control):Tag)) = "no":U THEN
                APPLY "CHOOSE":U TO hWidget .
            ELSE
                Consultingwerk.Windows.API.Win32:PostMessage (hWidget:HWND,
                                                              Consultingwerk.Windows.API.WinUser:BM_CLICK,
                                                              0,
                                                              0) .
       END.

    END METHOD.

    /**
     * Purpose: Closes the active PopupControlContainer component
     * Notes:
     */
    METHOD PROTECTED STATIC VOID ClosePopup ():

        oActivePopupControlContainer:Close() .

        oActivePopupControlContainer:Dispose () .
        DELETE OBJECT oActivePopupControlContainer NO-ERROR .

    END METHOD.

    /**
     * Purpose: Returns the number of widget handles in the given widget chain
     * Notes:   Counts the passed in widget handle (when valid) and it's siblings.
     *          Helpful to count from the SESSION:FIRST-... references
     * @param phWidget The handle of the first widget in the widget chain
     * @return The number of widgets in the widget chain
     */
    METHOD PUBLIC STATIC INTEGER Count (phWidget AS HANDLE):

        DEFINE VARIABLE iCount AS INTEGER NO-UNDO INIT 0 .

        DO WHILE VALID-HANDLE (phWidget):

            ASSIGN iCount   = iCount + 1
                   phWidget = phWidget:NEXT-SIBLING .
        END.

        RETURN iCount .

    END METHOD.

    /**
     * Purpose: Event handler for the Leave event of the Date Popup's
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE STATIC VOID DateLeaveHandler (sender AS System.Object,
                                                 e AS System.EventArgs):

        ClosePopup () .

    END METHOD.

    /**
     * Purpose: Opens the date picker popup
     * Notes:
     * @param poControl The reference to the Popup Button Control to open the popup for
     */
    METHOD PUBLIC STATIC VOID DatePopup (poControl AS System.Windows.Forms.Control):

        WidgetHelper:DatePopupClickHandler (poControl, System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Event handler for the Click handler of the DatePopup Buttons
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE STATIC VOID DatePopupClickHandler (sender AS System.Object,
                                                      e AS System.EventArgs):

        &IF DEFINED (WinKit) NE 0 &THEN
        DEFINE VARIABLE oPopupControlContainer AS Infragistics.Win.Misc.UltraPopupControlContainer               NO-UNDO .
        DEFINE VARIABLE oCalendar              AS Consultingwerk.WindowIntegrationKit.Controls.DatePickerControl NO-UNDO .
        DEFINE VARIABLE hWidget                AS WIDGET-HANDLE                                                  NO-UNDO .
        DEFINE VARIABLE oForm                  AS System.Windows.Forms.Form                                      NO-UNDO .
        DEFINE VARIABLE oControl               AS System.Windows.Forms.Control                                   NO-UNDO .
        DEFINE VARIABLE oPoint                 AS System.Drawing.Point                                           NO-UNDO .

        ASSIGN hWidget = WIDGET-HANDLE (ENTRY (1, UNBOX(CAST (sender, System.Windows.Forms.Control):Tag))) .

        oControl = CAST (sender, System.Windows.Forms.Control) .
        oForm = oControl:FindForm () .

        oPoint = oControl:Parent:PointToScreen (CAST (sender, System.Windows.Forms.Control):Location) .
        oPoint:Y = oPoint:Y + oControl:HEIGHT + 2.
        oPoint:X = oPoint:X - hWidget:WIDTH-PIXELS .

        oCalendar = NEW Consultingwerk.WindowIntegrationKit.Controls.DatePickerControl () .
        oCalendar:WidgetHandle = hWidget .
        oCalendar:Date = hWidget:INPUT-VALUE .

        oPopupControlContainer = NEW Infragistics.Win.Misc.UltraPopupControlContainer () .
        oPopupControlContainer:PopupControl = oCalendar .
        oPopupControlContainer:Show(oPoint) .
        oCalendar:Focus() .
        oCalendar:Leave:Subscribe (DateLeaveHandler) .
        oCalendar:DateChanged:Subscribe (DatePopupDateChangedHandler) .

        oActivePopupControlContainer = oPopupControlContainer .

        &ELSE
        UNDO, THROW NEW AppError ("Not supported without Winkit.", 0) .
        &ENDIF

    END METHOD.

    /**
     * Purpose: Event handler for the Changed event of the Date Popup Control
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE STATIC VOID DatePopupDateChangedHandler (sender AS System.Object,
                                                            e AS System.EventArgs):

        &IF DEFINED (WinKit) NE 0 &THEN
        DEFINE VARIABLE oDateControl AS Consultingwerk.WindowIntegrationKit.Controls.DatePickerControl NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE                                                         NO-UNDO .

        ASSIGN
            oDateControl = CAST (sender, Consultingwerk.WindowIntegrationKit.Controls.DatePickerControl)
            hWidget      = oDateControl:WidgetHandle .

        hWidget:SCREEN-VALUE = STRING (oDateControl:DATE) .

        APPLY "VALUE-CHANGED":U TO hWidget .

        WidgetHelper:ClosePopup () .

        &ELSE
        UNDO, THROW NEW AppError ("Not supported without Winkit.", 0) .
        &ENDIF

    END METHOD.
&ENDIF

    /**
     * Purpose: Filters a list of widget handle by the private data
     * Notes:   Supported operators are EQ, BEGINS, MATCHES, NE, BEGINS NOT, MATCHES NOT
     *          Silently ignores invalid handles or widgets that do not support the
     *          private data attribute
     * @param pcWidgetHandles The comma-delimited list of widget handles
     * @param poOperator The FilterByPrivateDataOperatorEnum that describes the operator to be used
     * @param pcValue The value to use for comparing the private data, when using the MATCHES Operator, include *
     * @return The list of the Widget handles matching the criteria
     */
    METHOD PUBLIC STATIC CHARACTER FilterByPrivateData (pcWidgetHandles AS CHARACTER,
                                                        poOperator AS FilterByPrivateDataOperatorEnum,
                                                        pcValue AS CHARACTER):

        DEFINE VARIABLE cReturn      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lMatches     AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE    NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cPrivateData AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poOperator """poOperator"":U"} .

        DO i = 1 TO NUM-ENTRIES (pcWidgetHandles) ON ERROR UNDO, THROW:

            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (i, pcWidgetHandles)) .

            IF NOT VALID-HANDLE (hWidget) THEN
                NEXT .

            IF NOT CAN-QUERY (hWidget, "PRIVATE-DATA":U) THEN
                NEXT .

            IF hWidget:PRIVATE-DATA = ? THEN
                ASSIGN cPrivateData = "?":U .
            ELSE
                ASSIGN cPrivateData = hWidget:PRIVATE-DATA .

            CASE poOperator:
                WHEN FilterByPrivateDataOperatorEnum:EQ THEN
                    lMatches = (cPrivateData = pcValue) .
                WHEN  FilterByPrivateDataOperatorEnum:Begins THEN
                    lMatches = (cPrivateData BEGINS pcValue) .
                WHEN  FilterByPrivateDataOperatorEnum:Matches THEN
                    lMatches = (cPrivateData MATCHES pcValue) .
                WHEN FilterByPrivateDataOperatorEnum:NE THEN
                    lMatches = (cPrivateData <> pcValue) .
                WHEN  FilterByPrivateDataOperatorEnum:BeginsNot THEN
                    lMatches = ((cPrivateData BEGINS pcValue) = FALSE) .
                WHEN  FilterByPrivateDataOperatorEnum:MatchesNot THEN
                    lMatches = ((cPrivateData MATCHES pcValue) = FALSE) .

                OTHERWISE lMatches = FALSE .
            END CASE .

            IF lMatches = TRUE THEN
                ASSIGN cReturn = cReturn + ",":U + STRING (hWidget) .
        END.

        RETURN TRIM (cReturn, ",":U) .

    END METHOD .

    /**
     * Purpose: Returns a comma delimited list of all Frames in a parent
     * Notes:   Does recursively process nested frames and field-groups
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @return A comma-delimited list of contained FRAME handles
     */
    METHOD PUBLIC STATIC CHARACTER FindAllFrames (phParent AS HANDLE):

        DEFINE VARIABLE cList   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF CAN-QUERY (hWidget, "FIRST-CHILD":U) THEN
                ASSIGN cList = TRIM (cList + ",":U +
                                     FindAllFrames (hWidget:FIRST-CHILD), ",":U) .

            IF hWidget:TYPE = "FRAME":U THEN
                ASSIGN cList = TRIM (cList + ",":U + STRING (hWidget), ",":U) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN cList .

    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of all visible Frames in a parent
     * Notes:   Does recursively process nested frames and field-groups
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @return A comma-delimited list of contained visible FRAME handles
     */
    METHOD PUBLIC STATIC CHARACTER FindAllVisibleFrames (phParent AS HANDLE):

        DEFINE VARIABLE cList   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF CAN-QUERY (hWidget, "FIRST-CHILD":U) THEN
                ASSIGN cList = TRIM (cList + ",":U +
                                     FindAllVisibleFrames (hWidget:FIRST-CHILD), ",":U) .

            IF hWidget:TYPE = "FRAME":U AND hWidget:VISIBLE = TRUE THEN
                ASSIGN cList = TRIM (cList + ",":U + STRING (hWidget), ",":U) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN cList .

    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of all Widgets in a parent of a
     *          given type
     * Notes:   Does recursively process nested frames
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @param pcWidgetType A comma delimited list of WIDGET types to search for
     * @return A comma-delimited list of matching WIDGET handles
     */
    METHOD PUBLIC STATIC CHARACTER FindAllWidgetsOfTypeRecursive (phParent AS HANDLE,
                                                                  pcWidgetType AS CHARACTER):

        DEFINE VARIABLE cList   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        IF phParent:TYPE = "FRAME":U THEN
            ASSIGN phParent = phParent:FIRST-CHILD .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF LOOKUP (hWidget:TYPE, pcWidgetType) > 0 THEN
                ASSIGN cList = cList + (IF cList > "":U THEN ",":U ELSE "":U) + STRING (hWidget) .

            IF hWidget:TYPE = "FRAME":U THEN
                ASSIGN cList = TRIM (cList + (IF cList > "":U THEN ",":U ELSE "":U) +
                               WidgetHelper:FindAllWidgetsOfTypeRecursive (hWidget, pcWidgetType), ",":U) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN TRIM (cList, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of all Widgets in a parent of a
     *          given type
     * Notes:   Does not recursively process nested frames
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @param pcWidgetType A comma delimited list of WIDGET types to search for
     * @return A comma-delimited list of matching WIDGET handles
     */
    METHOD PUBLIC STATIC CHARACTER FindAllWidgetsOfType (phParent AS HANDLE,
                                                         pcWidgetType AS CHARACTER):

        DEFINE VARIABLE cList   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        IF phParent:TYPE <> "FIELD-GROUP":U THEN
            ASSIGN phParent = WidgetHelper:FindChildWidgetOfType (phParent, "FIELD-GROUP":U) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF LOOKUP (hWidget:TYPE, pcWidgetType) > 0 THEN
                ASSIGN cList = cList + STRING (hWidget) + ",":U .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN TRIM (cList, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of all Widgets in a parent of a
     *          given type with the given PRIVATE-DATA value
     * Notes:   Does not recursively process nested frames
     * @param phParent The HANDLE of the Parent FRAME, WINDOW or FIELD-GROUP
     * @param pcWidgetType A comma delimited list of WIDGET types to search for
     * @param pcPrivateData The PRIVATE-DATA value to search for
     * @return A comma-delimited list of matching WIDGET handles
     */
    METHOD PUBLIC STATIC CHARACTER FindAllWidgetsOfTypeAndPrivateData (phParent AS HANDLE,
                                                                       pcWidgetType AS CHARACTER,
                                                                       pcPrivateData AS CHARACTER):

        DEFINE VARIABLE cList   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        IF phParent:TYPE <> "FIELD-GROUP":U THEN
            ASSIGN phParent = WidgetHelper:FindChildWidgetOfType (phParent, "FIELD-GROUP":U) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF LOOKUP (hWidget:TYPE, pcWidgetType) > 0 AND hWidget:PRIVATE-DATA = pcPrivateData THEN
                ASSIGN cList = cList + STRING (hWidget) + ",":U .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN TRIM (cList, ",":U) .

    END METHOD.

    /**
     * Purpose: Searches the Widget Tree frame for the bottom-most widget of the
     *          given Type
     * Notes:   Typically used to determine the bottom most BUTTON or RECTANGLE Widget in a
     *          frame for resizing when all BUTTON's are rendered in an Infragistics
     *          toolbar/ribbon
     *          When pcWidgetType is "BUTTON", we will also ignore rectangles that do
     *          only contain Button widgets.
     *
     * @param phParent The Frame handle to look in
     * @param pcWidgetType The Widget type that is ignored when searching for the bottom most widget.
     * @return The handle of the bottom most widget of the given type
     */
    METHOD PUBLIC STATIC HANDLE FindBottomMostWidgetOfType (phParent AS HANDLE,
                                                            pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hResult AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParent ""FRAME"":U} .

        ASSIGN hWidget = phParent:FIRST-CHILD:FIRST-CHILD
               hResult = hWidget .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE <> pcWidgetType THEN DO:
                hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:Y + hWidget:HEIGHT-PIXELS > hResult:Y + hWidget:HEIGHT-PIXELS THEN
                hResult = hWidget .

            hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN hResult .

    END METHOD.

    /**
     * Purpose: Searches the Widget Tree of the bottom most widget not of the given
     *          type
     * Notes:   Typically used to determine the bottom most non BUTTON Widget in a
     *          frame for resizing when all BUTTON's are rendered in an Infragistics
     *          toolbar/ribbon
     *          When pcWidgetType is "BUTTON", we will also ignore rectangles that do
     *          only contain Button widgets.
     * @param phParent The Frame handle to look in
     * @param pcWidgetType The Widget type that is ignored when searching for the bottom most widget.
     * @return The handle of the bottom most widget
     */
    METHOD PUBLIC STATIC HANDLE FindBottomMostWidgetNotOfType (phParent AS HANDLE,
                                                               pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hResult AS HANDLE NO-UNDO.
        DEFINE VARIABLE cRectangles AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParent ""FRAME"":U} .

        ASSIGN hWidget = phParent:FIRST-CHILD:FIRST-CHILD
               hResult = hWidget .

        IF pcWidgetType = "BUTTON":U THEN
            ASSIGN cRectangles = WidgetHelper:FindRectanglesSurroundingButtonsOnly (phParent) .

        DO WHILE VALID-HANDLE (hWidget):
            IF cRectangles > "":U AND
               LOOKUP (STRING (hWidget), cRectangles) > 0 THEN DO:

                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:TYPE = pcWidgetType THEN DO:
                hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:Y + hWidget:HEIGHT-PIXELS > hResult:Y + hWidget:HEIGHT-PIXELS THEN
                hResult = hWidget .

            hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN hResult .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Determins the bottom right most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindBottomRightMostPointInWindow (phWindow AS HANDLE):

        RETURN WidgetHelper:FindBottomRightMostPointInWindow (phWindow, ?, ?) .

    END METHOD .

    /**
     * Purpose: Determins the bottom right most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @param pcExceptWidgetTypes Comma delimited list of widget types to ignore
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindBottomRightMostPointInWindow (phWindow AS HANDLE,
                                                                                pcExceptWidgetTypes AS CHARACTER):

        RETURN WidgetHelper:FindBottomRightMostPointInWindow (phWindow, pcExceptWidgetTypes, ?) .

    END METHOD .

    /**
     * Purpose: Determins the bottom right most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @param pcExceptWidgetTypes Comma delimited list of widget types to ignore
     * @param pcExceptWidgetNames Comma delimited list of widget names to ignore
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindBottomRightMostPointInWindow (phWindow AS HANDLE,
                                                                                pcExceptWidgetTypes AS CHARACTER,
                                                                                pcExceptWidgetNames AS CHARACTER):

        DEFINE VARIABLE iMaxX    AS INTEGER              NO-UNDO INIT 0 .
        DEFINE VARIABLE iMaxY    AS INTEGER              NO-UNDO INIT 0 .
        DEFINE VARIABLE iFrameX  AS INTEGER              NO-UNDO .
        DEFINE VARIABLE iFrameY  AS INTEGER              NO-UNDO .
        DEFINE VARIABLE cFrames  AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE hFrame   AS HANDLE               NO-UNDO .
        DEFINE VARIABLE iFrame   AS INTEGER              NO-UNDO .
        DEFINE VARIABLE hWidget  AS HANDLE               NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow WidgetTypeEnum:Window} .

        ASSIGN cFrames = WidgetHelper:FindAllFrames (phWindow) .

        DO iFrame = 1 TO NUM-ENTRIES (cFrames) ON ERROR UNDO, THROW:

            ASSIGN hFrame   = WIDGET-HANDLE (ENTRY (iFrame, cFrames))
                   hWidget  = hFrame:FIRST-CHILD:FIRST-CHILD .

            WidgetHelper:AbsolutePositionOnWindow (hFrame,
                                                   OUTPUT iFrameX,
                                                   OUTPUT iFrameY) .

            widgets: DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:

                IF pcExceptWidgetTypes > "":U AND LOOKUP (hWidget:TYPE, pcExceptWidgetTypes) > 0 THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgets .
                END.

                IF pcExceptWidgetNames > "":U AND LOOKUP (hWidget:NAME, pcExceptWidgetNames) > 0 THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgets .
                END.

                IF hWidget:TYPE <> WidgetTypeEnum:Frame THEN DO:

                    IF hWidget:X + hWidget:WIDTH-PIXELS + iFrameX > iMaxX THEN
                        ASSIGN iMaxX = hWidget:X + hWidget:WIDTH-PIXELS + iFrameX .

                    IF hWidget:Y + hWidget:HEIGHT-PIXELS + iFrameY > iMaxY THEN
                        ASSIGN iMaxY = hWidget:Y + hWidget:HEIGHT-PIXELS + iFrameY .
                END.

                hWidget = hWidget:NEXT-SIBLING .
            END.
        END.

        IF iMaxX = 99999 THEN
            RETURN ? .
        ELSE
            RETURN NEW System.Drawing.Point (iMaxX, iMaxY) .

    END METHOD .
&ENDIF

    /**
     * Purpose: Searches for a Widget with the given name in the Parents (Frame or
     *          Window) widget tree
     * Notes:
     * @param phParent The handle of the parent widget
     * @param pcWidgetName The name of the child widget to return
     * @return The handle of the child widget
     */
    METHOD PUBLIC STATIC HANDLE FindChildWidgetByName (phParent AS HANDLE,
                                                       pcWidgetName AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hChild  AS HANDLE NO-UNDO.

        IF NOT VALID-HANDLE (phParent) THEN
            UNDO, THROW NEW AppError ("Invalid parent widget."{&TRAN}, 0) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):

            IF hWidget:NAME = pcWidgetName THEN
                RETURN hWidget .

            IF CAN-QUERY (hWidget, "FIRST-CHILD":U) THEN DO:

                ASSIGN hChild = FindChildWidgetByName (hWidget, pcWidgetName) .

                IF VALID-HANDLE (hChild) THEN
                    RETURN hChild .

            END.

            hWidget = hWidget:NEXT-SIBLING  .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Searches for all Widget with the given names in the Parents (Frame or
     *          Window) widget tree, the WidgetNames are space delimited, so that this
     *          function is compatible with AppBuilder generated lists
     * Notes:
     * @param phParent The parent FRAME or WIDGET handle
     * @param pcWidgetNames The space delimited list of widget names
     * @return The comma-delimited list of widget handles
     */
    METHOD PUBLIC STATIC CHARACTER FindChildWidgetsByName (phParent AS HANDLE,
                                                           pcWidgetNames AS CHARACTER):

        DEFINE VARIABLE cWidgetName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cHandles    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER NO-UNDO.
        DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO.

        IF NOT VALID-HANDLE (phParent) THEN
            UNDO, THROW NEW AppError ("Invalid parent widget."{&TRAN}, 0) .

        ASSIGN cHandles = FILL (",":U, NUM-ENTRIES (pcWidgetNames, " ":U) - 1) .

        DO i = 1 TO NUM-ENTRIES (pcWidgetNames," ":U):
            ASSIGN cWidgetName = ENTRY (i, pcWidgetNames, " ":U) .

            ASSIGN cValue = STRING (FindChildWidgetByName (phParent,
                                                           cWidgetName)) .

            IF cValue = ? THEN ASSIGN cValue = "?":U .

            ASSIGN ENTRY (i, cHandles) = cValue .

        END.

        RETURN cHandles .

    END METHOD.

    /**
     * Purpose: Searches for a the first Widget of the given SCREEN-VALUE in the
     *          Parents (Frame or Window) widget tree
     * Notes:   Useful for locating LITERAL's on the screen
     * @param phParent The handle of the parent widget
     * @param pcScreenValue The screen value of the widget to return
     * @param pcWidgetType The widget type of the widget to return
     * @return The handle of the found widget
     */
    METHOD PUBLIC STATIC HANDLE FindChildWidgetByScreenValueAndType (phParent AS HANDLE,
                                                                     pcScreenValue AS CHARACTER,
                                                                     pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hChild  AS HANDLE NO-UNDO.

        IF NOT VALID-HANDLE (phParent) THEN
            UNDO, THROW NEW AppError ("Invalid parent widget."{&TRAN}, 0) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):

            IF hWidget:TYPE = pcWidgetType AND hWidget:SCREEN-VALUE = pcScreenValue THEN
                RETURN hWidget .

            IF CAN-QUERY (hWidget, "FIRST-CHILD":U) THEN DO:

                ASSIGN hChild = FindChildWidgetByScreenValueAndType (hWidget, pcScreenValue, pcWidgetType) .

                IF VALID-HANDLE (hChild) THEN
                    RETURN hChild .
            END.

            hWidget = hWidget:NEXT-SIBLING  .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Searches for a the first Widget of a TYPE in the given list of types
     *          in the Parents (Frame or Window) widget tree
     * Notes:
     * @param phParent The handle of the PARENT
     * @param pcWidgetType A comma delimited list of Widget types to search for
     * @return The handle of the found widget
     */
    METHOD PUBLIC STATIC HANDLE FindChildWidgetOfType (phParent AS HANDLE,
                                                       pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hChild  AS HANDLE NO-UNDO.

        IF NOT VALID-HANDLE (phParent) THEN
            UNDO, THROW NEW AppError ("Invalid parent widget."{&TRAN}, 0) .

        ASSIGN hWidget = phParent:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):

            IF LOOKUP (hWidget:TYPE, pcWidgetType) > 0 THEN
                RETURN hWidget .

            IF CAN-QUERY (hWidget, "FIRST-CHILD":U) THEN DO:

                ASSIGN hChild = FindChildWidgetOfType (hWidget, pcWidgetType) .

                IF VALID-HANDLE (hChild) THEN
                    RETURN hChild .

            END.

            hWidget = hWidget:NEXT-SIBLING  .
        END.

        RETURN ? .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Returns the reference to the .NET Controlt that is associated with
     *          the passed in widget handle (using the Tag property of the .NET
     *          Control)
     * Notes:
     * @param poParentControl The reference to the parent control
     * @param phWidget The handle of the widget to return the Control for
     * @return The reference to the Control for the widget
     */
    METHOD PUBLIC STATIC System.Windows.Forms.Control FindControlForWidget (poParentControl AS System.Windows.Forms.Control,
                                                                            phWidget AS HANDLE):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER                      NO-UNDO.
        DEFINE VARIABLE pcWidget AS CHARACTER NO-UNDO.

        ASSIGN pcWidget = STRING (phWidget) .

        DO i = 0 TO poParentControl:Controls:Count - 1:

            oControl = poParentControl:Controls[i] .

            IF ENTRY(1, UNBOX (oControl:Tag)) = pcWidget THEN
                RETURN oControl .

        END.

        RETURN ? .

    END METHOD.
&ENDIF

    /**
     * Purpose: Locates all Frames that are located in the FRAME that contains the
     *          source rectangle (could be Rectangle, Frame or COM-FRAME)
     * Notes:   Useful for locating Frames that are used on an ABL tabfolder, i.e.
     *          by checking if that are located within the bounds of the COM-FRAME
     *          that parents the Active X TabFolder
     *          Currently tests if the top left corner (absolute to the Window) is
     *          within the Rectangle
     * @param phSourceRectangle The handle of the source rectangle
     * @return The comma-delimited list of the FRAME handles within the bounds
     */
    METHOD PUBLIC STATIC CHARACTER FindFramesWithinRectangle (phSourceRectangle AS HANDLE):

        DEFINE VARIABLE hParentFrame AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cFrames      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hFrame       AS HANDLE    NO-UNDO .
        DEFINE VARIABLE iSourceX     AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iSourceY     AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iChildX      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iChildY      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cReturn      AS CHARACTER NO-UNDO .

        hParentFrame = phSourceRectangle:FRAME .

        ASSIGN cFrames = WidgetHelper:FindAllWidgetsOfType (hParentFrame,
                                                            WidgetTypeEnum:Frame) .

        WidgetHelper:AbsolutePositionOnWindow (phSourceRectangle,
                                               OUTPUT iSourceX,
                                               OUTPUT iSourceY) .

        DO i = 1 TO NUM-ENTRIES (cFrames):
            ASSIGN hFrame = WIDGET-HANDLE (ENTRY (i, cFrames)) .

            WidgetHelper:AbsolutePositionOnWindow (hFrame,
                                                   OUTPUT iChildX,
                                                   OUTPUT iChildY) .

            IF iChildX >= iSourceX AND iChildX <= iSourceX + phSourceRectangle:WIDTH-PIXELS AND
               iChildY >= iSourceY AND iChildY <= iSourceY + phSourceRectangle:HEIGHT-PIXELS THEN

                ASSIGN cReturn = SUBSTITUTE ("&1,&2":U,
                                             cReturn,
                                             STRING (hFrame)) .
        END.

        /* Check Frames on Window in second pass */
        ASSIGN hFrame = phSourceRectangle:WINDOW:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hFrame):

            WidgetHelper:AbsolutePositionOnWindow (hFrame,
                                                   OUTPUT iChildX,
                                                   OUTPUT iChildY) .

            IF iChildX >= iSourceX AND iChildX <= iSourceX + phSourceRectangle:WIDTH-PIXELS AND
               iChildY >= iSourceY AND iChildY <= iSourceY + phSourceRectangle:HEIGHT-PIXELS THEN

                ASSIGN cReturn = SUBSTITUTE ("&1,&2":U,
                                             cReturn,
                                             STRING (hFrame)) .

            hFrame = hFrame:NEXT-SIBLING .
        END.

        RETURN TRIM (cReturn, ",":U) .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Returns a System.Drawing.Point with the coordinates of the
     *          innermost used point
     * Notes:   The actual used top and left edge of the frame contents
     * @param phFrame The handle of the FRAME
     * @return The inner most Point
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindInnerMostPoint (phFrame AS HANDLE):

        DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iMinX   AS INTEGER NO-UNDO INIT 0 .
        DEFINE VARIABLE iMinY   AS INTEGER NO-UNDO INIT 0.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phFrame ""FRAME"":U} .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF iMinX = 0 THEN
                iMinX = hWidget:X .
            ELSE
                iMinX = MINIMUM (iMinX, hWidget:X) .

            IF iMinY = 0 THEN
                iMinY = hWidget:Y .
            ELSE
                iMinY = MINIMUM (iMinY, hWidget:Y) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

        RETURN NEW System.Drawing.Point (iMinX, iMinY) .

    END METHOD.
&ENDIF

    /**
     * Purpose: Locates the lowest "hangig" widget in a frame that starts at
     *          a row above the pdeTopLimit and is of a type in the type list.
     * Notes:
     * @param phParentFrame The handle of the frame
     * @param pcTypeList The comma delimited list of widget types
     * @param pdeTopLimit The row above widgets should start
     * @return The handle of the widget
     */
    METHOD PUBLIC STATIC HANDLE FindLowestWidgetOfType (phParentFrame AS HANDLE,
                                                        pcTypeList AS CHARACTER,
                                                        pdeTopLimit AS DECIMAL):

        DEFINE VARIABLE hWidget              AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hCurrentLowestWidget AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iLow                 AS INTEGER NO-UNDO.

        IF NOT phParentFrame:TYPE = "FRAME":U THEN
            UNDO, THROW NEW AppError ("FindLowestWidgetOfType needs a FRAME widget as phParentFrame!"{&TRAN}, 0) .

        ASSIGN hWidget = phParentFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
              IF ListHelper:EntryIsInList (hWidget:TYPE, pcTypeList) AND
                 hWidget:ROW <= pdeTopLimit AND
                 (hWidget:Y + hWidget:HEIGHT-PIXELS) > iLow THEN DO:

                  hCurrentLowestWidget = hWidget .
                  iLow = hWidget:Y + hWidget:HEIGHT-PIXELS .

              END.

              hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN hCurrentLowestWidget .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Returns a System.Drawing.Point with the coordinates of the
     *          outermost used point - the actual used bottom and right edge of the
     *          frame contents.
     * Notes:
     * @param phFrame The handle of the FRAME
     * @return The outer most Point
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindOuterMostPoint (phFrame AS HANDLE):

        DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iMaxX   AS INTEGER NO-UNDO INIT 0 .
        DEFINE VARIABLE iMaxY   AS INTEGER NO-UNDO INIT 0.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phFrame ""FRAME"":U} .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF iMaxX = 0 THEN
                iMaxX = hWidget:X + hWidget:WIDTH-PIXELS .
            ELSE
                iMaxX = MAX (iMaxX, hWidget:X + hWidget:WIDTH-PIXELS) .

            IF iMaxY = 0 THEN
                iMaxY = hWidget:Y + hWidget:HEIGHT-PIXELS .
            ELSE
                iMaxY = MAX (iMaxY, hWidget:Y + hWidget:HEIGHT-PIXELS) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

        RETURN NEW System.Drawing.Point (iMaxX, iMaxY) .

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns a comma-delimited list of rectangles the do only surround
     *          buttons
     * Notes:   Useful in the WinKit to detect rectangles that might be hidden because
     *          all BUTTON Widgets are renderd as Infragistics widgets
     * @param phFrame The handle of the FRAME
     * @return The comma-delimited list of rectangles
     */
    METHOD PUBLIC STATIC CHARACTER FindRectanglesSurroundingButtonsOnly (phFrame AS HANDLE):

        DEFINE VARIABLE cRectangles AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hFieldGroup AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hRectangle  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lFoundOne   AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cResult     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAtLeastOne AS LOGICAL   NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phFrame WidgetTypeEnum:Frame} .

        ASSIGN hFieldGroup = phFrame:FIRST-CHILD
               cRectangles = WidgetHelper:FindAllWidgetsOfType (hFieldGroup,
                                                                WidgetTypeEnum:Rectangle).

        outerLoop: DO i = 1 TO NUM-ENTRIES (cRectangles):

            ASSIGN hRectangle = WIDGET-HANDLE (ENTRY (i, cRectangles)) .

            /* Search for other widgets (non-rectangles, non-button)
               where the Top/Left Pixel is container within this rectangle */

            ASSIGN hWidget   = hFieldGroup:FIRST-CHILD
                   lFoundOne = FALSE
                   lAtLeastOne = FALSE .

            innerLoop: DO WHILE VALID-HANDLE (hWidget):

                /* When searching for Widgets inside the current Rectangle,
                   ignore Buttons and Rectangles */
                IF hWidget:TYPE = WidgetTypeEnum:Button THEN DO:
                    IF     hWidget:X > hRectangle:X AND hWidget:X < hRectangle:X + hRectangle:WIDTH-PIXELS
                       AND hWidget:Y > hRectangle:Y AND hWidget:Y < hRectangle:Y + hRectangle:HEIGHT-PIXELS THEN

                       /* We've found at least on Button in the Rectangle */
                       ASSIGN lAtLeastOne = TRUE .

                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT innerLoop .
                END.

                IF     hWidget:X > hRectangle:X AND hWidget:X < hRectangle:X + hRectangle:WIDTH-PIXELS
                   AND hWidget:Y > hRectangle:Y AND hWidget:Y < hRectangle:Y + hRectangle:HEIGHT-PIXELS THEN DO:

                  /* Found a non button and non rectangle that is inside the current rectangle */
                    ASSIGN lFoundOne = TRUE .

                    LEAVE innerLoop .
                END.

                  ELSE DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT innerLoop .
                END.

            END .

            /* Return this Rectangle when we've found at least on Button, but nothing else in it. */
            IF lAtLeastOne = TRUE AND lFoundOne = FALSE THEN
                ASSIGN cResult = cResult + ",":U + STRING (hRectangle) .
        END .

        RETURN TRIM (cResult, ",":U) .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Determins the top left most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindTopLeftMostPointInWindow (phWindow AS HANDLE):

        RETURN WidgetHelper:FindTopLeftMostPointInWindow (phWindow, ?, ?) .

    END METHOD .

    /**
     * Purpose: Determins the top left most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @param pcExceptWidgetTypes Comma delimited list of widget types to ignore
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindTopLeftMostPointInWindow (phWindow AS HANDLE,
                                                                            pcExceptWidgetTypes AS CHARACTER):

        RETURN WidgetHelper:FindTopLeftMostPointInWindow (phWindow, pcExceptWidgetTypes, ?) .

    END METHOD .

    /**
     * Purpose: Determins the top left most used point in a Window
     * Notes:
     * @param phWindow The HANDLE of the Window widget
     * @param pcExceptWidgetTypes Comma delimited list of widget types to ignore
     * @param pcExceptWidgetNames Comma delimited list of widget names to ignore
     * @return The top left most used point in a Window
     */
    METHOD PUBLIC STATIC System.Drawing.Point FindTopLeftMostPointInWindow (phWindow AS HANDLE,
                                                                            pcExceptWidgetTypes AS CHARACTER,
                                                                            pcExceptWidgetNames AS CHARACTER):

        DEFINE VARIABLE iMinX    AS INTEGER              NO-UNDO INIT 99999 .
        DEFINE VARIABLE iMinY    AS INTEGER              NO-UNDO INIT 99999 .
        DEFINE VARIABLE iFrameX  AS INTEGER              NO-UNDO .
        DEFINE VARIABLE iFrameY  AS INTEGER              NO-UNDO .
        DEFINE VARIABLE cFrames  AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE hFrame   AS HANDLE               NO-UNDO .
        DEFINE VARIABLE iFrame   AS INTEGER              NO-UNDO .
        DEFINE VARIABLE hWidget  AS HANDLE               NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow WidgetTypeEnum:Window} .

        ASSIGN cFrames = WidgetHelper:FindAllFrames (phWindow) .

        DO iFrame = 1 TO NUM-ENTRIES (cFrames) ON ERROR UNDO, THROW:

            ASSIGN hFrame   = WIDGET-HANDLE (ENTRY (iFrame, cFrames))
                   hWidget  = hFrame:FIRST-CHILD:FIRST-CHILD .

            WidgetHelper:AbsolutePositionOnWindow (hFrame,
                                                   OUTPUT iFrameX,
                                                   OUTPUT iFrameY) .

            widgets: DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:

                IF pcExceptWidgetTypes > "":U AND LOOKUP (hWidget:TYPE, pcExceptWidgetTypes) > 0 THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgets .
                END.

                IF pcExceptWidgetNames > "":U AND LOOKUP (hWidget:NAME, pcExceptWidgetNames) > 0 THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgets .
                END.

                IF hWidget:TYPE <> WidgetTypeEnum:Frame THEN DO:

                    IF hWidget:X + iFrameX < iMinX THEN
                        ASSIGN iMinX = hWidget:X + iFrameX .

                    IF hWidget:Y + iFrameY < iMinY THEN
                        ASSIGN iMinY = hWidget:Y + iFrameY .
                END.

                hWidget = hWidget:NEXT-SIBLING .
            END.
        END.

        IF iMinX = 99999 THEN
            RETURN ? .
        ELSE
            RETURN NEW System.Drawing.Point (iMinX, iMinY) .

    END METHOD .
&ENDIF

    /**
     * Purpose: Searches the Widget Tree frame for the top-most widget of the given Type
     * Notes:   Typically used to determine the gop most BUTTON or RECTANGLE Widget in a
     *          frame for resizing when all BUTTON's are rendered in an Infragistics
     *          toolbar/ribbon
     *          When pcWidgetType is "BUTTON", we will also ignore rectangles that do
     *          only contain Button widgets.
     * @param phParent The Frame handle to look in
     * @param pcWidgetType The Widget type that is ignored when searching for the bottom most widget.
     * @return The handle of the bottom most widget of the given type
     */
    METHOD PUBLIC STATIC HANDLE FindTopMostWidgetOfType (phParent AS HANDLE,
                                                         pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE hResult AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParent ""FRAME"":U} .

        ASSIGN hWidget = phParent:FIRST-CHILD:FIRST-CHILD
               hResult = hWidget .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE <> pcWidgetType THEN DO:
                hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:Y < hResult:Y THEN
                hResult = hWidget .

            hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN hResult .

    END METHOD.

    /**
     * Purpose: Searches the Widget Tree of the top most widget not of the given
     *          type (accepts a list of types)
     * Notes:   Typically used to determine the topmost most non BUTTON Widget in a
     *          frame for resizing when all BUTTON's are rendered in an Infragistics
     *          toolbar/ribbon
     *          When pcWidgetType contains "BUTTON", we will also ignore rectangles that do
     *          only contain Button widgets.
     * @param phParent The Frame handle to look in
     * @param pcWidgetTypes The Widget type or list of widget type that is ignored when searching for the bottom most widget
     * @return The handle of the bottom most widget
     */
    METHOD PUBLIC STATIC HANDLE FindTopMostWidgetNotOfType (phParent AS HANDLE,
                                                            pcWidgetTypes AS CHARACTER):

        DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hResult     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cRectangles AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iTop        AS INTEGER   NO-UNDO INIT 99999.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phParent ""FRAME"":U} .

        ASSIGN hWidget = phParent:FIRST-CHILD:FIRST-CHILD  .

        IF ListHelper:EntryIsInList ("BUTTON":U, pcWidgetTypes) THEN
            ASSIGN cRectangles = WidgetHelper:FindRectanglesSurroundingButtonsOnly (phParent) .

        DO WHILE VALID-HANDLE (hWidget):
            IF cRectangles > "":U AND
               LOOKUP (STRING (hWidget), cRectangles) > 0 THEN DO:

                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF ListHelper:EntryIsInList (hWidget:TYPE, pcWidgetTypes) THEN DO:
                hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:Y < iTop THEN DO:
                iTop = hWidget:Y .
                hResult = hWidget .
            END.

            hWidget = hWidget:NEXT-SIBLING .
        END.

        RETURN hResult .

    END METHOD.

    /**
     * Purpose:  Returns a comma-delimited list of all widgets of the given type
     *           in located within the boundaries of a rectangle (in the same frame)
     * Notes:
     * @param phRectangle The handle of the rectangle widget
     * @param pcWidgetType The type of the widgets to locate
     * @return The comma-delimited list of the widgets
     */
    METHOD PUBLIC STATIC CHARACTER FindWidgetsInRectangle (phRectangle AS HANDLE,
                                                           pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hWidget  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iWidget  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cWidgets AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        cWidgets = WidgetHelper:FindAllWidgetsOfType (phRectangle:FRAME,
                                                      pcWidgetType) .

        DO iWidget = 1 TO NUM-ENTRIES (cWidgets):
            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (iWidget, cWidgets)) .

            IF hWidget:X >= phRectangle:X AND hWidget:X <= phRectangle:X + phRectangle:WIDTH-PIXELS AND
               hWidget:Y >= phRectangle:Y AND hWidget:Y <= phRectangle:Y + phRectangle:HEIGHT-PIXELS THEN

                ASSIGN cReturn = SUBSTITUTE ("&1,&2":U,
                                             cReturn,
                                             STRING (hWidget)) .
        END.

        RETURN TRIM (cReturn, ",":U) .

    END METHOD.

    /**
     * Purpose: Searches the Widget Tree Up for the next Window
     * Notes:
     * @param phWidget The handle of the widget to return the window for
     * @return The handle of the window widget
     */
    METHOD PUBLIC STATIC HANDLE FindWindow (phWidget AS HANDLE):

        DEFINE VARIABLE hParent AS HANDLE NO-UNDO.

        IF NOT VALID-HANDLE (phWidget) AND NOT CAN-QUERY (phWidget, "PARENT":U) THEN
            UNDO, THROW NEW AppError ("Invalid widget specified for method FindWindow."{&TRAN}, 0) .

        ASSIGN hParent = phWidget .

        DO WHILE VALID-HANDLE (hParent):
            IF hParent:TYPE = "WINDOW":U THEN
                RETURN hParent .

            IF VALID-HANDLE (hParent) AND CAN-QUERY (hParent, "PARENT":U) THEN
                hParent = hParent:PARENT .
            ELSE LEAVE .
        END.

        RETURN ? .

    END METHOD.


    /**
     * Purpose: Walks through all widgets in a FRAME (non-recursively) and returns
     *          the minimum size for the FRAME in pixels
     * Notes:   Recursion through nested FRAME's is really not a requirement here
     * @param phFrame The HANDLE of the FRAME to analyze
     * @param piMinWidth OUTPUT The minimum WIDTH in pixels for the FRAME
     * @param piMinHeight OUTPUT The minimum HEIGHT in pixels for the FRAME
     */
    METHOD PUBLIC STATIC VOID GetMinSize (phFrame AS HANDLE,
                                          OUTPUT piMinWidth AS INTEGER,
                                          OUTPUT piMinHeight AS INTEGER):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phFrame ""FRAME"":U} .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            ASSIGN piMinWidth  = MAX (piMinWidth, hWidget:X + hWidget:WIDTH-PIXELS)
                   piMinHeight = MAX (piMinHeight, hWidget:Y + hWidget:HEIGHT-PIXELS) .

            hWidget = hWidget:NEXT-SIBLING .
        END.

        ASSIGN piMinWidth  = piMinWidth + phFrame:BORDER-LEFT-PIXELS + phFrame:BORDER-RIGHT-PIXELS
               piMinHeight = piMinHeight + phFrame:BORDER-TOP-PIXELS + phFrame:BORDER-BOTTOM-PIXELS .

    END METHOD.

    /**
     * Purpose: Hides all widgets of the given type in the frame
     * Notes:
     * @param phFrame The handle of the FRAME
     * @param pcWidgetType The type of widgets to hide
     */
    METHOD PUBLIC STATIC VOID HideWidgets (phFrame AS HANDLE,
                                           pcWidgetType AS CHARACTER):

        HideWidgets (phFrame,
                     0,
                     pcWidgetType) .

    END METHOD .

    /**
     * Purpose: Hides all widgets of the given type in the frame
     * Notes:
     * @param phFrame The handle of the FRAME
     * @param pdeLimitRow The top most row of the widgets to hide
     * @param pcWidgetType The type of widgets to hide
     */
    METHOD PUBLIC STATIC VOID HideWidgets (phFrame AS HANDLE,
                                           pdeLimitRow AS DECIMAL,
                                           pcWidgetType AS CHARACTER):

        DEFINE VARIABLE hFieldGroup  AS HANDLE NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE NO-UNDO .

        IF pdeLimitRow > 0 THEN .
        ELSE pdeLimitRow = 9999.

        ASSIGN hFieldGroup = phFrame:FIRST-CHILD .

        ASSIGN hWidget = hFieldGroup:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE = pcWidgetType AND hWidget:ROW <= pdeLimitRow THEN
                hWidget:HIDDEN = TRUE .

            ASSIGN
                hWidget   = hWidget:NEXT-SIBLING .
        END.

    END METHOD.

    /**
     * Purpose: Left aligns the labels of widgets in a Frame
     * Notes:   Works on two corresponding comma-delimited lists, one with the
     *          current columns of the widgets and the other with the target columns
     *          of the labels
     * @param phFrame The handle of the frame containing the Widgets
     * @param pcWidgetColumns A comma-delimited list of columns of widgets
     * @param pcLabelColumns A comma-delimited list of columns that the widget's labels should be placed at
     */
    METHOD PUBLIC VOID LeftAlignWidgetLabels (phFrame AS HANDLE,
                                              pcWidgetColumns AS CHARACTER,
                                              pcLabelColumns AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE      NO-UNDO.
        DEFINE VARIABLE iEntry  AS INTEGER     NO-UNDO.

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:

            IF (hWidget:TYPE = "FILL-IN":U OR
                hWidget:TYPE = "COMBO-BOX":U) AND
               VALID-HANDLE (hWidget:SIDE-LABEL-HANDLE) THEN DO:

                iEntry = LOOKUP (STRING (hWidget:COLUMN), pcWidgetColumns) .

                IF iEntry > 0 THEN
                    ASSIGN hWidget:SIDE-LABEL-HANDLE:COLUMN = INTEGER (ENTRY (iEntry, pcLabelColumns)) .
            END.

            FINALLY:
                hWidget = hWidget:NEXT-SIBLING .
            END FINALLY.
        END.

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Loads an Image
     * Notes:
     * @param pcImageFileName The file name of the image
     * @return The reference to the Image
     */
    METHOD PUBLIC STATIC System.Drawing.Image LoadImage (pcImageFileName AS CHARACTER):

        DEFINE VARIABLE oImage      AS System.Drawing.Bitmap NO-UNDO .
        DEFINE VARIABLE oReturn     AS System.Drawing.Bitmap NO-UNDO .

        FILE-INFORMATION:FILE-NAME = pcImageFileName .

        /* Mike Fechner, Consultingwerk Ltd. 25.05.2012
           When the actual image file-name cannot be found, try with default
           image extensions (LOAD-IMAGE method of the Button Widget can add
           default extensions) */
        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
            FILE-INFO:FILE-NAME = pcImageFileName + ".bmp":U .

            IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                FILE-INFO:FILE-NAME = pcImageFileName + ".png":U .

                IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                    FILE-INFO:FILE-NAME = pcImageFileName + ".ico":U .

                    IF FILE-INFO:FULL-PATHNAME =  ? THEN
                        RETURN ? .
                END.
            END.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 21.02.2009
           Bug 1574: Support for icons, icons do not need to be made transparent */
        IF pcImageFileName MATCHES "*.ico":U THEN
            oImage = (NEW System.Drawing.Icon(FILE-INFORMATION:FULL-PATHNAME)):ToBitmap() NO-ERROR .
        ELSE DO:
            oImage = CAST (Consultingwerk.Util.ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME),
                           System.Drawing.Bitmap) .

            IF VALID-OBJECT(oImage) THEN DO:
                oReturn = Consultingwerk.Util.ImageHelper:MakeTransparent (oImage) .
                oImage:Dispose () .
                oImage = oReturn .
            END.
        END.

        RETURN oImage .

    END METHOD.
&ENDIF

    /**
     * Purpose: Returns a list of widget handles ordered by col and then row on the
     *          screen
     * Notes:
     * @param pcWidgetHandles The original (unordered list of widget handles
     * @return The ordered list of widget handles
     */
    METHOD PUBLIC STATIC CHARACTER OrderWidgetsByColRow (pcWidgetHandles AS CHARACTER):

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        EMPTY TEMP-TABLE ttWidgetsByLocation .

        WidgetHelper:PopulateWidgetTempTable (pcWidgetHandles) .

        FOR EACH ttWidgetsByLocation USE-INDEX LocationByColumn:
            ASSIGN cReturn = SUBSTITUTE ("&1,&2":U,
                                         cReturn,
                                         ttWidgetsByLocation.WidgetHandle) .
        END.

        RETURN TRIM (cReturn, ",":U) .

        FINALLY:
            EMPTY TEMP-TABLE ttWidgetsByLocation .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Returns a list of widget handles ordered by row and then col on the
     *          screen
     * Notes:
     * @param pcWidgetHandles The original (unordered list of widget handles
     * @return The ordered list of widget handles
     */
    METHOD PUBLIC STATIC CHARACTER OrderWidgetsByRowCol (pcWidgetHandles AS CHARACTER):

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        EMPTY TEMP-TABLE ttWidgetsByLocation .

        WidgetHelper:PopulateWidgetTempTable (pcWidgetHandles) .

        FOR EACH ttWidgetsByLocation USE-INDEX Location:
            ASSIGN cReturn = SUBSTITUTE ("&1,&2":U,
                                         cReturn,
                                         ttWidgetsByLocation.WidgetHandle) .
        END.

        RETURN TRIM (cReturn, ",":U) .

        FINALLY:
            EMPTY TEMP-TABLE ttWidgetsByLocation .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Populates (fills the
     * Notes:
     * @param pcWidgetHandles The original (unordered list of widget handles
     */
    METHOD PROTECTED STATIC VOID PopulateWidgetTempTable (pcWidgetHandles AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.

        ASSIGN iCount = NUM-ENTRIES (pcWidgetHandles) .

        DO i = 1 TO iCount:
            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (i, pcWidgetHandles)) .

            CREATE ttWidgetsByLocation .
            ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                   ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                   ttWidgetsByLocation.WidgetHandle = hWidget .
        END.

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Refreshes Buttons based on the ABL widget status (UltraButton / BUTTON)
     * Notes:
     * @param poParentControl The control container that conatins the buttons overlaying the ABL BUTTON widgets
     */
    METHOD PUBLIC STATIC VOID RefreshButtons (poParentControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE hWidget AS HANDLE                       NO-UNDO.

        {Consultingwerk/foreach.i System.Windows.Forms.Control oButton in poParentControl:Controls}

            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (1, UNBOX(oButton:Tag))) NO-ERROR .

&IF DEFINED (WinKit) &THEN
            IF VALID-OBJECT (oReplacedWidgetCustomizer) THEN DO:
                IF VALID-HANDLE (hWidget) THEN
                    oReplacedWidgetCustomizer:RefreshControlProperties (oButton,
                                                                        hWidget) .
            END.
            ELSE
&ENDIF
            DO:
                IF VALID-HANDLE (hWidget) AND hWidget:TYPE = "BUTTON":U THEN DO:
                    ASSIGN oButton:Enabled = hWidget:SENSITIVE
                           oButton:Visible = hWidget:VISIBLE AND hWidget:PARENT:VISIBLE .
                           oButton:Text    = hWidget:LABEL .
                END.
                IF VALID-HANDLE (hWidget) AND hWidget:TYPE = "FILL-IN":U THEN DO:
                    ASSIGN oButton:Enabled = hWidget:SENSITIVE
                           oButton:Visible = hWidget:VISIBLE AND hWidget:PARENT:VISIBLE .
                END.
            END.
        END.

    END METHOD .

    /**
     * Purpose: Overlays one widget with a .NET Controls of the
     *          given type (i.e. BUTTON Widget -> UltraButton Control).
     * Notes:
     * @param phWidget The handle of the widget to replace
     * @param poParentControl The target control container
     * @param poControlType The System.Type to overlay the widgets with
     * @param piRowOffset The row offset used by the parent Form (see EmbeddedWindowForm)
     * @param piColOffset The column offset used by the parent Form (see EmbeddedWindowForm)
     * @param plPostMessage When True then the original button will receive a BM_CLICK Message using the PostMessage API, otherwise APPLY CHOOSE is used
     * @return The reference to the generated Control
     */
    METHOD PUBLIC STATIC System.Windows.Forms.Control ReplaceWidget (phWidget           AS HANDLE,
                                                                     poParentControl    AS System.Windows.Forms.Control,
                                                                     poControlType      AS System.Type,
                                                                     piRowOffset        AS INTEGER,
                                                                     piColOffset        AS INTEGER,
                                                                     plPostMessage      AS LOGICAL):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oType    AS "System.Type[]":U            NO-UNDO .
        DEFINE VARIABLE oImage   AS System.Drawing.Image         NO-UNDO .

        DEFINE VARIABLE deX AS INTEGER NO-UNDO.
        DEFINE VARIABLE deY AS INTEGER NO-UNDO.

        oType = System.Type:EmptyTypes .

        Consultingwerk.Util.WidgetHelper:AbsolutePositionOnWindow (phWidget,
                                                                   OUTPUT deX,
                                                                   OUTPUT deY) .


        oControl = CAST (poControlType:GetConstructor(oType):Invoke(?),
                         System.Windows.Forms.Control) .

        oControl:Tag = STRING (phWidget) .

        oControl:Name = phWidget:NAME .
        oControl:Enabled = phWidget:SENSITIVE .
        oControl:Visible = phWidget:VISIBLE AND phWidget:PARENT:VISIBLE .

        IF CAN-QUERY (phWidget, "LABEL":U) THEN
            oControl:TEXT = phWidget:LABEL .

&IF DEFINED (Infragistics) NE 0 &THEN
        IF TYPE-OF (oControl, Infragistics.Win.Misc.UltraButton) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 26.10.2014
               Respect FOCUS setting of ABL Button Widget */
            CAST (oControl, Infragistics.Win.Misc.UltraButton):AcceptsFocus = NOT phWidget:NO-FOCUS .

            CAST (oControl, Infragistics.Win.Misc.UltraButton):ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton .

            CAST (oControl, Infragistics.Win.Misc.UltraButton):Click:Subscribe (Consultingwerk.Util.WidgetHelper:ClickHandler) .

            oControl:Tag = SUBSTITUTE ("&1,&2":U,
                                       phWidget,
                                       plPostMessage) .

            IF phWidget:IMAGE > "":U THEN DO:
                ASSIGN oImage = ?  .

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oReplacedWidgetCustomizer) THEN
                    ASSIGN oImage = oReplacedWidgetCustomizer:LoadCustomImage (oControl, phWidget) .
&ENDIF
                IF NOT VALID-OBJECT (oImage) THEN
                    ASSIGN oImage = Consultingwerk.Util.WidgetHelper:LoadImage (phWidget:IMAGE) .

                IF VALID-OBJECT (oImage) THEN DO:
                    IF NOT VALID-OBJECT (CAST (oControl, Infragistics.Win.Misc.UltraButton):Appearance) THEN
                        CAST (oControl, Infragistics.Win.Misc.UltraButton):Appearance = NEW Infragistics.Win.Appearance () .

                    CAST (oControl, Infragistics.Win.Misc.UltraButton):Appearance:Image = oImage .
                    CAST (oControl, Infragistics.Win.Misc.UltraButton):Appearance:ImageHAlign = Infragistics.Win.HAlign:Center .
                    CAST (oControl, Infragistics.Win.Misc.UltraButton):Appearance:ImageVAlign = Infragistics.Win.VAlign:Middle .

                    CAST (oControl, Infragistics.Win.Misc.UltraButton):ImageSize = NEW System.Drawing.Size (oImage:Width, oImage:Height) .
                    CAST (oControl, Infragistics.Win.Misc.UltraButton):ShowOutline = FALSE .
                    CAST (oControl, Infragistics.Win.Misc.UltraButton):ShowFocusRect = FALSE .
                END.
            END.
        END.
&ENDIF

        oControl:Size = NEW System.Drawing.Size (phWidget:WIDTH-PIXELS, phWidget:HEIGHT-PIXELS) .
        oControl:Location = NEW System.Drawing.Point (deX - piColOffset,
                                                      deY - piRowOffset) .

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oReplacedWidgetCustomizer) THEN
            oReplacedWidgetCustomizer:AssignControlProperties (oControl,
                                                               phWidget) .
&ENDIF

        poParentControl:Controls:Add (oControl) .
        oControl:BringToFront () .

&IF DEFINED (Infragistics) NE 0 &THEN
        IF TYPE-OF (oControl, Infragistics.Win.Misc.UltraButton) THEN DO:
            IF phWidget:WINDOW = phWidget THEN DO:
                poParentControl:FindForm():AcceptButton = CAST (oControl, Infragistics.Win.Misc.UltraButton) .
                oControl:Focus () .
            END.
        END.
&ENDIF
        RETURN oControl.

    END METHOD.

    /**
     * Purpose: Overlays one widget with a .NET Controls of the
     *          given type (i.e. BUTTON Widget -> UltraButton Control).
     * Notes:
     * @param phWidget The handle of the widget to replace
     * @param poParentControl The target control container
     * @param poControlType The System.Type to overlay the widgets with
     * @param piRowOffset The row offset used by the parent Form (see EmbeddedWindowForm)
     * @param piColOffset The column offset used by the parent Form (see EmbeddedWindowForm)
     * @return The reference to the generated Control
     */
    METHOD PUBLIC STATIC System.Windows.Forms.Control ReplaceWidget (phWidget           AS HANDLE,
                                                                     poParentControl    AS System.Windows.Forms.Control,
                                                                     poControlType      AS System.Type,
                                                                     piRowOffset        AS INTEGER,
                                                                     piColOffset        AS INTEGER):

        WidgetHelper:ReplaceWidget (phWidget,
                                    poParentControl,
                                    poControlType,
                                    piRowOffset,
                                    piColOffset,
                                    TRUE) .

    END METHOD .

    /**
     * Purpose: Overlays widgets of the given type with .NET Controls of the
     *          given type (i.e. BUTTON Widget -> UltraButton Control).
     * Notes:
     * @param phParentFieldGroup The parent fieldgroup to locate widgets
     * @param poParentControl The target control container
     * @param pcWidgetType The type of widgets to replace
     * @param poControlType The System.Type to overlay the widgets with
     * @param piRowOffset The row offset used by the parent Form (see EmbeddedWindowForm)
     * @param piColOffset The column offset used by the parent Form (see EmbeddedWindowForm)
     */
    METHOD PUBLIC STATIC VOID ReplaceWidgets (phParentFieldGroup AS HANDLE,
                                              poParentControl    AS System.Windows.Forms.Control,
                                              pcWidgetType       AS CHARACTER,
                                              poControlType      AS System.Type,
                                              piRowOffset        AS INTEGER,
                                              piColOffset        AS INTEGER):

        WidgetHelper:ReplaceWidgets (phParentFieldGroup,
                                     poParentControl,
                                     pcWidgetType,
                                     poControlType,
                                     piRowOffset,
                                     piColOffset,
                                     TRUE) .
    END METHOD .

    /**
     * Purpose: Overlays widgets of the given type with .NET Controls of the
     *          given type (i.e. BUTTON Widget -> UltraButton Control).
     * Notes:
     * @param phParentFieldGroup The parent fieldgroup to locate widgets
     * @param poParentControl The target control container
     * @param pcWidgetType The type of widgets to replace
     * @param poControlType The System.Type to overlay the widgets with
     * @param piRowOffset The row offset used by the parent Form (see EmbeddedWindowForm)
     * @param piColOffset The column offset used by the parent Form (see EmbeddedWindowForm)
     * @param plPostMessage When True then the original button will receive a BM_CLICK Message using the PostMessage API, otherwise APPLY CHOOSE is used
     */
    METHOD PUBLIC STATIC VOID ReplaceWidgets (phParentFieldGroup AS HANDLE,
                                              poParentControl    AS System.Windows.Forms.Control,
                                              pcWidgetType       AS CHARACTER,
                                              poControlType      AS System.Type,
                                              piRowOffset        AS INTEGER,
                                              piColOffset        AS INTEGER,
                                              plPostMessage      AS LOGICAL):

        DEFINE VARIABLE hWidget  AS HANDLE                       NO-UNDO.

        ASSIGN hWidget = phParentFieldGroup:FIRST-CHILD  .

        DO WHILE VALID-HANDLE (hWidget):

            IF hWidget:TYPE = "FRAME":U THEN DO:
                ReplaceWidgets (hWidget:FIRST-CHILD,
                                poParentControl,
                                pcWidgetType,
                                poControlType,
                                piRowOffset,
                                piColOffset,
                                plPostMessage) .

                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF hWidget:TYPE <> pcWidgetType THEN DO:
                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            ReplaceWidget (hWidget,
                           poParentControl,
                           poControlType,
                           piRowOffset,
                           piColOffset,
                           plPostMessage) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

    END METHOD.
&ENDIF

    /**
     * Purpose: Assigns a Background color to a Widget - only when the color is
     *          different from the current background color
     * Notes:   This is useful to prevent flickering that occurs when frequently
     *          setting a background color to a widget that was already set
     * @param phWidget The handle of the widget
     * @param piColor The background color to be set
     */
    METHOD PUBLIC STATIC VOID SetBackgroundColor (phWidget AS HANDLE,
                                                  piColor AS INTEGER):

        HandleAssert:CanSet (phWidget, "BGCOLOR":U) .

        IF phWidget:BGCOLOR <> piColor THEN
            phWidget:BGCOLOR = piColor .

    END METHOD.

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     * Notes:   Can also be invoked for the window
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER):

        WidgetHelper:SetFrameBackgroundColor (NEW SetFrameBackgroundColorParameter (phParent,
                                                                                    piColor)) .

    END METHOD.

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     * @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
     * @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER,
                                                       piBrowseColor AS INTEGER,
                                                       piTextboxColor AS INTEGER):
        SetFrameBackgroundColor (phParent,
                                 piColor,
                                 piBrowseColor,
                                 piTextboxColor,
                                 piTextboxColor,
                                 ?,
                                 ?,  /* Togglebox */
                                 ?,  /* Radioset */
                                 FALSE).

    END METHOD.

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     * @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
     * @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
     * @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER,
                                                       piBrowseColor AS INTEGER,
                                                       piTextboxColor AS INTEGER,
                                                       piDisabledtextColor AS INTEGER):

        SetFrameBackgroundColor (phParent,
                                 piColor,
                                 piBrowseColor,
                                 piTextboxColor,
                                 piDisabledtextColor,
                                 ?,
                                 ?,  /* Togglebox */
                                 ?,  /* Radioset */
                                 FALSE).

    END METHOD .

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     * @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
     * @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
     * @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
     * @param piBrowseColumnColor The color number to be used for BROWSE widgets LABEL-BGCOLOR
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER,
                                                       piBrowseColor AS INTEGER,
                                                       piTextboxColor AS INTEGER,
                                                       piDisabledtextColor AS INTEGER,
                                                       piBrowseColumnColor AS INTEGER):

        SetFrameBackgroundColor (phParent,
                                 piColor,
                                 piBrowseColor,
                                 piTextboxColor,
                                 piDisabledtextColor,
                                 piBrowseColumnColor,
                                 ?,  /* Togglebox */
                                 ?,  /* Radioset */
                                 FALSE).

    END METHOD.

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     * @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
     * @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
     * @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
     * @param piBrowseColumnColor The color number to be used for BROWSE widgets LABEL-BGCOLOR
     * @param piToggleBoxColor The color number to be used for TOGGLE-BOX widgets BGCOLOR
     * @param piRadiosetColor The color number to be used for RADIO-SET widgets BGCOLOR
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER,
                                                       piBrowseColor AS INTEGER,
                                                       piTextboxColor AS INTEGER,
                                                       piDisabledtextColor AS INTEGER,
                                                       piBrowseColumnColor AS INTEGER,
                                                       piToggleBoxColor AS INTEGER,
                                                       piRadiosetColor AS INTEGER):

        WidgetHelper:SetFrameBackgroundColor (NEW SetFrameBackgroundColorParameter (phParent,
                                                                                    piColor,
                                                                                    piBrowseColor,
                                                                                    piTextboxColor,
                                                                                    piDisabledtextColor,
                                                                                    piBrowseColumnColor,
                                                                                    piToggleBoxColor,
                                                                                    piRadiosetColor,
                                                                                    FALSE)) .
    END METHOD.

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param phParent The parent FRAME or WINDOW
     * @param piColor The color number to be used for FRAME and WINDOW widgets BGCOLOR
     * @param piBrowseColor The color number to be used for BROWSE widgets BGCOLOR
     * @param piTextboxColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR
     * @param piDisabledtextColor The color number to be used for FILL-IN, EDITOR, COMBO-BOX widgets BGCOLOR when disabled
     * @param piBrowseColumnColor The color number to be used for BROWSE widgets LABEL-BGCOLOR
     * @param piToggleBoxColor The color number to be used for TOGGLE-BOX widgets BGCOLOR
     * @param piRadiosetColor The color number to be used for RADIO-SET widgets BGCOLOR
     * @param plColorLiterals When True LITERAL or TEXT widgets BGCOLOR will be set to piColor as well
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (phParent AS HANDLE,
                                                       piColor AS INTEGER,
                                                       piBrowseColor AS INTEGER,
                                                       piTextboxColor AS INTEGER,
                                                       piDisabledtextColor AS INTEGER,
                                                       piBrowseColumnColor AS INTEGER,
                                                       piToggleBoxColor AS INTEGER,
                                                       piRadiosetColor AS INTEGER,
                                                       plColorLiterals AS LOGICAL):

        WidgetHelper:SetFrameBackgroundColor (NEW SetFrameBackgroundColorParameter (phParent,
                                                                                    piColor,
                                                                                    piBrowseColor,
                                                                                    piTextboxColor,
                                                                                    piDisabledtextColor,
                                                                                    piBrowseColumnColor,
                                                                                    piToggleBoxColor,
                                                                                    piRadiosetColor,
                                                                                    plColorLiterals)) .

    END METHOD .

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param poParameter The ISetFrameBackgroundColorParameter with the parameter values for this method
     */
    METHOD PUBLIC STATIC VOID SetFrameBackgroundColor (poParameter AS ISetFrameBackgroundColorParameter):

        IF NOT VALID-HANDLE (poParameter:PARENT) THEN
            RETURN .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF poParameter:LockWindowUpdate = TRUE AND VALID-HANDLE (poParameter:Parent:Window) THEN
            Consultingwerk.Windows.API.Win32:LockWindowUpdate (poParameter:Parent:Window:HWND) .
&ENDIF

        WidgetHelper:SetFrameBackgroundColorInternal (poParameter) .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF poParameter:LockWindowUpdate = TRUE AND VALID-HANDLE (poParameter:Parent:Window) THEN
            Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
&ENDIF
    END METHOD .

    /**
     * Purpose: Changes the background color of the frame and all contained frames
     *          and optionally the browse widgets
     * Notes:   Can also be invoked for the window, with the piBrowseColor = ? then
     *          the browse widgets won't be changed
     * @param poParameter The ISetFrameBackgroundColorParameter with the parameter values for this method
     */
    METHOD PRIVATE STATIC VOID SetFrameBackgroundColorInternal (poParameter AS ISetFrameBackgroundColorParameter):

        DEFINE VARIABLE hChild AS HANDLE NO-UNDO.

        IF (poParameter:Parent:TYPE = "DIALOG-BOX":U OR
            poParameter:Parent:TYPE = "FRAME":U OR
            poParameter:Parent:TYPE = "WINDOW":U) AND
            poParameter:Parent:BGCOLOR <> poParameter:Color THEN

            poParameter:Parent:BGCOLOR = poParameter:Color .

        ASSIGN hChild = IF CAN-QUERY (poParameter:Parent, "FIRST-CHILD":U) THEN poParameter:Parent:FIRST-CHILD
                        ELSE ?.

        widgetLoop: DO WHILE VALID-HANDLE (hChild):
            IF CAN-QUERY (hChild, "FIRST-CHILD":U) AND VALID-HANDLE (hChild:FIRST-CHILD) THEN
                Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColorInternal (NEW SetFrameBackgroundColorParameter (hChild,
                                                                                                                        poParameter)) .

&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oSetBackgroundColorFilter) THEN DO:
                IF oSetBackgroundColorFilter:FilterWidgetForSetBackgroundColor (hChild,
                                                                                poParameter) = TRUE THEN .
                ELSE DO:
                    hChild = hChild:NEXT-SIBLING .
                    NEXT widgetLoop.
                END.
            END.
&ENDIF

            IF poParameter:ColorLiterals AND (hChild:TYPE = "LITERAL":U OR hChild:TYPE = "TEXT":U) AND
                hChild:BGCOLOR <> poParameter:Color THEN
                hChild:BGCOLOR = poParameter:Color .

            IF poParameter:BrowseColor >= 0 AND hChild:TYPE = "BROWSE":U AND
                hChild:BGCOLOR <> poParameter:BrowseColor THEN
                hChild:BGCOLOR = poParameter:BrowseColor .

            IF poParameter:BrowseColor >= 0 AND hChild:TYPE = "SELECTION-LIST":U AND
                hChild:BGCOLOR <> poParameter:BrowseColor THEN
                hChild:BGCOLOR = poParameter:BrowseColor .

            IF poParameter:BrowseColumnColor >= 0 AND hChild:TYPE = "BROWSE":U AND
                hChild:LABEL-BGCOLOR <> poParameter:BrowseColumnColor THEN
                hChild:LABEL-BGCOLOR = poParameter:BrowseColumnColor .

            /* Set the Smart Lookup's Read Only Widget Box colour >>> */
            IF (hChild:TYPE = "FILL-IN":U OR hChild:TYPE = "EDITOR":U) AND hChild:READ-ONLY THEN DO:

              IF hChild:BGCOLOR <> poParameter:DisabledtextColor THEN
                  hChild:BGCOLOR = poParameter:DisabledtextColor .

              hChild = hChild:NEXT-SIBLING .
              NEXT widgetLoop.
            END.

            IF poParameter:TextboxColor >= 0 AND
               (hChild:TYPE = "FILL-IN":U OR hChild:TYPE = "EDITOR":U OR hChild:TYPE = "COMBO-BOX":U) AND
               hChild:SENSITIVE AND
               (hChild:BGCOLOR = ? OR hChild:BGCOLOR = poParameter:TextboxColor OR hChild:BGCOLOR = poParameter:DisabledtextColor) AND
               hChild:FGCOLOR <> poParameter:TextboxColor AND /* avoid East Frisia national flag, http://en.wikipedia.org/wiki/Otto_Waalkes */
                hChild:BGCOLOR <> poParameter:TextboxColor THEN
                hChild:BGCOLOR = poParameter:TextboxColor .

            IF poParameter:DisabledtextColor >= 0 AND
               (hChild:TYPE = "FILL-IN":U OR hChild:TYPE = "EDITOR":U OR hChild:TYPE = "COMBO-BOX":U) AND
               NOT hChild:SENSITIVE AND
               (hChild:BGCOLOR = ? OR hChild:BGCOLOR = poParameter:TextboxColor OR hChild:BGCOLOR = poParameter:DisabledtextColor) AND
               hChild:FGCOLOR <> poParameter:TextboxColor AND /* avoid East Frisia national flag, http://en.wikipedia.org/wiki/Otto_Waalkes */
                hChild:BGCOLOR <> poParameter:DisabledtextColor THEN
                hChild:BGCOLOR = poParameter:DisabledtextColor .

            IF poParameter:ToggleBoxColor >= 0 AND hChild:TYPE = "TOGGLE-BOX":U AND
                hChild:BGCOLOR <> poParameter:ToggleBoxColor THEN
                hChild:BGCOLOR = poParameter:ToggleBoxColor .

            IF poParameter:RadiosetColor >= 0 AND hChild:TYPE = "RADIO-SET":U AND
                hChild:BGCOLOR <> poParameter:RadiosetColor THEN
                hChild:BGCOLOR = poParameter:RadiosetColor .

            hChild = hChild:NEXT-SIBLING .
        END.

    END METHOD .

    /**
     * Purpose: Shows a MESSAGE with properties of the widget
     * Notes:
     * @param phWidget The handle of the widget
     */
    METHOD PUBLIC STATIC VOID ShowWidgetProperties (phWidget AS HANDLE):

        WidgetHelper:ShowWidgetProperties (phWidget, ?) .

    END METHOD .

    /**
     * Purpose: Shows a MESSAGE with properties of the widget
     * Notes:
     * @param phWidget The handle of the widget
     * @param pcTitle The title of the message
     */
    METHOD PUBLIC STATIC VOID ShowWidgetProperties (phWidget AS HANDLE,
                                                    pcTitle AS CHARACTER):

        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cText       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAttributes AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAttribute  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE    NO-UNDO .

        IF NOT VALID-HANDLE (phWidget) THEN DO:
            IF CharacterType:IsNullOrEmpty (pcTitle) THEN
                ASSIGN pcTitle = "Invalid widget"{&TRAN} .

            MESSAGE "<invalid widget handle>"{&TRAN}
                VIEW-AS ALERT-BOX TITLE pcTitle .
            RETURN .
        END.

        IF pcTitle > "":U THEN
            ASSIGN pcTitle = SUBSTITUTE ("&1 - &2":U, pcTitle, phWidget:TYPE) .
        ELSE
            ASSIGN pcTitle = phWidget:TYPE .

        ASSIGN cAttributes = ListHelper:SortList(LIST-QUERY-ATTRS (phWidget)) .

        loop: DO i = 1 TO NUM-ENTRIES (cAttributes):

            ASSIGN cAttribute = ENTRY (i, cAttributes) .

            /* Mike Fechner, Consultingwerk Ltd. 13.07.2015
               Need to verify as some methods called as attributes (like SERIALIZE-ROW on the buffer object handle)
               may kill the AVM (call #00318812)  */
            IF NOT WidgetAttributes:IsValid (cAttribute) THEN
                NEXT loop .

            DO ON ERROR UNDO, THROW:
                CREATE CALL hCall.

                ASSIGN hCall:IN-HANDLE = phWidget
                       hCall:CALL-TYPE = GET-ATTR-CALL-TYPE
                       hCall:CALL-NAME = cAttribute .

                hCall:INVOKE () NO-ERROR .

                IF ERROR-STATUS:NUM-MESSAGES > 0 THEN
                    NEXT loop .

                ASSIGN cText = cText + SUBSTITUTE ("&1 = &2~n":U, cAttribute, hCall:RETURN-VALUE) .

                @SuppressUnusedWarnings.
                CATCH err AS Progress.Lang.Error:
                    NEXT loop .
                END CATCH.

                FINALLY:
                    Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hCall) .
                END FINALLY.
            END.
        END.

        MESSAGE TRIM (cText, "~n":U)
            VIEW-AS ALERT-BOX TITLE pcTitle.

    END METHOD .

    /**
     * Purpose: Returns a comma delimited list of all the side-label-handles in a FRAME
     * Notes:
     * @param phFrame The handle of the FRAME to return the side label handles for
     * @return A comma delimited list of all side-label handles in the FRAME
     */
    METHOD PUBLIC STATIC CHARACTER SideLabelHandlesInFrame (phFrame AS HANDLE):

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        DEFINE VARIABLE cLabels AS CHARACTER NO-UNDO.

        hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:

            IF CAN-QUERY (hWidget, "SIDE-LABEL-HANDLE":U) AND
               VALID-HANDLE (hWidget:SIDE-LABEL-HANDLE)
                THEN
                    cLabels = cLabels + ",":U + STRING (hWidget:SIDE-LABEL-HANDLE) .

            FINALLY:
                IF VALID-HANDLE (hWidget) THEN
                    hWidget = hWidget:NEXT-SIBLING .
            END FINALLY.

        END.

        RETURN TRIM (cLabels, ",":U) .

    END METHOD .

    /**
     * Purpose: Determines if the inner widget is contained within the outer widget
     * Notes:
     * @param phOuter The handle of the outer widget
     * @param phInner The handle of the inner widget
     * @return Logical value indicating if the inner widget is located in the outer widget
     */
    METHOD PUBLIC STATIC LOGICAL WidgetContainedIn (phOuter AS HANDLE,
                                                    phInner AS HANDLE):

        RETURN phOuter:X < phInner:X AND phOuter:X + phOuter:WIDTH-PIXELS > phInner:X + phInner:WIDTH-PIXELS AND
               phOuter:Y < phInner:Y AND phOuter:Y + phOuter:HEIGHT-PIXELS > phInner:Y + phInner:HEIGHT-PIXELS .


    END METHOD .

    /**
     * Purpose: Returns a Label of a Widget, the Name - or <invalid> when the Widget
     *          is not valid
     * Notes:   Useful for debug messages
     * @param phWidget The handle of the Widget
     * @return The name of the Widget, the Lable or "<invalid>" when the Widget is not valid
     */
    METHOD PUBLIC STATIC CHARACTER WidgetLabel (phWidget AS HANDLE):

        IF NOT VALID-HANDLE (phWidget) THEN
            RETURN "<invalid>":U .

        IF CAN-QUERY (phWidget, "LABEL":U) AND phWidget:LABEL > "":U THEN
            RETURN phWidget:LABEL .

        IF phWidget:NAME > "":U THEN
            RETURN phWidget:NAME .

        RETURN "<no label>":U .

    END METHOD .

    /**
     * Purpose: Returns the Name of a Widget - or <invalid> when the Widget is not
     *          valid
     * Notes:   Useful for debug messages
     * @param phWidget The handle of the Widget
     * @return The name of the Widget or "<invalid>" when the Widget is not valid
     */
    METHOD PUBLIC STATIC CHARACTER WidgetName (phWidget AS HANDLE):

        IF VALID-HANDLE (phWidget) THEN
            RETURN phWidget:NAME .
        ELSE
            RETURN "<invalid>":U .

    END METHOD .

END CLASS.
