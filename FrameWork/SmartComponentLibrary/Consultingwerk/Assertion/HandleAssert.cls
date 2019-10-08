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
    File        : HandleAssert
    Purpose     : Static class that provide standarized validation routines
                  for (WIDGET-)HANDLE parameters.
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description : Routines in this class typically raise an error condition
                  when the condition is not met.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 28 13:05:38 CEST 2011
    Notes       : Static methods, throw error on failed test.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Assertion.HandleAssert:

    /**
     * Purpose: Disallow instance creation
     * Notes:
     */
    CONSTRUCTOR PRIVATE HandleAssert ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Validates that a (WIDGET-)HANDLE has the given property (writable)
     * Notes:
     * Throws: Consultingwerk.Exceptions.AssertException
     * @param phHandle The (WIDGET-)HANDLE to validate
     * @param pcProperty The name of the property
     */
    METHOD PUBLIC STATIC VOID CanQuery (phHandle AS HANDLE,
                                        pcProperty AS CHARACTER):

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phHandle} .

        IF NOT CAN-QUERY (phHandle, pcProperty) THEN
            UNDO, THROW NEW AssertException (SUBSTITUTE ("Property &1 can not be queried."{&TRAN},
                                                         pcProperty), 0) .

    END METHOD.

    /**
     * Purpose: Validates that a (WIDGET-)HANDLE has the given property (writable)
     * Notes:
     * Throws: Consultingwerk.Exceptions.AssertException
     * @param phHandle The (WIDGET-)HANDLE to validate
     * @param pcProperty The name of the property
     */
    METHOD PUBLIC STATIC VOID CanSet (phHandle AS HANDLE,
                                      pcProperty AS CHARACTER):

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phHandle} .

        IF NOT CAN-SET (phHandle, pcProperty) THEN
            UNDO, THROW NEW AssertException (SUBSTITUTE ("Property &1 can not be set."{&TRAN},
                                                         pcProperty), 0) .

    END METHOD.

    /**
     * Purpose: Validates a (WIDGET-)HANDLE and raises an AssertException
     *          when the widget is valid
     * Notes:
     * Throws: Consultingwerk.Assertion.AssertException
     * @param phHandle The (WIDGET-)HANDLE to validate
     */
    METHOD PUBLIC STATIC VOID InValidHandle (phHandle AS HANDLE):

        IF VALID-HANDLE (phHandle) THEN
            UNDO, THROW NEW AssertException ("Handle should not be valid."{&TRAN}, 0) .

    END METHOD.

    /**
     * Purpose: Validates a (WIDGET-)HANDLE and raises an AssertException
     *          when the widget is valid
     * Notes:
     * Throws: Consultingwerk.Assertion.AssertException
     * @param phHandle The (WIDGET-)HANDLE to validate
     * @param pcHandleDesc The optional description of the HANDLE used in the InvalidHandleException
     */
    METHOD PUBLIC STATIC VOID InValidHandle (phHandle AS HANDLE, pcHandleDesc AS CHARACTER):

        IF VALID-HANDLE (phHandle) THEN
            UNDO, THROW NEW AssertException (SUBSTITUTE ("Handle &1 should not be valid."{&TRAN}, pcHandleDesc), 0) .

    END METHOD.

    /**
     * Purpose: Validates a (WIDGET-)HANDLE and raises an InvalidHandleException
     *          when the widget is invalid
     * Notes:
     * Throws: Consultingwerk.Exceptions.InvalidHandleException
     * @param phHandle The (WIDGET-)HANDLE to validate
     */
    METHOD PUBLIC STATIC VOID ValidHandle (phHandle AS HANDLE):

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phHandle} .

    END METHOD.

    {Consultingwerk/Assertion/IsWidgetType.i AsyncRequest}
    {Consultingwerk/Assertion/IsWidgetType.i Browse}
    {Consultingwerk/Assertion/IsWidgetType.i BrowseColumn}
    {Consultingwerk/Assertion/IsWidgetType.i Buffer}
    {Consultingwerk/Assertion/IsWidgetType.i BufferField}
    {Consultingwerk/Assertion/IsWidgetType.i Button}
    {Consultingwerk/Assertion/IsWidgetType.i ComboBox}
    {Consultingwerk/Assertion/IsWidgetType.i ControlFrame}
    {Consultingwerk/Assertion/IsWidgetType.i DataRelation}
    {Consultingwerk/Assertion/IsWidgetType.i Dataset}
    {Consultingwerk/Assertion/IsWidgetType.i DataSource}
    {Consultingwerk/Assertion/IsWidgetType.i DialogBox}
    {Consultingwerk/Assertion/IsWidgetType.i Editor}
    {Consultingwerk/Assertion/IsWidgetType.i FillIn}
    {Consultingwerk/Assertion/IsWidgetType.i FieldGroup}
    {Consultingwerk/Assertion/IsWidgetType.i Frame}
    {Consultingwerk/Assertion/IsWidgetType.i Image}
    {Consultingwerk/Assertion/IsWidgetType.i Literal}
    {Consultingwerk/Assertion/IsWidgetType.i Menu}
    {Consultingwerk/Assertion/IsWidgetType.i MenuItem}
    {Consultingwerk/Assertion/IsWidgetType.i RadioSet}
    {Consultingwerk/Assertion/IsWidgetType.i Procedure}
    {Consultingwerk/Assertion/IsWidgetType.i Query}
    {Consultingwerk/Assertion/IsWidgetType.i Rectangle}
    {Consultingwerk/Assertion/IsWidgetType.i SaxAttributes}
    {Consultingwerk/Assertion/IsWidgetType.i SaxReader}
    {Consultingwerk/Assertion/IsWidgetType.i Server}
    {Consultingwerk/Assertion/IsWidgetType.i ServerSocket}
    {Consultingwerk/Assertion/IsWidgetType.i SelectionList}
    {Consultingwerk/Assertion/IsWidgetType.i Slider}
    {Consultingwerk/Assertion/IsWidgetType.i SoapFault}
    {Consultingwerk/Assertion/IsWidgetType.i SoapFaultDetail}
    {Consultingwerk/Assertion/IsWidgetType.i SoapHeader}
    {Consultingwerk/Assertion/IsWidgetType.i SoapHeaderEntryRef}
    {Consultingwerk/Assertion/IsWidgetType.i Socket}
    {Consultingwerk/Assertion/IsWidgetType.i Stream}
    {Consultingwerk/Assertion/IsWidgetType.i SubMenu}
    {Consultingwerk/Assertion/IsWidgetType.i TempTable}
    {Consultingwerk/Assertion/IsWidgetType.i Text}
    {Consultingwerk/Assertion/IsWidgetType.i ToggleBox}
    {Consultingwerk/Assertion/IsWidgetType.i Window}
    {Consultingwerk/Assertion/IsWidgetType.i XDocument}
    {Consultingwerk/Assertion/IsWidgetType.i XNodeRef}

    /**
     * Purpose: Validates a (WIDGET-)HANDLE and raises an InvalidHandleException
     *          when the widget is invalid
     * Notes:
     * Throws: Consultingwerk.Exceptions.InvalidHandleException
     * @param phHandle The (WIDGET-)HANDLE to validate
     * @param pcHandleDesc The optional description of the HANDLE used in the InvalidHandleException
     */
    METHOD PUBLIC STATIC VOID ValidHandle (phHandle AS HANDLE, pcHandleDesc AS CHARACTER):

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phHandle pcHandleDesc} .

    END METHOD.

    /**
     * Purpose: Validates the type of the given widget
     * Notes:
     * Throws:  Consultingwerk.Exceptions.InvalidTypeException or InvalidHandleException
     * @param phHandle The (WIDGET-)HANDLE to validate
     * @param pcType The expected Widget TYPE
     */
    METHOD PUBLIC STATIC VOID WidgetType (phHandle AS HANDLE, pcType AS CHARACTER):

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phHandle pcType}

    END METHOD.

END CLASS.
