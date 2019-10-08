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
    File        : DatasetAssert
    Purpose     : Provides generic methods for validating of ProDataset object handles
    Syntax      :
    Description : Routines in this class typically raise an error condition
                  when the condition is not met.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 20 00:20:12 CEST 2011
    Notes       : Static methods, throw error on failed test.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Assertion.DatasetAssert:

    /**
     * Purpose: Disallow instance creation
     * Notes:
     */
    CONSTRUCTOR PRIVATE DatasetAssert ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Verifies that the passed in ProDataset handle has a buffer with the
     *          given name
     * Notes:
     * Throws:  Consultingwerk.Assertion.AssertException
     * @param phDataset The handle of the ProDataset
     * @param pcBufferName The name of the Buffer that should exist
     */
    METHOD PUBLIC STATIC VOID HasBuffer (phDataset AS HANDLE,
                                         pcBufferName AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset WidgetTypeEnum:Dataset} .

        hBuffer = phDataset:GET-BUFFER-HANDLE (pcBufferName) NO-ERROR .

        IF NOT VALID-HANDLE (hBuffer) THEN
            UNDO, THROW NEW AssertException (SUBSTITUTE ("The dataset has &1 has no buffer &2."{&TRAN},
                                                         phDataset:NAME,
                                                         pcBufferName),
                                             0) .

    END METHOD.

END CLASS.
