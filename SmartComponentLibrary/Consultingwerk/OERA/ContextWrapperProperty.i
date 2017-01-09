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
    File        : ContextWrapperProperty.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 20 01:09:21 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

    /*------------------------------------------------------------------------------
        Purpose: Get's a {1} property from the eContextProperties table
        Notes:
        @param pcPropertyName The name of the property
        @return The property value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC {1} Get{1}ContextProperty (pcPropertyName AS CHARACTER):


        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i SessionManager:ContextDataset "'Context Dataset'{&TRAN}"} .

        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eContextProperties":U) .

        ASSIGN hBuffer    = SessionManager:ContextDataset::eContextProperties
               cFieldName = "Value{1}":U .

        BufferAssert:HasField (hBuffer, cFieldName) .

        hBuffer:FIND-UNIQUE  (SUBSTITUTE ("WHERE eContextProperties.PropertyName = &1":U,
                              QUOTER (pcPropertyName))) NO-ERROR .

        IF NOT hBuffer:AVAILABLE THEN DO:
            hBuffer:BUFFER-CREATE () .
            hBuffer::PropertyName = pcPropertyName .
        END.

        RETURN hBuffer:BUFFER-FIELD (cFieldName):BUFFER-VALUE .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Set's a {1} property value in the eContextProperties table
        Notes:
        @param pcPropertyName The name of the property
        @param pxPropertyValue The value of the property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID Set{1}ContextProperty (pcPropertyName  AS CHARACTER,
                                                     pxPropertyValue AS {1}):


        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i SessionManager:ContextDataset "'Context Dataset'{&TRAN}"} .

        DatasetAssert:HasBuffer (SessionManager:ContextDataset,
                                 "eContextProperties":U) .

        ASSIGN hBuffer    = SessionManager:ContextDataset::eContextProperties
               cFieldName = "Value{1}":U .

        BufferAssert:HasField (hBuffer, cFieldName) .

        hBuffer:FIND-UNIQUE  (SUBSTITUTE ("WHERE eContextProperties.PropertyName = &1":U,
                              QUOTER (pcPropertyName))) NO-ERROR .

        IF NOT hBuffer:AVAILABLE THEN DO:
            hBuffer:BUFFER-CREATE () .
            hBuffer::PropertyName = pcPropertyName .
        END.

        hBuffer:BUFFER-FIELD (cFieldName):BUFFER-VALUE = pxPropertyValue .

    END METHOD.
