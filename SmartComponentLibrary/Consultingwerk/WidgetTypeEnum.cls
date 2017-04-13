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
    File        : WidgetTypeEnum
    Purpose     : Widget TYPE Constants (CHARACTER)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 28 16:05:31 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

CLASS Consultingwerk.WidgetTypeEnum:

    DEFINE PUBLIC STATIC PROPERTY AsyncRequest       AS CHARACTER NO-UNDO INITIAL "ASYNC-REQUEST":U        GET .
    DEFINE PUBLIC STATIC PROPERTY Browse             AS CHARACTER NO-UNDO INITIAL "BROWSE":U               GET .
    DEFINE PUBLIC STATIC PROPERTY BrowseColumn       AS CHARACTER NO-UNDO INITIAL "BROWSE-COLUMN":U        GET .
    DEFINE PUBLIC STATIC PROPERTY Buffer             AS CHARACTER NO-UNDO INITIAL "BUFFER":U               GET .
    DEFINE PUBLIC STATIC PROPERTY BufferField        AS CHARACTER NO-UNDO INITIAL "BUFFER-FIELD":U         GET .
    DEFINE PUBLIC STATIC PROPERTY Button             AS CHARACTER NO-UNDO INITIAL "BUTTON":U               GET .
    DEFINE PUBLIC STATIC PROPERTY ComboBox           AS CHARACTER NO-UNDO INITIAL "COMBO-BOX":U            GET .
    DEFINE PUBLIC STATIC PROPERTY ControlFrame       AS CHARACTER NO-UNDO INITIAL "CONTROL-FRAME":U        GET .
    DEFINE PUBLIC STATIC PROPERTY DataRelation       AS CHARACTER NO-UNDO INITIAL "DATA-RELATION":U        GET .
    DEFINE PUBLIC STATIC PROPERTY Dataset            AS CHARACTER NO-UNDO INITIAL "DATASET":U              GET .
    DEFINE PUBLIC STATIC PROPERTY DataSource         AS CHARACTER NO-UNDO INITIAL "DATA-SOURCE":U          GET .
    DEFINE PUBLIC STATIC PROPERTY DialogBox          AS CHARACTER NO-UNDO INITIAL "DIALOG-BOX":U           GET .
    DEFINE PUBLIC STATIC PROPERTY Editor             AS CHARACTER NO-UNDO INITIAL "EDITOR":U               GET .
    DEFINE PUBLIC STATIC PROPERTY FillIn             AS CHARACTER NO-UNDO INITIAL "FILL-IN":U              GET .
    DEFINE PUBLIC STATIC PROPERTY FieldGroup         AS CHARACTER NO-UNDO INITIAL "FIELD-GROUP":U          GET .
    DEFINE PUBLIC STATIC PROPERTY Frame              AS CHARACTER NO-UNDO INITIAL "FRAME":U                GET .
    DEFINE PUBLIC STATIC PROPERTY Image              AS CHARACTER NO-UNDO INITIAL "IMAGE":U                GET .
    DEFINE PUBLIC STATIC PROPERTY Literal            AS CHARACTER NO-UNDO INITIAL "LITERAL":U              GET .
    DEFINE PUBLIC STATIC PROPERTY Menu               AS CHARACTER NO-UNDO INITIAL "MENU":U                 GET .
    DEFINE PUBLIC STATIC PROPERTY MenuItem           AS CHARACTER NO-UNDO INITIAL "MENU-ITEM":U            GET .
    DEFINE PUBLIC STATIC PROPERTY RadioSet           AS CHARACTER NO-UNDO INITIAL "RADIO-SET":U            GET .
    DEFINE PUBLIC STATIC PROPERTY Procedure          AS CHARACTER NO-UNDO INITIAL "PROCEDURE":U            GET .
    DEFINE PUBLIC STATIC PROPERTY Query              AS CHARACTER NO-UNDO INITIAL "QUERY":U                GET .
    DEFINE PUBLIC STATIC PROPERTY Rectangle          AS CHARACTER NO-UNDO INITIAL "RECTANGLE":U            GET .
    DEFINE PUBLIC STATIC PROPERTY SaxAttributes      AS CHARACTER NO-UNDO INITIAL "SAX-ATTRIBUTES":U       GET .
    DEFINE PUBLIC STATIC PROPERTY SaxReader          AS CHARACTER NO-UNDO INITIAL "SAX-READER":U           GET .
    DEFINE PUBLIC STATIC PROPERTY Server             AS CHARACTER NO-UNDO INITIAL "SERVER":U               GET .
    DEFINE PUBLIC STATIC PROPERTY ServerSocket       AS CHARACTER NO-UNDO INITIAL "SERVER-SOCKET":U        GET .
    DEFINE PUBLIC STATIC PROPERTY SelectionList      AS CHARACTER NO-UNDO INITIAL "SELECTION-LIST":U       GET .
    DEFINE PUBLIC STATIC PROPERTY Slider             AS CHARACTER NO-UNDO INITIAL "SLIDER":U               GET .
    DEFINE PUBLIC STATIC PROPERTY SoapFault          AS CHARACTER NO-UNDO INITIAL "SOAP-FAULT":U           GET .
    DEFINE PUBLIC STATIC PROPERTY SoapFaultDetail    AS CHARACTER NO-UNDO INITIAL "SOAP-FAULT-DETAIL":U    GET .
    DEFINE PUBLIC STATIC PROPERTY SoapHeader         AS CHARACTER NO-UNDO INITIAL "SOAP-HEADER":U          GET .
    DEFINE PUBLIC STATIC PROPERTY SoapHeaderEntryRef AS CHARACTER NO-UNDO INITIAL "SOAP-HEADER-ENTRYREF":U GET .
    DEFINE PUBLIC STATIC PROPERTY Socket             AS CHARACTER NO-UNDO INITIAL "SOCKET":U               GET .
    DEFINE PUBLIC STATIC PROPERTY Stream             AS CHARACTER NO-UNDO INITIAL "STREAM":U               GET .
    DEFINE PUBLIC STATIC PROPERTY SubMenu            AS CHARACTER NO-UNDO INITIAL "SUB-MENU":U             GET .
    DEFINE PUBLIC STATIC PROPERTY TempTable          AS CHARACTER NO-UNDO INITIAL "TEMP-TABLE":U           GET .
    DEFINE PUBLIC STATIC PROPERTY Text               AS CHARACTER NO-UNDO INITIAL "TEXT":U                 GET .
    DEFINE PUBLIC STATIC PROPERTY ToggleBox          AS CHARACTER NO-UNDO INITIAL "TOGGLE-BOX":U           GET .
    DEFINE PUBLIC STATIC PROPERTY Window             AS CHARACTER NO-UNDO INITIAL "WINDOW":U               GET .
    DEFINE PUBLIC STATIC PROPERTY XDocument          AS CHARACTER NO-UNDO INITIAL "X-DOCUMENT":U           GET .
    DEFINE PUBLIC STATIC PROPERTY XNodeRef           AS CHARACTER NO-UNDO INITIAL "X-NODEREF":U            GET .

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WidgetTypeEnum ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.