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
    File        : DataTypeEnum
    Purpose     : DATA-TYPE Constants (CHARACTER)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 07 23:43:36 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.DataTypeEnum:

    DEFINE PUBLIC STATIC PROPERTY BLOB               AS CHARACTER NO-UNDO INITIAL "BLOB":U                 GET .
    DEFINE PUBLIC STATIC PROPERTY CHARACTER          AS CHARACTER NO-UNDO INITIAL "CHARACTER":U            GET .
    DEFINE PUBLIC STATIC PROPERTY CLOB               AS CHARACTER NO-UNDO INITIAL "CLOB":U                 GET .
    DEFINE PUBLIC STATIC PROPERTY COM-HANDLE         AS CHARACTER NO-UNDO INITIAL "COM-HANDLE":U           GET .
    DEFINE PUBLIC STATIC PROPERTY DATE               AS CHARACTER NO-UNDO INITIAL "DATE":U                 GET .
    DEFINE PUBLIC STATIC PROPERTY DATETIME           AS CHARACTER NO-UNDO INITIAL "DATETIME":U             GET .
    DEFINE PUBLIC STATIC PROPERTY DATETIME-TZ        AS CHARACTER NO-UNDO INITIAL "DATETIME-TZ":U          GET .
    DEFINE PUBLIC STATIC PROPERTY DECIMAL            AS CHARACTER NO-UNDO INITIAL "DECIMAL":U              GET .
    DEFINE PUBLIC STATIC PROPERTY HANDLE             AS CHARACTER NO-UNDO INITIAL "HANDLE":U               GET .
    DEFINE PUBLIC STATIC PROPERTY INT64              AS CHARACTER NO-UNDO INITIAL "INT64":U                GET .
    DEFINE PUBLIC STATIC PROPERTY INTEGER            AS CHARACTER NO-UNDO INITIAL "INTEGER":U              GET .
    DEFINE PUBLIC STATIC PROPERTY LOGICAL            AS CHARACTER NO-UNDO INITIAL "LOGICAL":U              GET .
    DEFINE PUBLIC STATIC PROPERTY ProgressLangObject AS CHARACTER NO-UNDO INITIAL "Progress.Lang.Object":U GET .
    DEFINE PUBLIC STATIC PROPERTY RAW                AS CHARACTER NO-UNDO INITIAL "RAW":U                  GET .
    DEFINE PUBLIC STATIC PROPERTY RECID              AS CHARACTER NO-UNDO INITIAL "RECID":U                GET .
    DEFINE PUBLIC STATIC PROPERTY ROWID              AS CHARACTER NO-UNDO INITIAL "ROWID":U                GET .

END CLASS.
