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
    
    DEFINE PUBLIC STATIC PROPERTY BLOB               AS CHARACTER NO-UNDO INIT "BLOB":U                 GET . 
    DEFINE PUBLIC STATIC PROPERTY CHARACTER          AS CHARACTER NO-UNDO INIT "CHARACTER":U            GET . 
    DEFINE PUBLIC STATIC PROPERTY CLOB               AS CHARACTER NO-UNDO INIT "CLOB":U                 GET . 
    DEFINE PUBLIC STATIC PROPERTY COM-HANDLE         AS CHARACTER NO-UNDO INIT "COM-HANDLE":U           GET . 
    DEFINE PUBLIC STATIC PROPERTY DATE               AS CHARACTER NO-UNDO INIT "DATE":U                 GET . 
    DEFINE PUBLIC STATIC PROPERTY DATETIME           AS CHARACTER NO-UNDO INIT "DATETIME":U             GET . 
    DEFINE PUBLIC STATIC PROPERTY DATETIME-TZ        AS CHARACTER NO-UNDO INIT "DATETIME-TZ":U          GET . 
    DEFINE PUBLIC STATIC PROPERTY DECIMAL            AS CHARACTER NO-UNDO INIT "DECIMAL":U              GET . 
    DEFINE PUBLIC STATIC PROPERTY HANDLE             AS CHARACTER NO-UNDO INIT "HANDLE":U               GET . 
    DEFINE PUBLIC STATIC PROPERTY INT64              AS CHARACTER NO-UNDO INIT "INT64":U                GET . 
    DEFINE PUBLIC STATIC PROPERTY INTEGER            AS CHARACTER NO-UNDO INIT "INTEGER":U              GET . 
    DEFINE PUBLIC STATIC PROPERTY LOGICAL            AS CHARACTER NO-UNDO INIT "LOGICAL":U              GET . 
    DEFINE PUBLIC STATIC PROPERTY ProgressLangObject AS CHARACTER NO-UNDO INIT "Progress.Lang.Object":U GET .
    DEFINE PUBLIC STATIC PROPERTY RECID              AS CHARACTER NO-UNDO INIT "RECID":U                GET .
    DEFINE PUBLIC STATIC PROPERTY ROWID              AS CHARACTER NO-UNDO INIT "ROWID":U                GET .
    
END CLASS.
