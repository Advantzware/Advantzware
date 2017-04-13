/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IoModeEnum
    Purpose     : Character Constant enumeration for the iomode parameter of
                  the Progress.Lang.ParameterList:SetParameter () method
                  (ABL 11.6 reflection)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 16 11:32:01 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.IoModeEnum:

    DEFINE PUBLIC STATIC PROPERTY Input                  AS CHARACTER NO-UNDO INITIAL "INPUT":U                     GET .
    DEFINE PUBLIC STATIC PROPERTY Output                 AS CHARACTER NO-UNDO INITIAL "OUTPUT":U                    GET .
    DEFINE PUBLIC STATIC PROPERTY InputOutput            AS CHARACTER NO-UNDO INITIAL "INPUT-OUTPUT":U              GET .
    DEFINE PUBLIC STATIC PROPERTY InputAppend            AS CHARACTER NO-UNDO INITIAL "OUTPUT APPEND":U             GET .
    DEFINE PUBLIC STATIC PROPERTY OutputBind             AS CHARACTER NO-UNDO INITIAL "OUTPUT BIND":U               GET .
    DEFINE PUBLIC STATIC PROPERTY InputByReference       AS CHARACTER NO-UNDO INITIAL "INPUT BY-REFERENCE":U        GET .
    DEFINE PUBLIC STATIC PROPERTY OutputByReference      AS CHARACTER NO-UNDO INITIAL "OUTPUT BY-REFERENCE":U       GET .
    DEFINE PUBLIC STATIC PROPERTY InputOutputByReference AS CHARACTER NO-UNDO INITIAL "INPUT-OUTPUT BY-REFERENCE":U GET .
    DEFINE PUBLIC STATIC PROPERTY InputBind              AS CHARACTER NO-UNDO INITIAL "INPUT BIND":U                GET .

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor for the IoModeEnum class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED IoModeEnum ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
