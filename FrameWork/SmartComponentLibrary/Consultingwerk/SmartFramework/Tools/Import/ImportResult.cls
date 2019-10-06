/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ImportResult
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 24 21:20:45 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.Import.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Tools.Import.ImportResult:

    /**
     * Purpose: Gets and sets the number of created records
     * Notes:
     */
    DEFINE PUBLIC PROPERTY CreatedRecords AS INTEGER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the number of modified records
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ModifiedRecords AS INTEGER NO-UNDO
    GET.
    SET.

END CLASS.
