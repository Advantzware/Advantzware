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
    File        : MessageGroups
    Purpose     : Provides constant values with the names of the SmartFramework
                  message groups
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 01 15:28:35 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.MessageGroups:

    /**
     * Purpose: OpenEdge runtime error messages
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY OpenEdge AS CHARACTER NO-UNDO INIT "OpenEdge":U
    PUBLIC GET.
    PRIVATE SET.

    /**
     * Purpose: SmartFramework messages
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY SFR AS CHARACTER NO-UNDO INIT "SFR":U
    PUBLIC GET.
    PRIVATE SET.

    /**
     * Purpose: Standard validation messages
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY VALMSG AS CHARACTER NO-UNDO INIT "VALMSG":U
    PUBLIC GET.
    PRIVATE SET.

END CLASS.
