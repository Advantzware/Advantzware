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
    File        : BufferHandleEventArgs
    Purpose     : General purpose event argument that passes a buffer handle
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 01 08:26:16 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.BufferHandleEventArgs
    INHERITS EventArgs:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Handle of the Buffer associated with the event
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BufferHandleEventArgs class
        Notes:
        @param phBufferHandle The handle of the buffer associated with the event
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BufferHandleEventArgs (phBufferHandle AS HANDLE):
        SUPER ().

        HandleAssert:WidgetType(phBufferHandle, WidgetTypeEnum:Buffer) .

        ASSIGN THIS-OBJECT:BufferHandle = phBufferHandle .

    END CONSTRUCTOR.

END CLASS.
