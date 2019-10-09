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
    File        : AssignKeyFieldsEventArgs
    Purpose     : Event Argument class for the AssignKeyFields event of
                  the SmartDatasetAdapter class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 07 22:32:11 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*  FROM PROPATH .
USING System.EventArgs FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Base.AssignKeyFieldsEventArgs
    INHERITS System.EventArgs:

    /*------------------------------------------------------------------------------
        Purpose: Get's and sets if an event handler has actually assigned the
                 key fields and the SmartDatasetAdapter does not need to use it's
                 default values (based on GUID or random numbers, based on the
                 data type).
        Notes:   A handler of the AssignKeyFields event is supposed to set this property
                 to TRUE to signal that the SmartGroupCreateSource does not need to
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Assigned AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the new ProDataset buffer that should have the
                 key fields assigned
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the KeyFields (comma delimited) that need to be
                 assigned by the event handler
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyFields AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AssignKeyFieldsEventArgs property
        Notes:
        @param phBufferHandle The Buffer handle for the BufferHandle property
        @param pcKeyFields The key fields value for the KeyFields property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AssignKeyFieldsEventArgs (phBufferHandle AS HANDLE,
                                                 pcKeyFields AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:BufferHandle = phBufferHandle
               THIS-OBJECT:KeyFields    = pcKeyFields .

    END CONSTRUCTOR.

END CLASS.
