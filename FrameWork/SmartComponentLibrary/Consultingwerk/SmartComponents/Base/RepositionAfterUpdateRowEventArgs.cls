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
    File        : RepositionAfterUpdateRowEventArgs
    Purpose     : Argument for the RepositionAfterUpdateRow event of the
                  SmartDatasetAdapter and SmartBusinessEntityAdapter. Allows
                  customized handling of repositioning to the updated row
                  (e.g. open the query with different arguments), allows to
                  prevent the error on repositioning when e.g. the query
                  needs to be opened with a different query string to match
                  the new row
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 01 11:55:23 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.RepositionAfterUpdateRowEventArgs
    INHERITS EventArgs:

    /*------------------------------------------------------------------------------
        Purpose: Returns if the row was created or not
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Created AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the query
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the event was handled by an subscriber and the
                 error should not be shown to the user
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ROWID that was attempted to reposition to
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Rowid AS ROWID NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RepositionAfterUpdateRowEventArgs class
        Notes:
        @param phQuery The handle of the QUERY
        @param proRowid The ROWID of the record that was attempted to reposition to
        @param plCreated Logical value indicating if the row was created
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositionAfterUpdateRowEventArgs (phQuery AS HANDLE,
                                                          proRowid AS ROWID,
                                                          plCreated AS LOGICAL):
        SUPER ().

        ASSIGN THIS-OBJECT:QueryHandle = phQuery
               THIS-OBJECT:Rowid       = proRowid
               THIS-OBJECT:Created     = plCreated .

    END CONSTRUCTOR.

END CLASS.
