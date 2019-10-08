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
    File        : RecordIdentifier
    Purpose     : A structure that can holds a ROWID as well as a buffer
                  name
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 03 15:24:44 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.RecordIdentifier
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Database Name component
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Buffer or Table Name component
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Rowid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Rowid AS ROWID NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the RowState
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RowState AS INTEGER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordIdentifier class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordIdentifier ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordIdentifier class
        Notes:
        @param pcDatabaseName The database name
        @param pcBufferName The buffer or table name
        @param proRowid The rowid
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordIdentifier (pcDatabaseName AS CHARACTER,
                                         pcBufferName AS CHARACTER,
                                         proRowid AS ROWID):
        THIS-OBJECT ().

        ASSIGN
            THIS-OBJECT:DatabaseName = pcDatabaseName
            THIS-OBJECT:BufferName   = pcBufferName
            THIS-OBJECT:Rowid        = proRowid .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordIdentifier class
        Notes:
        @param pcDatabaseName The database name
        @param pcBufferName The buffer or table name
        @param proRowid The rowid
        @param piRowState The RowState value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordIdentifier (pcDatabaseName AS CHARACTER,
                                         pcBufferName AS CHARACTER,
                                         proRowid AS ROWID,
                                         piRowState AS INTEGER):
        THIS-OBJECT ().

        ASSIGN
            THIS-OBJECT:DatabaseName = pcDatabaseName
            THIS-OBJECT:BufferName   = pcBufferName
            THIS-OBJECT:Rowid        = proRowid
            THIS-OBJECT:RowState     = piRowState .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordIdentifier class
        Notes:
        @param pcBufferName The buffer or table name
        @param proRowid The rowid
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordIdentifier (pcBufferName AS CHARACTER,
                                         proRowid AS ROWID):
        THIS-OBJECT ().

        ASSIGN
            THIS-OBJECT:BufferName   = pcBufferName
            THIS-OBJECT:Rowid        = proRowid .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordIdentifier class
        Notes:
        @param pcBufferName The buffer or table name
        @param proRowid The rowid
        @param piRowState The RowState value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RecordIdentifier (pcBufferName AS CHARACTER,
                                         proRowid AS ROWID,
                                         piRowState AS INTEGER):
        THIS-OBJECT ().

        ASSIGN
            THIS-OBJECT:BufferName   = pcBufferName
            THIS-OBJECT:Rowid        = proRowid
            THIS-OBJECT:RowState     = piRowState .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a string that represents the current object.
        Notes:
        @return A string that represents the current object.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():

        IF THIS-OBJECT:DatabaseName > "":U THEN
            RETURN SUBSTITUTE ("Database Name: &1&4Buffer Name: &2&4Rowid: &3"{&TRAN},
                               THIS-OBJECT:DatabaseName,
                               THIS-OBJECT:BufferName,
                               THIS-OBJECT:Rowid,
                               "~n":U) .
        ELSE
            RETURN SUBSTITUTE ("Buffer Name: &1&3Rowid: &2"{&TRAN},
                               THIS-OBJECT:BufferName,
                               THIS-OBJECT:Rowid,
                               "~n":U) .
    END METHOD .

END CLASS.
