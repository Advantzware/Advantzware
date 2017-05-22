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
    File        : RecordPositionEnum
    Purpose     : Enumeration for possible record position values
    Syntax      :
    Description : Used by the SmartDataAdapter:RecordPosition property
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 15:21:48 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH  .

CLASS Consultingwerk.SmartComponents.Enum.RecordPositionEnum:

    /*------------------------------------------------------------------------------
        Purpose: The query is positioned at the first record
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY FirstRecord AS CHARACTER NO-UNDO INITIAL "FirstRecord":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The query is positioned at the last record
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LastRecord AS CHARACTER NO-UNDO INITIAL "LastRecord":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The query is positioned not at the first record, not at the last
                 record. So somewhere in the middle of the result set
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NotFirstOrLast AS CHARACTER NO-UNDO INITIAL "NotFirstOrLast":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: No record is available
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NoRecordAvailable AS CHARACTER NO-UNDO INITIAL "NoRecordAvailable":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The query is positioned at the only record (First and Last)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY OnlyRecord AS CHARACTER NO-UNDO INITIAL "OnlyRecord":U
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RecordPositionEnum class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE RecordPositionEnum ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Validates a character against the list of valid RercordPositionEnum
                 values
        Notes:
        @param pcRecordPosition Character value of a record position value
        @return Logical value indicating if the Character value stands for a valid enum member
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsValid (pcRecordPosition AS CHARACTER):

        RETURN ListHelper:EntryIsInList (pcRecordPosition, "FirstRecord,LastRecord,NotFirstOrLast,NoRecordAvailable,OnlyRecord":U) .

    END METHOD.

END CLASS.
