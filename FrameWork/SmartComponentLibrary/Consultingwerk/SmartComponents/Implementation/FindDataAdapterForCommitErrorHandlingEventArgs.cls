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
    File        : FindDataAdapterForCommitErrorHandlingEventArgs
    Purpose     : Argument for the FindDataAdapterForCommitErrorHandling
                  event of the SmartBusinessEntityAdapter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 09 10:11:09 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.FindDataAdapterForCommitErrorHandlingEventArgs
    INHERITS EventArgs:

    /**
     * Purpose: Gets and sets the reference to the SmartDatasetAdapter
     * Notes:   Defaults to the Adapter that was found by the SmartBusinessEntityAdapter
     *          might be set to another adapter instance, e.g. when a child
     *          form had to be reopened first
     */
    DEFINE PUBLIC PROPERTY Adapter AS SmartDatasetAdapter NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the Record Identifier identifying
     *          the record that caused the commit error
     * Notes:
     */
    DEFINE PUBLIC PROPERTY RecordIdentifier AS RecordIdentifier NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the FindDataAdapterForCommitErrorHandlingEventArgs class
     * Notes:
     * @param poAdapter The reference to the SmartDatasetAdapter located by the SmartBusinessEntityAdapter (may be unknown)
     * @param poRecordIdentifier The reference to the Record Identifier
     */
    CONSTRUCTOR PUBLIC FindDataAdapterForCommitErrorHandlingEventArgs (poAdapter AS SmartDatasetAdapter,
                                                                       poRecordIdentifier AS RecordIdentifier):

        ASSIGN THIS-OBJECT:Adapter          = poAdapter
               THIS-OBJECT:RecordIdentifier = poRecordIdentifier .

    END CONSTRUCTOR.

END CLASS.
