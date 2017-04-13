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
    File        : RelationDatasetController
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.04.2013 21:42:53
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.OERA.*                           FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*       FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Windows.Forms.*                          FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.SmartFramework.System.RelationDatasetController
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF :

    { Consultingwerk/SmartFramework/System/dsRelation.i }
    { Consultingwerk/SmartFramework/System/dsMissingRelations.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller
        Notes:   Interface IDatasetController member
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RelationDatasetController class
        Notes:   Assigns the DatasetHandle property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RelationDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsRelation:HANDLE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object detaching from the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Imports missing relations form the database
        Notes:   Shows an import Dialog an allows the user to select relations to be
                 created
    ------------------------------------------------------------------------------*/
    &IF DEFINED (DotNetAccessible) NE 0 &THEN
    METHOD PUBLIC VOID ImportFromDatabase ():

        DEFINE VARIABLE oForm         AS GetDatabaseNameForm                  NO-UNDO .
        DEFINE VARIABLE oForm2        AS SelectRelationsToImportForm          NO-UNDO .
        DEFINE VARIABLE oResult       AS GetDatabaseNamesResult               NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult                         NO-UNDO .
        DEFINE VARIABLE hDataset      AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oParameter    AS GetMissingDatabaseRelationsParameter NO-UNDO .

        oResult = NEW GetDatabaseNamesResult () .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.System.TableBusinessEntity":U,
                                                       "GetDatabaseNames":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oResult) .

        IF VALID-HANDLE (hDataset) THEN
            DELETE OBJECT hDataset .

        ASSIGN hDataset = ? .

        oForm = NEW GetDatabaseNameForm () .
        oForm:DatabaseNames = oResult:DatabaseNames .
        oForm:Text = "Import Relations form Database Schema"{&TRAN} .

        WAIT-FOR oForm:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        IF CharacterType:IsNullOrEmpty (oForm:DatabaseName) THEN
            RETURN .

        ASSIGN oParameter = NEW GetMissingDatabaseRelationsParameter (oForm:DatabaseName) .

        DATASET dsMissingRelations:EMPTY-DATASET () .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.System.RelationBusinessEntity":U,
                                                       "GetMissingDatabaseRelations":U,
                                                       INPUT-OUTPUT DATASET dsMissingRelations,
                                                       oParameter) .

        oForm2 = NEW SelectRelationsToImportForm () .
        oForm2:TempTableAdapter:TempTableHandle = TEMP-TABLE ttRelation:HANDLE .
        oForm2:TempTableAdapter:RetrieveData () .

        WAIT-FOR oForm2:ShowDialog() SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        oForm2:GetDataset (OUTPUT DATASET-HANDLE hDataset) .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.System.RelationBusinessEntity":U,
                                                       "CreateMissingDatabaseRelations":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       oParameter) .

        SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT):RetrieveData () .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD.
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object attaching to the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

END CLASS.
