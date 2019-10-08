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
    File        : RepositoryObjectImporter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : mikef
    Created     : Mon Jan 02 11:23:37 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Framework.Session.*                FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Tools.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*            FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Tools.RepositoryObjectImporter:

    {Consultingwerk/SmartFramework/Repository/Object/Export/dsObjectMaster.i}

    /**
     * Purpose: Loads an SmartObjectMaster and related data from a .smartrepo file
     * Notes:
     * @param pcFileName The name of the .smartrepo file to import
     * @param plSetModifiedStatus Logical value indicating if the imported object should be set as modified
     */
    METHOD PUBLIC VOID LoadRepositoryObject (pcFileName AS CHARACTER,
                                             plSetModifiedStatus AS LOGICAL):

        DEFINE VARIABLE cFileName           AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE hDataset            AS HANDLE                         NO-UNDO .
        DEFINE VARIABLE oModified           AS ModifiedStateDatasetModel      NO-UNDO .
        DEFINE VARIABLE oConfigurationValue AS ConfigurationValueDatasetModel NO-UNDO .
        DEFINE VARIABLE dtFileModDateTime   AS DATETIME                       NO-UNDO .

        FileAssert:Exists (pcFileName) .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        ASSIGN cFileName         = FILE-INFORMATION:FULL-PATHNAME
               dtFileModDateTime = DATETIME (FILE-INFORMATION:FILE-MOD-DATE, FILE-INFORMATION:FILE-MOD-TIME * 1000) .

        oConfigurationValue = ConfigurationValueDatasetModel:ByName ("NewestRepositoryObjectImport":U) .

        IF NOT oConfigurationValue:SmartConfigurationValue:Available THEN DO:
            oConfigurationValue:TrackingChanges = TRUE .
            oConfigurationValue:SmartConfigurationValue:Create().
            oConfigurationValue:SmartConfigurationValue:ConfigurationValueName     = "NewestRepositoryObjectImport":U .
            oConfigurationValue:SmartConfigurationValue:ConfigurationValueDateTime = DATETIME (FILE-INFORMATION:FILE-MOD-DATE, FILE-INFORMATION:FILE-MOD-TIME * 1000) .
            oConfigurationValue:SaveChanges().
        END.
        ELSE IF dtFileModDateTime > oConfigurationValue:SmartConfigurationValue:ConfigurationValueDateTime THEN DO:
            oConfigurationValue:TrackingChanges = TRUE .
            oConfigurationValue:SmartConfigurationValue:ConfigurationValueDateTime = DATETIME (FILE-INFORMATION:FILE-MOD-DATE, FILE-INFORMATION:FILE-MOD-TIME * 1000) .
            oConfigurationValue:SaveChanges().
        END.

        CREATE DATASET hDataset .
        hDataset:CREATE-LIKE (DATASET dsObjectMaster:HANDLE) .

        hDataset:READ-XML ("file":U, cFileName, "EMPTY":U, ?, ?) .

        hDataset::eSmartObjectMaster:FIND-FIRST () .

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity":U,
                                    NEW FetchDataRequest ("*":U,
                                                          SUBSTITUTE ("for each eSmartObjectMaster where eSmartObjectMaster.ObjectMasterGuid = &1":U,
                                                                      QUOTER (hDataset::eSmartObjectMaster::ObjectMasterGuid)),
                                                          0),
                                    OUTPUT DATASET dsObjectMaster) .

        DatasetHelper:SetTrackingChanges(DATASET dsObjectMaster:HANDLE, TRUE) .

        THIS-OBJECT:SynchronizeData (hDataset::eSmartObjectMaster:HANDLE,   BUFFER eSmartObjectMaster:HANDLE,   "ObjectMasterGuid":U) .
        THIS-OBJECT:SynchronizeData (hDataset::eSmartObjectInstance:HANDLE, BUFFER eSmartObjectInstance:HANDLE, "ObjectInstanceGuid":U) .
        THIS-OBJECT:SynchronizeData (hDataset::eSmartLink:HANDLE,           BUFFER eSmartLink:HANDLE,           "LinkGuid":U) .
        THIS-OBJECT:SynchronizeData (hDataset::eSmartPage:HANDLE,           BUFFER eSmartPage:HANDLE,           "PageGuid":U) .
        THIS-OBJECT:SynchronizeData (hDataset::eSmartAttributeValue:HANDLE, BUFFER eSmartAttributeValue:HANDLE, "AttributeValueGuid":U) .

        ServiceInterface:SaveChanges ("Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity":U,
                                      INPUT-OUTPUT DATASET dsObjectMaster) .

        DatasetHelper:ThrowDatasetErrors(DATASET dsObjectMaster:HANDLE) .

        IF plSetModifiedStatus THEN DO:
            hDataset::eSmartObjectMaster:FIND-FIRST () .
            hDataset::eSmartModifiedState:FIND-FIRST () NO-ERROR .

            oModified = NEW ModifiedStateDatasetModel() .
            oModified:SmartModifiedState:Filter:ModifiedStateTable:EQ ("SmartObjectMaster":U) .
            oModified:SmartModifiedState:Filter:ModifiedStateKeyFieldValues:EQ (hDataset::eSmartObjectMaster::ObjectMasterGuid):Run () .

            oModified:TrackingChanges = TRUE .

            IF NOT oModified:SmartModifiedState:Available THEN DO:
                oModified:SmartModifiedState:Create() .
                oModified:SmartModifiedState:ModifiedStateTable          = "SmartObjectMaster":U .
                oModified:SmartModifiedState:ModifiedStateKeyFieldValues = hDataset::eSmartObjectMaster::ObjectMasterGuid .
            END.

            IF hDataset::eSmartModifiedState:AVAILABLE THEN DO:
                oModified:SmartModifiedState:ModifiedStateTimeStamp = hDataset::eSmartModifiedState::ModifiedStateTimeStamp .
                oModified:SmartModifiedState:ModifiedStateUser      = hDataset::eSmartModifiedState::ModifiedStateUser .
            END .
            ELSE DO:
                oModified:SmartModifiedState:ModifiedStateTimeStamp = NOW .
                oModified:SmartModifiedState:ModifiedStateUser      = SessionManager:UserName .
            END.

            oModified:SaveChanges() .
        END.

        FINALLY:
            ErrorHelper:ResetErrorStatus() .
            GarbageCollectorHelper:DeleteObject (hDataset) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Synchronizes the records in the source and target buffer
     * Notes:
     * @param phSourceBuffer The handle of the source buffer (from the .smartrepo file)
     * @param phTargetBuffer The handle of the target buffer (current repository data)
     * @param pcKeyField The name of the primary unique key field of the buffer
     */
    METHOD PROTECTED VOID SynchronizeData (phSourceBuffer AS HANDLE,
                                           phTargetBuffer AS HANDLE,
                                           pcKeyField AS CHARACTER):

        DEFINE VARIABLE hQuery1 AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hQuery2 AS HANDLE    NO-UNDO .

        /* Synchronize from source to target */
        hQuery1 = QueryHelper:CreatePreparedQuery (phSourceBuffer) .

        DO WHILE NOT hQuery1:QUERY-OFF-END:
            phTargetBuffer:FIND-UNIQUE (SUBSTITUTE ("where &1 = &2":U,
                                                    pcKeyField,
                                                    QUOTER (phSourceBuffer:BUFFER-FIELD (pcKeyField):BUFFER-VALUE))) NO-ERROR .

            IF NOT phTargetBuffer:AVAILABLE THEN
                phTargetBuffer:BUFFER-CREATE () .

            phTargetBuffer:BUFFER-COPY (phSourceBuffer) .

            hQuery1:GET-NEXT () .
        END.

        /* Delete records in target table that are not present in the source table */
        hQuery2 = QueryHelper:CreatePreparedQuery (phTargetBuffer) .

        DO WHILE NOT hQuery2:QUERY-OFF-END:
            phSourceBuffer:FIND-UNIQUE (SUBSTITUTE ("where &1 = &2":U,
                                                    pcKeyField,
                                                    QUOTER (phTargetBuffer:BUFFER-FIELD (pcKeyField):BUFFER-VALUE))) NO-ERROR .

            IF NOT phSourceBuffer:AVAILABLE THEN
                phTargetBuffer:BUFFER-DELETE () .

            hQuery2:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hQuery1) .
        END FINALLY.

    END METHOD.

END CLASS.
