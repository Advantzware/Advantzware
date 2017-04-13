/**********************************************************************
 * Copyright (C) 2006-2017 by Consultingwerk Ltd. ("CW") -            *
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
    File        : RepositoryObjectExporter
    Purpose     : Exports repository object to XML files
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 01 23:10:24 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                        FROM PROPATH .
USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.SmartFramework.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*              FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Object.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Tools.*             FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Repository.Tools.RepositoryObjectExporter:

    {Consultingwerk/SmartFramework/Repository/Object/Export/dsObjectMaster.i}
    {Consultingwerk/SmartFramework/System/dsModifiedState.i &SUFFIX=_mod}

    /**
     * Purpose: Dumps all repository objects in the given module
     * Notes:
     * @param pcBaseFolder The base folder to dump repository objects to
     * @param pcModule The name of the product module to dump from
     * @param plResetModifiedStatus Logical flag indicating if the modified flag should be removed
     */
    METHOD PUBLIC VOID DumpRepositoryObjectsByModule (pcBaseFolder AS CHARACTER,
                                                      pcModule AS CHARACTER,
                                                      plResetModifiedStatus AS LOGICAL):

        DEFINE VARIABLE oModule  AS ModuleDatasetModel NO-UNDO .
        DEFINE VARIABLE hDataset AS HANDLE             NO-UNDO .
        DEFINE VARIABLE hQuery   AS HANDLE             NO-UNDO .

        oModule = NEW ModuleDatasetModel () .

        oModule:SmartModule:Filter:ModuleName:EQ (pcModule):Run () .

        BufferAssert:IsAvailable (oModule:SmartModule) .

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U,
                                    NEW FetchDataRequest ("eSmartObjectMaster":U,
                                                          SUBSTITUTE ("for each eSmartObjectMaster where eSmartObjectMaster.ModuleGuid = &1":U, QUOTER (oModule:SmartModule:ModuleGuid)),
                                                          0),
                                    OUTPUT DATASET-HANDLE hDataset) .

        hQuery = QueryHelper:CreatePreparedQuery (hDataset::eSmartObjectMaster) .

        DO WHILE NOT hQuery:QUERY-OFF-END:

            THIS-OBJECT:DumpRepositoryObject (hDataset::eSmartObjectMaster::ObjectMasterGuid,
                                              pcBaseFolder,
                                              plResetModifiedStatus) .
            hQuery:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hDataset) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Dumps all modified repository objects
     * Notes:
     * @param pcBaseFolder The base folder to dump repository objects to
     * @param plResetModifiedStatus Logical flag indicating if the modified flag should be removed
     */
    METHOD PUBLIC VOID DumpModifiedRepositoryObjects (pcBaseFolder AS CHARACTER,
                                                      plResetModifiedStatus AS LOGICAL):

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.ModifiedStateBusinessEntity":U,
                                    NEW FetchDataRequest ("*":U, 0),
                                    OUTPUT DATASET dsModifiedState_mod) .

        FOR EACH eSmartModifiedState_mod ON ERROR UNDO, THROW:

            THIS-OBJECT:DumpRepositoryObject (eSmartModifiedState_mod.ModifiedStateKeyFieldValues,
                                              pcBaseFolder,
                                              plResetModifiedStatus) .
        END.

        FINALLY:
            DATASET dsModifiedState_mod:EMPTY-DATASET () .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Dumps the given Repository Object
     * Notes:
     * @param pcObjectMasterGuid The object master guid to dump
     * @param pcBaseFolder The base folder to dump repository objects to
     * @param plResetModifiedStatus Logical flag indicating if the modified flag should be removed
     */
    METHOD PUBLIC VOID DumpRepositoryObject (pcObjectMasterGuid AS CHARACTER,
                                             pcBaseFolder AS CHARACTER,
                                             plResetModifiedStatus AS LOGICAL):

        DEFINE VARIABLE oModule          AS ModuleDatasetModel        NO-UNDO .
        DEFINE VARIABLE oModified        AS ModifiedStateDatasetModel NO-UNDO .
        DEFINE VARIABLE oMessageProvider AS IMessageProvider          NO-UNDO .
        DEFINE VARIABLE cPath            AS CHARACTER                 NO-UNDO .

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity":U,
                                    NEW FetchDataRequest ("*":U,
                                                          SUBSTITUTE ("for each eSmartObjectMaster where eSmartObjectMaster.ObjectMasterGuid = &1":U, QUOTER (pcObjectMasterGuid)),
                                                          0),
                                    OUTPUT DATASET dsObjectMaster) .

        FIND FIRST eSmartObjectMaster NO-ERROR .

        IF NOT AVAILABLE eSmartObjectMaster THEN
            UNDO,THROW NEW Exception ("Invalid Object Master Guid."{&TRAN}, 0) .

        IF SESSION:BATCH-MODE THEN
            MESSAGE "Dumping repository object:":U eSmartObjectMaster.ObjectName .

        oModule = NEW ModuleDatasetModel (eSmartObjectMaster.ModuleGuid) .

        IF oModule:SmartModule:ModuleDataFolderWhenAvailable("":U) > "":U THEN DO:

            IF pcBaseFolder = "":U THEN
                ASSIGN pcBaseFolder = ".":U .

            ASSIGN cPath = SUBSTITUTE ("&1/&2":U,
                                       pcBaseFolder,
                                       oModule:SmartModule:ModuleDataFolder) .

            IF FileHelper:Exists (cPath) = FALSE THEN
                FileHelper:CreateDirectory(cPath) .

            DATASET dsObjectMaster:WRITE-XML ("file":U,
                                              SUBSTITUTE ("&1/&2/&3.smartrepo":U,
                                                          pcBaseFolder,
                                                          oModule:SmartModule:ModuleDataFolder,
                                                          eSmartObjectMaster.ObjectName),
                                              TRUE, ?, ?) .

            IF plResetModifiedStatus THEN DO:
                oModified = NEW ModifiedStateDatasetModel() .
                oModified:SmartModifiedState:Filter:ModifiedStateTable:EQ ("SmartObjectMaster":U) .
                oModified:SmartModifiedState:Filter:ModifiedStateKeyFieldValues:EQ (pcObjectMasterGuid):Run () .

                IF oModified:SmartModifiedState:Available THEN DO:
                    oModified:TrackingChanges = TRUE .
                    oModified:SmartModifiedState:Delete() .
                    oModified:SaveChanges() .
                END.
            END.
        END.
        ELSE DO:
            oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .

            IF oModule:SmartModule:Available THEN DO:
                IF VALID-OBJECT (oMessageProvider) THEN
                    UNDO, THROW NEW MissingModuleDataFolderException (oMessageProvider:GetMessageText("SFR":U, 404, oModule:SmartModule:ModuleName),
                                                                      404,
                                                                      oModule:SmartModule:ModuleName) .
                ELSE
                    UNDO, THROW NEW MissingModuleDataFolderException (oModule:SmartModule:ModuleName) .
            END.
            ELSE
                UNDO, THROW NEW Exception ("Invalid Module for Object to dump."{&TRAN}, 0) .
        END.

        FINALLY:
            ErrorHelper:ResetErrorStatus() .
            DATASET dsObjectMaster:EMPTY-DATASET () .
        END FINALLY.

    END METHOD.

END CLASS.
