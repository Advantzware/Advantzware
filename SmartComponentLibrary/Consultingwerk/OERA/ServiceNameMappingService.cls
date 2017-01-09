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
    File        : ServiceNameMappingService
    Purpose     : Default implementation of IServiceNameMappingService
                  Interface. Supports the mapping of the business service
                  name (e.g. Business Entity or Business Task) to the
                  actual class name
    Syntax      :
    Description : Supports mapping of short names to actual class names
                  (SCL-1171)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 04 22:31:04 CET 2016
    Notes       : Reads service mapping from a file .servicenamemapping
                  From Consultingwerk/OERA/ttServiceNameMapping.i
                  Supports custom log file messages using the "ServiceNameMappingService"
                  custom message sub system
                  Performs automatic mapping based on the "autoServiceNameMapping"
                  entry in the .applicationsettings/.restapplicationsettings
                  file, e.g.
                  "autoServiceNameMapping": "Consultingwerk\\SmartFramework\\*BusinessEntity.r,Consultingwerk\\SmartComponentsDemo\\*BusinessEntity.r",
                  "autoServiceNameMappingThrowOnInvalidFolder": "false",
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Framework.*  FROM PROPATH.
USING Consultingwerk.OERA.*       FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceNameMappingService
    IMPLEMENTS IServiceNameMappingService:

    {Consultingwerk/OERA/ttServiceNameMapping.i}
    {Consultingwerk/Util/TempTables/ttFileNames.i}

    /**
     * Purpose: Constructor for the ServiceNameMappingService class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ServiceNameMappingService ():

        DEFINE VARIABLE cFileName AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oConfig   AS IConfigurationProvider NO-UNDO .

        SUPER ().

        ASSIGN cFileName = FileHelper:FindFile (".servicenamemapping":U) .

        IF cFileName > "":U THEN
            TEMP-TABLE ttServiceNameMapping:READ-XML ("file":U, cFileName, ?, ?, ?) .

        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Loading service name mappings~n":U +
                                            "###    File Name:          &1~n":U +
                                            "###    Absolute File Name: &2~n":U +
                                            "########################################################################~n":U,
                                            ".servicenamemapping":U,
                                            cFileName), "ServiceNameMappingService":U) .

        oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfig) AND oConfig:GetValue ("autoServiceNameMapping":U) > "":U THEN
            THIS-OBJECT:AutoMapServiceNames (oConfig:GetValue ("autoServiceNameMapping":U),
                                             oConfig:GetValue ("autoServiceNameMappingThrowOnInvalidFolder":U)) .

    END CONSTRUCTOR.

    /**
     * Purpose: Performs automatic mapping of Business Entity file names to
     *          Short names
     * Notes:   See SCL1592
     * @param pcMapping The value of the autoServiceNameMapping setting (IConfigurationProvider)
     * @param pcThrowOnInvalidDirectory Throw errors on invalid directory entries
     */
    METHOD PROTECTED VOID AutoMapServiceNames (pcMapping AS CHARACTER,
                                               pcThrowOnInvalidDirectory AS CHARACTER):

        DEFINE VARIABLE i                        AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cMappingEntry            AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cDirectory               AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFullDirectory           AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFileSpec                AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cClassName               AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cShortName               AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oClass                   AS Progress.Lang.Class NO-UNDO .
        DEFINE VARIABLE lCanFind                 AS LOGICAL             NO-UNDO .
        DEFINE VARIABLE lThrowOnInvalidDirectory AS LOGICAL             NO-UNDO INIT FALSE .

        IF pcThrowOnInvalidDirectory > "":U THEN
            ASSIGN lThrowOnInvalidDirectory = DataTypeHelper:ToLogical(pcThrowOnInvalidDirectory) .

        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Auto Mapping Business Services~n":U +
                                            "###    Mapping: &1~n":U +
                                            "########################################################################~n":U,
                                            pcMapping), "ServiceNameMappingService":U) .

        mappingEntryLoop:
        DO i = 1 TO NUM-ENTRIES (pcMapping):

            ASSIGN cMappingEntry  = REPLACE (ENTRY (i, pcMapping), "~\":U, "/":U)

                   cDirectory     = SUBSTRING (cMappingEntry, 1, R-INDEX (cMappingEntry, "/":U) - 1)
                   cFileSpec      = SUBSTRING (cMappingEntry, R-INDEX (cMappingEntry, "/":U) + 1)
                   cFullDirectory = FileHelper:FindFile(cDirectory)
                   .

            IF cFullDirectory = ? THEN DO:
                IF lThrowOnInvalidDirectory THEN
                    UNDO, THROW NEW Exception (SUBSTITUTE ("Invalid directory entry &1 in autoServiceNameMapping":U,
                                                           cMappingEntry), 0) .

                LogManager:WriteMessage(SUBSTITUTE ("### Invalid Directory &1 in mapping entry &2":U,
                                                    cDirectory,
                                                    cMappingEntry), "ServiceNameMappingService":U) .
                LogManager:WriteMessage("### Skipping entry":U, "ServiceNameMappingService":U) .
                LogManager:WriteMessage("########################################################################":U,
                                        "ServiceNameMappingService":U) .

                NEXT mappingEntryLoop.
            END.

            LogManager:WriteMessage(SUBSTITUTE ("### Searching in Directory &1 (&2) for &3":U,
                                                cDirectory,
                                                cFullDirectory,
                                                cFileSpec), "ServiceNameMappingService":U) .

            FileHelper:GetFileListNoDotNet(cFullDirectory, cFileSpec, OUTPUT TABLE ttFileNames) .

            FOR EACH ttFileNames ON ERROR UNDO, THROW:

                ASSIGN cClassName = ClassHelper:FileNameToClassName (ttFileNames.FileName)
                       oClass     = Progress.Lang.Class:GetClass (cClassName) .

                IF oClass:IsA ("Consultingwerk.OERA.IBusinessService":U) THEN DO:

                    ASSIGN cShortName = ENTRY (NUM-ENTRIES (cClassName, ".":U), cClassName, ".":U)
                           lCanFind   = CAN-FIND (ttServiceNameMapping WHERE ttServiceNameMapping.SourceServiceName = cShortName).

                    LogManager:WriteMessage(SUBSTITUTE ("### &1 ==> &2 &3":U,
                                                        cClassName,
                                                        cShortName,
                                                        STRING (lCanFind, "(already registered)/ ":U)), "ServiceNameMappingService":U) .

                    IF NOT lCanFind THEN DO:
                        CREATE ttServiceNameMapping .
                        ASSIGN ttServiceNameMapping.MappedServiceName = cClassName
                               ttServiceNameMapping.SourceServiceName = cShortName .
                        RELEASE ttServiceNameMapping .
                    END.
                END.

            END.

            LogManager:WriteMessage("########################################################################":U,
                                    "ServiceNameMappingService":U) .
        END.

    END METHOD.

    /**
     * Purpose: Returns the actual business service name for the providede class
     *          name
     * Notes:
     * @param pcServiceName The service name as provided by the consumer
     * @return The name of the actual business service implementing class
     */
    METHOD PUBLIC CHARACTER GetBusinessServiceName (pcServiceName AS CHARACTER):

        IF NOT CAN-FIND (ttServiceNameMapping WHERE ttServiceNameMapping.SourceServiceName = pcServiceName) THEN
            RETURN pcServiceName .

        FIND ttServiceNameMapping WHERE ttServiceNameMapping.SourceServiceName = pcServiceName  .

        RETURN ttServiceNameMapping.MappedServiceName .

    END METHOD .

END CLASS.
