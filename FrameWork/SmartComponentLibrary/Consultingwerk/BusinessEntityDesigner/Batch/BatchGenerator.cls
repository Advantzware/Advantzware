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
    File        : BatchGenerator
    Purpose     : Generates 1 Temp-Table Business Entities for a set
                  of database tables, useful in prototyping scenarios
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 02 16:06:20 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*   FROM PROPATH .
USING Consultingwerk.OERA.*                             FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Batch.BatchGenerator:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }

    /**
     * Purpose: Raised when the status of this component changes
     * Notes:
     * @param sender The object that raised the StatusChanged event
     * @param e The ProcessingStatusChangedEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT StatusChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS ProcessingStatusChangedEventArgs).

    /**
     * Purpose: Builds the name of a Business Entity
     * Notes:
     * @param plCamelCase When true, the first letter of the table name will be forced to uppercase for the Business Entity Name
     * @param pcTableName The name of the database table name
     * @return The base business Entity Name
     */
    METHOD PROTECTED CHARACTER BuildBusinessEntityName (plCamelcase AS LOGICAL,
                                                        pcTableName AS CHARACTER):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO .

        ASSIGN cTableName = ENTRY (NUM-ENTRIES (pcTableName, ".":U), pcTableName, ".":U) .

        IF plCamelcase THEN
            ASSIGN SUBSTRING (cTableName, 1, 1, "CHARACTER":U) = UPPER (SUBSTRING (cTableName, 1, 1, "CHARACTER":U)) .

        RETURN cTableName .
    END METHOD.

    /**
     * Purpose: Generates a list of Business Entities
     * Notes:
     * @param pcPackage The source package for the generated business entities
     * @param plCreateSubpackage Create distinct sub-package per table
     * @param pcBusinessEntityNamePattern The name pattern for the Business Entity class name, using &1 for the Table name
     * @param pcDataAccessNamePattern The name pattern for the Data Access class name, using &1 for the Table name
     * @param pcDatasetControllerNamePattern The name pattern for the Dataset Controller class name, using &1 for the Table name
     * @param plCamelCase When true, the first letter of the table name will be forced to uppercase for the Business Entity Name
     * @param pcDatabase The database name to generate business entities for
     * @param pcTableNames The name pattern (using the CAN-DO function) the database tables need to match for being generated
     * @param plGenerateDatasetModel Generate DatasetModel classes along with the Business Entity
     */
    METHOD PUBLIC VOID GenerateBusinessEntities (pcPackage AS CHARACTER,
                                                 plCreateSubpackage AS LOGICAL,
                                                 pcBusinessEntityNamePattern AS CHARACTER,
                                                 pcDataAccessNamePattern AS CHARACTER,
                                                 pcDatasetControllerNamePattern AS CHARACTER,
                                                 plCamelcase AS LOGICAL,
                                                 pcDatabase AS CHARACTER,
                                                 pcTableNames AS CHARACTER,
                                                 plGenerateDatasetModel AS LOGICAL):

        DEFINE VARIABLE hDatabaseDataset    AS HANDLE                           NO-UNDO .
        DEFINE VARIABLE hQuery              AS HANDLE                           NO-UNDO .
        DEFINE VARIABLE cBusinessEntityName AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE cTableName          AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE e                   AS ProcessingStatusChangedEventArgs NO-UNDO .

        ServiceInterface:FetchData ("Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaBusinessEntity":U,
                                    ?,
                                    OUTPUT DATASET-HANDLE hDatabaseDataset) .

        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hDatabaseDataset::eTable,
                                                                      SUBSTITUTE ("FOR EACH eTable WHERE BusinessEntityName = ~"&1~"":U,
                                                                                  pcDatabase)) .

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            ASSIGN cTableName = hDatabaseDataset::eTable::TempTableName
                   cTableName = ENTRY (NUM-ENTRIES (cTableName, ".":U), cTableName, ".":U) .

            IF ListHelper:CanDo (pcTableNames, cTableName) THEN DO:

                ASSIGN e = NEW ProcessingStatusChangedEventArgs (SUBSTITUTE ("Processing table &1"{&TRAN}, hDatabaseDataset::eTable::TempTableName)) .

                THIS-OBJECT:OnStatusChanged (e) .

                IF VALID-OBJECT (e) AND e:Cancel THEN
                    RETURN .

                ASSIGN cBusinessEntityName = BuildBusinessEntityName (plCamelcase,
                                                                      hDatabaseDataset::eTable::TempTableName) .

                IF plCreateSubpackage THEN
                    FileHelper:CreateDirectory (REPLACE (SUBSTITUTE ("&1.&2":U, pcPackage, cTableName), ".":U, "~\":U)) .

                GenerateBusinessEntity (hDatabaseDataset,
                                        IF plCreateSubpackage THEN SUBSTITUTE ("&1.&2":U, pcPackage, cTableName)
                                                              ELSE pcPackage,
                                        cBusinessEntityName,
                                        pcBusinessEntityNamePattern,
                                        pcDataAccessNamePattern,
                                        pcDatasetControllerNamePattern,
                                        pcDatabase,
                                        hDatabaseDataset::eTable::TempTableName,
                                        plGenerateDatasetModel) .
            END.

            hQuery:GET-NEXT .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hDatabaseDataset) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Generates a list of Business Entities
     * Notes:
     * @param pcPackage The source package for the generated business entities
     * @param pcBusinessEntityNamePattern The name pattern for the Business Entity class name, using &1 for the Table name
     * @param pcDataAccessNamePattern The name pattern for the Data Access class name, using &1 for the Table name
     * @param pcDatasetControllerNamePattern The name pattern for the Dataset Controller class name, using &1 for the Table name
     * @param plCamelCase When true, the first letter of the table name will be forced to uppercase for the Business Entity Name
     * @param pcDatabase The database name to generate business entities for
     * @param pcTableNames The name pattern (using the CAN-DO function) the database tables need to match for being generated
     */
    METHOD PUBLIC VOID GenerateBusinessEntities (pcPackage AS CHARACTER,
                                                 pcBusinessEntityNamePattern AS CHARACTER,
                                                 pcDataAccessNamePattern AS CHARACTER,
                                                 pcDatasetControllerNamePattern AS CHARACTER,
                                                 plCamelcase AS LOGICAL,
                                                 pcDatabase AS CHARACTER,
                                                 pcTableNames AS CHARACTER):

        THIS-OBJECT:GenerateBusinessEntities (pcPackage,
                                              FALSE,
                                              pcBusinessEntityNamePattern,
                                              pcDataAccessNamePattern,
                                              pcDatasetControllerNamePattern,
                                              plCamelcase,
                                              pcDatabase,
                                              pcTableNames,
                                              FALSE).

    END METHOD .

    /**
     * Purpose: Generates a single Business Entity
     * Notes:
     * @param phDatasetDataset The handle to the database with the schema information
     * @param pcPackage The source package for the generated business entities
     * @param pcBusinessEntityName The base name for the Business Entity, Data Access and Dataset Controller class name
     * @param pcBusinessEntityNamePattern The name pattern for the Business Entity class name, using &1 for the Table name
     * @param pcDataAccessNamePattern The name pattern for the Data Access class name, using &1 for the Table name
     * @param pcDatasetControllerNamePattern The name pattern for the Dataset Controller class name, using &1 for the Table name
     * @param pcDatabase The database name to generate business entities for
     * @param pcTableName The name of the database tables to be generated
     * @param plGenerateDatasetModel Generate DatasetModel classes along with the Business Entity
     */
    METHOD PROTECTED VOID GenerateBusinessEntity (phDatasetDataset AS HANDLE,
                                                  pcPackage AS CHARACTER,
                                                  pcBusinessEntityName AS CHARACTER,
                                                  pcBusinessEntityNamePattern AS CHARACTER,
                                                  pcDataAccessNamePattern AS CHARACTER,
                                                  pcDatasetControllerNamePattern AS CHARACTER,
                                                  pcDatabase AS CHARACTER,
                                                  pcTableName AS CHARACTER,
                                                  plGenerateDatasetModel AS LOGICAL):

        DEFINE VARIABLE cPath        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.

        DEFINE VARIABLE oGenerator      AS IBusinessEntityGenerator  NO-UNDO .
        DEFINE VARIABLE oModelGenerator AS ModelClassGeneratorPlugin NO-UNDO .

        ASSIGN cPath = PathFromPackage (pcPackage) .

        DATASET dsBusinessEntity:EMPTY-DATASET () .

        CREATE eBusinessEntity .
        ASSIGN eBusinessEntity.BusinessEntityName        = SUBSTITUTE (pcBusinessEntityNamePattern, pcBusinessEntityName)
               eBusinessEntity.BusinessEntityPurpose     = SUBSTITUTE ("Generated Business Entity for &1":U, pcTableName)
               eBusinessEntity.BusinessEntityDescription = SUBSTITUTE ("Generated Business Entity for &1":U, pcTableName)
               eBusinessEntity.BusinessEntityPackage     = pcPackage
               eBusinessEntity.DataAccessName            = SUBSTITUTE (pcDataAccessNamePattern, pcBusinessEntityName)
               eBusinessEntity.DataAccessPackage         = pcPackage
               eBusinessEntity.DatasetControllerName     = SUBSTITUTE (pcDatasetControllerNamePattern, pcBusinessEntityName)
               eBusinessEntity.DatasetControllerPackage  = pcPackage
               eBusinessEntity.DatasetName               = SUBSTITUTE ("ds&1":U, pcBusinessEntityName)
               eBusinessEntity.DatasetPath               = cPath
               eBusinessEntity.DefaultTablePath          = cPath
               eBusinessEntity.DefaultTablePrefix        = BusinessEntityDesignerSettings:DefaultTablePrefix
               eBusinessEntity.DefaultTableSuffix        = BusinessEntityDesignerSettings:DefaultTableSuffix
               eBusinessEntity.DefaultBeforeTablePrefix  = BusinessEntityDesignerSettings:DefaultBeforeTablePrefix
               eBusinessEntity.DefaultBeforeTableSuffix  = BusinessEntityDesignerSettings:DefaultBeforeTableSuffix
            .

        ASSIGN hBuffer = phDatasetDataset::eTable .

        CREATE eTable .
        BUFFER eTable:BUFFER-COPY (hBuffer) .

        ASSIGN eTable.BusinessEntityName  = eBusinessEntity.BusinessEntityName
               eTable.EntityClassName     = ENTRY (NUM-ENTRIES (eTable.TempTableName, ".":U), eTable.TempTableName, ".":U)
               eTable.EntityPackageName   = pcPackage
               eTable.SourceTableNames    = ENTRY (NUM-ENTRIES (pcTableName, ".":U), pcTableName, ".":U)
               eTable.SourceBufferNames   = ENTRY (NUM-ENTRIES (pcTableName, ".":U), pcTableName, ".":U)
               eTable.TempTableName       = BusinessEntityDesignerSettings:DefaultTablePrefix +
                                            SUBSTRING (ENTRY (NUM-ENTRIES (eTable.SourceTableNames, ".":U), eTable.SourceTableNames, ".":U),
                                                       1,
                                                       32 - LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix, "CHARACTER":U) - LENGTH (BusinessEntityDesignerSettings:DefaultTableSuffix, "CHARACTER":U), "CHARACTER":U) +
                                            BusinessEntityDesignerSettings:DefaultTableSuffix
               eTable.TempTablePath       = cPath
               eTable.TempTableBeforeName = BusinessEntityDesignerSettings:DefaultBeforeTablePrefix +
                                            SUBSTRING (ENTRY (NUM-ENTRIES (eTable.SourceTableNames, ".":U), eTable.SourceTableNames, ".":U),
                                                       1,
                                                       32 - LENGTH (BusinessEntityDesignerSettings:DefaultBeforeTablePrefix, "CHARACTER":U) - LENGTH (BusinessEntityDesignerSettings:DefaultBeforeTableSuffix, "CHARACTER":U), "CHARACTER":U) +
                                            BusinessEntityDesignerSettings:DefaultBeforeTableSuffix
               eTable.LayoutColumn        = 50
               eTable.LayoutRow           = 50
               eTable.LayoutWidth         = 100
               eTable.LayoutHeight        = 400
            .

        ASSIGN hBuffer = phDatasetDataset::eField
               hQuery  = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer,
                                                                              SUBSTITUTE ("FOR EACH eField WHERE BusinessEntityName = ~"&1~" AND eField.TempTableName = ~"&2~"":U,
                                                                                          phDatasetDataset::eTable::BusinessEntityName,
                                                                                          phDatasetDataset::eTable::TempTableName)) .

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            CREATE eField.
            BUFFER eField:BUFFER-COPY (hBuffer) .

            ASSIGN eField.BusinessEntityName = eTable.BusinessEntityName
                   eField.TempTableName      = eTable.TempTableName
                   eField.FieldSource        = SUBSTITUTE ("&1.&2":U, ENTRY (NUM-ENTRIES (pcTableName, ".":U), pcTableName, ".":U), eField.FieldName)

                   /* Mike Fechner, Consultingwerk Ltd. 26.01.2013
                      Replace % with "Percent" as the % sign is not supported as a field name in most environments */
                   eField.FieldName          = REPLACE (eField.FieldName, "%":U, "Percent":U)
                   eField.FieldSerializeName = eField.FieldName
                   eField.FieldDataType      = UPPER (eField.FieldDataType)
                   .

            hQuery:GET-NEXT .
        END.

        ASSIGN hBuffer = phDatasetDataset::eIndex
               hQuery  = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer,
                                                                              SUBSTITUTE ("FOR EACH eIndex WHERE BusinessEntityName = ~"&1~" AND eIndex.TempTableName = ~"&2~"":U,
                                                                                          phDatasetDataset::eTable::BusinessEntityName,
                                                                                          phDatasetDataset::eTable::TempTableName)) .

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            CREATE eIndex.
            BUFFER eIndex:BUFFER-COPY (hBuffer) .

            ASSIGN eIndex.BusinessEntityName = eTable.BusinessEntityName
                   eIndex.TempTableName      = eTable.TempTableName

                   /* Mike Fechner, Consultingwerk Ltd. 26.01.2013
                      Replace % with "Percent" as the % sign is not supported as a field name in most environments */
                   eIndex.FieldNames = REPLACE (eIndex.FieldNames, "%":U, "Percent":U)
                   .

            hQuery:GET-NEXT .
        END.

        DATASET dsBusinessEntity:WRITE-XML ("FILE":U,
                                            SUBSTITUTE ("&1/&2.bedgm":U, cPath, eBusinessEntity.BusinessEntityName),
                                            TRUE) .

        oGenerator = DYNAMIC-NEW (BusinessEntityDesignerSettings:BusinessEntityGenerator) () .
        oGenerator:SilentMode = TRUE  .

        oGenerator:GenerateCode (DATASET dsBusinessEntity) .
        oGenerator:Compile (DATASET dsBusinessEntity) .

        DELETE OBJECT oGenerator .

        IF plGenerateDatasetModel THEN DO:
            oModelGenerator = NEW ModelClassGeneratorPlugin () .
            oModelGenerator:GenerateModelClasses (DATASET dsBusinessEntity BY-REFERENCE) .
        END.

    END METHOD .

    /**
     * Purpose: Raises the StatusChanged
     * Notes:
     * @param e The ProcessingStatusChangedEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnStatusChanged (e AS ProcessingStatusChangedEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "StatusChanged":U) .

        THIS-OBJECT:StatusChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Translates a package name into a file path
     * Notes:
     * @param pcPackage The Package to convert into a file path
     * @return The relative file path derived from the package namea
     */
    METHOD PROTECTED CHARACTER PathFromPackage (pcPackage AS CHARACTER):

        RETURN REPLACE (pcPackage, ".":U, "/":U) .

    END METHOD.

END CLASS.
