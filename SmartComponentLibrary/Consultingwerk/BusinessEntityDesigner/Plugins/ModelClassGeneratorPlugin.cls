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
    File        : ModelClassGeneratorPlugin
    Purpose     : Business Entity Designer Plugin that integrates the
                  Generator for Dataset/TableModel classes (DatasetModel,
                  TableModel, TableFilter)
    Syntax      :
    Description : Plugin generates DatasetModel classes and related code
                  whenever the Business Entity is generated or regenerated.
                  DatasetModel code generation can be suspended by setting the
                  "ModelClassGenerator.Disabled" custom property to "false"
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 02 22:03:43 CEST 2012
    Notes       : Custom constructors that should not be replaced when the
                  DatasetModel class is regenerated, need to be annotated
                  using the @CustomConstructor. annotation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING com.joanju.proparse.NodeTypes                                       FROM ASSEMBLY .
USING Consultingwerk.*                                                    FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                             FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGenerator.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Services.*                    FROM PROPATH .
USING Consultingwerk.Exceptions.*                                         FROM PROPATH .
USING Consultingwerk.Framework.*                                          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                     FROM PROPATH .
USING Consultingwerk.Studio.*                                             FROM PROPATH.
USING Consultingwerk.Studio.Proparse.*                                    FROM PROPATH .
USING Consultingwerk.Util.*                                               FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                 FROM PROPATH .
USING org.prorefactor.core.JPNode                                         FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                                        FROM ASSEMBLY .
USING Progress.Lang.*                                                     FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY"}

    &scoped-define TableModelClassTemplate tablemodel.template
    &scoped-define TableModelClassGeneratedTemplate tablemodel_generated.template
    &scoped-define TableFilterClassTemplate tablefilter.template
    &scoped-define DatasetModelClassTemplate datasetmodel.template
    &scoped-define DatasetModelClassGeneratedTemplate datasetmodel_generated.template

    DEFINE VARIABLE cOutputFolder AS CHARACTER NO-UNDO.

    DEFINE TEMP-TABLE ttSearchCodeConstructor NO-UNDO
        FIELD TempTableName AS CHARACTER
        FIELD IndexName     AS CHARACTER
        FIELD Parameters    AS CHARACTER
        INDEX Parameters IS UNIQUE PRIMARY Parameters .

    /**
     * Purpose: Ignore status messages and questions
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SilentMode AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Event handler for the AfterGenerateSource event of the
     *          BusinessEntityDatasetController
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID AfterGenerateSourceHandler (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE oStatusManager AS IStatusManager NO-UNDO .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Generating Dataset Model Classes (ModelClassGeneratorPlugin)."{&TRAN}) .

        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Directory exists */

        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Write protected */

        ASSIGN cOutputFolder = THIS-OBJECT:FindOutputFolder () .

        THIS-OBJECT:GenerateModelClasses () .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus () .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Validates that the target folders exists of prompts for create
     * Notes:
     * @return Logical value indicating if the folders exists (true) or the generation should be cancelled
     */
    METHOD PROTECTED LOGICAL CheckDirectoriesExist ():

        DEFINE VARIABLE oReturn AS DialogResultEnum NO-UNDO .

        DEFINE BUFFER b_eTable FOR eTable .

        FIND FIRST eBusinessEntity .

        FOR EACH b_eTable OF eBusinessEntity:
            IF b_eTable.EntityPackageName > "":U THEN DO:
                FILE-INFORMATION:FILE-NAME = cOutputFolder + REPLACE (b_eTable.EntityPackageName, ".":U, "~\":U) .

                IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                    IF THIS-OBJECT:SilentMode = FALSE THEN
                        oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                        (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cOutputFolder + REPLACE (b_eTable.EntityPackageName, ".":U, "~\":U)),
                                         "Business Entity Generator"{&TRAN},
                                         SUBSTITUTE ("The directory ~"&1~" used for the Business Entity Table ~"&2~" does not exist."{&TRAN},
                                                     cOutputFolder + REPLACE (b_eTable.EntityPackageName, ".":U, "~\":U), b_eTable.TempTableName)) .
                    ELSE
                        oReturn = DialogResultEnum:DialogResultYes .

                    IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                        System.IO.Directory:CreateDirectory (cOutputFolder + REPLACE (b_eTable.EntityPackageName, ".":U, "~\":U)) .
                    ELSE RETURN FALSE .
                END.
            END.
        END.

        RETURN TRUE .

    END METHOD.

    /**
     * Purpose: Verifies the path to a template file
     * Notes:   Throws an error, when the template file cannot be found
     * @param pcFileName The relative template file name
     * @return The absolute path to the template file
     */
    METHOD PROTECTED CHARACTER CheckTemplatePath (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            RETURN FILE-INFORMATION:FULL-PATHNAME .

        UNDO, THROW NEW AppError (SUBSTITUTE ("The template file ~"&1~" cannot be found."{&TRAN},
                                              pcFileName), 0) .

    END METHOD.

    /**
     * Purpose: Returns the output folder (source code folder for business logic)
     * Notes:
     * @return The output folder
     */
    METHOD PROTECTED CHARACTER FindOutputFolder ():

        DEFINE VARIABLE cSourceFolder AS CHARACTER NO-UNDO.

        IF BusinessEntityDesignerSettings:IncludeFilesBaseFolder > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:IncludeFilesBaseFolder .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cSourceFolder = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:IncludeFilesBaseFolder),
                                          0) .
        END.
        ELSE
            ASSIGN cSourceFolder = "":U .

        RETURN cSourceFolder .

    END METHOD .

    /**
     * Purpose: Returns the code for the search constructors
     * Notes:
     * @return The source code for the search constructors
     */
    METHOD PROTECTED LONGCHAR GenerateConstructors ():

        DEFINE VARIABLE cConstructor      AS LONGCHAR  NO-UNDO .

        DEFINE VARIABLE cParameter        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cParameterComment AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFilter           AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cIndexField       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndexField       AS INTEGER   NO-UNDO.

        DEFINE BUFFER b_eTable                    FOR eTable .
        DEFINE BUFFER b_eField                    FOR eField .
        DEFINE BUFFER b_eIndex                    FOR eIndex .
        DEFINE BUFFER b_eBusinessEntity           FOR eBusinessEntity .

        FIND FIRST b_eBusinessEntity .

        FOR EACH b_eIndex WHERE b_eIndex.DefaultSearchCodeGeneration = TRUE,
           FIRST b_eTable WHERE b_eTable.TempTableName               = b_eIndex.TempTableName:

            ASSIGN cParameter        = "":U
                   cParameterComment = "":U
                   cFilter           = "":U
                   .

            DO iIndexField = 1 TO NUM-ENTRIES (b_eIndex.FieldNames):
                ASSIGN cIndexField = ENTRY (iIndexField, b_eIndex.FieldNames) .

                FIND FIRST b_eField WHERE b_eField.TempTableName = b_eIndex.TempTableName
                                      AND b_eField.Fieldname     = cIndexField NO-ERROR .

                IF NOT AVAILABLE b_eField THEN
                    NEXT .

                IF cParameter > "":U THEN
                    cParameter = cParameter + ", ":U .

                ASSIGN cParameter        = cParameter + SUBSTITUTE (BusinessEntityGenerator:CaseKeywords("p&1 AS &2":U, "p&1":U),
                                                       cIndexField,
                                                       BusinessEntityGenerator:CaseKeywords (b_eField.FieldDataType))
                       cParameterComment = cParameterComment +
                                           SUBSTITUTE ("        @param p&1 The filter value for the &2.&1 field&3":U,
                                                       cIndexField,
                                                       b_eIndex.TempTableName,
                                                       System.Environment:NewLine)
                       cFilter           = cFilter +
                                           SUBSTITUTE (BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:&1:Filter:&2:EQ (p&2) .&3":U, ":&1:Filter:&2:EQ (p":U),
                                                       b_eTable.EntityClassName,
                                                       cIndexField,
                                                       System.Environment:NewLine)
                                .
            END.

            ASSIGN cConstructor = cConstructor + SUBSTITUTE ("    /*------------------------------------------------------------------------------&9":U  +
                                                             "        Purpose: Constructor for the &1DatasetModel class&9":U  +
                                                             "        Notes:   &9":U  +
                                                             "&3    ------------------------------------------------------------------------------*/&9":U  +
                                                             BusinessEntityGenerator:CaseKeywords("    CONSTRUCTOR PUBLIC &1DatasetModel (&2):&9":U, "&1DatasetModel":U)  +
                                                             BusinessEntityGenerator:CaseKeywords("        SUPER ().&9":U)  +
                                                             "&9":U  +
                                                             "&5&9":U  +
                                                             BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:&4:Filter:Run () .&9":U, ":Filter:Run ()":U)  +
                                                             "&9":U  +
                                                             BusinessEntityGenerator:CaseKeywords("    END CONSTRUCTOR.&9":U)  +
                                                             "&9":U,
                                                             SUBSTRING (b_eBusinessEntity.DatasetName, 3, -1, "CHARACTER":U),
                                                             cParameter,
                                                             cParameterComment,
                                                             b_eTable.EntityClassName,
                                                             cFilter,
                                                             "",
                                                             "",
                                                             "",
                                                             System.Environment:NewLine) .

        END.

        RETURN cConstructor .

    END METHOD.

    /**
     * Purpose: Generates the Model Classes
     * Notes:
     */
    METHOD PROTECTED VOID GenerateModelClasses ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cFileName           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO EXTENT 5.
        DEFINE VARIABLE cProperties         AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cDataType           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cPropertyType       AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cConstructor        AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cMethods            AS LONGCHAR           NO-UNDO .

        DEFINE VARIABLE cTableModelName     AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTableFilterName    AS CHARACTER          NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        DEFINE VARIABLE cCustomType         AS CHARACTER          NO-UNDO .

        DEFINE BUFFER b_eBusinessEntity           FOR eBusinessEntity .
        DEFINE BUFFER b_eBusinessEntityProperties FOR eBusinessEntityProperties .
        DEFINE BUFFER b_eTable                    FOR eTable .
        DEFINE BUFFER b_eField                    FOR eField .

        DEFINE BUFFER b_eFieldProperties          FOR eFieldProperties .

        FIND FIRST b_eBusinessEntity .

        IF NOT b_eBusinessEntity.GenerateBusinessEntity THEN
            RETURN .

        IF CAN-FIND (FIRST b_eBusinessEntityProperties WHERE b_eBusinessEntityProperties.PropertyName  = "ModelClassGenerator.Disabled":U
                                                         AND b_eBusinessEntityProperties.PropertyValue = "yes":U) THEN
            RETURN .

        ASSIGN
            cTemplate[1] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\Model~\{&TableModelClassGeneratedTemplate}":U)
            cTemplate[2] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\Model~\{&TableModelClassTemplate}":U)
            cTemplate[3] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\Model~\{&DatasetModelClassGeneratedTemplate}":U)
            cTemplate[4] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\Model~\{&DatasetModelClassTemplate}":U)
            cTemplate[5] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\Model~\{&TableFilterClassTemplate}":U)
                .

        /* Mike Fechner, Consultingwerk Ltd. 30.04.2014
           SCL-166: Warnings about duplicate search code constructors */
        THIS-OBJECT:ValidateSearchCodeIndexes () .

        /* Mike Fechner, Consultingwerk Ltd. 08.02.2015
           SCL-394: Check for create folder */
        IF NOT THIS-OBJECT:CheckDirectoriesExist () THEN
            RETURN .

        FOR EACH b_eTable ON ERROR UNDO, THROW:

            IF CharacterType:IsNullOrEmpty (b_eTable.EntityPackageName) OR
               CharacterType:IsNullOrEmpty (b_eTable.EntityClassName) THEN
                UNDO, THROW NEW Exception (SUBSTITUTE ("Unable to generate Model Class for Table &1. Entity Package Name or Entity Class Name not set."{&TRAN}, b_eTable.TempTableName), 0) .

            ASSIGN
                cTableModelName   = SUBSTITUTE ("&1TableModel":U, b_eTable.EntityClassName)
                cTableFilterName  = SUBSTITUTE ("&1TableModelFilter":U, b_eTable.EntityClassName)
                cProperties       = "":U
                cConstructor      = "":U .

            /* Generation of the Filter Class */
            COPY-LOB FROM FILE cTemplate[5] TO lcSource .

            ASSIGN cProperties = "":U .

            /* Currently no flter support for extent fields (cannot be indexed on the DB anyway) */
            FOR EACH b_eField WHERE b_eField.TempTableName  = b_eTable.TempTableName
                                AND b_eField.FieldExtent    <= 1
                                AND (b_eField.FieldDataType = "Character":U OR
                                     b_eField.FieldDataType = "Date":U OR
                                     b_eField.FieldDataType = "DateTime":U OR
                                     b_eField.FieldDataType = "DateTime-Tz":U OR
                                     b_eField.FieldDataType = "Decimal":U OR
                                     b_eField.FieldDataType = "Int64":U OR
                                     b_eField.FieldDataType = "Integer":U OR
                                     b_eField.FieldDataType = "Logical":U OR
                                     b_eField.FieldDataType = "Rowid":U)
                              BY b_eField.FieldOrder:

                CASE b_eField.FieldDataType:
                    WHEN "DATETIME":U THEN
                        ASSIGN cDataType = "DateTime":U .
                    WHEN "DATETIME-TZ":U THEN
                        ASSIGN cDataType = "DateTimeTz":U .
                    OTHERWISE
                        ASSIGN cDataType   = StringHelper:CamelCaseWord (REPLACE (b_eField.FieldDataType, "-":U, "":U)) .
                END CASE .

                ASSIGN cProperties = cProperties + SUBSTITUTE ("    /*------------------------------------------------------------------------------&3":U +
                                                               "        Purpose: Returns the &2Filter for the &1 field&3":U +
                                                               "        Notes:   &3":U +
                                                               "    ------------------------------------------------------------------------------*/&3":U +
                                                               BusinessEntityGenerator:CaseKeywords("    DEFINE PUBLIC PROPERTY &1 AS ":U) + "&2Filter":U + BusinessEntityGenerator:CaseKeywords(" NO-UNDO &3":U) +
                                                               BusinessEntityGenerator:CaseKeywords("    GET:&3":U) +
                                                               BusinessEntityGenerator:CaseKeywords("        RETURN":U) + " oTableModel:New&2Filter":U + BusinessEntityGenerator:CaseKeywords(" (~"&1~":U, THIS-OBJECT) .&3":U) +
                                                               BusinessEntityGenerator:CaseKeywords("    END GET.&3":U) +
                                                               "&3":U,
                                                               b_eField.FieldName,
                                                               cDataType,
                                                               System.Environment:NewLine) .
            END.

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          cTableFilterName,
                                          b_eTable.EntityPackageName,
                                          cTableModelName,
                                          cProperties)
                   lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "TableModelFilter":U))

                   lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW))

                   cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.&1":U,
                                                cTableFilterName,
                                                b_eTable.EntityPackageName)).

            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetModel":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            IF NOT FileHelper:Exists (cFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cFileName) THEN
                COPY-LOB FROM lcSource TO FILE cFileName .

            /* Generation of the generated portion of the TableModel class */
            COPY-LOB FROM FILE cTemplate[1] TO lcSource .

            ASSIGN cProperties = "":U .

            /* Currently no support for extent fields (cannot be indexed on the DB anyway) */
            FOR EACH b_eField WHERE b_eField.TempTableName  = b_eTable.TempTableName
                                AND b_eField.FieldExtent    <= 1
                                AND (b_eField.FieldDataType = "Character":U OR
                                     b_eField.FieldDataType = "Date":U OR
                                     b_eField.FieldDataType = "DateTime":U OR
                                     b_eField.FieldDataType = "DateTime-Tz":U OR
                                     b_eField.FieldDataType = "Decimal":U OR
                                     b_eField.FieldDataType = "Handle":U OR
                                     b_eField.FieldDataType = "Int64":U OR
                                     b_eField.FieldDataType = "Integer":U OR
                                     b_eField.FieldDataType = "Logical":U OR
                                     b_eField.FieldDataType = "Clob":U OR
                                     b_eField.FieldDataType = "Recid":U OR
                                     b_eField.FieldDataType = "Rowid":U)
                              BY b_eField.FieldOrder:

                IF b_eField.FieldDataType = "CLOB":U THEN
                    ASSIGN cDataType     = "Longchar":U
                           cPropertyType = "LONGCHAR":U .
                ELSE
                    ASSIGN cDataType     = StringHelper:CamelCaseWord (REPLACE (b_eField.FieldDataType, "-":U, "":U))
                           cPropertyType = BusinessEntityGenerator:CaseKeywords (b_eField.FieldDataType) .

                ASSIGN cCustomType = "":U .

                FIND b_eFieldProperties WHERE b_eFieldProperties.TempTableName = b_eField.TempTableName
                                          AND b_eFieldProperties.FieldName     = b_eField.FieldName
                                          AND b_eFieldProperties.PropertyName  = "ModelClassGenerator.CustomPropertyType":U NO-ERROR .

                IF AVAILABLE b_eFieldProperties THEN
                    ASSIGN cCustomType = b_eFieldProperties.PropertyValue .
                ELSE
                    ErrorHelper:ResetErrorStatus() .

                IF (cDataType = "INTEGER":U OR cDataType = "CHARACTER":U) AND cCustomType > "":U THEN
                    ASSIGN cProperties = cProperties + SUBSTITUTE ("    /*------------------------------------------------------------------------------&3":U +
                                                                   "        Purpose: Gets and sets the current &1 value&3":U +
                                                                   "        Notes:   &3":U +
                                                                   "    ------------------------------------------------------------------------------*/&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    DEFINE &4 PROPERTY &1 AS &6 NO-UNDO&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    GET:&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        RETURN CAST (THIS-OBJECT:":U) + "Get&2EnumFieldValue (~"&1~":U, ~"&6~":U), &6) .&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    END GET . &3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    &5 SET (":U) + "arg":U + BusinessEntityGenerator:CaseKeywords(" AS &6):&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:":U) + "Set&2EnumFieldValue (~"&1~":U, arg) .&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    END SET .&3":U) +
                                                                   "&3":U +
                                                                   "    /*------------------------------------------------------------------------------&3":U +
                                                                   "        Purpose: Returns the value of the &1 field when a record is available&3":U +
                                                                   "        Notes:   Returns the default value, when no record&3":U +
                                                                   "        @param pDefaultValue The default value to return when no record is available&3":U +
                                                                   "        @return The field value or the default value when no record is available&3":U +
                                                                   "    ------------------------------------------------------------------------------*/&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    METHOD &4 &6 &1WhenAvailable (pDefaultValue AS &6):&3":U, "&1WhenAvailable ":U + CHR(1) + "(pDefaultValue ":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        IF THIS-OBJECT:BufferHandle:Available THEN &3":U, ":BufferHandle:":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("            RETURN CAST(THIS-OBJECT:":U) + "Get&2EnumFieldValue (~"&1~":U, ~"&6~":U), &6) .&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("        ELSE &3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("            RETURN pDefaultValue .&3":U, " pDefaultValue .":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    END METHOD .&3":U) +
                                                                   "&3":U,
                                                                   b_eField.FieldName,
                                                                   cDataType,
                                                                   System.Environment:NewLine,
                                                                   BusinessEntityGenerator:CaseKeywords(b_eField.FieldGetAccess),
                                                                   BusinessEntityGenerator:CaseKeywords(b_eField.FieldSetAccess),
                                                                   cCustomType) .
                ELSE
                    ASSIGN cProperties = cProperties + SUBSTITUTE ("    /*------------------------------------------------------------------------------&3":U +
                                                                   "        Purpose: Gets and sets the current &1 value&3":U +
                                                                   "        Notes:   &3":U +
                                                                   "    ------------------------------------------------------------------------------*/&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    DEFINE &4 PROPERTY &1 AS &6 NO-UNDO&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    GET:&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        RETURN THIS-OBJECT:":U) + "Get&2FieldValue (~"&1~":U) .&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    END GET . &3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    &5 SET (":U) + "arg":U + BusinessEntityGenerator:CaseKeywords(" AS &6):&3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:":U) + "Set&2FieldValue (~"&1~":U, arg) .&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    END SET .&3":U) +
                                                                   "&3":U +
                                                                   "    /*------------------------------------------------------------------------------&3":U +
                                                                   "        Purpose: Returns the value of the &1 field when a record is available&3":U +
                                                                   "        Notes:   Returns the default value, when no record&3":U +
                                                                   "        @param pDefaultValue The default value to return when no record is available&3":U +
                                                                   "        @return The field value or the default value when no record is available&3":U +
                                                                   "    ------------------------------------------------------------------------------*/&3":U +
                                                                   BusinessEntityGenerator:CaseKeywords("    METHOD &4 &6 &1WhenAvailable (pDefaultValue AS &6):&3":U, "&1WhenAvailable ":U + CHR(1) + "(pDefaultValue ":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        IF THIS-OBJECT:BufferHandle:Available THEN &3":U, ":BufferHandle:":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("            RETURN THIS-OBJECT:BufferHandle:BUFFER-FIELD (~"&1~":U):BUFFER-VALUE .&3":U, ":BufferHandle:":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("        ELSE &3":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("            RETURN pDefaultValue .&3":U, " pDefaultValue .":U) +
                                                                   BusinessEntityGenerator:CaseKeywords("    END METHOD .&3":U) +
                                                                   "&3":U,
                                                                   b_eField.FieldName,
                                                                   cDataType,
                                                                   System.Environment:NewLine,
                                                                   BusinessEntityGenerator:CaseKeywords(b_eField.FieldGetAccess),
                                                                   BusinessEntityGenerator:CaseKeywords(b_eField.FieldSetAccess),
                                                                   cPropertyType) .
            END.

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          cTableModelName,
                                          b_eTable.EntityPackageName,
                                          cTableFilterName,
                                          cProperties)
                   lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "TableModelGenerated":U))

                   lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW))

                   cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.&1":U,
                                                SUBSTITUTE ("&1_Generated":U, cTableModelName),
                                                b_eTable.EntityPackageName)).

            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetModel":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            IF NOT FileHelper:Exists (cFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cFileName) THEN
                COPY-LOB FROM lcSource TO FILE cFileName .

            /* Generation of the TableModel class */
            ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.&1":U,
                                                cTableModelName,
                                                b_eTable.EntityPackageName)) .

            IF NOT FileHelper:Exists (cFileName) THEN DO:

                COPY-LOB FROM FILE cTemplate[2] TO lcSource .

                ASSIGN lcSource = SUBSTITUTE (lcSource,
                                              cTableModelName,
                                              b_eTable.EntityPackageName)
                       lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                       lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                       lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                          b_eBusinessEntity.BusinessEntityName,
                                                                                                                          "TableModel":U))
                       lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW)) .

                oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetModel":U, cFileName) .

                IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                    IF oWriteFileEventArgs:Cancel THEN
                        NEXT .

                    IF oWriteFileEventArgs:NewFileName > "":U THEN
                        cFileName = oWriteFileEventArgs:NewFileName .
                END.

                IF NOT FileHelper:Exists (cFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cFileName) THEN
                    COPY-LOB FROM lcSource TO FILE cFileName .
            END.
        END.

        ASSIGN cMethods = THIS-OBJECT:GenerateModelMethods () .

        /* Generation of the generated dataset model class */
        COPY-LOB FROM FILE cTemplate[3] TO lcSource .

        ASSIGN cProperties  = "":U
               cConstructor = "":U .

        FOR EACH b_eTable:

            ASSIGN cProperties = cProperties + SUBSTITUTE ("    /*------------------------------------------------------------------------------&2":U +
                                                           "        Purpose: Returns the reference to the &1 table model&2":U +
                                                           "        Notes:   &2":U +
                                                           "    ------------------------------------------------------------------------------*/&2":U +
                                                           BusinessEntityGenerator:CaseKeywords("    DEFINE PUBLIC PROPERTY &1 AS &1TableModel NO-UNDO&2":U, "&1TableModel ":U) +
                                                           BusinessEntityGenerator:CaseKeywords("    GET.&2":U) +
                                                           BusinessEntityGenerator:CaseKeywords("    PRIVATE SET.&2":U) +
                                                           "&2":U,
                                                           b_eTable.EntityClassName,
                                                           System.Environment:NewLine)

                   cConstructor = cConstructor + SUBSTITUTE (BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:&1 = NEW &1TableModel (THIS-OBJECT, BUFFER &3:HANDLE) .&2":U, "&1TableModel":U) +
                                                             "&2":U +
                                                             BusinessEntityGenerator:CaseKeywords("        THIS-OBJECT:TableModels:Add (THIS-OBJECT:&1) .&2":U, ":TableModels:Add ":U) +
                                                             "&2":U,
                                                             b_eTable.EntityClassName,
                                                             System.Environment:NewLine,
                                                             b_eTable.TempTableName).
        END.

        FIND FIRST b_eTable .

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      SUBSTRING (b_eBusinessEntity.DatasetName, 3, -1, "CHARACTER":U),
                                      b_eTable.EntityPackageName,
                                      b_eBusinessEntity.DatasetName,
                                      cConstructor,
                                      cProperties,
                                      b_eBusinessEntity.BusinessEntityPackage,
                                      b_eBusinessEntity.BusinessEntityName,
                                      REPLACE (b_eBusinessEntity.DatasetPath, "~\":U, "/":U),
                                      cMethods)
               lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                  b_eBusinessEntity.BusinessEntityName,
                                                                                                                  "DatasetModelGenerated":U))
               lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW)) .

        ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                               (SUBSTITUTE ("&2.&1DatasetModel_Generated":U,
                                            SUBSTRING (b_eBusinessEntity.DatasetName, 3, -1, "CHARACTER":U),
                                            b_eTable.EntityPackageName)) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetModel":U, cFileName) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                NEXT .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cFileName = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cFileName) THEN
            COPY-LOB FROM lcSource TO FILE cFileName .

        /* Generation of the Dataset Model */
        ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                               (SUBSTITUTE ("&2.&1DatasetModel":U,
                                            SUBSTRING (b_eBusinessEntity.DatasetName, 3, -1, "CHARACTER":U),
                                            b_eTable.EntityPackageName)) .

        IF NOT FileHelper:Exists (cFileName) THEN DO:

            COPY-LOB FROM FILE cTemplate[4] TO lcSource .

            ASSIGN cConstructor = THIS-OBJECT:GenerateConstructors () .

            FIND FIRST b_eTable .

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          SUBSTRING (b_eBusinessEntity.DatasetName, 3, -1, "CHARACTER":U),
                                          b_eTable.EntityPackageName,
                                          cConstructor)
                   lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "DatasetModel":U))
                   lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW)) .

            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetModel":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            IF NOT FileHelper:Exists (cFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cFileName) THEN
                COPY-LOB FROM lcSource TO FILE cFileName .
        END.
        ELSE
            THIS-OBJECT:UpdateSearchConstructors (cFileName) .

    END METHOD.

    /**
     * Purpose: Generates the Model Classes
     * Notes:   This routine is intended to be used from outside of the Business Entity
     *          designer (automatic source code generation, unit testing)
     * @param dsBusinessEntity Business Entity with the Design dataset, called with the BY-REFERENCE keyword
     */
    METHOD PUBLIC VOID GenerateModelClasses (DATASET FOR dsBusinessEntity):

        ASSIGN cOutputFolder = THIS-OBJECT:FindOutputFolder () .

        THIS-OBJECT:GenerateModelClasses () .

    END METHOD .

    /**
     * Purpose: Generates methods for the datasetmodel_generated class
     * Notes:
     * @return The source code of the generated methods
     */
    METHOD PROTECTED LONGCHAR GenerateModelMethods ():

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity .

        DEFINE VARIABLE lcReturn      AS LONGCHAR                                    NO-UNDO .
        DEFINE VARIABLE lcMethod      AS LONGCHAR                                    NO-UNDO .
        DEFINE VARIABLE cClassName    AS CHARACTER                                   NO-UNDO .
        DEFINE VARIABLE cClassFile    AS CHARACTER                                   NO-UNDO .

        DEFINE VARIABLE oParser       AS Consultingwerk.Studio.ClassAnnotationParser NO-UNDO .
        DEFINE VARIABLE oList         AS Consultingwerk.Studio.ListClassMember       NO-UNDO .
        DEFINE VARIABLE oAnnotation   AS Consultingwerk.Studio.Annotation            NO-UNDO .
        DEFINE VARIABLE cTemplate     AS CHARACTER                                   NO-UNDO .
        DEFINE VARIABLE cTemplateFile AS CHARACTER                                   NO-UNDO .
        DEFINE VARIABLE oMember       AS Consultingwerk.Studio.ClassMember           NO-UNDO .
        DEFINE VARIABLE iMember       AS INTEGER                                     NO-UNDO .
        DEFINE VARIABLE cParameter    AS CHARACTER                                   NO-UNDO .

        FIND FIRST b_eBusinessEntity .

        ASSIGN cClassName = SUBSTITUTE ("&1.&2":U,
                                        b_eBusinessEntity.BusinessEntityPackage,
                                        b_eBusinessEntity.BusinessEntityName)

               cClassFile = ClassHelper:ClassNameToFile (cClassName) .

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2014
           SLC-359 - PROPATH lookup */
        FILE-INFORMATION:FILE-NAME = cClassFile.
        cClassFile = FILE-INFORMATION:FULL-PATHNAME .

        oParser = NEW Consultingwerk.Studio.ClassAnnotationParser (cClassFile) .
        oList   = oParser:GetClassMemberAnnotations() .

        memberloop: DO iMember = 1 TO oList:Count:
            oMember = oList:GetItem (iMember) .

            IF NOT oMember:MemberType = Consultingwerk.Studio.ClassMemberTypeEnum:Method THEN
                NEXT memberloop.

            oAnnotation = oMember:Annotations:FindByName ("@InvokeMethod":U) .

            IF NOT VALID-OBJECT (oAnnotation) THEN
                NEXT memberloop.

            IF VALID-OBJECT (oAnnotation:Parameters:FindByName("template":U)) THEN
                ASSIGN cTemplate = oAnnotation:Parameters:FindByName("template":U):Value.
            ELSE
                ASSIGN cTemplate = "invoke-receive-dataset":U .

            ASSIGN cTemplateFile = TemplateHelper:Find (SUBSTITUTE ("DatasetModelInvokeMethods~\&1.template":U, cTemplate)) .

            IF NUM-ENTRIES (oMember:Parameters) > 1 THEN DO:
                ASSIGN cParameter = ENTRY (2, oMember:Parameters)
                       cParameter = REPLACE (cParameter, "INPUT-OUTPUT":U, "":U)
                       cParameter = REPLACE (cParameter, "INPUT":U, "":U)
                       cParameter = REPLACE (cParameter, "OUTPUT":U, "":U)
                       cParameter = TRIM (cParameter).
            END.
            ELSE
                ASSIGN cParameter = "Consultingwerk.Serializable":U .

            COPY-LOB FROM FILE cTemplateFile TO lcMethod .

            ASSIGN lcMethod = SUBSTITUTE (lcMethod,
                                         oMember:Name,
                                         cParameter,
                                         b_eBusinessEntity.DatasetName) .

            ASSIGN lcReturn = lcReturn + lcMethod .
        END.

        RETURN lcReturn .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err, "Error generating proxy methods in the DatasetModel":U) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oList) .
            GarbageCollectorHelper:DeleteObject (oParser) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Event handler for the GetCustomPropertyNamesHandler event of the
     *          BusinessEntityDatasetController
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The GetCustomPropertyNamesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID GetCustomPropertyNamesHandler (sender AS Progress.Lang.Object,
                                                         e AS GetCustomPropertyNamesEventArgs):

        IF e:Scope = CustomPropertiesScopeEnum:BusinessEntity THEN
            e:PropertyNames:Add ("ModelClassGenerator.Disabled":U) .
        ELSE IF e:Scope = CustomPropertiesScopeEnum:Field THEN
            e:PropertyNames:Add ("ModelClassGenerator.CustomPropertyType":U) .

    END METHOD .

    /**
     * Purpose: Event handler for the GetCustomPropertyNamesHandler event of the
     *          BusinessEntityDatasetController
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The GetCustomPropertyNamesEventArgs with the data for this event
     */
    METHOD PROTECTED VOID GetCustomPropertyValuesHandler (sender AS Progress.Lang.Object,
                                                          e AS GetCustomPropertyValuesEventArgs):

        DEFINE VARIABLE oCustomTypes AS CustomPropertyTypeProvider NO-UNDO .

        IF e:Scope = CustomPropertiesScopeEnum:BusinessEntity AND
           e:PropertyName = "ModelClassGenerator.Disabled":U THEN DO:
            e:PropertyValues:Add ("yes":U) .
            e:PropertyValues:Add ("no":U) .
        END.
        ELSE IF e:Scope = CustomPropertiesScopeEnum:Field AND
            e:PropertyName = "ModelClassGenerator.CustomPropertyType":U THEN DO:

            ASSIGN oCustomTypes = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGenerator.CustomPropertyTypeProvider
                                                                "NEW Consultingwerk.BusinessEntityDesigner.Plugins.ModelClassGenerator.CustomPropertyTypeProvider ()"} .

            oCustomTypes:AddPropertyValues (e:PropertyValues) .
        END.

    END METHOD .

    /**
     * Purpose: Method called when the Business Entity Designer will get closed
     * Notes:
     */
    METHOD PUBLIC VOID Shutdown ():

    END METHOD.

    /**
     * Purpose: Method called when the Business Entity Designer is starting and
     *          has loaded the plugin.
     * Notes:   Passes references to some key components of the Designer to
     *          the plugin, use this to initialize the plugin, subscribe to events
     * @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
     * @param poController The BusinessEntityDatasetController
     * @param poForm The main Form of the BusinessEntityDesigner
     * @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
     */
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController,
                                poForm AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm,
                                poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager):

        IF VALID-OBJECT (poController) THEN DO:
            poController:AfterGenerateSource:Subscribe (AfterGenerateSourceHandler) .
            poController:AfterRegenerateSource:Subscribe (AfterGenerateSourceHandler) .
            poController:GetCustomPropertyNames:Subscribe (GetCustomPropertyNamesHandler) .
            poController:GetCustomPropertyValues:Subscribe (GetCustomPropertyValuesHandler) .
        END.

    END METHOD.

    /**
     * Purpose: Updates the search constructors in an existing dataset model class
     * Notes:
     * @param pcFileName The file name of the dataset model class
     */
    METHOD PROTECTED VOID UpdateSearchConstructors (pcFileName AS CHARACTER):

        DEFINE VARIABLE oParseUnit    AS ParseUnit      NO-UNDO .
        DEFINE VARIABLE oJPNode       AS JPNode         NO-UNDO .
        DEFINE VARIABLE oConstructors AS JPNodesWrapper NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER        NO-UNDO .
        DEFINE VARIABLE lcSource      AS LONGCHAR       NO-UNDO .
        DEFINE VARIABLE cConstructors AS LONGCHAR       NO-UNDO .

        DEFINE VARIABLE oNodes        AS JPNode         NO-UNDO EXTENT .
        DEFINE VARIABLE oPrevNode     AS JPNode         NO-UNDO .

        DEFINE BUFFER b_eIndex FOR eIndex .

        IF NOT CAN-FIND (FIRST b_eIndex WHERE b_eIndex.DefaultSearchCodeGeneration) THEN
            RETURN .

        COPY-LOB FROM FILE pcFileName TO lcSource .
        ASSIGN lcSource = REPLACE (lcSource, CHR (13) + CHR (10), "~n":U) .

        /* SCL-1547 : We need to ensure that line break is consistent in the file */
        COPY-LOB FROM lcSource TO FILE pcFileName .

        cConstructors = TRIM (REPLACE (THIS-OBJECT:GenerateConstructors (), CHR (13) + CHR (10), "~n":U), "~n":U) .

        Consultingwerk.Studio.Proparse.ProparseHelper:SetProparseEnvironment() .
        Consultingwerk.Studio.Proparse.ProparseHelper:ExportDatabaseSchema () .
        Consultingwerk.Studio.Proparse.ProparseHelper:Initialize() .

        oParseUnit = ProparseHelper:ParseFile (pcFileName) .

        oJPNode = oParseUnit:getTopNode() .

        oJPNode = ProparseHelper:FindChildNodeOfNodeType (oJPNode, "CLASS":U) .
        oJPNode = ProparseHelper:FindChildNodeOfNodeType (oJPNode, "Code_block":U) .

        oConstructors = NEW JPNodesWrapper (oJPNode) .
        oConstructors:NodeTypeFilter = "CONSTRUCTOR":U .

        {Consultingwerk/foreachABL.i JPNode oConstructor in oConstructors}

            IF ProparseHelper:ParseParameterList (oConstructor) > "":U THEN
                i = i + 1 .

        END .

        IF i > 0 THEN DO:
            EXTENT (oNodes) = i .
            i = 0 .

            {Consultingwerk/foreachABL.i JPNode oConstructor in oConstructors nodefine}

                IF ProparseHelper:ParseParameterList (oConstructor) > "":U THEN DO:
                    i = i + 1.

                    oNodes [i] = oConstructor .
                END .
            END .

            replaceLoop: DO i = EXTENT (oNodes) TO 1 BY -1:
                oPrevNode = oNodes[i]:prevNode () .

                IF NodeTypes:getTypeName(oPrevNode:getType()) = "ANNOTATION":U AND
                   oPrevNode:getText()                        = "@CustomConstructor":U THEN

                   /* Skip constructor */
                   NEXT replaceLoop.

                ProparseHelper:ReplaceRoutineNode (oNodes[i],
                                                   "":U,
                                                   FALSE,
                                                   TRUE) .
            END.
        END.

        oConstructor = ProparseHelper:FindChildNodeOfNodeType (oJPNode, "CONSTRUCTOR":U) .

        IF VALID-OBJECT (oConstructor) THEN
            ProparseHelper:ReplaceRoutineNode (oConstructor,
                                               oConstructor:toStringFulltext() + "~n":U + "~n":U +
                                               cConstructors,
                                               FALSE,
                                               TRUE) .
        ELSE
            MessageFormHelper:ShowMessage (SUBSTITUTE ("Unable to update search constructors in Dataset Model Class~n&1 without a default constructor."{&TRAN},
                                                       pcFileName),
                                           "Dataset Model Generator"{&TRAN},
                                           MessageFormImages:ImageWarning) .

    END METHOD.

    /**
     * Purpose: Validates that the indexes flagged with "Default Search Code Generation"
     *          do not cause constructors with duplicate signatures
     * Notes:
     */
    METHOD PROTECTED VOID ValidateSearchCodeIndexes ():

        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eIndex FOR eIndex .
        DEFINE BUFFER b_eField FOR eField .

        DEFINE VARIABLE iIndexField AS INTEGER NO-UNDO.
        DEFINE VARIABLE cIndexField AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cParameters AS CHARACTER NO-UNDO.

        EMPTY TEMP-TABLE ttSearchCodeConstructor .

        indexloop:
        FOR EACH b_eTable,
            EACH b_eIndex OF b_eTable WHERE b_eIndex.DefaultSearchCodeGeneration = TRUE ON ERROR UNDO, THROW:

            ASSIGN cParameters = "":U .

            DO iIndexField = 1 TO NUM-ENTRIES (b_eIndex.FieldNames):
                ASSIGN cIndexField = ENTRY (iIndexField, b_eIndex.FieldNames) .

                FIND FIRST b_eField WHERE b_eField.TempTableName = b_eIndex.TempTableName
                                      AND b_eField.Fieldname     = cIndexField NO-ERROR .

                IF NOT AVAILABLE b_eField THEN
                    NEXT .

                ASSIGN cParameters = cParameters + ",":U + b_eField.FieldDataType .
            END.

            ASSIGN cParameters = TRIM (cParameters, ",":U) .

            IF CAN-FIND (FIRST ttSearchCodeConstructor WHERE ttSearchCodeConstructor.Parameters = cParameters) THEN DO:

                FIND FIRST ttSearchCodeConstructor WHERE ttSearchCodeConstructor.Parameters = cParameters .

                MessageFormHelper:ShowMessage ("Duplicate constructor signatures found.~n"{&TRAN} +
                                               SUBSTITUTE ("Indexes &1.&2 and &3.&4 will cause identical constructor signatures.~n"{&TRAN},
                                                           ttSearchCodeConstructor.TempTableName,
                                                           ttSearchCodeConstructor.IndexName,
                                                           b_eTable.TempTableName,
                                                           b_eIndex.IndexName) +
                                               "The result of this are compile errors on the DatasetModel class."{&TRAN},
                                               "Model Class Generator Plugin"{&TRAN},
                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) .

                NEXT indexloop .
            END.

            CREATE ttSearchCodeConstructor.
            ASSIGN ttSearchCodeConstructor.TempTableName = b_eTable.TempTableName
                   ttSearchCodeConstructor.IndexName     = b_eIndex.IndexName
                   ttSearchCodeConstructor.Parameters    = cParameters .

            RELEASE ttSearchCodeConstructor .
        END.

        FINALLY:
            EMPTY TEMP-TABLE ttSearchCodeConstructor .
        END FINALLY.

    END METHOD .

END CLASS.
