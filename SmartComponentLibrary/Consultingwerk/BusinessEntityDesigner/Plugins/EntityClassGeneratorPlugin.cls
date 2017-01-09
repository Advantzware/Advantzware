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
    File        : EntityClassGeneratorPlugin
    Purpose     : Business Entity Designer Plugin that integrates the
                  Generator for ORM value classes.
    Syntax      :
    Description : The Plugin generates Entity classes and related code
                  whenever the Business Entity is generated or regenerated.
                  Entity class code generation can be suspended by setting
                  the "EntityClassGenerator.Disabled" custom property to "false".
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 02 22:03:43 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*  FROM PROPATH .
USING Consultingwerk.Framework.*                        FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.EntityClassGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY"}

    &scoped-define EntityClassTemplate entitytable.template
    &scoped-define EntityClassGeneratedTemplate entitytable_generated.template
    &scoped-define ListTemplate listentitytable.template
    &scoped-define ListGeneratedTemplate listentitytable_generated.template

    DEFINE VARIABLE cOutputFolder AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterGenerateSource event of the
                 BusinessEntityDatasetController
        Notes:
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterGenerateSourceHandler (sender AS Progress.Lang.Object,
                                                      e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE oStatusManager AS IStatusManager NO-UNDO .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Generating Entity Classes (EntityClassGeneratorPlugin)."{&TRAN}) .

        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Directory exists */

        /* Mike Fechner, Consultingwerk Ltd. 02.04.2012
           To-Do: Check Write protected */

        ASSIGN cOutputFolder = THIS-OBJECT:FindOutputFolder () .

        THIS-OBJECT:GenerateEntityClasses () .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies the path to a template file
        Notes:   Throws an error, when the template file cannot be found
        @param pcFileName The relative template file name
        @return The absolute path to the template file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER CheckTemplatePath (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            RETURN FILE-INFORMATION:FULL-PATHNAME .

        UNDO, THROW NEW AppError (SUBSTITUTE ("The template file ~"&1~" cannot be found."{&TRAN},
                                              pcFileName), 0) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the output folder (source code folder for business logic)
        Notes:
        @return The output folder
    ------------------------------------------------------------------------------*/
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

    /*------------------------------------------------------------------------------
        Purpose: Generates the Entity Classes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateEntityClasses ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE cFileName           AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO EXTENT 4.
        DEFINE VARIABLE cProperties         AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE cDataType           AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cFieldOptions       AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE cConstructor        AS LONGCHAR           NO-UNDO.

        DEFINE VARIABLE cAssignFromBuffer   AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE cAssignToBuffer     AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE iExtent             AS INTEGER            NO-UNDO.
        DEFINE VARIABLE cFormat             AS CHARACTER          NO-UNDO.

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        DEFINE BUFFER b_eBusinessEntity           FOR eBusinessEntity .
        DEFINE BUFFER b_eTable                    FOR eTable .
        DEFINE BUFFER b_eTable2                   FOR eTable .
        DEFINE BUFFER b_eDataRelation             FOR eDataRelation .
        DEFINE BUFFER b_eField                    FOR eField .
        DEFINE BUFFER b_eBusinessEntityProperties FOR eBusinessEntityProperties .

        IF CAN-FIND (FIRST b_eBusinessEntityProperties WHERE b_eBusinessEntityProperties.PropertyName  = "EntityClassGenerator.Disabled":U
                                                         AND b_eBusinessEntityProperties.PropertyValue = "yes":U) THEN
            RETURN .

        ASSIGN
            cTemplate[1] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&EntityClassGeneratedTemplate}":U)
            cTemplate[2] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&EntityClassTemplate}":U)
            cTemplate[3] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&ListGeneratedTemplate}":U)
            cTemplate[4] = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&ListTemplate}":U)
                .

        FIND FIRST b_eBusinessEntity .

        IF NOT b_eBusinessEntity.GenerateBusinessEntity THEN
            RETURN .

        FOR EACH b_eTable:
            ASSIGN
                cProperties       = "":U
                cConstructor      = "":U
                cAssignFromBuffer = "":U
                cAssignToBuffer   = "":U .

            COPY-LOB FROM FILE cTemplate[1] TO lcSource .

            FOR EACH b_eDataRelation WHERE b_eDataRelation.ParentTempTableName = b_eTable.TempTableName
                                       AND b_eDataRelation.ChildPropertyName   > "":U,
                FIRST b_eTable2 WHERE b_eTable2.TempTableName = b_eDataRelation.ChildTempTableName:

                IF b_eDataRelation.ChildCardinality = "ONE":U THEN
                    ASSIGN cProperties = cProperties +
                                         SUBSTITUTE ("    DEFINE PUBLIC PROPERTY &1 AS &3.&2 NO-UNDO GET. PROTECTED SET. &4&4":U,
                                                     b_eDataRelation.ChildPropertyName,
                                                     b_eTable2.EntityClassName,
                                                     b_eTable2.EntityPackageName,
                                                     System.Environment:NewLine) .
                ELSE DO:
                    ASSIGN cProperties = cProperties +
                                         SUBSTITUTE ("    DEFINE PUBLIC PROPERTY &1 AS &3.List&2 NO-UNDO GET. PROTECTED SET. &4&4":U,
                                                     b_eDataRelation.ChildPropertyName,
                                                     b_eTable2.EntityClassName,
                                                     b_eTable2.EntityPackageName,
                                                     System.Environment:NewLine)

                           cConstructor = cConstructor +
                                          SUBSTITUTE ("        THIS-OBJECT:&1 = NEW &3.List&2 () .&4&4":U,
                                                      b_eDataRelation.ChildPropertyName,
                                                      b_eTable2.EntityClassName,
                                                      b_eTable2.EntityPackageName,
                                                      System.Environment:NewLine) .
                END.
            END.

            FOR EACH b_eField WHERE b_eField.TempTableName = b_eTable.TempTableName
                                 BY b_eField.FieldOrder:
                ASSIGN cFieldOptions = "":U .

                CASE b_eField.FieldDataType:
                    WHEN Consultingwerk.DataTypeEnum:BLOB THEN DO:
                        ASSIGN cDataType = "MEMPTR":U

                               cAssignFromBuffer = cAssignFromBuffer +
                                                   SUBSTITUTE ("        COPY-LOB FROM phBuffer::&1 TO THIS-OBJECT:&1 .&2&2":U,
                                                               b_eField.FieldName,
                                                               System.Environment:NewLine)
                               cAssignToBuffer   = cAssignToBuffer +
                                                   SUBSTITUTE ("        COPY-LOB FROM THIS-OBJECT:&1 TO phBuffer::&1 .&2&2":U,
                                                               b_eField.FieldName,
                                                               System.Environment:NewLine)
                                                               .

                    END.
                    WHEN Consultingwerk.DataTypeEnum:CLOB THEN DO:
                        ASSIGN cDataType = "LONGCHAR":U

                               cAssignFromBuffer = cAssignFromBuffer +
                                                   SUBSTITUTE ("        COPY-LOB FROM phBuffer::&1 TO THIS-OBJECT:&1 .&2&2":U,
                                                               b_eField.FieldName,
                                                               System.Environment:NewLine)
                               cAssignToBuffer   = cAssignToBuffer +
                                                   SUBSTITUTE ("        COPY-LOB FROM THIS-OBJECT:&1 TO phBuffer::&1 .&2&2":U,
                                                               b_eField.FieldName,
                                                               System.Environment:NewLine)
                                                               .
                    END.
                    OTHERWISE DO:
                        ASSIGN cDataType = b_eField.FieldDataType .

                        IF b_eField.FieldExtent > 0 THEN DO:
                            ASSIGN cFormat = FormatHelper:IntegerFormatForMax (b_eField.FieldExtent) .

                            DO iExtent = 1 TO b_eField.FieldExtent:
                                ASSIGN
                                    cAssignFromBuffer = cAssignFromBuffer +
                                                        SUBSTITUTE ("        ASSIGN THIS-OBJECT:&1&3 = phBuffer:BUFFER-FIELD(~"&1~":U):BUFFER-VALUE (&3) .&2&2":U,
                                                                    b_eField.FieldName,
                                                                    System.Environment:NewLine,
                                                                    STRING (iExtent, cFormat))
                                    cAssignToBuffer   = cAssignToBuffer +
                                                        SUBSTITUTE ("        ASSIGN phBuffer:BUFFER-FIELD(~"&1~":U):BUFFER-VALUE (&3) = THIS-OBJECT:&1&3 .&2&2":U,
                                                                    b_eField.FieldName,
                                                                    System.Environment:NewLine,
                                                                    STRING (iExtent, cFormat))
                                                                    .
                            END.
                        END.
                        ELSE
                            ASSIGN
                                cAssignFromBuffer = cAssignFromBuffer +
                                                    SUBSTITUTE ("        ASSIGN THIS-OBJECT:&1 = phBuffer::&1 .&2&2":U,
                                                                b_eField.FieldName,
                                                                System.Environment:NewLine)
                                cAssignToBuffer   = cAssignToBuffer +
                                                    SUBSTITUTE ("        ASSIGN phBuffer::&1 = THIS-OBJECT:&1 .&2&2":U,
                                                                b_eField.FieldName,
                                                                System.Environment:NewLine)
                                                                .
                    END.
                END CASE .

                IF b_eField.FieldExtent > 0 THEN DO:
                    ASSIGN cFormat = FormatHelper:IntegerFormatForMax (b_eField.FieldExtent) .

                    DO iExtent = 1 TO b_eField.FieldExtent:
                        ASSIGN cProperties = cProperties +
                                             SUBSTITUTE ("    DEFINE &5 PROPERTY &1&7 AS &2 &3 NO-UNDO&4    GET.&4":U +
                                                         "    &6 SET (arg as &2):&4":U +
                                                         "        THIS-OBJECT:&1&7 = arg .&4":U +
                                                         "        THIS-OBJECT:SetModified () .&4":U +
                                                         "        On&1&7Changed (Consultingwerk.EventArgs:Empty) .&4":U +
                                                         "    END SET .&4&4":U +
                                                         "    /*------------------------------------------------------------------------------&4":U +
                                                         "        Purpose: Raised when the &1&7 property value changes&4":U +
                                                         "        Notes:           &4":U +
                                                         "        @param sender The reference to the object that raised the event&4":U +
                                                         "        @param e The Consultingwerk.EventArgs with the data for this event &4":U +
                                                         "    ------------------------------------------------------------------------------*/&4":U +
                                                         "    DEFINE PUBLIC EVENT &1&7Changed SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .&4&4":U +
                                                         "    /*------------------------------------------------------------------------------&4":U +
                                                         "        Purpose: Raises the &1&7Changed event&4":U +
                                                         "        Notes:           &4":U +
                                                         "        @param e The Consultingwerk.EventArgs with the data for this event &4":U +
                                                         "    ------------------------------------------------------------------------------*/&4":U +
                                                         "    METHOD PROTECTED VOID On&1&7Changed (e AS Consultingwerk.EventArgs):&4":U +
                                                         "        THIS-OBJECT:&1&7Changed:Publish (THIS-OBJECT, e) . &4":U +
                                                         "    END METHOD .&4&4":U,
                                                         b_eField.FieldName,
                                                         cDataType,
                                                         cFieldOptions,
                                                         System.Environment:NewLine,
                                                         b_eField.FieldGetAccess,
                                                         b_eField.FieldSetAccess,
                                                         STRING (iExtent, cFormat)) .
                    END.
                END .
                ELSE
                    ASSIGN cProperties = cProperties +
                                         SUBSTITUTE ("    DEFINE &5 PROPERTY &1 AS &2 &3 NO-UNDO&4    GET.&4":U +
                                                     "    &6 SET (arg as &2):&4":U +
                                                     "        THIS-OBJECT:&1 = arg .&4":U +
                                                     "        THIS-OBJECT:SetModified () .&4":U +
                                                     "        On&1Changed (Consultingwerk.EventArgs:Empty) .&4":U +
                                                     "    END SET .&4&4":U +
                                                         "    /*------------------------------------------------------------------------------&4":U +
                                                         "        Purpose: Raised when the &1 property value changes&4":U +
                                                         "        Notes:           &4":U +
                                                         "        @param sender The reference to the object that raised the event&4":U +
                                                         "        @param e The Consultingwerk.EventArgs with the data for this event &4":U +
                                                         "    ------------------------------------------------------------------------------*/&4":U +
                                                     "    DEFINE PUBLIC EVENT &1Changed SIGNATURE VOID (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .&4&4":U +
                                                         "    /*------------------------------------------------------------------------------&4":U +
                                                         "        Purpose: Raises the &1&7Changed event&4":U +
                                                         "        Notes:           &4":U +
                                                         "        @param e The Consultingwerk.EventArgs with the data for this event &4":U +
                                                         "    ------------------------------------------------------------------------------*/&4":U +
                                                     "    METHOD PROTECTED VOID On&1Changed (e AS Consultingwerk.EventArgs):&4":U +
                                                     "        THIS-OBJECT:&1Changed:Publish (THIS-OBJECT, e) . &4":U +
                                                     "    END METHOD .&4&4":U,
                                                     b_eField.FieldName,
                                                     cDataType,
                                                     cFieldOptions,
                                                     System.Environment:NewLine,
                                                     b_eField.FieldGetAccess,
                                                     b_eField.FieldSetAccess) .
            END.

            /* Copy to-one tables */
            FOR EACH b_eDataRelation WHERE b_eDataRelation.ParentTempTableName = b_eTable.TempTableName
                                       AND b_eDataRelation.ChildPropertyName   > "":U,
                FIRST b_eTable2 WHERE b_eTable2.TempTableName = b_eDataRelation.ChildTempTableName:

                IF b_eDataRelation.ChildCardinality = "ONE":U THEN
                    ASSIGN cAssignFromBuffer = cAssignFromBuffer +
                                               SUBSTITUTE ("        hBuffer = THIS-OBJECT:GetChildRecord (phBuffer, ~"&2~":U) .&5":U +
                                                           "        &5":U +
                                                           "        IF hBuffer:AVAILABLE THEN &5":U +
                                                           "            THIS-OBJECT:&1 = NEW &4.&3 (hBuffer) .&5":U +
                                                           "        ELSE &5":U +
                                                           "            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (THIS-OBJECT:&1) .&5&5":U,
                                                           b_eDataRelation.ChildPropertyName,
                                                           b_eDataRelation.DataRelationName,
                                                           b_eTable2.EntityClassName,
                                                           b_eTable2.EntityPackageName,
                                                           System.Environment:NewLine) .
                ELSE
                    ASSIGN cAssignFromBuffer = cAssignFromBuffer +
                                               SUBSTITUTE ("        THIS-OBJECT:&1:Clear () .&5":U +
                                                           "        THIS-OBJECT:GetChildRecords (phBuffer,&5":U +
                                                           "                                     ~"&2~":U,&5":U +
                                                           "                                     THIS-OBJECT:&1,&5":U +
                                                           "                                     ~"&4.&3~":U) .&5&5":U,
                                                           b_eDataRelation.ChildPropertyName,
                                                           b_eDataRelation.DataRelationName,
                                                           b_eTable2.EntityClassName,
                                                           b_eTable2.EntityPackageName,
                                                           System.Environment:NewLine) .
            END.

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          b_eTable.EntityClassName,
                                          b_eTable.EntityPackageName,
                                          cProperties,
                                          cConstructor,
                                          STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                          cAssignFromBuffer,
                                          cAssignToBuffer)

                   cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.&1_Generated":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
               user name, replace @USER@, with the current user name */
            ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "EntityClassGenerated":U)) .

            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            COPY-LOB FROM lcSource TO FILE cFileName .

            ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.&1":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            FILE-INFORMATION:FILE-NAME = cFileName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
                COPY-LOB FROM FILE cTemplate[2] TO lcSource .

                ASSIGN lcSource = SUBSTITUTE (lcSource,
                                              b_eTable.EntityClassName,
                                              b_eTable.EntityPackageName,
                                              STRING (NOW, "99.99.9999 HH:MM:SS":U)) .

                /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
                   user name, replace @USER@, with the current user name */
                ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                       lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                       lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                          b_eBusinessEntity.BusinessEntityName,
                                                                                                                          "EntityClass":U)) .


                oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

                IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                    IF oWriteFileEventArgs:Cancel THEN
                        NEXT .

                    IF oWriteFileEventArgs:NewFileName > "":U THEN
                        cFileName = oWriteFileEventArgs:NewFileName .
                END.

                COPY-LOB FROM lcSource TO FILE cFileName .
            END.
        END .

        /* Generate List classes for to-many child relations */
        FOR EACH b_eDataRelation WHERE b_eDataRelation.ChildCardinality = "MANY":U,
            FIRST b_eTable WHERE b_eTable.TempTableName = b_eDataRelation.ChildTempTableName:

            COPY-LOB FROM FILE cTemplate[3] TO lcSource .

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          b_eTable.EntityClassName,
                                          b_eTable.EntityPackageName,
                                          STRING (NOW, "99.99.9999 HH:MM:SS":U))

                   cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.List&1_Generated":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
               user name, replace @USER@, with the current user name */
            ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "EntityListGenerated":U)) .


            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            COPY-LOB FROM lcSource TO FILE cFileName .

            ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.List&1":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            FILE-INFORMATION:FILE-NAME = cFileName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
                COPY-LOB FROM FILE cTemplate[4] TO lcSource .

                ASSIGN lcSource = SUBSTITUTE (lcSource,
                                              b_eTable.EntityClassName,
                                              b_eTable.EntityPackageName,
                                              "":U,
                                              "":U,
                                              STRING (NOW, "99.99.9999 HH:MM:SS":U)) .

                /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
                   user name, replace @USER@, with the current user name */
                ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                       lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                       lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                          b_eBusinessEntity.BusinessEntityName,
                                                                                                                          "EntityList":U)) .

                oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

                IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                    IF oWriteFileEventArgs:Cancel THEN
                        NEXT .

                    IF oWriteFileEventArgs:NewFileName > "":U THEN
                        cFileName = oWriteFileEventArgs:NewFileName .
                END.

                COPY-LOB FROM lcSource TO FILE cFileName .
            END.
        END .

        /* Generate List classes for top-nav-buffers */
        FOR EACH b_eTable:
            IF CAN-FIND (b_eDataRelation WHERE b_eDataRelation.ChildTempTableName = b_eTable.TempTableName) THEN
                NEXT .

            COPY-LOB FROM FILE cTemplate[3] TO lcSource .

            ASSIGN lcSource = SUBSTITUTE (lcSource,
                                          b_eTable.EntityClassName,
                                          b_eTable.EntityPackageName,
                                          STRING (NOW, "99.99.9999 HH:MM:SS":U))

                   cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.List&1_Generated":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
               user name, replace @USER@, with the current user name */
            ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                      b_eBusinessEntity.BusinessEntityName,
                                                                                                                      "EntityListGenerated":U)) .

            oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

            IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                IF oWriteFileEventArgs:Cancel THEN
                    NEXT .

                IF oWriteFileEventArgs:NewFileName > "":U THEN
                    cFileName = oWriteFileEventArgs:NewFileName .
            END.

            COPY-LOB FROM lcSource TO FILE cFileName .

            ASSIGN cFileName = cOutputFolder + ClassHelper:ClassNameToFile
                                   (SUBSTITUTE ("&2.List&1":U,
                                                b_eTable.EntityClassName,
                                                b_eTable.EntityPackageName)).

            FILE-INFORMATION:FILE-NAME = cFileName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
                COPY-LOB FROM FILE cTemplate[4] TO lcSource .

                ASSIGN lcSource = SUBSTITUTE (lcSource,
                                              b_eTable.EntityClassName,
                                              b_eTable.EntityPackageName,
                                              "":U,
                                              "":U,
                                              STRING (NOW, "99.99.9999 HH:MM:SS":U)) .

                /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
                   user name, replace @USER@, with the current user name */
                ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, BusinessEntityGenerator:GetUserName())
                       lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
                       lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(b_eBusinessEntity.BusinessEntityPackage,
                                                                                                                          b_eBusinessEntity.BusinessEntityName,
                                                                                                                          "EntityList":U)) .

                oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Entity Class":U, cFileName) .

                IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                    IF oWriteFileEventArgs:Cancel THEN
                        NEXT .

                    IF oWriteFileEventArgs:NewFileName > "":U THEN
                        cFileName = oWriteFileEventArgs:NewFileName .
                END.

                COPY-LOB FROM lcSource TO FILE cFileName .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the Entity Classes
        Notes:   This routine is intended to be used from outside of the Business Entity
                 designer (automatic source code generation, unit testing)
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BY-REFERENCE keyword
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateEntityClasses (DATASET FOR dsBusinessEntity):

        THIS-OBJECT:GenerateEntityClasses () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the GetCustomPropertyNamesHandler event of the
                 BusinessEntityDatasetController
        Notes:
        @param sender The reference to the object that raised the event
        @param e The GetCustomPropertyNamesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetCustomPropertyNamesHandler (sender AS Progress.Lang.Object,
                                                         e AS GetCustomPropertyNamesEventArgs):

        IF e:Scope = CustomPropertiesScopeEnum:BusinessEntity THEN
            e:PropertyNames:Add ("EntityClassGenerator.Disabled":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the GetCustomPropertyNamesHandler event of the
                 BusinessEntityDatasetController
        Notes:
        @param sender The reference to the object that raised the event
        @param e The GetCustomPropertyNamesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetCustomPropertyValuesHandler (sender AS Progress.Lang.Object,
                                                          e AS GetCustomPropertyValuesEventArgs):

        IF e:Scope = CustomPropertiesScopeEnum:BusinessEntity AND
           e:PropertyName = "EntityClassGenerator.Disabled":U THEN DO:
            e:PropertyValues:Add ("yes":U) .
            e:PropertyValues:Add ("no":U) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer will get closed
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer is starting and
                 has loaded the plugin.
        Notes:   Passes references to some key components of the Designer to
                 the plugin, use this to initialize the plugin, subscribe to events
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController,
                                poForm AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm,
                                poToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager):

        poController:AfterGenerateSource:Subscribe (AfterGenerateSourceHandler) .
        poController:AfterRegenerateSource:Subscribe (AfterGenerateSourceHandler) .

        poController:GetCustomPropertyNames:Subscribe (GetCustomPropertyNamesHandler) .

    END METHOD.

END CLASS.
