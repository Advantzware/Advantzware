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
    File        : BusinessEntityGenerator
    Purpose     : Generator for Business Entity and Data Access classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 17 22:32:09 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Services.*               FROM PROPATH .
USING Consultingwerk.Exceptions.*                                    FROM PROPATH .
USING Consultingwerk.Framework.*                                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                         FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                FROM PROPATH .
USING Consultingwerk.OERA.*                                          FROM PROPATH .
USING Consultingwerk.Studio.*                                        FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*                               FROM PROPATH .
USING Consultingwerk.Studio.SmartRefactor.*                          FROM PROPATH.
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Consultingwerk.Util.Forms.*                                    FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                            FROM PROPATH .
USING org.prorefactor.core.*                                         FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                                   FROM ASSEMBLY .
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.BusinessEntityGenerator
    IMPLEMENTS IBusinessEntityGenerator:

    /* Mike Fechner, Consultingwerk Ltd. 01.11.2014
       SCL-516: Dataset is protected to allow access from derived (customer extended) implementations
                of the BusinessEntityGenerator class */
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &ACCESS=PROTECTED }

    &scoped-define TempTableTemplate temp-table.template
    &scoped-define TempTableShortTemplate temp-table-short.template
    &scoped-define TempTableShortTemplateNoBefore temp-table-short-no-before.template
    &scoped-define DatasetTemplate dataset.template
    &scoped-define BusinessEntityTemplate businessentity.template
    &scoped-define DataAccessObjectTemplate dataaccess.template
    &scoped-define DatasetControllerTemplate datasetcontroller.template
    &scoped-define GenerateXsdTemplate generate-xsd.template

    DEFINE TEMP-TABLE ttDataSourceNames NO-UNDO
        FIELD DataSourceName AS CHARACTER
        FIELD TempTableName  AS CHARACTER
        INDEX DataSourceName IS UNIQUE PRIMARY DataSourceName
        INDEX TempTableName  IS UNIQUE TempTableName .

    DEFINE VARIABLE cUserName        AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cSourceFolder    AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cRCodeFolder     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cSourceFolderGui AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cRCodeFolderGui  AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cIncludeFolder   AS CHARACTER NO-UNDO .

    DEFINE VARIABLE lcTempTables     AS LONGCHAR  NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Business Entity Designer writes to an output file
        Notes:   Cancelable event, that can cancel the write of a single file. To
                 Cancel the whole business entity generation, event handlers should
                 throw an error
        @param sender The object that raised the WriteFile event
        @param e The WriteFileEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT WriteFile SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                         e AS WriteFileEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns a structure that contains the most recently generated file
                 names
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Generated AS Generated NO-UNDO
    GET.
    PROTECTED SET .

    /*------------------------------------------------------------------------------
        Purpose: Attempt to open files in OpenEdge Architect / Progress Developer Studio
                 for OpenEdge when done.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OpenFilesInIDE AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Ignore status messages on completion
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SilentMode AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: When SilentMode = TRUE this property gets and sets if the method
                 UpdateDataAccessObject should be updating the Data Access object
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SilentModeUpdateDataAccess AS LOGICAL NO-UNDO INITIAL TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityGenerator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityGenerator ():
        SUPER ().

        THIS-OBJECT:EvaluateFolderNames() .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the input string in the selected keyboard casing
        Notes:   See BusinessEntityDesignerSettings
        @param pcString The string value to fix the casing for
        @return The string in the desired casing
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER CaseKeywords (pcString AS CHARACTER):

        IF BusinessEntityDesignerSettings:KeywordCasing = "LOWER":U THEN
            RETURN LC (pcString) .
        ELSE
            RETURN UPPER (pcString) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the input string in the selected keyboard casing
        Notes:   See BusinessEntityDesignerSettings
        @param pcString The string value to fix the casing for
        @param pcKeepCase CHR(1) delimited list of strings contained in the pcString that should keep heir casing as provided in this parameter
        @return The string in the desired casing
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER CaseKeywords (pcString AS CHARACTER,
                                                 pcKeepCase AS CHARACTER):

        DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cText AS CHARACTER NO-UNDO.

        ASSIGN pcString = BusinessEntityGenerator:CaseKeywords (pcString) .

        DO i = 1 TO NUM-ENTRIES (pcKeepCase, CHR (1)):
            ASSIGN cText = ENTRY (i, pcKeepCase, CHR (1)) .

            ASSIGN pcString = REPLACE (pcString, cText, cText) .
        END.

        RETURN pcString .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies that all output directories exist
        Notes:
        @param plIncludeFilesOnly Only verify the directories of the include files
        @return Logical value indicating if the code generation should continue (FALSE will cancel the generator)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL CheckDirectoriesExist (plIncludeFilesOnly AS LOGICAL):

        DEFINE VARIABLE oReturn AS DialogResultEnum NO-UNDO .

        FIND FIRST eBusinessEntity .

        IF plIncludeFilesOnly = FALSE AND eBusinessEntity.BusinessEntityPackage > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = cSourceFolder + REPLACE (eBusinessEntity.BusinessEntityPackage, ".":U, "~\":U) .

            IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cSourceFolder + REPLACE (eBusinessEntity.BusinessEntityPackage, ".":U, "~\":U)),
                                     "Business Entity Generator"{&TRAN},
                                     SUBSTITUTE ("The directory ~"&1~" used for the Business Entity ~"&2~" does not exist."{&TRAN},
                                                 cSourceFolder + REPLACE (eBusinessEntity.BusinessEntityPackage, ".":U, "~\":U), eBusinessEntity.BusinessEntityName)) .
                ELSE
                    oReturn = DialogResultEnum:DialogResultYes .

                IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                    System.IO.Directory:CreateDirectory (cSourceFolder + REPLACE (eBusinessEntity.BusinessEntityPackage, ".":U, "~\":U)) .
                ELSE RETURN FALSE .
            END.
        END.

        IF plIncludeFilesOnly = FALSE AND eBusinessEntity.DataAccessPackage > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = cSourceFolder + REPLACE (eBusinessEntity.DataAccessPackage, ".":U, "~\":U) .

            IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cSourceFolder + REPLACE (eBusinessEntity.DataAccessPackage, ".":U, "~\":U)),
                                     "Business Entity Generator"{&TRAN},
                                     SUBSTITUTE ("The directory ~"&1~" used for the Data Access Object ~"&2~" does not exist."{&TRAN},
                                                 cSourceFolder + REPLACE (eBusinessEntity.DataAccessPackage, ".":U, "~\":U), eBusinessEntity.DataAccessName)) .
                ELSE
                    oReturn = DialogResultEnum:DialogResultYes .

                IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                    System.IO.Directory:CreateDirectory (cSourceFolder + REPLACE (eBusinessEntity.DataAccessPackage, ".":U, "~\":U)) .
                ELSE RETURN FALSE .
            END.
        END.

        IF plIncludeFilesOnly = FALSE AND eBusinessEntity.DatasetControllerPackage > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = cSourceFolderGui + REPLACE (eBusinessEntity.DatasetControllerPackage, ".":U, "~\":U) .

            IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cSourceFolderGui + REPLACE (eBusinessEntity.DatasetControllerPackage, ".":U, "~\":U)),
                                     "Business Entity Generator"{&TRAN},
                                     SUBSTITUTE ("The directory ~"&1~" used for the Dataset Controller ~"&2~" does not exist."{&TRAN},
                                                 cSourceFolderGui + REPLACE (eBusinessEntity.DatasetControllerPackage, ".":U, "~\":U), eBusinessEntity.DatasetControllerName)) .
                ELSE
                    oReturn = DialogResultEnum:DialogResultYes .

                IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                    System.IO.Directory:CreateDirectory (cSourceFolderGui + REPLACE (eBusinessEntity.DatasetControllerPackage, ".":U, "~\":U)) .
                ELSE RETURN FALSE .
            END.
        END.

        IF eBusinessEntity.DatasetPath > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = cIncludeFolder + eBusinessEntity.DatasetPath .

            IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cIncludeFolder + eBusinessEntity.DatasetPath),
                                     "Business Entity Generator"{&TRAN},
                                     SUBSTITUTE ("The directory ~"&1~" used for the dataset ~"&2~" does not exist."{&TRAN},
                                                 cIncludeFolder + eBusinessEntity.DatasetPath, eBusinessEntity.DatasetName)) .
                ELSE
                    oReturn = DialogResultEnum:DialogResultYes .

                IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                    System.IO.Directory:CreateDirectory (cIncludeFolder + eBusinessEntity.DatasetPath) .
                ELSE RETURN FALSE .
            END.
        END.

        FOR EACH eTable ON ERROR UNDO, THROW:
            IF eTable.TempTablePath > "":U THEN DO:
                FILE-INFORMATION:FILE-NAME = cIncludeFolder + eTable.TempTablePath .

                IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
                    IF THIS-OBJECT:SilentMode = FALSE THEN
                        oReturn = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                        (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cIncludeFolder + eTable.TempTablePath),
                                         "Business Entity Generator"{&TRAN},
                                         SUBSTITUTE ("The directory ~"&1~" used for temp-table ~"&2~" does not exist."{&TRAN},
                                                     cIncludeFolder + eTable.TempTablePath, eTable.TempTableName)) .
                    ELSE
                        oReturn = DialogResultEnum:DialogResultYes .

                    IF oReturn:Equals (DialogResultEnum:DialogResultYes) THEN
                            System.IO.Directory:CreateDirectory (cIncludeFolder + eTable.TempTablePath) .
                        ELSE RETURN FALSE .
                END.
            END.
        END.

        RETURN TRUE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates a template file and throws an Error when the template
                 cannot be located
        Notes:
        @param pcFileName The relative name of the template file
        @return Full pathname of the template
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER CheckTemplatePath (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            RETURN FILE-INFORMATION:FULL-PATHNAME .

        UNDO, THROW NEW AppError (SUBSTITUTE ("The template file ~"&1~" cannot be found."{&TRAN},
                                              pcFileName), 0) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if files are write protected and optionally prompts if files
                 should be overwritten
        Notes:
        @param plIncludeFilesOnly Logical flag indicating if only the include files should be verified
        @param plAskOverwrite Logical flag indicating if the Business Entity Generator should prompt for overwriting files
        @return Logical value indicating if no file is write protected
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL CheckNotWriteProtected (plIncludeFilesOnly AS LOGICAL,
                                                     plAskOverwrite AS LOGICAL):

        DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.

        FIND FIRST eBusinessEntity .

        IF NOT plIncludeFilesOnly THEN DO:
            ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.cls":U,
                                             REPLACE(eBusinessEntity.BusinessEntityPackage, ".":U, "/":U),
                                             eBusinessEntity.BusinessEntityName) .

            FILE-INFORMATION:FILE-NAME = cOutputFile .

            IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                    RETURN FALSE .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
                Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                                   MessageFormImages:ImageWarning) .

                RETURN FALSE .
            END.

            ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.cls":U,
                                             REPLACE(eBusinessEntity.DataAccessPackage, ".":U, "/":U),
                                             eBusinessEntity.DataAccessName) .

            FILE-INFORMATION:FILE-NAME = cOutputFile .

            IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                    RETURN FALSE .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
                Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                                   MessageFormImages:ImageWarning) .

                RETURN FALSE .
            END.

            ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.cls":U,
                                             REPLACE(eBusinessEntity.DatasetControllerPackage, ".":U, "/":U),
                                             eBusinessEntity.DatasetControllerName) .

            FILE-INFORMATION:FILE-NAME = cOutputFile .

            IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                    RETURN FALSE .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
                Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                                   MessageFormImages:ImageWarning) .

                RETURN FALSE .
            END.
        END.

        ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.i":U, eBusinessEntity.DatasetPath, eBusinessEntity.DatasetName) .

        FILE-INFORMATION:FILE-NAME = cOutputFile .

        IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                RETURN FALSE .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                               MessageFormImages:ImageWarning) .

            RETURN FALSE .
        END.

        ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.xsd":U, eBusinessEntity.DatasetPath, eBusinessEntity.DatasetName) .

        FILE-INFORMATION:FILE-NAME = cOutputFile .

        IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                RETURN FALSE .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                               MessageFormImages:ImageWarning) .

            RETURN FALSE .
        END.

        IF eBusinessEntity.DistinctTempTableIncludeFiles = TRUE THEN DO:
            FOR EACH eTable OF eBusinessEntity ON ERROR UNDO, THROW:
                ASSIGN cOutputFile = SUBSTITUTE ("&1~\&2.i":U, eTable.TempTablePath, eTable.TempTableName) .

                FILE-INFORMATION:FILE-NAME = cOutputFile .

                IF plAskOverwrite AND FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                    IF THIS-OBJECT:PromptForOverwrite (FILE-INFORMATION:FULL-PATHNAME) = FALSE THEN
                        RETURN FALSE .

                IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN DO:
                    Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ('The file "&1" is write protected'{&TRAN}, cOutputFile),
                                                                       MessageFormImages:ImageWarning) .

                    RETURN FALSE .
                END.
            END.
        END.

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the child tables of the given temp-table
        Notes:
        @param pcParentTable The name of the parent table to return the child tables of
        @return Comma delimited list of child temp-tables of the parent table
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ChildTablesOf (pcParentTable AS CHARACTER):

        DEFINE BUFFER b_eDataRelation FOR eDataRelation .

        DEFINE VARIABLE cChildTables AS CHARACTER NO-UNDO.

        FOR EACH b_eDataRelation WHERE b_eDataRelation.ParentTempTableName   = pcParentTable
                                   AND b_eDataRelation.DataRelationNotActive = FALSE
                                   AND b_eDataRelation.DataRelationRecursive = FALSE ON ERROR UNDO, THROW:

            ASSIGN cChildTables = cChildTables + b_eDataRelation.ChildTempTableName + ",":U +
                                  THIS-OBJECT:ChildTablesOf (b_eDataRelation.ChildTempTableName) + ",":U.
        END.

        RETURN REPLACE (TRIM (cChildTables, ",":U), ",,":U, ",":U) .

    END METHOD.

    /**
     * Purpose: Empties all Nodes sibling and children of the given poNode
     * Notes:   Used to clear the old AssignSkipDictionary statement when refreshing
     * @param poNode Node to clear
     * @param poRefactorSession Current refactoring session, used to replace node values
     */
    METHOD PRIVATE VOID ClearRestOfLine (poNode AS JPNode,
                                         poRefactorSession AS RefactorSession):

        DO WHILE VALID-OBJECT (poNode) ON ERROR UNDO, THROW:
            IF poNode:getText() > "":U THEN DO:
                poRefactorSession:ReplaceNodeContent(poNode, "":U).
                poNode = poNode:nextSibling ().
            END.
            ELSE DO:
                ClearRestOfLine (poNode:nextNode(),
                                 poRefactorSession).
                poNode = poNode:nextSibling ().
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Complies the generated BusinessEntity, DataAcess and DatasetController
                 classes
        Notes:
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Compile (DATASET FOR dsBusinessEntity):

        /* The generated file names */
        @SuppressUnusedWarnings.
        DEFINE VARIABLE cBusinessEntity    AS CHARACTER NO-UNDO .
        @SuppressUnusedWarnings.
        DEFINE VARIABLE cDataAccess        AS CHARACTER NO-UNDO .
        @SuppressUnusedWarnings.
        DEFINE VARIABLE cDatasetController AS CHARACTER NO-UNDO .

        DEFINE VARIABLE oStatusManager     AS IStatusManager NO-UNDO .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Compiling Business Entity code."{&TRAN}) .

        /* Mike Fechner, Consultingwerk Ltd. 11.04.2011
           Warning message when starting with not compile session */
        IF NOT SessionHelper:AllowsCompile() THEN DO:
            MessageFormHelper:ShowMessage ("You ABL session is not enabled for compile."{&TRAN} + System.Environment:NewLine +
                                           "The Business Entity Generator is unable to compile the generated source code."{&TRAN},
                                           "Business Entity Designer"{&TRAN},
                                           MessageFormImages:ImageError) .
            RETURN FALSE .
        END.

        FIND FIRST eBusinessEntity .

        ASSIGN cBusinessEntity = TRIM(SUBSTITUTE ("&3&1~\&2.cls":U,
                                                  REPLACE(eBusinessEntity.BusinessEntityPackage, ".":U, "~\":U),
                                                  eBusinessEntity.BusinessEntityName,
                                                  cSourceFolder), "~\":U) .

        ASSIGN cDataAccess = TRIM(SUBSTITUTE ("&3&1~\&2.cls":U,
                                              REPLACE(eBusinessEntity.DataAccessPackage, ".":U, "~\":U),
                                              eBusinessEntity.DataAccessName,
                                              cSourceFolder), "~\":U) .

        IF BusinessEntityDesignerSettings:CompileDatasetController THEN
            ASSIGN cDatasetController = TRIM(SUBSTITUTE ("&3&1~\&2.cls":U,
                                                         REPLACE(eBusinessEntity.DatasetControllerPackage, ".":U, "~\":U),
                                                         eBusinessEntity.DatasetControllerName,
                                                         cSourceFolderGui), "~\":U) .

        IF     NOT eBusinessEntity.GenerateBusinessEntity
           AND NOT eBusinessEntity.GenerateDataAccess
           AND NOT (BusinessEntityDesignerSettings:CompileDatasetController AND
                    eBusinessEntity.GenerateDatasetController ) THEN DO:

            IF NOT THIS-OBJECT:SilentMode THEN
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                        ("Nothing to compile!"{&TRAN},
                         "Business Entity Generator"{&TRAN}) .

            RETURN FALSE .
        END.

        IF eBusinessEntity.GenerateBusinessEntity THEN
            CompileFile (cBusinessEntity, cRCodeFolder) .
        ELSE
            ASSIGN cBusinessEntity = "Not compiled"{&TRAN} .

        IF eBusinessEntity.GenerateDataAccess THEN
            CompileFile (cDataAccess, cRCodeFolder) .
        ELSE
            ASSIGN cDataAccess = "Not compiled"{&TRAN} .

        IF BusinessEntityDesignerSettings:CompileDatasetController AND
           eBusinessEntity.GenerateDatasetController THEN
            CompileFile (cDatasetController, cRCodeFolderGui) .

        ELSE
            ASSIGN cDatasetController = "Not compiled"{&TRAN} .

        IF NOT THIS-OBJECT:SilentMode THEN
            Consultingwerk.Util.MessageFormHelper:ShowMessage
                    ("The compilation of the Business Entity completed successfully!"{&TRAN},
                     "Business Entity Generator"{&TRAN},
                     SUBSTITUTE ("Business Entity: &1&4Data Access Object: &2&4Dataset Controller: &3"{&TRAN},
                                 cBusinessEntity,
                                 cDataAccess,
                                 cDatasetController,
                                 CHR(13) + CHR(10))) .

        RETURN TRUE .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Compiles a file
        Notes:
        @param pcFileName The file name to compile
        @param pcSaveInto The directory to compile into
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CompileFile (pcFileName AS CHARACTER,
                                       pcSaveInto AS CHARACTER):

        Consultingwerk.Studio.CompilerHelper:CompileFile (pcFileName,
                                                          pcSaveInto) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Evaluates the source and r-code folder names
        Notes:   Variable cSourceFolder and cRCodeFolder
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EvaluateFolderNames ():

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolder > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolder .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cSourceFolder = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolder),
                                          0) .
        END.
        ELSE
            ASSIGN cSourceFolder = "":U .

        IF BusinessEntityDesignerSettings:RCodeBaseFolder > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:RCodeBaseFolder .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cRCodeFolder = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("R-Code folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:RCodeBaseFolder),
                                          0) .
        END.
        ELSE
            ASSIGN cRCodeFolder = "":U .

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolderGui > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolderGui .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cSourceFolderGui = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolderGui),
                                          0) .
        END.
        ELSE
            ASSIGN cSourceFolderGui = "":U .

        IF BusinessEntityDesignerSettings:RCodeBaseFolderGui > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:RCodeBaseFolderGui .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cRCodeFolderGui = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("R-Code folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:RCodeBaseFolderGui),
                                          0) .
        END.
        ELSE
            ASSIGN cRCodeFolderGui = "":U .

        IF BusinessEntityDesignerSettings:IncludeFilesBaseFolder > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:IncludeFilesBaseFolder .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cIncludeFolder = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:IncludeFilesBaseFolder),
                                          0) .
        END.
        ELSE
            ASSIGN cIncludeFolder = "":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the Business Entity and returns the JPNode representing the
                 ValidateData method
        Notes:
        @param poParseUnit
        @return The JPNode representing the ValidateData method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JPNode FindValidateDataMethod (poParseUnit AS ParseUnit):

        DEFINE VARIABLE oClassNode  AS JPNode        NO-UNDO .
        DEFINE VARIABLE oCodeBlock  AS JPNode        NO-UNDO .
        DEFINE VARIABLE oNode       AS JPNode        NO-UNDO .
        DEFINE VARIABLE oID         AS JPNode        NO-UNDO .

        oClassNode = ProparseHelper:FindChildNodeOfNodeType (poParseUnit:getTopNode(),
                                                             "CLASS":U) .

        oCodeBlock = ProparseHelper:FindChildNodeOfNodeType (oClassNode,
                                                             "Code_block":U) .

        IF NOT VALID-OBJECT (oCodeBlock) THEN
            RETURN ? .

        oNode = oCodeBlock:firstchild () .

        DO WHILE VALID-OBJECT (oNode):

            IF com.joanju.proparse.NodeTypes:getTypeName(oNode:getType()) =  "METHOD":U THEN DO:

                oID = ProparseHelper:FindChildNodeOfNodeType (oNode,
                                                              "ID":U) .

                IF oID:getText () = "ValidateData":U THEN
                    RETURN oNode .

            END.

            oNode = oNode:nextSibling () .
        END.

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the Business Entity
        Notes:
        @return The name of the generated business entity
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateBusinessEntity ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .

        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lcReceiveData       AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lcCalcBlock         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&BusinessEntityTemplate}":U) .

        FIND FIRST eBusinessEntity .

        COPY-LOB FROM FILE cTemplate TO lcSource .

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      eBusinessEntity.BusinessEntityName,
                                      eBusinessEntity.BusinessEntityPackage,
                                      eBusinessEntity.DatasetName,
                                      REPLACE (eBusinessEntity.DatasetPath, "~\":U, "/":U),
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                      eBusinessEntity.BusinessEntityPurpose,
                                      eBusinessEntity.BusinessEntityDescription,
                                      eBusinessEntity.DataAccessName,
                                      eBusinessEntity.DataAccessPackage) .

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(eBusinessEntity.BusinessEntityPackage,
                                                                                                                  eBusinessEntity.BusinessEntityName,
                                                                                                                  "BusinessEntity":U)).

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                              REPLACE(eBusinessEntity.BusinessEntityPackage, ".":U, "/":U),
                                              eBusinessEntity.BusinessEntityName,
                                              cSourceFolder), "/":U) .

        /* Mike Fechner, Consultingwerk Ltd. 24.09.2012
           Generate Calculated Fields in ReceiveData */
        FOR EACH eTable ON ERROR UNDO, THROW:
            ASSIGN lcCalcBlock = "":U .

            FOR EACH eField OF eTable WHERE eField.FieldCalcExpression > "":U ON ERROR UNDO, THROW:

                ASSIGN lcCalcBlock = lcCalcBlock +
                                     SUBSTITUTE (CaseKeywords("            ASSIGN &1.&2 = (&3) .":U),
                                                 eField.TempTableName,
                                                 eField.FieldName,
                                                 eField.FieldCalcExpression) +
                                     CHR (10) .
            END.

            IF lcCalcBlock > "":U THEN DO:
               ASSIGN lcReceiveData = lcReceiveData +
                                      CHR (10) +
                                      SUBSTITUTE (CaseKeywords("        FOR EACH &1 ON ERROR UNDO, THROW:":U), eTable.TempTableName) +
                                      CHR(10) +
                                      lcCalcBlock +
                                      CaseKeywords("        END.":U) +
                                      CHR(10).
            END.
        END.

        ASSIGN lcSource = REPLACE (lcSource, "@CALC_FIELDS@":U, lcReceiveData)
               lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Business Entity":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cOutputFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cOutputFile) THEN
        COPY-LOB FROM lcSource
                 TO FILE cOutputFile .

        FileHelper:EnsureFileNameCase (cOutputFile) .

        THIS-OBJECT:UpdateBusinessEntityValidation (cOutputFile) .

        RETURN cOutputFile .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the source code
        Notes:   This method will overwrite eventually existing source code
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GenerateCode (DATASET FOR dsBusinessEntity):

        /* The generated file names */
        DEFINE VARIABLE cBusinessEntity    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDataAccess        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDatasetController AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDataset           AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTempTables        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cXsdFile           AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oStatusManager     AS IStatusManager NO-UNDO .

        DEFINE BUFFER eBusinessEntity FOR eBusinessEntity .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Generating Business Entity code."{&TRAN}) .

        ASSIGN THIS-OBJECT:Generated = ?
               lcTempTables          = "":U .

        THIS-OBJECT:ValidateDesign () .

        IF NOT THIS-OBJECT:CheckDirectoriesExist (FALSE) THEN
            RETURN FALSE .

        IF NOT THIS-OBJECT:CheckNotWriteProtected (FALSE, TRUE) THEN
            RETURN FALSE .

        FIND FIRST eBusinessEntity .

        ASSIGN cUserName = BusinessEntityGenerator:GetUserName() .

        ASSIGN cTempTables        = GenerateTempTables ()
               cDataset           = GenerateDataset () .

        IF eBusinessEntity.GenerateBusinessEntity THEN
             cBusinessEntity = GenerateBusinessEntity () .

        IF eBusinessEntity.GenerateDataAccess THEN
             cDataAccess = GenerateDataAccessObject () .

        IF eBusinessEntity.GenerateDatasetController THEN
            cDatasetController = GenerateDatasetController () .

        ASSIGN cXsdFile = GenerateXmlSchema() .

        IF THIS-OBJECT:OpenFilesInIDE THEN
            OpenFilesInIDE (SUBSTITUTE ("&1,&2,&3,&4,&5,&6":U,
                                        cBusinessEntity,
                                        cDataAccess,
                                        cDatasetController,
                                        cDataset,
                                        cTempTables,
                                        cXsdFile)) .

        ASSIGN THIS-OBJECT:Generated = NEW Generated (cBusinessEntity,
                                 cDataAccess,
                                 cDatasetController,
                                 cDataset,
                                 cTempTables,
                                                      cXsdFile) .

/*        IF NOT THIS-OBJECT:SilentMode THEN                                                                                                                  */
/*            Consultingwerk.Util.MessageFormHelper:ShowMessage                                                                                               */
/*                    ("The Business Entity Generator completed successfully!"{&TRAN},                                                                        */
/*                     "Business Entity Generator"{&TRAN},                                                                                                    */
/*                     SUBSTITUTE ("Business Entity: &1&6Data Access Object: &2&6Dataset Controller: &3&6Dataset: &4&6Temp Table(s):&5&6XML Schema:&7"{&TRAN},*/
/*                                 cBusinessEntity,                                                                                                           */
/*                                 cDataAccess,                                                                                                               */
/*                                 cDatasetController,                                                                                                        */
/*                                 cDataset,                                                                                                                  */
/*                                 cTempTables,                                                                                                               */
/*                                 CHR(13) + CHR(10),                                                                                                         */
/*                                 cXsdFile)) .                                                                                                               */

        RETURN TRUE .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates ther Data Access Object
        Notes:
        @return The name of the generated data access object
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateDataAccessObject ():

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE cOutputFile        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cTemplate          AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&DataAccessObjectTemplate}":U) .

        COPY-LOB FROM FILE cTemplate TO lcSource .

        FIND FIRST eBusinessEntity .

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      eBusinessEntity.DataAccessName,
                                      eBusinessEntity.DataAccessPackage,
                                      eBusinessEntity.DatasetName,
                                      REPLACE (eBusinessEntity.DatasetPath, "~\":U, "/":U),
                                      TRIM (lcDefineDataSource, CHR (10)),
                                      TRIM (lcAttachDataSource, CHR (10)),
                                      TRIM (lcDetachDataSource, CHR (10)),
                                      lcSourceColumn,
                                      lcDefaultQuery)

        /* Mike Fechner, Consultingwerk Ltd. 12.02.2011
           time stamp, replace from @DATE@, not substitute as substitute only handles 9 items */
               lcSource = REPLACE (lcSource, "@DATE@":U, STRING (NOW, "99.99.9999 HH:MM:SS":U))
               .

        /* Marko Rüterbories, Consultingwerk Ltd. 01.12.2016
           SCL-1377 generate AssignSkipDictionary, replace @AssignSkipDictionary@ */
        ASSIGN lcSource = REPLACE (lcSource, "@AssignSkipDictionary@":U, GetAssignSkipDictionaryComplete ()).

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(eBusinessEntity.BusinessEntityPackage,
                                                                                                                  eBusinessEntity.BusinessEntityName,
                                                                                                                  "DataAccess":U))
               lcSource = StringHelper:FixLineEnding (lcSource) .

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                              REPLACE(eBusinessEntity.DataAccessPackage, ".":U, "/":U),
                                              eBusinessEntity.DataAccessName,
                                              cSourceFolder), "/":U) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cOutputFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cOutputFile) THEN
        COPY-LOB FROM lcSource
                 TO FILE cOutputFile .

        FileHelper:EnsureFileNameCase (cOutputFile) .

        RETURN cOutputFile .

        FINALLY:
            EMPTY TEMP-TABLE ttDataSourceNames .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the dataset include file
        Notes:
        @return The file name of the generated dataset include file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateDataset ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE lcIncludes          AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE lcTables            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cTopNavBuffers      AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTables             AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE i                   AS INTEGER            NO-UNDO .
        DEFINE VARIABLE lcRelations         AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO.

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&DatasetTemplate}":U) .

        FIND FIRST eBusinessEntity .

        COPY-LOB FROM FILE cTemplate TO lcSource .

        ASSIGN cTopNavBuffers = THIS-OBJECT:GetTopNavBuffers() .

        DO i = 1 TO NUM-ENTRIES (cTopNavBuffers):
            ASSIGN cTables = cTables + ENTRY (i, cTopNavBuffers) + ",":U .

            ASSIGN cTables = ListHelper:AddUniqueEntries (cTables, THIS-OBJECT:ChildTablesOf (ENTRY (i, cTopNavBuffers)))  + ",":U.
        END.

        ASSIGN cTables = TRIM (cTables, ",":U) .

        tableLoop:
        DO i = 1 TO NUM-ENTRIES (cTables):
            IF ENTRY (i, cTables) = "":U THEN
                NEXT tableLoop .

            FIND eTable WHERE eTable.TempTableName = ENTRY (i, cTables) NO-ERROR  .

            IF NOT AVAILABLE eTable THEN
                NEXT tableLoop .

            ASSIGN lcIncludes = lcIncludes +
                                SUBSTITUTE ("~{ &1&3&2.i &4~}":U,
                                            REPLACE (eTable.TempTablePath, "~\":U, "/":U),
                                            eTable.TempTableName,
                                            (IF eTable.TempTablePath > "":U THEN "/":U ELSE "":U),
                                            (IF eTable.NoBeforeTable THEN "&NO-BEFORE=YES ":U ELSE "":U)) + CHR (10)
                   lcTables   = lcTables + eTable.TempTableName + "~{&SUFFIX}, ":U .

        END.

        FOR EACH eDataRelation WHERE eDataRelation.BusinessEntityName = eBusinessEntity.BusinessEntityName ON ERROR UNDO, THROW:
            ASSIGN lcRelations = lcRelations +
                                 SUBSTITUTE (CaseKeywords("    DATA-RELATION &1 FOR &2~{&&SUFFIX}, &3~{&&SUFFIX} &4        RELATION-FIELDS (&5)":U),
                                             eDataRelation.DataRelationName,
                                             eDataRelation.ParentTempTableName,
                                             eDataRelation.ChildTempTableName,
                                             CHR(10),
                                             eDataRelation.RelationFields) + CHR(10) .

            IF eDataRelation.DataRelationReposition OR
               eDataRelation.DataRelationNested     OR
               eDataRelation.DataRelationNotActive  OR
               eDataRelation.DataRelationRecursive  THEN

               ASSIGN lcRelations = lcRelations + "        ":U +
                                    (IF eDataRelation.DataRelationReposition THEN CaseKeywords("REPOSITION ":U) ELSE "":U) +
                                    (IF eDataRelation.DataRelationNested     THEN CaseKeywords("NESTED ":U) + (IF eDataRelation.DataRelationForeignKeyHidden THEN CaseKeywords("FOREIGN-KEY-HIDDEN ":U) ELSE "":U) ELSE "":U) +
                                    (IF eDataRelation.DataRelationNotActive  THEN CaseKeywords("NOT-ACTIVE ":U) ELSE "":U) +
                                    (IF eDataRelation.DataRelationRecursive  THEN CaseKeywords("RECURSIVE ":U) ELSE "":U) +
                                    CHR(10)
                                    .
        END.

        ASSIGN lcTables = TRIM (TRIM(lcTables), ",":U) .

        IF eBusinessEntity.DistinctTempTableIncludeFiles = FALSE THEN
            ASSIGN lcIncludes = lcTempTables .

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      eBusinessEntity.DatasetName,
                                      lcTables,
                                      lcIncludes,
                                      lcRelations,
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                      eBusinessEntity.BusinessEntityPurpose,
                                      eBusinessEntity.BusinessEntityDescription) .

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(eBusinessEntity.BusinessEntityPackage,
                                                                                                                  eBusinessEntity.BusinessEntityName,
                                                                                                                  "Dataset":U))
               lcSource = StringHelper:FixLineEnding (lcSource) .

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.i":U,
                                              eBusinessEntity.DatasetPath,
                                              eBusinessEntity.DatasetName,
                                              cIncludeFolder), "/":U) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Dataset":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cOutputFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cOutputFile) THEN
        COPY-LOB FROM lcSource TO FILE cOutputFile .

        FileHelper:EnsureFileNameCase (cOutputFile) .

        RETURN cOutputFile .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the dataset controller
        Notes:
        @return The file name of the generated dataset controller
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateDatasetController ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .

        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&DatasetControllerTemplate}":U) .

        FIND FIRST eBusinessEntity .

        COPY-LOB FROM FILE cTemplate TO lcSource .

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      eBusinessEntity.DatasetControllerName,
                                      eBusinessEntity.DatasetControllerPackage,
                                      eBusinessEntity.DatasetName,
                                      REPLACE (eBusinessEntity.DatasetPath, "~\":U, "/":U),
                                      STRING (NOW, "99.99.9999 HH:MM:SS":U)) .

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, BusinessEntityGenerator:GenerateFileAnnotation(eBusinessEntity.BusinessEntityPackage,
                                                                                                                  eBusinessEntity.BusinessEntityName,
                                                                                                                  "DatasetController":U))
               lcSource = StringHelper:FixLineEnding (lcSource) .

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                         REPLACE(eBusinessEntity.DatasetControllerPackage, ".":U, "/":U),
                                         eBusinessEntity.DatasetControllerName,
                                         cSourceFolderGui), "/":U) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("DatasetController":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cOutputFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cOutputFile) THEN
        COPY-LOB FROM lcSource TO FILE cOutputFile .

        FileHelper:EnsureFileNameCase (cOutputFile) .

        RETURN cOutputFile .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates the DataSource Object Details
        Notes:
        @param lcDefineDataSource The code block for the data source definition
        @param lcAttachDataSource The code block for attaching the data sources
        @param lcDetachDataSource The code block the detaching the data sources
        @param lcSourceColumn The code block for the SourceColumn method
        @param lcDefaultQuery The code block for the SourceDefaultQuery method
        @param poDataSourceNames The CharacterDictionary of data source names
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource AS LONGCHAR,
                                                           OUTPUT lcAttachDataSource AS LONGCHAR,
                                                           OUTPUT lcDetachDataSource AS LONGCHAR,
                                                           OUTPUT lcSourceColumn     AS LONGCHAR,
                                                           OUTPUT lcDefaultQuery     AS LONGCHAR,
                                                           OUTPUT poDataSourceNames  AS CharacterDictionary):

        DEFINE VARIABLE cSourceBuffer      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cShortTableNames   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cEntry             AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lcDefineBuffers    AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cDataSourceName    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTmpDataSourceName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j                  AS INTEGER   NO-UNDO .

        DEFINE VARIABLE iTable             AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cQuery             AS CHARACTER NO-UNDO .

        DEFINE VARIABLE cFieldMapping      AS CHARACTER NO-UNDO .

        EMPTY TEMP-TABLE ttDataSourceNames .

        ASSIGN poDataSourceNames = NEW CharacterDictionary () .

        FIND FIRST eBusinessEntity .

        FOR EACH eTable WHERE eTable.BusinessEntityName = eBusinessEntity.BusinessEntityName ON ERROR UNDO, THROW:

            IF eTable.SourceTableNames > "":U THEN DO:

                ASSIGN cDataSourceName = ValidABLIdentifier ("src_":U + TRIM(REPLACE (THIS-OBJECT:UnqualifiedTableNames (eTable.SourceTableNames), ",":U, "":U)))
                       i = 0 .

                /* Mike Fechner, Consultingwerk Ltd. 12.02.2011
                   Ensure unique DATA-SOURCE Names */
                IF CAN-FIND (ttDataSourceNames WHERE ttDataSourceNames.DataSourceName = cDataSourceName) THEN DO:

                    dataSourceNameLoop: REPEAT ON ERROR UNDO, THROW:
                        ASSIGN i                  = i + 1
                               cTmpDataSourceName = cDataSourceName + STRING (i) .

                        IF NOT CAN-FIND (ttDataSourceNames WHERE ttDataSourceNames.DataSourceName = cTmpDataSourceName) THEN
                            LEAVE dataSourceNameLoop .
                    END .

                    ASSIGN cDataSourceName = cTmpDataSourceName .
                END.

                CREATE ttDataSourceNames .
                ASSIGN ttDataSourceNames.TempTableName  = eTable.TempTableName
                       ttDataSourceNames.DataSourceName = cDataSourceName .
                RELEASE ttDataSourceNames .

                poDataSourceNames:Add (eTable.TempTableName,
                                       cDataSourceName) .

                IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN DO:

                    IF eTable.SourceBufferNames > "":U THEN
                        cSourceBuffer = ENTRY (1, eTable.SourceBufferNames) .
                    ELSE
                        cSourceBuffer = eTable.SourceTableNames .

                    ASSIGN lcDefineDataSource = lcDefineDataSource +
                                                SUBSTITUTE (CaseKeywords("    DEFINE PRIVATE DATA-SOURCE &2 FOR &1 &3.":U),
                                                            cSourceBuffer, cDataSourceName, eTable.SourceBufferKeys) + CHR (10) .

                    IF cSourceBuffer <> eTable.SourceTableNames THEN
                        ASSIGN lcDefineBuffers = lcDefineBuffers +
                                                 SUBSTITUTE (CaseKeywords("    DEFINE PRIVATE BUFFER &1 FOR &2 .":U),
                                                             cSourceBuffer, eTable.SourceTableNames) + CHR (10) .
                END.
                ELSE IF NUM-ENTRIES (eTable.SourceTableNames) > 1 THEN DO:
                    ASSIGN cSourceBuffer = "":U .

                    DO i = 1 TO NUM-ENTRIES (eTable.SourceTableNames):
                        IF NUM-ENTRIES (eTable.SourceBufferNames) >= i AND
                           ENTRY (i, eTable.SourceBufferNames) > "":U THEN

                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceBufferNames)), ",":U) .
                        ELSE
                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceTableNames)), ",":U) .

                        IF ENTRY (i, cSourceBuffer) <> ENTRY (i, eTable.SourceTableNames) THEN
                            ASSIGN lcDefineBuffers = lcDefineBuffers +
                                                     SUBSTITUTE (CaseKeywords("    DEFINE PRIVATE BUFFER &1 FOR &2 .":U),
                                                                 ENTRY (i, cSourceBuffer), ENTRY (i, eTable.SourceTableNames)) + CHR (10) .
                    END.

                    ASSIGN cShortTableNames = eTable.SourceTableNames .

                    DO j = 1 TO NUM-ENTRIES (cShortTableNames):
                        ASSIGN cEntry                      = ENTRY (j, cShortTableNames)
                               ENTRY (j, cShortTableNames) = ENTRY (NUM-ENTRIES (cEntry, ".":U), cEntry, ".":U) .
                    END.

                    ASSIGN lcDefineDataSource = lcDefineDataSource +
                                                SUBSTITUTE (CaseKeywords("    DEFINE QUERY &1 FOR &2 .":U) + CHR(10) +
                                                            CaseKeywords("    DEFINE PRIVATE DATA-SOURCE &3 FOR QUERY &1 &4.":U),
                                                            ValidABLIdentifier ("qry_":U + REPLACE(cShortTableNames, ",":U, "":U)),
                                                            REPLACE(cSourceBuffer, ",":U, ", ":U),
                                                            cDataSourceName,
                                                            eTable.SourceBufferKeys) + CHR (10) .
                END.
            END. /* eTable.SourceTableNames > "" */

            IF NUM-ENTRIES (eTable.SourceTableNames) > 0 THEN DO:
                ASSIGN cFieldMapping = GetFieldMapping (eTable.TempTableName, 38) .

                ASSIGN lcAttachDataSource = lcAttachDataSource +
                                            SUBSTITUTE (CaseKeywords("        THIS-OBJECT:AttachDataSource (BUFFER &1:HANDLE,~n":U, ":AttachDataSource ":U)  +
                                                        CaseKeywords("                                      DATA-SOURCE &2:HANDLE, ~"~n":U) +
                                                        CaseKeywords("&3~":U) .":U),
                                                        eTable.TempTableName,
                                                        cDataSourceName,
                                                        cFieldMapping) + CHR(10)

                       lcDetachDataSource = lcDetachDataSource +
                                            SUBSTITUTE (CaseKeywords("        BUFFER &1:DETACH-DATA-SOURCE () .":U),
                                                        eTable.TempTableName) + CHR(10)

                       lcSourceColumn = lcSourceColumn +
                                        SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                    CaseKeywords("                cMap = DATA-SOURCE &2:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . ":U, "cMap = ":U),
                                                    eTable.TempTableName,
                                                    cDataSourceName) + CHR (10) .

                       IF eTable.SourceDefaultQuery > "":U THEN DO:
                           eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, " ":U + CHR(13), "":U) .
                           eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, CHR(13), "":U) .
                           eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, CHR(10), " ":U) .

                           lcDefaultQuery = lcDefaultQuery +
                                            SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10)+
                                                        CaseKeywords("                RETURN ~"&3~":U. ":U),
                                                        eTable.TempTableName,
                                                        REPLACE(eTable.SourceTableNames, ",":U, "":U),
                                                        REPLACE (eTable.SourceDefaultQuery, "~"":U, "~~~"":U)) + CHR (10) .
                       END.
                       ELSE
                           IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN
                               lcDefaultQuery = lcDefaultQuery +
                                                SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                            CaseKeywords("                RETURN ~"FOR EACH &2 INDEXED-REPOSITION~":U. ":U),
                                                            eTable.TempTableName,
                                                            THIS-OBJECT:UnqualifiedTableNames(cSourceBuffer)) + CHR (10) .

                           ELSE DO:
                               /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
                                  Joined Query for DataSource */
                               ASSIGN cQuery = SUBSTITUTE(CaseKeywords("FOR EACH &1":U), ENTRY (1, cSourceBuffer)) .

                               DO iTable = 2 TO NUM-ENTRIES (cSourceBuffer):
                                    ASSIGN cQuery = cQuery + SUBSTITUTE (CaseKeywords(", FIRST &2 OF &1":U),
                                                                         ENTRY (1, THIS-OBJECT:UnqualifiedTableNames(cSourceBuffer)),
                                                                         ENTRY (iTable, THIS-OBJECT:UnqualifiedTableNames(cSourceBuffer))) .
                               END.

                               ASSIGN cQuery = cQuery + CaseKeywords(" INDEXED-REPOSITION":U)

                                      lcDefaultQuery = lcDefaultQuery +
                                                       SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                                   CaseKeywords("                RETURN ~"&2~":U. ":U),
                                                                   eTable.TempTableName,
                                                                   cQuery) + CHR (10) .
                           END. /* ELSE DO: */
            END. /* IF NUM-ENTRIES (eTable.SourceTableNames) > 0 */

        END. /* FOR EACH eTable */

        IF lcDefineBuffers > "":U THEN
            ASSIGN lcDefineDataSource = lcDefineBuffers + CHR (10) +
                                        lcDefineDataSource .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code for the @FILEANNOTATION@ based on the
                 role of the file and the business entity
        Notes:
        @param pcEntityPackage The Business Entity package
        @param pcEntityName The Business Entity name
        @param pcFileType The file type
        @return The source code for the @FILEANNOTATION@
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GenerateFileAnnotation (pcEntityPackage AS CHARACTER,
                                                           pcEntityName AS CHARACTER,
                                                           pcFileType AS CHARACTER):

        DEFINE VARIABLE cEntity AS CHARACTER NO-UNDO.

        IF pcEntityPackage > "":U THEN
            ASSIGN cEntity = SUBSTITUTE ("&1.&2":U, pcEntityPackage, pcEntityName) .
        ELSE
            ASSIGN cEntity = pcEntityName .

        RETURN SUBSTITUTE ('@BusinessEntityGenerator (entityname="&1", type="&2") .':U,
                           cEntity, pcFileType) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the temp-table include files
        Notes:
        @return Comma-delimited list of the generated temp-table include files
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateTempTables ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cFields             AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cIndexes            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cBeforeTable        AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTableOptions       AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE i                   AS INTEGER            NO-UNDO .

        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTemplate1          AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cTemplate2          AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cReturn             AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cDataType           AS CHARACTER          NO-UNDO .

        DEFINE VARIABLE lcAnnotations       AS LONGCHAR           NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        FIX-CODEPAGE (lcAnnotations) = Codepages:UTF-8 .

        FIND FIRST eBusinessEntity .

        IF eBusinessEntity.DistinctTempTableIncludeFiles THEN
            ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&TempTableTemplate}":U) .
        ELSE
            ASSIGN cTemplate1 = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&TempTableShortTemplate}":U)
                   cTemplate2 = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&TempTableShortTemplateNoBefore}":U) .

        tableLoop:
        FOR EACH eTable WHERE eTable.BusinessEntityName = eBusinessEntity.BusinessEntityName ON ERROR UNDO, THROW:

            IF eBusinessEntity.DistinctTempTableIncludeFiles = FALSE THEN DO:
               IF eTable.NoBeforeTable = FALSE THEN
                   ASSIGN cTemplate = cTemplate1 .
               ELSE
                   ASSIGN cTemplate = cTemplate2 .
            END.

            COPY-LOB FROM FILE cTemplate TO lcSource .

            ASSIGN cFields = "":U .

            FOR EACH eField WHERE eField.TempTableName = eTable.TempTableName
                               BY eField.FieldOrder:

                IF eField.FieldDataType = "PROGRESS.LANG.OBJECT":U THEN
                    ASSIGN cDataType = "Progress.Lang.Object":U .
                ELSE
                    ASSIGN cDataType = CaseKeywords(eField.FieldDataType) .

                ASSIGN cFields = cFields +
                                 SUBSTITUTE (CaseKeywords("    FIELD &1 AS &2":U),
                                             eField.FieldName,
                                             cDataType) .

                IF eField.FieldExtent > 0 THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" EXTENT &1":U), eField.FieldExtent) .

                IF eField.FieldFormat > "":U AND eField.FieldDataType <> "RAW":U AND eField.FieldDataType <> "Progress.Lang.Object":U THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" FORMAT ~"&1~":U":U), REPLACE(REPLACE (eField.FieldFormat, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .

                /* Mike Fechner, Consultingwerk Ltd. 15.11.2014
                   SCL-548 - don't generate INITIAL for RAW and ROWID, to avoid the following error (MEMPTR not valid in temp-tables):
                   Bad option(s) was used for a RAW, ROWID, MEMPTR or non-displayable variable definition. (1843) */
                IF eField.FieldDataType <> "RAW":U AND eField.FieldDataType <> "ROWID":U THEN DO:
                    IF eField.FieldInitialUnknownValue = TRUE THEN
                        ASSIGN cFields = cFields + CaseKeywords(" INITIAL ?":U) .
                    ELSE IF eField.FieldInitial > "":U THEN
                        ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" INITIAL ~"&1~":U":U), REPLACE(REPLACE (eField.FieldInitial, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .
                END.

                IF eField.FieldLabel > "":U AND eField.FieldDataType <> "RAW":U AND eField.FieldDataType <> "ROWID":U THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" LABEL ~"&1~":T":U), REPLACE(REPLACE (eField.FieldLabel, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .

                IF eField.FieldCaseSensitive THEN
                    ASSIGN cFields = cFields + CaseKeywords(" CASE-SENSITIVE":U) .

                IF eField.FieldSerializeHidden THEN
                    ASSIGN cFields = cFields + CaseKeywords(" SERIALIZE-HIDDEN":U) .

                IF eField.FieldSerializeName > "":U AND eField.FieldSerializeName <> eField.FieldName THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" SERIALIZE-NAME ~"&1~":U":U), REPLACE(REPLACE (eField.FieldSerializeName, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .

                IF eField.FieldXmlDataType > "":U AND eField.FieldXmlDataType <> DataTypeHelper:DefaultXmlSchemaType (eField.FieldDataType) THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" XML-DATA-TYPE ~"&1~":U":U), REPLACE(REPLACE (eField.FieldXmlDataType, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .

                IF eField.FieldXmlNodeType > "":U AND eField.FieldXmlNodeType <> "ELEMENT":U THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" XML-NODE-TYPE ~"&1~":U":U), REPLACE(REPLACE (eField.FieldXmlNodeType, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .
                ELSE
                    IF eField.FieldDataType = "Progress.Lang.Object":U THEN
                        ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" XML-NODE-TYPE ~"&1~":U":U), CaseKeywords("HIDDEN":U)) .

                IF eField.FieldXmlNodeName > "":U AND eField.FieldXmlNodeName <> eField.FieldName THEN
                    ASSIGN cFields = cFields + SUBSTITUTE (CaseKeywords(" XML-NODE-NAME ~"&1~":U":U), REPLACE(REPLACE (eField.FieldXmlNodeName, "~"":U, "~~~"":U), "~{":U, "~~~{":U)) .

                ASSIGN cFields = cFields + CHR (10) .
            END.

            ASSIGN cIndexes = "":U .

            indexLoop:
            FOR EACH eIndex WHERE eIndex.TempTableName = eTable.TempTableName
                               BY eIndex.IndexOrder:

                IF eIndex.FieldNames > "":U THEN .
                ELSE
                    NEXT indexLoop .

                ASSIGN cIndexes = cIndexes + CaseKeywords("    INDEX ":U) + eIndex.IndexName .

                IF eIndex.IndexUnique OR eIndex.IndexPrimary OR eIndex.IndexWordIndex THEN DO:
                    ASSIGN cIndexes = cIndexes + CaseKeywords(" AS":U) .

                    IF eIndex.IndexUnique THEN
                        ASSIGN cIndexes = cIndexes + CaseKeywords(" UNIQUE":U) .
                    IF eIndex.IndexPrimary THEN
                        ASSIGN cIndexes = cIndexes + CaseKeywords(" PRIMARY":U) .
                    IF eIndex.IndexWordIndex THEN
                        ASSIGN cIndexes = cIndexes + CaseKeywords(" WORD-INDEX":U) .
                END.

                DO i = 1 TO NUM-ENTRIES (eIndex.FieldNames):
                    ASSIGN cIndexes = cIndexes + " ":U + ENTRY (i, eIndex.FieldNames) + " ":U
                                                       + CaseKeywords (ENTRY (i, eIndex.FieldSort)).
                END.

                ASSIGN cIndexes = cIndexes + CHR (10) .
            END.

            ASSIGN cTableOptions = "":U .

            IF eTable.NamespaceUri > "":U THEN
                ASSIGN cTableOptions = cTableOptions + SUBSTITUTE (CaseKeywords("NAMESPACE-URI ~"&1~":U ":U), eTable.NamespaceUri) .
            IF eTable.NamespacePrefix > "":U THEN
                ASSIGN cTableOptions = cTableOptions + SUBSTITUTE (CaseKeywords("NAMESPACE-PREFIX ~"&1~":U ":U), eTable.NamespacePrefix) .
            IF eTable.XmlNodeName > "":U THEN
                ASSIGN cTableOptions = cTableOptions + SUBSTITUTE (CaseKeywords("XML-NODE-NAME ~"&1~":U ":U), eTable.XmlNodeName) .
            IF eTable.SerializeName > "":U THEN
                ASSIGN cTableOptions = cTableOptions + SUBSTITUTE (CaseKeywords("SERIALIZE-NAME ~"&1~":U ":U), eTable.SerializeName) .

            ASSIGN cBeforeTable = (IF eTable.TempTableBeforeName > "":U THEN CaseKeywords("BEFORE-TABLE":U) ELSE "":U)
                   lcSource = SUBSTITUTE (lcSource,
                                          eTable.TempTableName,
                                          eTable.TempTableBeforeName,
                                          cBeforeTable,
                                          cFields,
                                          cIndexes,
                                          STRING (NOW, "99.99.9999 HH:MM:SS":U),
                                          eTable.TempTablePurpose,
                                          eTable.TempTableDescription,
                                          cTableOptions) .

            /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
               user name, replace @USER@, with the current user name */
            ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
                   lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())

            /* Annotations */
                   lcAnnotations = BusinessEntityGenerator:GenerateFileAnnotation (eBusinessEntity.BusinessEntityPackage,
                                                                                   eBusinessEntity.BusinessEntityName,
                                                                                   "TempTable":U)

                   lcSource = REPLACE (lcSource, "@FILEANNOTATION@":U, lcAnnotations) .

            ASSIGN lcAnnotations = "":U .

            IF CAN-FIND (FIRST eIndex WHERE eIndex.TempTableName = eTable.TempTableName
                                        AND eIndex.IndexPrimary
                                        AND eIndex.IndexUnique) THEN DO:

                FIND FIRST eIndex WHERE eIndex.TempTableName = eTable.TempTableName
                                    AND eIndex.IndexPrimary
                                    AND eIndex.IndexUnique .

                ASSIGN lcAnnotations = SUBSTITUTE ("@openapi.openedge.entity.primarykey (fields=~"&1~").~n":U,
                                                   REPLACE (eIndex.FieldNames, " ":U, "":U)) .
            END.

            FOR EACH eField WHERE eField.TempTableName     = eTable.TempTableName
                              AND eField.FieldSemanticType > "":U ON ERROR UNDO, THROW:

                ASSIGN lcAnnotations = lcAnnotations +
                                       SUBSTITUTE ("@openapi.openedge.entity.field.property (field=~"&1~", name=~"semanticType~", value=~"&2~").~n":U,
                                                   eField.FieldName,
                                                   eField.FieldSemanticType) .
            END .

            ASSIGN lcSource = REPLACE (lcSource, "@STARNOVAANNOTATIONS@":U, lcAnnotations) .
                   lcSource = StringHelper:FixLineEnding (lcSource) .

            IF eBusinessEntity.DistinctTempTableIncludeFiles THEN DO:
                ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.i":U,
                                                      eTable.TempTablePath,
                                                      eTable.TempTableName,
                                                      cIncludeFolder), "/":U) .

                oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("TempTable":U, cOutputFile) .

                IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
                    IF oWriteFileEventArgs:Cancel THEN
                        NEXT tableLoop .

                    IF oWriteFileEventArgs:NewFileName > "":U THEN
                        cOutputFile = oWriteFileEventArgs:NewFileName .
                END.

                IF NOT FileHelper:Exists (cOutputFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cOutputFile) THEN
                COPY-LOB FROM lcSource TO FILE cOutputFile .

                FileHelper:EnsureFileNameCase (cOutputFile) .

                ASSIGN cReturn = cReturn + cOutputFile + ",":U.
            END.
            ELSE
                ASSIGN lcTempTables = lcTempTables + lcSource .
        END.

        RETURN TRIM (cReturn, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Generates XML schema representation of the Dataset
        Notes:   Uses DATASET:WRITE-XMLSCHEMA method
        @return The file name of the generated xsd file
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateXmlSchema ():

        DEFINE VARIABLE cXsdFile            AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cTempFileDotP       AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO.
        DEFINE VARIABLE i                   AS INTEGER            NO-UNDO.
        DEFINE VARIABLE cMessage            AS CHARACTER          NO-UNDO.

        DEFINE VARIABLE cTemplate           AS CHARACTER          NO-UNDO.

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        ASSIGN cTemplate = CheckTemplatePath (BusinessEntityDesignerSettings:TemplateFolder + "~\{&GenerateXsdTemplate}":U) .

        FIND FIRST eBusinessEntity .

        COPY-LOB FROM FILE cTemplate TO lcSource .

        ASSIGN cXsdFile      = TRIM(SUBSTITUTE ("&3&1/&2.xsd":U,
                                                eBusinessEntity.DatasetPath,
                                                eBusinessEntity.DatasetName,
                                                cSourceFolder), "/":U)
               cTempFileDotP = SUBSTITUTE ("&1~\&2.p":U,
                                           SESSION:TEMP-DIRECTORY,
                                           GUID).

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("XML Schema":U, cXsdFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cXsdFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cXsdFile = oWriteFileEventArgs:NewFileName .
        END.

        ASSIGN lcSource = SUBSTITUTE (lcSource,
                                      eBusinessEntity.DatasetName,
                                      cIncludeFolder +
                                      REPLACE (eBusinessEntity.DatasetPath, "~\":U, "/":U)) .

        /* Mike Fechner, Consultingwerk Ltd. 18.06.2012
           user name, replace @USER@, with the current user name */
        ASSIGN lcSource = REPLACE (lcSource, "@USER@":U, cUserName)
               lcSource = REPLACE (lcSource, "@USERID@":U, BusinessEntityGenerator:GetUserId ())
               lcSource = StringHelper:FixLineEnding (lcSource) .

        COPY-LOB FROM lcSource
                 TO FILE cTempFileDotP .

        /* Mike Fechner, Consultingwerk Ltd. 11.04.2011
           Warning message when starting with not compile session */
        IF NOT SessionHelper:AllowsCompile() THEN DO:
            MessageFormHelper:ShowMessage ("You ABL session is not enabled for compile."{&TRAN} + System.Environment:NewLine +
                                           "The Business Entity Generator is unable to generate the XML Schema definition file."{&TRAN},
                                           "Business Entity Designer"{&TRAN},
                                           MessageFormImages:ImageWarning) .
            RETURN ? .
        END.

        COMPILE VALUE (cTempFileDotP) .

        /* Mike Fechner, Consultingwerk Ltd. 11.04.2011
           Avoid STOP condition when program cannot be started due to -rr */
        DO ON STOP UNDO, NEXT:
            RUN VALUE (cTempFileDotP) .
        END.

        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN cMessage = "":U .
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                ASSIGN cMessage = cMessage + ERROR-STATUS:GET-MESSAGE (i) +
                                  (IF i > 1 THEN System.Environment:NewLine ELSE "":U).
            END.

            Consultingwerk.Util.MessageFormHelper:ShowMessage (cMessage,
                                                               "XML Schema generation"{&TRAN},
                                                               MessageFormImages:ImageWarning) .
            RETURN ?  .
        END.

        RETURN cXsdFile .

        FINALLY:
            OS-DELETE VALUE (cTempFileDotP) NO-ERROR .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Locates the JPNode representing an Annotation in the source code
        Notes:
        @param pcFileName The file name to parse
        @param pcAnnotation The name of the annotation
        @return The JPNode reference representing the first occurance of the given annotation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED JPNode GetAnnotationNode (pcFileName AS CHARACTER,
                                               pcAnnotation AS CHARACTER):

        DEFINE VARIABLE oParseUnit AS ParseUnit NO-UNDO .

        RETURN THIS-OBJECT:GetAnnotationNode (pcFileName,
                                              pcAnnotation,
                                              OUTPUT oParseUnit) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the JPNode representing an Annotation in the source code
        Notes:
        @param pcFileName The file name to parse
        @param pcAnnotation The name of the annotation
        @param poParseUnit Output Parameter, the ParseUnit used
        @return The JPNode reference representing the first occurance of the given annotation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED JPNode GetAnnotationNode (pcFileName AS CHARACTER,
                                               pcAnnotation AS CHARACTER,
                                               OUTPUT poParseUnit AS ParseUnit):

        DEFINE VARIABLE oParser           AS AnnotationParser NO-UNDO .
        DEFINE VARIABLE oListAnnotation   AS ListAnnotation   NO-UNDO .

        ASSIGN oParser         = NEW AnnotationParser (pcFileName)
               poParseUnit     = oParser:ParseUnit
               oListAnnotation = oParser:FindAnnotations (pcAnnotation) .

        IF oListAnnotation:Count > 0 THEN DO:
            IF TYPE-OF (oListAnnotation:GetItem (1):Node,
                        JPNode) THEN
                RETURN CAST (oListAnnotation:GetItem (1):Node,
                             JPNode) .
        END.
        ELSE
            RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates the lines a start and end annotation are contained in a
                 source code unit
        Notes:
        @param pcFileName The file name to parse
        @param pcStartAnnotation The name of the start annotation
        @param pcEndAnnotation The name of the end annotation
        @param pcAnnotationFileName OUTPUT The file name containing the annotations
        @param piStart The line number of the starting annotation
        @param piEnd The line number of the ending annotation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetAnnotationToFrom (pcFileName AS CHARACTER,
                                               pcStartAnnotation AS CHARACTER,
                                               pcEndAnnotation AS CHARACTER,
                                               OUTPUT pcAnnotationFileName AS CHARACTER,
                                               OUTPUT piStart AS INTEGER,
                                               OUTPUT piEnd AS INTEGER):

        DEFINE VARIABLE cFile             AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oParser           AS AnnotationParser NO-UNDO .
        DEFINE VARIABLE oListAnnotation   AS ListAnnotation   NO-UNDO .

        DEFINE VARIABLE iFrom             AS INTEGER          NO-UNDO .
        DEFINE VARIABLE iTo               AS INTEGER          NO-UNDO .

        ASSIGN oParser         = NEW AnnotationParser (pcFileName)
               oListAnnotation = oParser:FindAnnotations (pcStartAnnotation) .

        IF oListAnnotation:Count > 0 THEN
            ASSIGN cFile = oListAnnotation:GetItem (1):FileName
                   iFrom = oListAnnotation:GetItem (1):Line .
        ELSE DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Definition!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("Unable to find the &1 annotation."{&TRAN},
                                                       pcStartAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        ASSIGN oListAnnotation = oParser:FindAnnotations (pcEndAnnotation) .

        IF oListAnnotation:Count > 0 THEN DO:
            ASSIGN iTo   = oListAnnotation:GetItem (1):Line .

            IF cFile <> oListAnnotation:GetItem (1):FileName THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the DataSource Definition!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 and &2 annotations are not contained in the same file."{&TRAN},
                                                           pcStartAnnotation,
                                                           pcEndAnnotation),
                                               MessageFormImages:ImageWarning) .

                RETURN .
            END.
        END.
        ELSE DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Definition!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("Unable to find the &1 annotation."{&TRAN},
                                                       pcEndAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        IF iFrom >= iTo THEN DO:
            MessageFormHelper:ShowMessage ("Unable to update the DataSource Definition!"{&TRAN},
                                           "Business Entity Generator"{&TRAN},
                                           SUBSTITUTE ("The &1 and &2 annotations are not in the right order."{&TRAN},
                                                       pcStartAnnotation,
                                                       pcEndAnnotation),
                                           MessageFormImages:ImageWarning) .
            RETURN .
        END.

        ASSIGN pcAnnotationFileName = cFile
               piStart              = iFrom
               piEnd                = iTo .

    END METHOD .

    /**
     * Purpose: Returns the full list of all AssignSkipDictionary entries
     * Notes:   Used for the initial code generation to replace the placeholder
     * @return Formatted list of all AssignSkipDictionary entries
     */
    METHOD PROTECTED CHARACTER GetAssignSkipDictionaryComplete ():

        DEFINE BUFFER eTable FOR eTable.

        DEFINE VARIABLE cAssignSkipComplete AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTableSkipList      AS CHARACTER NO-UNDO.

        FOR EACH eTable ON ERROR UNDO, THROW:

            cTableSkipList = THIS-OBJECT:GetAssignSkipDictionaryEntry (eTable.TempTableName).

            IF cTableSkipList > "":U THEN
                ASSIGN
                    cAssignSkipComplete = SUBSTITUTE ("&1&2&3":U,
                                                      cAssignSkipComplete,
                                                      CHR (10),
                                                      cTableSkipList)
                    .

        END.

        cAssignSkipComplete = TRIM (cAssignSkipComplete, CHR (10)).

        RETURN cAssignSkipComplete.

    END METHOD.

    /**
     * Purpose: Generate the Buffer specific AssignSkipDictionary statement
     * Notes:
     * @param pcBufferName Name of the Buffer to create the AssignSkipDictionary statement for
     * @return the AssignSkipDictionary statement
     */
    METHOD PROTECTED CHARACTER GetAssignSkipDictionaryEntry (pcBufferName AS CHARACTER):

        DEFINE BUFFER eField FOR eField.

        DEFINE VARIABLE cAssignSkipList           AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAssignSkipDictionaryEntry AS CHARACTER NO-UNDO .

        FOR EACH eField WHERE eField.TempTableName = pcBufferName
                          AND eField.FieldSkipAssignment = TRUE ON ERROR UNDO, THROW:

            cAssignSkipList = cAssignSkipList + ",":U + eField.FieldName.

        END.

        IF cAssignSkipList > "":U THEN
            cAssignSkipDictionaryEntry =  SUBSTITUTE ("        THIS-OBJECT:AssignSkipDictionary:Add (~"&1~":U, ~"&2~":U).":U,
                                                      pcBufferName,
                                                      TRIM (cAssignSkipList, ",":U)).

        RETURN cAssignSkipDictionaryEntry.

    END METHOD.

    /**
     * Purpose: Finds the END Node of the default Constructor
     * Notes:
     * @param poParseUnit The ParseUnit context to be used as the source
     * @return The END Constructor Node
     */
    METHOD PRIVATE JPNode GetEndNodeOfConstructor (poParseUnit AS ParseUnit):

        DEFINE VARIABLE oCodeblock   AS JPNode                        NO-UNDO .
        DEFINE VARIABLE oWrapper     AS JPNodesWrapper                NO-UNDO .

        oCodeblock = ProparseHelper:GetChildNodeByPath (poParseUnit:getTopNode(), "CLASS~\Code_block":U) .

        oWrapper = NEW JPNodesWrapper (oCodeblock) .
        oWrapper:NodeTypeFilter = "CONSTRUCTOR":U .

        {Consultingwerk/foreachABL.i JPNode oConstructor IN oWrapper}

            IF ProparseHelper:ParseParameterList (oConstructor) = "INPUT HANDLE":U THEN

               RETURN ProparseHelper:FindChildNodeOfNodeType  (oConstructor, "END":U) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the field mapping pairs for the named temp-table
        Notes:
        @param pcTempTableName The name of the temp-table to return the field mappings for
        @param piSpaces The number of SPACE Characters (CHR(32)) to prefix each entry with
        @return The list of the field mappings
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetFieldMapping (pcTempTableName AS CHARACTER,
                                                piSpaces AS INTEGER):

        DEFINE BUFFER b_eField FOR eField .

        DEFINE VARIABLE cMapping AS CHARACTER NO-UNDO.

        FOR EACH b_eField WHERE b_eField.TempTableName = pctempTableName
                            AND b_eField.FieldSource   > "":U
                             BY b_eField.FieldOrder ON ERROR UNDO, THROW:

            IF piSpaces > 0 THEN
                ASSIGN cMapping = cMapping + FILL (" ":U, piSpaces) +
                                             SUBSTITUTE ("&1,&2,":U,
                                                         b_eField.FieldName,
                                                         b_eField.FieldSource) + "~n":U.
            ELSE
                ASSIGN cMapping = cMapping + SUBSTITUTE ("&1,&2,":U,
                                                         b_eField.FieldName,
                                                         b_eField.FieldSource) .
        END.

        IF piSpaces > 0 THEN
            ASSIGN cMapping = TRIM (cMapping, "~n":U) .

        RETURN TRIM (cMapping, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of the top-nav buffers of the generated dataset
        Notes:
        @return The comma delimited list of the top-nav buffers
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetTopNavBuffers ():

        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eDataRelation FOR eDataRelation .

        DEFINE VARIABLE cTopNavBuffers AS CHARACTER NO-UNDO.

        FOR EACH b_eTable ON ERROR UNDO, THROW:
            IF NOT CAN-FIND (b_eDataRelation WHERE b_eDataRelation.BusinessEntityName    = b_eTable.BusinessEntityName
                                               AND b_eDataRelation.ChildTempTableName    = b_eTable.TempTableName
                                               AND b_eDataRelation.DataRelationNotActive = FALSE

                                               /* SCL-628: No recursive tables */
                                               AND b_eDataRelation.ParentTempTableName <> b_eTable.TempTableName) THEN

                ASSIGN cTopNavBuffers = cTopNavBuffers + ",":U + b_eTable.TempTableName .
        END.

        RETURN TRIM (cTopNavBuffers, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the current windows user name
        Notes:
        @return The Display name of the current windows user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetUserName ():

        RETURN Consultingwerk.Windows.API.Win32:GetUserName (Consultingwerk.Windows.API.ExtendedNameFormatEnum:NameDisplay).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the current windows user d
        Notes:
        @return The IDof the current windows user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetUserId ():

        RETURN System.Environment:UserName.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Business Entity design contains validations
        Notes:
        @return Logical value indicating if the Business Entity design contains validations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasValidations():

        DEFINE BUFFER b_eField FOR eField .

        FOR EACH b_eField ON ERROR UNDO, THROW:

            IF LENGTH (b_eField.FieldValidationInstance, "CHARACTER":U) > 0 THEN
                RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the WriteFile event
        Notes:
        @param e The WriteFileEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnWriteFile (e AS WriteFileEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "WriteFile":U) .

        BusinessEntityGenerator:WriteFile:Publish (?, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the generated files in the IDE
        Notes:
        @param pcFileNames The comma delimited list of file names to open in the IDE
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenFilesInIDE (pcFileNames AS CHARACTER):

        DEFINE VARIABLE cCurrentPath  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cIDEConfig    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cProject      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iPortNumber   AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cMessageTitle AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState() .

        FILE-INFORMATION:FILE-NAME = ".":U .

        ASSIGN cMessageTitle = "Opening of generated files in {&ProgressIDE} failed."{&TRAN}
               cCurrentPath  = FILE-INFORMATION:FULL-PATHNAME
               cIDEConfig    = Consultingwerk.Framework.Registry:GetRegistryValue
                                        ("USER":U,
                                         "Software~\Consultingwerk Ltd.~\IDE Support":U,
                                         cCurrentPath) .

        IF cIDEConfig > "":U THEN .
        ELSE DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Unable to read the IDE configuration from the registry."{&TRAN},
                                                               cMessageTitle,
                                                               MessageFormImages:ImageWarning) .
            RETURN .
        END.

        ASSIGN cProject    = ENTRY (1, cIDEConfig, " ":U)
               iPortNumber = INTEGER (ENTRY (2, cIDEConfig, " ":U))
               NO-ERROR .

        IF iPortNumber > 100 THEN .
        ELSE DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("Invalid IDE configuration: &1"{&TRAN}, cIDEConfig),
                                                               cMessageTitle,
                                                               MessageFormImages:ImageWarning) .
            RETURN .
        END.

        filenamesLoop:
        DO i = NUM-ENTRIES (pcFileNames) TO 1 BY -1 ON ERROR UNDO, THROW:
            ASSIGN cFileName = ENTRY (i, pcFileNames) .

            /* Mike Fechner, Consultingwerk Ltd. 19.03.2013
               Ignore empty entries */
            IF CharacterType:IsNullOrEmpty (cFileName) THEN
                NEXT filenamesLoop .

            Consultingwerk.Util.OpenEdgeArchitectHelper:SendRequest
                     ("IDE openEditor ":U
                      + QUOTER(cProject) + " ":U
                      + QUOTER(cFileName) + " ":U
                      + QUOTER("":U), iPortNumber) .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("&1&2&3":U,
                                                                               err:GetMessage(1),
                                                                               System.Environment:NewLine,
                                                                               cIDEConfig),
                                                                   cMessageTitle,
                                                                   MessageFormImages:ImageError) .
                RETURN .
            END CATCH.
        END.

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState() .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts for overwriting an existing file
        Notes:
        @param pcFileName The file name to be used in the prompt dialog
        @return Logical value indicating the response of the current user
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL PromptForOverwrite (pcFileName AS CHARACTER):

        DEFINE VARIABLE oResponse AS DialogResultEnum NO-UNDO .

        IF THIS-OBJECT:SilentMode = TRUE THEN
            RETURN TRUE .

        oResponse = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                    (SUBSTITUTE ("Overwrite the file ~"&1~"?"{&TRAN}, pcFileName),
                                     "Business Entity Generator"{&TRAN}) .

        IF oResponse = DialogResultEnum:DialogResultNo THEN
            RETURN FALSE .

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the WriteFile event from the Business Entity Dataset Controller
        Notes:
        @param pcFilePurpose The purpose of the file being written
        @param pcFileName The name of the file being written
        @return The WriteFileEventArgs with the result of the event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC WriteFileEventArgs RaiseWriteFileEvent (pcFilePurpose AS CHARACTER,
                                                                 pcFileName AS CHARACTER):

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        oWriteFileEventArgs = NEW WriteFileEventArgs (pcFilePurpose,
                                                      pcFileName) .

        RETURN oWriteFileEventArgs .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Re-generates the source code
        Notes:   Currently this will only re-generate the dataset and temp-table
                 definitions, but leave the business entity and data access code
                 untouched
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RegenerateCode (DATASET FOR dsBusinessEntity):

        /* The generated file names */
        DEFINE VARIABLE cBusinessEntity   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cDataAccess       AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cDataset          AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cTempTables       AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cXsdFile          AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oStatusManager    AS IStatusManager   NO-UNDO .

        DEFINE BUFFER eBusinessEntity FOR eBusinessEntity .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Updating Business Entity code."{&TRAN}) .

        ASSIGN THIS-OBJECT:Generated = ?
               lcTempTables          = "":U .

        THIS-OBJECT:ValidateDesign () .

        FIND FIRST eBusinessEntity .

        IF NOT THIS-OBJECT:CheckDirectoriesExist (TRUE) THEN
            RETURN FALSE .

        IF NOT THIS-OBJECT:CheckNotWriteProtected (TRUE, FALSE) THEN
            RETURN FALSE .

        ASSIGN cUserName = BusinessEntityGenerator:GetUserName() .

        ASSIGN cTempTables        = THIS-OBJECT:GenerateTempTables ()
               cDataset           = THIS-OBJECT:GenerateDataset () .
               cXsdFile           = THIS-OBJECT:GenerateXmlSchema() .

        IF eBusinessEntity.GenerateDataAccess THEN
            cDataAccess = THIS-OBJECT:UpdateDataAccessObject () .

        IF eBusinessEntity.GenerateBusinessEntity THEN
            cBusinessEntity = THIS-OBJECT:UpdateBusinessEntity () .

        IF THIS-OBJECT:OpenFilesInIDE THEN
            OpenFilesInIDE (SUBSTITUTE ("&1,&2,&3,&4,&5,&6,&7":U,
                                        cBusinessEntity,
                                        cDataAccess,
                                        cDataset,
                                        cTempTables,
                                        cXsdFile)) .

        ASSIGN THIS-OBJECT:Generated = NEW Generated ("":U,
                                 "":U,
                                 "":U,
                                 cDataset,
                                 cTempTables,
                                                      cXsdFile) .

/*        IF NOT THIS-OBJECT:SilentMode THEN                                             */
/*            Consultingwerk.Util.MessageFormHelper:ShowMessage                          */
/*                    ("The Business Entity Generator completed successfully!"{&TRAN},   */
/*                     "Business Entity Generator"{&TRAN},                               */
/*                     SUBSTITUTE ("Dataset: &4&6Temp Table(s):&5&6XML Schema:&7"{&TRAN},*/
/*                                 "":U,                                                 */
/*                                 "":U,                                                 */
/*                                 "":U,                                                 */
/*                                 cDataset,                                             */
/*                                 cTempTables,                                          */
/*                                 CHR(13) + CHR(10),                                    */
/*                                 cXsdFile)) .                                          */

        RETURN TRUE .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of unqualified table names by removing the database prefix
        Notes:
        @param pcQualifiedTableNames A comma delimited list of potentially DB qualified table names
        @return The list of unqualified table names by removing the database prefix
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UnqualifiedTableNames (pcQualifiedTableNames AS CHARACTER):

        DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

        DO i = 1 TO NUM-ENTRIES (pcQualifiedTableNames):

            ASSIGN cTable = ENTRY (i, pcQualifiedTableNames)
                   cTable = ENTRY (NUM-ENTRIES (cTable, ".":U), cTable, ".":U) .

            ENTRY (i, pcQualifiedTableNames) = cTable .
        END.

        RETURN pcQualifiedTableNames.

    END METHOD .

    /**
     * Purpose: Updates the AssignSkipDictionary entries in the Constructor
     * Notes:
     * @param pcFileName The name of the Data Access source code
     */
    METHOD PROTECTED VOID UpdateAssignSkipDictionary (pcFileName AS CHARACTER):

        DEFINE VARIABLE oParser                AS AssignSkipDictionaryParser    NO-UNDO .
        DEFINE VARIABLE oParseUnit             AS ParseUnit                     NO-UNDO .
        DEFINE VARIABLE oResult                AS ListAssignSkipDictionaryEntry NO-UNDO .
        DEFINE VARIABLE oEntry                 AS AssignSkipDictionaryEntry     NO-UNDO .
        DEFINE VARIABLE iEntry                 AS INTEGER                       NO-UNDO .

        DEFINE VARIABLE oRefactorSession       AS RefactorSession               NO-UNDO .
        DEFINE VARIABLE cTempFile              AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE oReplaceNode           AS JPNode                        NO-UNDO .
        DEFINE VARIABLE cTableUpdated          AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cEntry                 AS CHARACTER                     NO-UNDO .

        DEFINE BUFFER eTable FOR eTable.

        ProparseHelper:Initialize () .
        ProparseHelper:ExportDatabaseSchema () .

        ASSIGN
            oParseUnit       = ProparseHelper:ParseFile (pcFileName)
            oRefactorSession = NEW RefactorSession (oParseUnit)
            oParser          = NEW AssignSkipDictionaryParser ()
            oResult          = oParser:Parse (oParseUnit)
            .

        /* Refresh or remove current AssignSkipDictionary entries */
        IF oResult:Count > 0 THEN DO:
            DO iEntry = 1 TO oResult:Count ON ERROR UNDO, THROW:
                oEntry = oResult:GetItem (iEntry).

                cTableUpdated = SUBSTITUTE ("&1,&2":U,
                                            cTableUpdated,
                                            oEntry:BufferName).

                oReplaceNode = ProparseHelper:FindFirstChildByPositionInCode (oEntry:JPNode).
                oRefactorSession:ReplaceNodeContent (oReplaceNode, TRIM (GetAssignSkipDictionaryEntry (oEntry:BufferName))).

                THIS-OBJECT:ClearRestOfLine (oReplaceNode:nextSibling (),
                                             oRefactorSession).

                oRefactorSession:ReplaceNodeContent (oReplaceNode:parent ():nextSibling (), "":U).

            END.
        END.

        /* Add additional AssignSkipDictionary entries */
        oReplaceNode = ?.

        TableLoop:
        FOR EACH eTable ON ERROR UNDO, THROW:

            IF LOOKUP (eTable.TempTableName, cTableUpdated) > 0 THEN
                NEXT TableLoop.

            IF NOT VALID-OBJECT (oReplaceNode) THEN
                oReplaceNode = GetEndNodeOfConstructor (oParseUnit).

            ASSIGN cEntry = GetAssignSkipDictionaryEntry (eTable.TempTableName) .

            IF cEntry > "":U THEN
                oRefactorSession:InsertBeforeNode (oReplaceNode, cEntry + "~n":U, TRUE).
        END.

        IF oRefactorSession:HasTasks THEN DO ON ERROR UNDO, THROW:
            ASSIGN cTempFile = FileHelper:GetTempFileName () .

            oRefactorSession:Save (cTempFile) .

            GarbageCollectorHelper:DeleteObject (oRefactorSession) .

            OS-COPY VALUE (cTempFile) VALUE (pcFileName) .

            FINALLY:
                OS-DELETE VALUE (cTempFile) .
            END FINALLY.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Business Entity by merging the current source code
                 with the model
        Notes:   Using Proparse to parse the file
        @return The file name of the Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UpdateBusinessEntity ():

        DEFINE VARIABLE oResponse       AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE cBusinessEntity AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE lcSource        AS LONGCHAR         NO-UNDO .

        FIND FIRST eBusinessEntity .

        ASSIGN cBusinessEntity = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                                  REPLACE(eBusinessEntity.BusinessEntityPackage, ".":U, "/":U),
                                                  eBusinessEntity.BusinessEntityName,
                                                  cSourceFolder), "/":U) .

        FILE-INFORMATION:FILE-NAME = cBusinessEntity .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            cBusinessEntity = FILE-INFORMATION:FULL-PATHNAME .
        ELSE DO:
            oResponse = Consultingwerk.Util.MessageFormHelper:AskQuestion
                            (SUBSTITUTE ("The Data Business Entity ~"&1~" does not exist. Should it be generated?"{&TRAN}, cBusinessEntity),
                             "Business Entity Generator"{&TRAN}) .

            IF oResponse = DialogResultEnum:DialogResultYes THEN DO:
                THIS-OBJECT:GenerateBusinessEntity() .
                RETURN cBusinessEntity .
            END.
            ELSE
                /* When the file does not exist, there is nothing to regenerate */
                RETURN "":U .
        END.

        oResponse = Consultingwerk.Util.MessageFormHelper:AskQuestion
                        (SUBSTITUTE ("Update the Business Entity ~"&1~"?"{&TRAN}, cBusinessEntity),
                         "Business Entity Generator"{&TRAN}) .

        IF oResponse = DialogResultEnum:DialogResultNo THEN
            RETURN "":U .

        /* SCL-1618 : Ensure consistent line break */
        COPY-LOB FROM FILE cBusinessEntity TO lcSource .
        ASSIGN lcSource = REPLACE (lcSource, CHR (13) + CHR (10), "~n":U) .
        COPY-LOB FROM lcSource TO FILE cBusinessEntity .

        THIS-OBJECT:UpdateBusinessEntityValidation (cBusinessEntity) .

        RETURN cBusinessEntity .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Business Entity Validation
        Notes:   Replaces source code within @GeneratedValidationStart. and
                 @GeneratedValidationEnd. annotations.
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateBusinessEntityValidation (pcFileName AS CHARACTER):

        DEFINE VARIABLE oValidationGenerator        AS IBusinessEntityValidationGenerator NO-UNDO .
        DEFINE VARIABLE lcValidationCode            AS LONGCHAR                           NO-UNDO .
        DEFINE VARIABLE oParser                     AS AnnotationParser                   NO-UNDO .
        DEFINE VARIABLE oNode                       AS JPNode                             NO-UNDO .
        DEFINE VARIABLE oListAnnotation             AS ListAnnotation                     NO-UNDO .
        DEFINE VARIABLE iFrom                       AS INTEGER                            NO-UNDO INITIAL ? .
        DEFINE VARIABLE iTo                         AS INTEGER                            NO-UNDO INITIAL ? .
        DEFINE VARIABLE cBusinessEntityFullFileName AS CHARACTER                          NO-UNDO .
        DEFINE VARIABLE cAnnotationFullFileName     AS CHARACTER                          NO-UNDO .
        DEFINE VARIABLE cStartAnnotation            AS CHARACTER                          NO-UNDO INITIAL "@GeneratedValidationStart":U.
        DEFINE VARIABLE cEndAnnotation              AS CHARACTER                          NO-UNDO INITIAL "@GeneratedValidationEnd":U.
        DEFINE VARIABLE lcSource                    AS LONGCHAR                           NO-UNDO .
        DEFINE VARIABLE i                           AS INTEGER                            NO-UNDO .
        DEFINE VARIABLE oWriteFileEventArgs         AS WriteFileEventArgs                 NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 18.05.2015
           SCL-424 : Validation properties when not running on OE10 */
        &IF NOT PROVERSION BEGINS "10" &THEN
        oValidationGenerator = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IBusinessEntityValidationGenerator
                                                             "NEW BusinessEntityValidationGenerator ()"} .

        ASSIGN oParser         = NEW AnnotationParser (pcFileName)
               oListAnnotation = oParser:FindAnnotations (cStartAnnotation) .

        /* No validations in design, no annotations in code */
        IF oListAnnotation:Count = 0 AND NOT THIS-OBJECT:HasValidations () THEN
            RETURN .

        IF oListAnnotation:Count > 0 THEN DO:

            ASSIGN cBusinessEntityFullFileName = FileHelper:FindFile(pcFileName)
                   cAnnotationFullFileName     = FileHelper:FindFile(oListAnnotation:GetItem (1):FileName)
                   iFrom                       = oListAnnotation:GetItem (1):Line .

            IF cBusinessEntityFullFileName <> cAnnotationFullFileName THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the Business Entity Validation!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 annotation is not contained directly in the Business Entity."{&TRAN},
                                                           cStartAnnotation),
                                               MessageFormImages:ImageWarning) .
                RETURN .
           END.

           /* When there is a start annotation, we expect the end annotation as well */
           ASSIGN oListAnnotation = oParser:FindAnnotations (cEndAnnotation) .

           IF oListAnnotation:Count = 0 THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the Business Entity Validation!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 annotation is not contained in the Business Entity but not the &2 annotation."{&TRAN},
                                                           cStartAnnotation, cEndAnnotation),
                                               MessageFormImages:ImageWarning) .
                RETURN .
           END.

            ASSIGN cAnnotationFullFileName = FileHelper:FindFile(oListAnnotation:GetItem (1):FileName)
                   iTo                     = oListAnnotation:GetItem (1):Line .

            IF cBusinessEntityFullFileName <> cAnnotationFullFileName THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the Business Entity Validation!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 annotation is not contained directly in the Business Entity."{&TRAN},
                                                           cStartAnnotation),
                                               MessageFormImages:ImageWarning) .
                RETURN .
           END.

            IF NOT iTo >= iFrom THEN DO:
                MessageFormHelper:ShowMessage ("Unable to update the DataSource Definition!"{&TRAN},
                                               "Business Entity Generator"{&TRAN},
                                               SUBSTITUTE ("The &1 and &2 annotations are not in the right order."{&TRAN},
                                                           cStartAnnotation,
                                                           cEndAnnotation),
                                               MessageFormImages:ImageWarning) .
                RETURN .
            END.
        END.
        ELSE
            ASSIGN cBusinessEntityFullFileName = FileHelper:FindFile(pcFileName) .

        lcValidationCode = oValidationGenerator:GenerateValidationCode (DATASET dsBusinessEntity BY-REFERENCE) .

        /* Replace text within annotations */
        COPY-LOB FROM FILE cBusinessEntityFullFileName TO lcSource .

        IF iFrom > 0 AND iTo > 0 THEN DO:
            DO i = iFrom TO iTo:
                lcSource = Consultingwerk.Util.ListHelper:RemoveEntry (lcSource,
                                                                       iFrom,
                                                                       CHR (10)) .
            END.
        END .
        ELSE DO:
            /* Insert just before the END METHOD of ValidateData */
            oNode = THIS-OBJECT:FindValidateDataMethod (oParser:ParseUnit) .

            oNode = oNode:lastChild ():prevNode () .

            ASSIGN iFrom = oNode:getLine () - 1.
        END.

        lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                               iFrom,
                                                               TRIM(lcValidationCode, CHR(10)),
                                                               CHR (10)) .

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("BusinessEntity":U, pcFileName) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                pcFileName = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (pcFileName) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, pcFileName) THEN
            COPY-LOB FROM lcSource TO FILE pcFileName .
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Data Acess Object by merging the current source code
                 with the model
        Notes:   Using Proparse to parse the file
        @return The file name of the Data Access Object
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UpdateDataAccessObject ():

        DEFINE VARIABLE oResponse         AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE cDataAccessObject AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE lcSource          AS LONGCHAR         NO-UNDO .

        FIND FIRST eBusinessEntity .

        ASSIGN cDataAccessObject = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                                    REPLACE(eBusinessEntity.DataAccessPackage, ".":U, "/":U),
                                                    eBusinessEntity.DataAccessName,
                                                    cSourceFolder), "/":U) .

        FILE-INFORMATION:FILE-NAME = cDataAccessObject .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            cDataAccessObject = FILE-INFORMATION:FULL-PATHNAME .
        ELSE DO:
            oResponse = Consultingwerk.Util.MessageFormHelper:AskQuestion
                            (SUBSTITUTE ("The Data Access Object ~"&1~" does not exist. Should it be generated?"{&TRAN}, cDataAccessObject),
                             "Business Entity Generator"{&TRAN}) .

            IF oResponse = DialogResultEnum:DialogResultYes THEN DO:
                THIS-OBJECT:GenerateDataAccessObject () .
                RETURN cDataAccessObject .
            END.
            ELSE
                /* When the file does not exist, there is nothing to regenerate */
                RETURN "":U .
        END.

        IF THIS-OBJECT:SilentMode = FALSE THEN
        oResponse = Consultingwerk.Util.MessageFormHelper:AskQuestion
                        (SUBSTITUTE ("Update the Data Access Object ~"&1~"?"{&TRAN}, cDataAccessObject),
                         "Business Entity Generator"{&TRAN}) .
        ELSE DO:
            IF THIS-OBJECT:SilentModeUpdateDataAccess = TRUE THEN
                oResponse = DialogResultEnum:DialogResultYes .
            ELSE
                oResponse = DialogResultEnum:DialogResultNo .
        END.

        IF oResponse = DialogResultEnum:DialogResultNo THEN
            RETURN "":U .

        /* SCL-1618 : Ensure consistent line break */
        COPY-LOB FROM FILE cDataAccessObject TO lcSource .
        ASSIGN lcSource = REPLACE (lcSource, CHR (13) + CHR (10), "~n":U) .
        COPY-LOB FROM lcSource TO FILE cDataAccessObject .

        THIS-OBJECT:UpdateDataSourceDefinition (cDataAccessObject) .
        THIS-OBJECT:UpdateAttachDataSource (cDataAccessObject) .
        THIS-OBJECT:UpdateDetachDataSource (cDataAccessObject) .
        THIS-OBJECT:UpdateSourceColumn (cDataAccessObject) .
        THIS-OBJECT:UpdateSourceDefaultQuery (cDataAccessObject) .
        THIS-OBJECT:UpdateAssignSkipDictionary (cDataAccessObject) .

        RETURN cDataAccessObject .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Attach Data Source Method
        Notes:
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateAttachDataSource (pcFileName AS CHARACTER):

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE cSourceFile        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iStart             AS INTEGER             NO-UNDO .
        DEFINE VARIABLE iEnd               AS INTEGER             NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER             NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        THIS-OBJECT:GetAnnotationToFrom (pcFileName,
                                         "@AttachDataSourcesStart":U,
                                         "@AttachDataSourcesEnd":U,
                                         OUTPUT cSourceFile,
                                         OUTPUT iStart,
                                         OUTPUT iEnd) .

        IF iStart > 0 AND iEnd > 0 AND cSourceFile > "":U THEN .
        ELSE
            RETURN .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        COPY-LOB FROM FILE cSourceFile TO lcSource .

        DO i = iStart + 1 TO iEnd - 1:
            lcSource = Consultingwerk.Util.ListHelper:RemoveEntry (lcSource,
                                                                   iStart + 1,
                                                                   CHR (10)) .
        END.

        lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                               iStart + 1,
                                                               TRIM(lcAttachDataSource, CHR(10)),
                                                               CHR (10)) .

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cSourceFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cSourceFile) THEN
            COPY-LOB FROM lcSource TO FILE cSourceFile .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Data Source Definition
        Notes:
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateDataSourceDefinition (pcFileName AS CHARACTER):

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE cSourceFile        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iStart             AS INTEGER             NO-UNDO .
        DEFINE VARIABLE iEnd               AS INTEGER             NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER             NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        THIS-OBJECT:GetAnnotationToFrom (pcFileName,
                                         "@DefineDataSourcesStart":U,
                                         "@DefineDataSourcesEnd":U,
                                         OUTPUT cSourceFile,
                                         OUTPUT iStart,
                                         OUTPUT iEnd) .

        IF iStart > 0 AND iEnd > 0 AND cSourceFile > "":U THEN .
        ELSE
            RETURN .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        COPY-LOB FROM FILE cSourceFile TO lcSource .

        DO i = iStart + 1 TO iEnd - 1:
            lcSource = Consultingwerk.Util.ListHelper:RemoveEntry (lcSource,
                                                                   iStart + 1,
                                                                   CHR (10)) .
        END.

        lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                               iStart + 1,
                                                               TRIM(STRING (lcDefineDataSource), CHR(10)),
                                                               CHR (10)) .

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cSourceFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cSourceFile) THEN
        COPY-LOB FROM lcSource TO FILE cSourceFile .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the Detach Data Source Method
        Notes:
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateDetachDataSource (pcFileName AS CHARACTER):

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE cSourceFile        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iStart             AS INTEGER             NO-UNDO .
        DEFINE VARIABLE iEnd               AS INTEGER             NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER             NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        THIS-OBJECT:GetAnnotationToFrom (pcFileName,
                                         "@DetachDataSourcesStart":U,
                                         "@DetachDataSourcesEnd":U,
                                         OUTPUT cSourceFile,
                                         OUTPUT iStart,
                                         OUTPUT iEnd) .

        IF iStart > 0 AND iEnd > 0 AND cSourceFile > "":U THEN .
        ELSE
            RETURN .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        COPY-LOB FROM FILE cSourceFile TO lcSource .

        DO i = iStart + 1 TO iEnd - 1:
            lcSource = Consultingwerk.Util.ListHelper:RemoveEntry (lcSource,
                                                                   iStart + 1,
                                                                   CHR (10)) .
        END.

        lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                               iStart + 1,
                                                               TRIM(STRING (lcDetachDataSource), CHR(10)),
                                                               CHR (10)) .

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN  .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cSourceFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cSourceFile) THEN
        COPY-LOB FROM lcSource TO FILE cSourceFile .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the SourecDefault Method
        Notes:
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateExistingSourceDefaultQuery (pcFileName AS CHARACTER):

        DEFINE VARIABLE cSourceFile         AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cSourceBuffer       AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cQuery              AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE oNode               AS JPNode              NO-UNDO .
        DEFINE VARIABLE oEndCaseNode        AS JPNode              NO-UNDO .
        DEFINE VARIABLE oOtherwiseNode      AS JPNode              NO-UNDO .

        DEFINE VARIABLE oParser             AS CaseStatementParser NO-UNDO .
        DEFINE VARIABLE oParseUnit          AS ParseUnit           NO-UNDO .
        DEFINE VARIABLE oRefactorSession    AS RefactorSession     NO-UNDO .
        DEFINE VARIABLE oList               AS ListCaseWhen        NO-UNDO .
        DEFINE VARIABLE oCaseWhen           AS CaseWhen            NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource  AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource  AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource  AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn      AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery      AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames    AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE cTable              AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iTable              AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cTempFile           AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs  NO-UNDO .

        DEFINE VARIABLE oReturnNode         AS JPNode              NO-UNDO .
        DEFINE VARIABLE oQStringNode        AS JPNode              NO-UNDO .
        DEFINE VARIABLE oDialogResultEnum   AS DialogResultEnum    NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        oNode = THIS-OBJECT:GetAnnotationNode (pcFileName,
                                               "@SourceDefaultQueryCaseBlock":U,
                                               OUTPUT oParseUnit) .

        IF NOT VALID-OBJECT (oNode) THEN
            RETURN .

        ASSIGN oParser          = NEW CaseStatementParser (oNode:nextSibling ())
               oRefactorSession = NEW RefactorSession (oParseUnit)
               oList            = oParser:GetListCaseWhen ()
               oEndCaseNode     = oParser:GetEndCaseNode()
               oOtherwiseNode   = oParser:GetOtherwiseNode()
               cSourceFile      = oNode:getFilename ().

        IF NOT VALID-OBJECT (oEndCaseNode) THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nEND CASE statement not found."{&TRAN}, 0) .

        IF oEndCaseNode:getFilename () <> oNode:getFilename () THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nEND CASE is not in the same file as the CASE Statement."{&TRAN}, 0) .

        IF VALID-OBJECT (oOtherwiseNode) THEN DO:
            IF oOtherwiseNode:getFilename () <> oNode:getFilename () THEN
                UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nOTHERWISE is not in the same file as the CASE Statement."{&TRAN}, 0) .
        END.

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        DO i = 1 TO NUM-ENTRIES (oDataSourceNames:Keys,
                                 oDataSourceNames:KeyDelimiter):

            ASSIGN cTable = ENTRY (i,
                                   oDataSourceNames:Keys,
                                   oDataSourceNames:KeyDelimiter) .

            FIND eTable WHERE eTable.TempTableName = cTable .

            /* When there is no option for this table, add one just before the end */
            IF oList:Exists (cTable) THEN DO:

                oCaseWhen = oList:Find (cTable) .

                IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN DO:

                    IF eTable.SourceBufferNames > "":U THEN
                        cSourceBuffer = ENTRY (1, eTable.SourceBufferNames) .
                    ELSE
                        cSourceBuffer = eTable.SourceTableNames .
                END.
                ELSE IF NUM-ENTRIES (eTable.SourceTableNames) > 1 THEN DO:
                    ASSIGN cSourceBuffer = "":U .

                    DO i = 1 TO NUM-ENTRIES (eTable.SourceTableNames):
                        IF NUM-ENTRIES (eTable.SourceBufferNames) >= i AND
                           ENTRY (i, eTable.SourceBufferNames) > "":U THEN

                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceBufferNames)), ",":U) .
                        ELSE
                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceTableNames)), ",":U) .
                    END.
                END.

                IF eTable.SourceDefaultQuery > "":U THEN DO:
                    eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, " ":U + CHR(13), "":U) .
                    eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, CHR(13), "":U) .
                    eTable.SourceDefaultQuery = REPLACE (eTable.SourceDefaultQuery, CHR(10), " ":U) .

                    lcDefaultQuery = SUBSTITUTE ("~"&3~":U":U,
                                                 eTable.TempTableName,
                                                 REPLACE (eTable.SourceTableNames, ",":U, "":U),
                                                 REPLACE (eTable.SourceDefaultQuery, "~"":U, "~~~"":U)) .
                END.
                ELSE
                    IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN
                        lcDefaultQuery = SUBSTITUTE ("~"FOR EACH &2 INDEXED-REPOSITION~":U":U,
                                                     eTable.TempTableName,
                                                     THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer)) .

                    ELSE DO:
                        /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
                           Joined Query for DataSource */
                        ASSIGN cQuery = SUBSTITUTE(CaseKeywords("FOR EACH &1":U), ENTRY (1, cSourceBuffer)) .

                        DO iTable = 2 TO NUM-ENTRIES (cSourceBuffer):
                             ASSIGN cQuery = cQuery + SUBSTITUTE (CaseKeywords(", FIRST &2 OF &1":U),
                                                                  ENTRY (1, THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer)),
                                                                  ENTRY (iTable, THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer))) .
                        END.

                        ASSIGN cQuery = cQuery + CaseKeywords(" INDEXED-REPOSITION":U)

                               lcDefaultQuery = SUBSTITUTE ("~"&2~":U":U,
                                                            eTable.TempTableName,
                                                            cQuery) .
                    END.

                oReturnNode = ProparseHelper:FindChildNodeOfNodeType (oCaseWhen:Node, "RETURN":U) .

                IF VALID-OBJECT (oReturnNode) THEN DO:
                    oQStringNode = ProparseHelper:FindChildNodeOfNodeType (oReturnNode, "QSTRING":U) .

                    IF VALID-OBJECT (oQStringNode) THEN DO:

                        IF TRIM (STRING (lcDefaultQuery)) <> TRIM (oQStringNode:toStringFulltext()) THEN DO:

                            oDialogResultEnum = MessageFormHelper:AskQuestion (SUBSTITUTE ("Update the Default Query for Temp-Table: &1?~n~nCurrent: &2~nNew: &3"{&TRAN},
                                                                                           cTable,
                                                                                           oQStringNode:toStringFulltext(),
                                                                                           lcDefaultQuery),
                                                                               "Business Entity Generator"{&TRAN}) .

                            IF oDialogResultEnum:ToLogical () = TRUE THEN DO:
                                oRefactorSession:ReplaceNodeContent (oQStringNode, STRING (lcDefaultQuery)) .
                            END.
                        END.
                    END.
                END.
            END.
        END.

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        GarbageCollectorHelper:DeleteObject (oParser) .
        GarbageCollectorHelper:DeleteObject (oParseUnit) .

        IF oRefactorSession:HasTasks THEN DO ON ERROR UNDO, THROW:
            ASSIGN cTempFile = FileHelper:GetTempFileName () .

            oRefactorSession:Save (cTempFile) .

            GarbageCollectorHelper:DeleteObject (oRefactorSession) .

            OS-COPY VALUE (cTempFile) VALUE (pcFileName) .

            FINALLY:
                OS-DELETE VALUE (cTempFile) .
            END FINALLY.
        END.

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW Exception (err, "Unable to update the SourceColumn method."{&TRAN}, 0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the SourceColumn Method
        Notes:
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSourceColumn (pcFileName AS CHARACTER):

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE cSourceFile        AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE oNode              AS JPNode              NO-UNDO .
        DEFINE VARIABLE oEndCaseNode       AS JPNode              NO-UNDO .
        DEFINE VARIABLE oOtherwiseNode     AS JPNode              NO-UNDO .

        DEFINE VARIABLE oParser            AS CaseStatementParser NO-UNDO .
        DEFINE VARIABLE oList              AS ListCaseWhen        NO-UNDO .
        DEFINE VARIABLE iLine              AS INTEGER             NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE cTable             AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        oNode = THIS-OBJECT:GetAnnotationNode (pcFileName,
                                               "@SourceColumnCaseBlock":U) .

        IF NOT VALID-OBJECT (oNode) THEN
            RETURN .

        ASSIGN oParser        = NEW CaseStatementParser (oNode:nextSibling ())
               oList          = oParser:GetListCaseWhen ()
               oEndCaseNode   = oParser:GetEndCaseNode()
               oOtherwiseNode = oParser:GetOtherwiseNode()
               cSourceFile    = oNode:getFilename ().

        IF NOT VALID-OBJECT (oEndCaseNode) THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceColumn method.~nEND CASE statement not found."{&TRAN}, 0) .

        IF oEndCaseNode:getFilename () <> oNode:getFilename () THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceColumn method.~nEND CASE is not in the same file as the CASE Statement."{&TRAN}, 0) .

        IF VALID-OBJECT (oOtherwiseNode) THEN DO:
            IF oOtherwiseNode:getFilename () <> oNode:getFilename () THEN
                UNDO, THROW NEW AppError ("Unable to update the SourceColumn method.~nOTHERWISE is not in the same file as the CASE Statement."{&TRAN}, 0) .

            ASSIGN iLine = oOtherwiseNode:getLine () .
        END.
        ELSE
            ASSIGN iLine = oEndCaseNode:getLine () .

        COPY-LOB FROM FILE cSourceFile TO lcSource .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        DO i = 1 TO NUM-ENTRIES (oDataSourceNames:Keys,
                                 oDataSourceNames:KeyDelimiter):

            ASSIGN cTable = ENTRY (i,
                                   oDataSourceNames:Keys,
                                   oDataSourceNames:KeyDelimiter) .

            /* When there is no option for this table, add one just before the end */
            IF NOT oList:EXISTS (cTable) THEN DO:

                lcSourceColumn = SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                             CaseKeywords("                cMap = DATA-SOURCE &2:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . ":U, "cMap =":U),
                                             cTable,
                                             oDataSourceNames:GetValue (cTable)) .
                lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                                       iLine,
                                                                       TRIM(STRING (lcSourceColumn), CHR(10)),
                                                                       CHR (10)) .
            END.
        END.

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cSourceFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cSourceFile) THEN
        COPY-LOB FROM lcSource TO FILE cSourceFile .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW Exception (err, "Unable to update the SourceColumn method."{&TRAN}, 0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the SourecDefault Method
        Notes:   This method adds new SourceDefaultQuery CASE branches. The method
                 UpdateExistingSourceDefaultQuery verifies existing CASE branches.
        @param pcFileName The name of the Data Access source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSourceDefaultQuery (pcFileName AS CHARACTER):

        DEFINE VARIABLE lcSource           AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE cSourceFile        AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cSourceBuffer      AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cQuery             AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE oNode              AS JPNode              NO-UNDO .
        DEFINE VARIABLE oEndCaseNode       AS JPNode              NO-UNDO .
        DEFINE VARIABLE oOtherwiseNode     AS JPNode              NO-UNDO .

        DEFINE VARIABLE oParser            AS CaseStatementParser NO-UNDO .
        DEFINE VARIABLE oList              AS ListCaseWhen        NO-UNDO .
        DEFINE VARIABLE iLine              AS INTEGER             NO-UNDO .

        DEFINE VARIABLE lcDefineDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcAttachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDetachDataSource AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcSourceColumn     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE lcDefaultQuery     AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oDataSourceNames   AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE cTable             AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iTable             AS INTEGER             NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        oNode = THIS-OBJECT:GetAnnotationNode (pcFileName,
                                               "@SourceDefaultQueryCaseBlock":U) .

        IF NOT VALID-OBJECT (oNode) THEN
            RETURN .

        ASSIGN oParser        = NEW CaseStatementParser (oNode:nextSibling ())
               oList          = oParser:GetListCaseWhen ()
               oEndCaseNode   = oParser:GetEndCaseNode()
               oOtherwiseNode = oParser:GetOtherwiseNode()
               cSourceFile    = oNode:getFilename ().

        IF NOT VALID-OBJECT (oEndCaseNode) THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nEND CASE statement not found."{&TRAN}, 0) .

        IF oEndCaseNode:getFilename () <> oNode:getFilename () THEN
            UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nEND CASE is not in the same file as the CASE Statement."{&TRAN}, 0) .

        IF VALID-OBJECT (oOtherwiseNode) THEN DO:
            IF oOtherwiseNode:getFilename () <> oNode:getFilename () THEN
                UNDO, THROW NEW AppError ("Unable to update the SourceDefaultQuery method.~nOTHERWISE is not in the same file as the CASE Statement."{&TRAN}, 0) .

            ASSIGN iLine = oOtherwiseNode:getLine () .
        END.
        ELSE
            ASSIGN iLine = oEndCaseNode:getLine () .

        COPY-LOB FROM FILE cSourceFile TO lcSource .

        THIS-OBJECT:GenerateDataAccessObjectDetails (OUTPUT lcDefineDataSource,
                                                     OUTPUT lcAttachDataSource,
                                                     OUTPUT lcDetachDataSource,
                                                     OUTPUT lcSourceColumn,
                                                     OUTPUT lcDefaultQuery,
                                                     OUTPUT oDataSourceNames) .

        DO i = 1 TO NUM-ENTRIES (oDataSourceNames:Keys,
                                 oDataSourceNames:KeyDelimiter):

            ASSIGN cTable = ENTRY (i,
                                   oDataSourceNames:Keys,
                                   oDataSourceNames:KeyDelimiter) .

            FIND eTable WHERE eTable.TempTableName = cTable .

            /* When there is no option for this table, add one just before the end */
            IF NOT oList:EXISTS (cTable) THEN DO:

                IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN DO:

                    IF eTable.SourceBufferNames > "":U THEN
                        cSourceBuffer = ENTRY (1, eTable.SourceBufferNames) .
                    ELSE
                        cSourceBuffer = eTable.SourceTableNames .
                END.
                ELSE IF NUM-ENTRIES (eTable.SourceTableNames) > 1 THEN DO:
                    ASSIGN cSourceBuffer = "":U .

                    DO i = 1 TO NUM-ENTRIES (eTable.SourceTableNames):
                        IF NUM-ENTRIES (eTable.SourceBufferNames) >= i AND
                           ENTRY (i, eTable.SourceBufferNames) > "":U THEN

                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceBufferNames)), ",":U) .
                        ELSE
                            cSourceBuffer = TRIM(SUBSTITUTE ("&1,&2":U,
                                                             cSourceBuffer,
                                                             ENTRY (i, eTable.SourceTableNames)), ",":U) .
                    END.
                END.

                IF eTable.SourceDefaultQuery > "":U THEN
                    lcDefaultQuery = SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                 CaseKeywords("                RETURN ~"&3~":U. ":U),
                                                 eTable.TempTableName,
                                                 REPLACE (eTable.SourceTableNames, ",":U, "":U),
                                                 REPLACE (eTable.SourceDefaultQuery, "~"":U, "~~~"":U)) + CHR (10) .
                ELSE
                    IF NUM-ENTRIES (eTable.SourceTableNames) = 1 THEN
                        lcDefaultQuery = SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                     CaseKeywords("                RETURN ~"FOR EACH &2 INDEXED-REPOSITION~":U. ":U),
                                                     eTable.TempTableName,
                                                     THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer)) + CHR (10) .

                    ELSE DO:
                        /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
                           Joined Query for DataSource */
                        ASSIGN cQuery = SUBSTITUTE(CaseKeywords("FOR EACH &1":U), ENTRY (1, cSourceBuffer)) .

                        DO iTable = 2 TO NUM-ENTRIES (cSourceBuffer):
                             ASSIGN cQuery = cQuery + SUBSTITUTE (CaseKeywords(", FIRST &2 OF &1":U),
                                                                  ENTRY (1, THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer)),
                                                                  ENTRY (iTable, THIS-OBJECT:UnqualifiedTableNames (cSourceBuffer))) .
                        END.

                        ASSIGN cQuery = cQuery + CaseKeywords(" INDEXED-REPOSITION":U)

                               lcDefaultQuery = SUBSTITUTE (CaseKeywords("            WHEN ~"&1~":U THEN":U) + CHR(10) +
                                                            CaseKeywords("                RETURN ~"&2~":U. ":U),
                                                            eTable.TempTableName,
                                                            cQuery) + CHR (10) .
                    END.

                lcSource = Consultingwerk.Util.ListHelper:InsertEntry (lcSource,
                                                                       iLine,
                                                                       TRIM(STRING (lcDefaultQuery), CHR(10)),
                                                                       CHR (10)) .
            END.
        END.

        lcSource = StringHelper:FixLineEnding (lcSource) .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("Data Access Object":U, cSourceFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cSourceFile = oWriteFileEventArgs:NewFileName .
        END.

        IF NOT FileHelper:Exists (cSourceFile) OR CodeGeneratorHelper:HasNonCommentChanges(lcSource, cSourceFile) THEN
        COPY-LOB FROM lcSource TO FILE cSourceFile .

        THIS-OBJECT:UpdateExistingSourceDefaultQuery (pcFileName) .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW Exception (err, "Unable to update the SourceColumn method."{&TRAN}, 0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Limits the length of an identifier to 32 characters (ABL maximum)
        Notes:   Avoid error: Identifier was left blank or is more than 32 characters. (1700)
        @param pcIdentifier The intended ABL identifier
        @return The identifier name rduced to 32 characters
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ValidABLIdentifier (pcIdentifier AS CHARACTER):

        RETURN SUBSTRING (pcIdentifier, 1, 32, "CHARACTER":U).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Validates the Design information
        Notes:   Throws errors when there are issues in the Design
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ValidateDesign ():

        DEFINE VARIABLE iEntries AS INTEGER          NO-UNDO .
        DEFINE VARIABLE cField   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER          NO-UNDO .
        DEFINE VARIABLE iIndex   AS INTEGER          NO-UNDO .
        DEFINE VARIABLE oResult  AS DialogResultEnum NO-UNDO .

        DEFINE BUFFER eBusinessEntity FOR eBusinessEntity .
        DEFINE BUFFER eDataRelation   FOR eDataRelation .
        DEFINE BUFFER eTable          FOR eTable .
        DEFINE BUFFER eField          FOR eField .

        FIND FIRST eBusinessEntity NO-ERROR .

        IF NOT CAN-FIND (FIRST eTable) THEN
            UNDO, THROW NEW AppError ("Unable to generate code when there is not at least a single temp-table."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 06.11.2011
           Validation for Package name - as the templates are typically not
           prepared for Business Entities without a package name (problems
           with dataset path) */
        IF eBusinessEntity.BusinessEntityPackage > "":U THEN .
        ELSE
            UNDO, THROW NEW AppError ("Unable to generate code without Business Entity package."{&TRAN}, 0) .

        IF eBusinessEntity.DataAccessPackage > "":U THEN .
        ELSE
            UNDO, THROW NEW AppError ("Unable to generate code without Data Access package."{&TRAN}, 0) .

        IF eBusinessEntity.DatasetControllerPackage > "":U THEN .
        ELSE
            UNDO, THROW NEW AppError ("Unable to generate code without Dataset Controller package."{&TRAN}, 0) .

        /* Validate index information */
        FOR EACH eIndex ON ERROR UNDO, THROW:

            DO i = 1 TO NUM-ENTRIES (eIndex.FieldNames):

                FIND eField WHERE eField.TempTableName = eIndex.TempTableName
                              AND eField.FieldName     = ENTRY (i, eIndex.FieldNames) NO-ERROR .

                IF NOT AVAILABLE eField THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Index field &1 of index &2 is not available in table &3."{&TRAN},
                                                          ENTRY (i, eIndex.FieldNames),
                                                          eIndex.IndexName,
                                                          eIndex.TempTableName), 0) .
                IF LOOKUP (ENTRY (i, eIndex.FieldSort), "ASC,ASCENDING,DESC,DESCENDING":U) = 0 THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid sort option in index &1 in table &2."{&TRAN},
                                                          eIndex.IndexName,
                                                          eIndex.TempTableName), 0).
            END.
        END.

        FOR EACH eTable ON ERROR UNDO, THROW:
            ASSIGN iIndex = 0 .

            FOR EACH eIndex OF eTable WHERE eIndex.IndexPrimary:
                ASSIGN iIndex = iIndex + 1 .
            END.

            IF iIndex = 0 THEN DO:
                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oResult = Consultingwerk.Util.MessageFormHelper:AskQuestion
                        (SUBSTITUTE ("No primary index defined for Temp-Table &1. Do you wish to continue?"{&TRAN},
                                     eTable.TempTableName), "Business Entity Designer"{&TRAN}) .
                ELSE
                    oResult = DialogResultEnum:DialogResultYes .

                IF oResult = DialogResultEnum:DialogResultYes THEN .
                ELSE
                    UNDO, THROW NEW AppError ("Generation has been cancelled on your request."{&TRAN}, 0).
            END.
            ELSE IF iIndex > 1 THEN
                UNDO, THROW NEW AppError (SUBSTITUTE ("More than one primary Index defined for Temp-Table &1."{&TRAN},
                                                      eTable.TempTableName), 0).

            FOR EACH eIndex OF eTable WHERE eIndex.IndexWordIndex
                                        AND (eIndex.IndexUnique OR eIndex.IndexPrimary) ON ERROR UNDO, THROW:
                UNDO, THROW NEW AppError (SUBSTITUTE ("Index &2 of Table &1: A word index cannot be defined unique or primary."{&TRAN},
                                                      eTable.TempTableName,
                                                      eIndex.IndexName), 0).

            END.

            IF NOT CAN-FIND (FIRST eIndex OF eTable WHERE eIndex.IndexPrimary
                                                      AND eIndex.IndexUnique) THEN DO:

                IF THIS-OBJECT:SilentMode = FALSE THEN
                    oResult = Consultingwerk.Util.MessageFormHelper:AskQuestion
                        (SUBSTITUTE ("No primary-unique index defined for Temp-Table &1. Do you wish to continue?"{&TRAN},
                                     eTable.TempTableName),
                         "Business Entity Designer"{&TRAN},
                         "It is recommended to define unique-primary indexs with every temp-table of a ProDataset."{&TRAN}) .
                ELSE
                    oResult = DialogResultEnum:DialogResultYes .

                IF oResult = DialogResultEnum:DialogResultYes THEN .
                ELSE
                    UNDO, THROW NEW AppError ("Generation has been cancelled on your request."{&TRAN}, 0).
            END.
        END.

        FOR EACH eDataRelation ON ERROR UNDO, THROW:
            /* Relation-Fields entries need to be > 0 and even number */
            ASSIGN iEntries = NUM-ENTRIES (eDataRelation.RelationFields) .

            IF iEntries > 0 THEN .
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Relation-Fields may not be empty."{&TRAN},
                                                      eDataRelation.DataRelationName),
                                           0) .

            IF iEntries MODULO 2 = 0 THEN .
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Relation-Fields must contain an even number of fields."{&TRAN},
                                                      eDataRelation.DataRelationName),
                                           0) .

            /* Fields need to exist in parent and source table */
            DO i = 1 TO iEntries - 1 BY 2:
                ASSIGN cField = ENTRY (i, eDataRelation.RelationFields) .

                CASE NUM-ENTRIES (cField, ".":U):
                    WHEN 0 THEN UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Relation-Fields may not be empty."{&TRAN},
                                                                      eDataRelation.DataRelationName),
                                                          0) .

                    WHEN 1 THEN IF NOT CAN-FIND (FIRST eField WHERE eField.TempTableName = eDataRelation.ParentTempTableName
                                                                AND eField.FieldName     = cField) THEN
                                    UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Parent Table Field &2."{&TRAN},
                                                                          eDataRelation.DataRelationName,
                                                                          cField),
                                                          0) .
                    WHEN 2 THEN DO:
                                IF ENTRY (1, cField, ".":U) <> eDataRelation.ParentTempTableName OR
                                   NOT CAN-FIND (FIRST eField WHERE eField.TempTableName = eDataRelation.ParentTempTableName
                                                                AND eField.FieldName     = ENTRY (2, cField, ".":U)) THEN
                                    UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Field &2."{&TRAN},
                                                                          eDataRelation.DataRelationName,
                                                                          cField),
                                                          0) .
                            END .
                    OTHERWISE
                            UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Field &2."{&TRAN},
                                                                  eDataRelation.DataRelationName,
                                                                  cField),
                                                      0) .
                END CASE .

                ASSIGN cField = ENTRY (i + 1, eDataRelation.RelationFields) .

                CASE NUM-ENTRIES (cField, ".":U):
                    WHEN 0 THEN UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Relation-Fields may not be empty."{&TRAN},
                                                                      eDataRelation.DataRelationName),
                                                          0) .

                    WHEN 1 THEN IF NOT CAN-FIND (FIRST eField WHERE eField.TempTableName = eDataRelation.ChildTempTableName
                                                                AND eField.FieldName     = cField) THEN
                                    UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Child Table Field &2."{&TRAN},
                                                                          eDataRelation.DataRelationName,
                                                                          cField),
                                                          0) .
                    WHEN 2 THEN DO:
                                IF ENTRY (1, cField, ".":U) <> eDataRelation.ChildTempTableName OR
                                   NOT CAN-FIND (FIRST eField WHERE eField.TempTableName = eDataRelation.ChildTempTableName
                                                                AND eField.FieldName     = ENTRY (2, cField, ".":U)) THEN
                                    UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Field &2."{&TRAN},
                                                                          eDataRelation.DataRelationName,
                                                                          cField),
                                                          0) .
                            END .
                    OTHERWISE
                            UNDO, THROW NEW AppError (SUBSTITUTE ("Error validating Data-Relation &1: Invalid Field &2."{&TRAN},
                                                                  eDataRelation.DataRelationName,
                                                                  cField),
                                                      0) .
                END CASE .
            END.
        END.

    END METHOD.

END CLASS.
