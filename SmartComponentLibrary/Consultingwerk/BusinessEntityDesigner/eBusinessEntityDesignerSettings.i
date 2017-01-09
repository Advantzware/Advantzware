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
    File        : eBusinessEntityDesignerSettings.i
    Purpose     : Temp-Table definition for the Settings/Options of the
                  Business Entity Designer

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 21 22:36:33 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eBusinessEntityDesignerSettings NO-UNDO
       {&REFERENCE-ONLY}

    FIELD BusinessEntityGenerator       AS CHARACTER INIT "Consultingwerk.BusinessEntityDesigner.Generator.BusinessEntityGenerator":U
    FIELD FieldNameGeneratorService     AS CHARACTER INIT "Consultingwerk.BusinessEntityDesigner.Services.FieldNameGeneratorService":U
    FIELD DataRelationFieldsService     AS CHARACTER INIT "Consultingwerk.BusinessEntityDesigner.Services.DataRelationFieldsService":U
    FIELD TemplateFolder                AS CHARACTER INIT "Consultingwerk\BusinessEntityDesigner\Generator\Templates":U
    FIELD TableNameGeneratorService     AS CHARACTER INIT "Consultingwerk.BusinessEntityDesigner.Services.TableNameGeneratorService":U
    FIELD RelationNameSubstitute        AS CHARACTER INIT "&1&2Relation":U
    FIELD DefaultTablePrefix            AS CHARACTER INIT "e":U
    FIELD DefaultTableSuffix            AS CHARACTER INIT "":U
    FIELD DefaultBeforeTablePrefix      AS CHARACTER INIT "e":U
    FIELD DefaultBeforeTableSuffix      AS CHARACTER INIT "Before":U
    FIELD BusinessEntityDesignerPlugins AS CHARACTER INIT "":U
    FIELD RCodeBaseFolder               AS CHARACTER INIT ".":U
    FIELD SourceCodeBaseFolder          AS CHARACTER INIT ".":U
    FIELD RCodeBaseFolderGui            AS CHARACTER INIT "":U
    FIELD SourceCodeBaseFolderGui       AS CHARACTER INIT "":U
    FIELD IncludeFilesBaseFolder        AS CHARACTER INIT "":U
    FIELD OpenFilesInIDE                AS LOGICAL   INIT TRUE
    FIELD CompileDatasetController      AS LOGICAL   INIT TRUE
    FIELD CustomServices                AS CHARACTER INIT "":U
    FIELD PromptForSaveBeforeGenerate   AS LOGICAL   INIT FALSE
    FIELD SynchronizeTempTablesOnOpen   AS LOGICAL   INIT FALSE
    FIELD LauncherCommandLine           AS CHARACTER INIT "":U
    FIELD LauncherProversion            AS CHARACTER INIT "":U
    FIELD DistinctTempTableIncludeFiles AS LOGICAL   INIT TRUE
    FIELD ReplaceHyphenInTableName      AS LOGICAL   INIT FALSE
    FIELD ReplaceUnderScoreInTableName  AS LOGICAL   INIT FALSE
    FIELD UseLocalFileHistory           AS LOGICAL   INIT FALSE
    FIELD SplitArrayFields              AS LOGICAL   INIT FALSE
    FIELD GenerateDatasetController     AS LOGICAL   INIT TRUE
    FIELD KeywordCasing                 AS CHARACTER INIT "UPPER":U
    FIELD ApacheAntCommandLine          AS CHARACTER INIT "ant.bat":U
    FIELD ViewUnderscoreTables          AS LOGICAL   INIT FALSE
    FIELD ShowConfirmation              AS LOGICAL   INIT TRUE
    .
