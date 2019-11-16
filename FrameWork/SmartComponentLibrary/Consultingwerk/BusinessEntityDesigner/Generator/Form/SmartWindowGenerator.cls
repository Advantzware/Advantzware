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
    File        : SmartWindowGenerator
    Purpose     : Generator for SmartWindowForm classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 10:05:14 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Form.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                        FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Form.SmartWindowGenerator
    IMPLEMENTS ISmartWindowGenerator, IContainerControlGenerator
    INHERITS ContainerControlGenerator:

    DEFINE TEMP-TABLE ttMethods NO-UNDO
        FIELD Name AS CHARACTER
        FIELD Code AS CLOB
        INDEX Name Name .

    DEFINE VARIABLE cSourceFolder AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Columns in the BindingSource Schema
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceColumns AS "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartWindowForm
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose:
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetControllerName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityJoin property of the SmartBusinessEntityBindingSource
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityName property of the SmartBusinessEntityBindingSource
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityTable property of the SmartBusinessEntityBindingSource
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityView property of the SmartBusinessEntityBindingSource
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Form Title (Text Property of the generated Form)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FormTitle AS CHARACTER NO-UNDO
    GET .
    SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of Grid Columns
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GridColumns AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Package Name for the generated SmartWindowForm
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Ignore status messages on completion
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SilentMode AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation of
                 the .resx file
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResxTemplateName AS CHARACTER INITIAL "":U NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TemplateName AS CHARACTER INITIAL "":U NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the SmartViewer to be used in the SmartWindowForm
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewerName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWindowGenerator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWindowGenerator ():
        SUPER ().

        THIS-OBJECT:BindingSourceColumns = NEW "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a method in the container control
        Notes:
        @param pcMethodName The name of the method to create
        @param pcCode The source code of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateMethod (pcMethodName AS CHARACTER,
                                     pcCode AS LONGCHAR):

        CREATE ttMethods .
        ASSIGN ttMethods.Name = pcMethodName .

        COPY-LOB FROM pcCode TO ttMethods.Code .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Evaluates the source folder names
        Notes:   Variable cSourceFolder
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EvaluateFolderName ():

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolderGui > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolderGui .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U AND FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U THEN
                ASSIGN cSourceFolder = FILE-INFORMATION:FULL-PATHNAME + "~\":U.
            ELSE
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN},
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolderGui),
                                          0) .
        END.
        ELSE
            ASSIGN cSourceFolder = "":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the SmartWindowForm
        Notes:
        @return The file name of the generated source
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cResXFile           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE oReply              AS DialogResultEnum   NO-UNDO .
        DEFINE VARIABLE cFolder             AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        THIS-OBJECT:EvaluateFolderName () .

        ASSIGN cFolder = cSourceFolder + REPLACE(THIS-OBJECT:PackageName, ".":U, "~\":U) .

        FILE-INFORMATION:FILE-NAME = cFolder .

        IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
            IF THIS-OBJECT:SilentMode = FALSE THEN
                oReply = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cFolder),
                                 "Smart Form Generator"{&TRAN},
                                 SUBSTITUTE ("The directory ~"&1~" used for the Smart Form  ~"&2~" does not exist."{&TRAN},
                                             cFolder, THIS-OBJECT:ClassName)) .
            ELSE
                oReply = DialogResultEnum:DialogResultYes .

            IF oReply:Equals (DialogResultEnum:DialogResultYes) THEN
                System.IO.Directory:CreateDirectory (cFolder) .
            ELSE RETURN ? .
        END.

        IF THIS-OBJECT:TemplateName > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/UI/":U + THIS-OBJECT:TemplateName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                FILE-INFORMATION:FILE-NAME = THIS-OBJECT:TemplateName .
        END.
        ELSE
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/UI/smartwindow.template":U .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcSource .

        ASSIGN lcSource = REPLACE (lcSource, "@WINDOW_NAME@":U,                 THIS-OBJECT:ClassName)
               lcSource = REPLACE (lcSource, "@WINDOW_PACKAGE@":U,              THIS-OBJECT:PackageName)
               lcSource = REPLACE (lcSource, "@WINDOW_TITLE@":U,                THIS-OBJECT:FormTitle)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_JOIN@":U,  THIS-OBJECT:EntityJoin)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_NAME@":U,  THIS-OBJECT:EntityName)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_TABLE@":U, THIS-OBJECT:EntityTable)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_VIEW@":U,  THIS-OBJECT:EntityView)
               lcSource = REPLACE (lcSource, "@BINDINGSOURCE_NUM_FIELDS@":U,    STRING (THIS-OBJECT:BindingSourceColumns:Count))

               lcSource = REPLACE (lcSource, "@DATASET_CONTROLLER_TYPE@":U,     THIS-OBJECT:DatasetControllerName)
               lcSource = REPLACE (lcSource, "@VIEWER_NAME@":U,                 THIS-OBJECT:ViewerName)
               lcSource = REPLACE (lcSource, "@VIEWER_VARIABLENAME@":U,         THIS-OBJECT:GenerateViewerVariableName())

               lcSource = REPLACE (lcSource, "@DEFINE_CONTROL_VARIABLES@":U,    THIS-OBJECT:GenerateDefineControlVariables () )
               lcSource = REPLACE (lcSource, "@CONTROL_SUPPORT_VARIABLES@":U,   THIS-OBJECT:GenerateControlSupportVariables () )
               lcSource = REPLACE (lcSource, "@CONTROL_NEW@":U,                 THIS-OBJECT:GenerateControlNews ())
               lcSource = REPLACE (lcSource, "@CONTROL_BEGININIT@":U,           THIS-OBJECT:GenerateControlInit (TRUE))
               lcSource = REPLACE (lcSource, "@CONTROL_ENDINIT@":U,             THIS-OBJECT:GenerateControlInit (FALSE))
               lcSource = REPLACE (lcSource, "@BINDINGSOURCE_NEW_FIELDS@":U,    THIS-OBJECT:GenerateBindingSourceNewFields ())
               lcSource = REPLACE (lcSource, "@CONTROLS_PROPERTY_ASSIGN@":U,    THIS-OBJECT:GenerateControlsPropertyAssign ())

               lcSource = REPLACE (lcSource, "@GRID_NUM_COLUMNS@":U,            STRING (THIS-OBJECT:BindingSourceColumns:Count))
               lcSource = REPLACE (lcSource, "@GRID_COLUMN_ASSIGN_TO_ARRAY@":U, THIS-OBJECT:GenerateColumnAssignToArray ())
               lcSource = REPLACE (lcSource, "@GRID_COLUMN_DEFINE_AND_NEW@":U,  THIS-OBJECT:GenerateColumnDefineAndNew ())
               lcSource = REPLACE (lcSource, "@GRID_COLUMN_PROPERTIES@":U,      THIS-OBJECT:GenerateGridColumnProperties ())
               lcSource = REPLACE (lcSource, "@GRID_SETTINGS_KEY@":U,           GUID)

               lcSource = REPLACE (lcSource, "@USER@":U,                        BusinessEntityGenerator:GetUserName ())
               lcSource = REPLACE (lcSource, "@USERID@":U,                      BusinessEntityGenerator:GetUserId ())
               lcSource = REPLACE (lcSource, "@DATE@":U,                        STRING (NOW, "99.99.9999 HH:MM:SS":U))
               lcSource = REPLACE (lcSource, "@ADDITIONAL_METHODS@":U,          THIS-OBJECT:GetMethods ()) .
               .

        ASSIGN cOutputFile = TRIM(SUBSTITUTE ("&3&1/&2.cls":U,
                                              REPLACE(THIS-OBJECT:PackageName, ".":U, "/":U),
                                              THIS-OBJECT:ClassName,
                                              cSourceFolder), "/":U) .

        IF Consultingwerk.Util.FileHelper:Exists (cOutputFile) THEN DO:

            ASSIGN cOutputFile = REPLACE (cOutputFile, "/":U, "~\":U) .

            IF THIS-OBJECT:SilentMode = FALSE THEN
                oReply = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                (SUBSTITUTE ("Overwrite file  ~"&1~"?"{&TRAN}, cOutputFile),
                                 "Smart Form Generator"{&TRAN},
                                 SUBSTITUTE ("The file ~"&1~" for Viewer ~"&2~" does already exist."{&TRAN},
                                             cOutputFile, THIS-OBJECT:ClassName)) .
            ELSE
                oReply = DialogResultEnum:DialogResultYes .

            IF NOT oReply:Equals (DialogResultEnum:DialogResultYes) THEN
                RETURN cOutputFile .
        END.

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("SmartWindowForm":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        COPY-LOB FROM lcSource TO FILE cOutputFile .

        ASSIGN cResXFile = TRIM(SUBSTITUTE ("&3&1/&2.resx":U,
                                            REPLACE(THIS-OBJECT:PackageName, ".":U, "/":U),
                                            THIS-OBJECT:ClassName,
                                            cSourceFolder), "/":U) .

        IF THIS-OBJECT:ResxTemplateName > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/UI/":U + THIS-OBJECT:ResxTemplateName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                FILE-INFORMATION:FILE-NAME = THIS-OBJECT:ResxTemplateName .

            IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
                FILE-INFORMATION:FILE-NAME = TemplateHelper:Find ("UI/":U + THIS-OBJECT:ResxTemplateName)   .
        END.
        ELSE
            FILE-INFORMATION:FILE-NAME = TemplateHelper:Find ("UI/smartwindow.template.resx":U)  .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcSource .

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("resx file":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cResXFile = oWriteFileEventArgs:NewFileName .
        END.

        COPY-LOB FROM lcSource TO FILE cResXFile .

        RETURN cOutputFile .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Code block that creates the Binding Source schema
        Notes:
        @return The Code block that creates the Binding Source schema
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateBindingSourceNewFields ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in this-object:BindingSourceColumns}

            ASSIGN i           = i + 1
                   lcCodeBlock = lcCodeBlock + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                                 "        arrayvar13[&1] = NEW Progress.Data.ColumnPropDesc(~"&2~":U, ~"&3~":U, Progress.Data.DataType:&4).~n":U
                                                           ELSE
                                                                 "        arrayvar13[&1] = new Progress.Data.ColumnPropDesc(~"&2~":u, ~"&3~":u, Progress.Data.DataType:&4).~n":U,
                                                           i,
                                                           oColumn:ColumnName,
                                                           oColumn:ColumnLabel,
                                                           oColumn:ColumnType:ToString ()) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the code block that assigns the columns to the array
        Notes:
        @return The code block with the assigns
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateColumnAssignToArray ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER  NO-UNDO.

        DO i = 1 TO THIS-OBJECT:BindingSourceColumns:Count:

            ASSIGN lcCodeBlock = lcCodeBlock + SUBSTITUTE ("        arrayvar12[&1] = ultraGridColumn&1.~n":U,
                                                           i) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Code block that defines and NEW's the Grid Columns
        Notes:
        @return The code block the defines and NEW's the Grid Columns
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateColumnDefineAndNew ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in this-object:BindingSourceColumns}

            ASSIGN i           = i + 1
                   lcCodeBlock = lcCodeBlock + SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"true~":U).~n":U +
                                                           (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                           "        DEFINE VARIABLE ultraGridColumn&1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.~n":U
                                                           ELSE
                                                           "        define variable ultraGridColumn&1 as Infragistics.Win.UltraWinGrid.UltraGridColumn no-undo.~n":U) +

                                                           (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                           "        ultraGridColumn&1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn(~"&2~":U).~n":U
                                                           ELSE
                                                           "        ultraGridColumn&1 = new Infragistics.Win.UltraWinGrid.UltraGridColumn(~"&2~":u).~n":U),
                                                           i,
                                                           oColumn:ColumnName) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Code block that assigns the properties for the the Grid Columns
        Notes:
        @return The code block that assigns the properties for the the Grid Columns
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateGridColumnProperties ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in this-object:BindingSourceColumns}

            ASSIGN i           = i + 1
                   lcCodeBlock = lcCodeBlock + SUBSTITUTE ("        ultraGridColumn&1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.~n":U +
                                                           "        ultraGridColumn&1:Header:VisiblePosition = &2.~n":U,
                                                           i,
                                                           i - 1)  .

            IF NOT THIS-OBJECT:GridColumns:ContainsValue (oColumn:ColumnName) THEN
                ASSIGN lcCodeBlock = lcCodeBlock + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                                            "        ultraGridColumn&1:Hidden = TRUE.~n":U
                                                               ELSE
                                                                            "        ultraGridColumn&1:Hidden = true.~n":U,
                                                               i) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Variable Name for the Viewer, based on the ViewerName
                 (class name of the Viewer)
        Notes:
        @return The variable name for the viewer instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GenerateViewerVariableName ():

        DEFINE VARIABLE cVariableName AS CHARACTER NO-UNDO.

        ASSIGN cVariableName                                  = ENTRY (NUM-ENTRIES (THIS-OBJECT:ViewerName, ".":U),
                                                                       THIS-OBJECT:ViewerName,
                                                                       ".":U)
               SUBSTRING (cVariableName, 1, 1, "CHARACTER":U) = LC (SUBSTRING (cVariableName, 1, 1, "CHARACTER":U))
               cVariableName                                  = THIS-OBJECT:GenerateObjectName (cVariableName) .

        RETURN cVariableName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code of the additional methods (event handlers)
        Notes:
        @return The source code of all additional methods (event handlers)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GetMethods ():

        DEFINE VARIABLE lcCode   AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcMethod AS LONGCHAR NO-UNDO .

        FOR EACH ttMethods ON ERROR UNDO, THROW:

            COPY-LOB FROM ttMethods.Code TO lcMethod .

            ASSIGN lcCode = lcCode + lcMethod .

        END.

        RETURN lcCode .

    END METHOD.

END CLASS.
