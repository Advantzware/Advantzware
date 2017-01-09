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
    File        : SmartViewerGenerator
    Purpose     : Generator for SmartViewerControl Classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 10:05:14 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                          FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartViewerGenerator
    IMPLEMENTS ISmartViewerGenerator, IContainerControlGenerator
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
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the generated User Control should contain a
                 Binding Source or not
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateBindingSource AS LOGICAL INITIAL TRUE NO-UNDO
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
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO
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
        Purpose: Constructor for the SmartViewerGenerator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartViewerGenerator ():
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
        Purpose: Generates the source code for the SmartViewerControl
        Notes:
        @return The file name of the generated source
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ():

        DEFINE VARIABLE lcSource            AS LONGCHAR           NO-UNDO .
        DEFINE VARIABLE cOutputFile         AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE oReply              AS DialogResultEnum   NO-UNDO .
        DEFINE VARIABLE cFolder             AS CHARACTER          NO-UNDO .

        DEFINE VARIABLE oWriteFileEventArgs AS WriteFileEventArgs NO-UNDO .

        EMPTY TEMP-TABLE ttMethods .

        THIS-OBJECT:EvaluateFolderName () .

        ASSIGN cFolder = cSourceFolder + REPLACE(THIS-OBJECT:PackageName, ".":U, "~\":U) .

        FILE-INFORMATION:FILE-NAME = cFolder .

        IF FILE-INFORMATION:FULL-PATHNAME = ? OR (NOT FILE-INFORMATION:FILE-TYPE MATCHES "*D*":U) THEN DO:
            IF THIS-OBJECT:SilentMode = FALSE THEN
                oReply = Consultingwerk.Util.MessageFormHelper:AskQuestion
                                (SUBSTITUTE ("Create directory ~"&1~"?"{&TRAN}, cFolder),
                                 "Smart Viewer Generator"{&TRAN},
                                 SUBSTITUTE ("The directory ~"&1~" used for the Smart Viewer Control ~"&2~" does not exist."{&TRAN},
                                             cFolder, THIS-OBJECT:ClassName)) .
            ELSE
                oReply = DialogResultEnum:DialogResultYes .

            IF oReply:Equals (DialogResultEnum:DialogResultYes) THEN
                System.IO.Directory:CreateDirectory (cFolder) .
            ELSE RETURN ? .
        END.

        IF THIS-OBJECT:CreateBindingSource = TRUE THEN
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/UI/smartviewer.template":U .
        ELSE
            FILE-INFORMATION:FILE-NAME = BusinessEntityDesignerSettings:TemplateFolder + "/UI/smartviewer_no_bindingsource.template":U .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcSource .

        ASSIGN lcSource = REPLACE (lcSource, "@VIEWER_NAME@":U,                 THIS-OBJECT:ClassName)
               lcSource = REPLACE (lcSource, "@VIEWER_PACKAGE@":U,              THIS-OBJECT:PackageName)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_JOIN@":U,  THIS-OBJECT:EntityJoin)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_NAME@":U,  THIS-OBJECT:EntityName)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_TABLE@":U, THIS-OBJECT:EntityTable)
               lcSource = REPLACE (lcSource, "@BINDING_SOURCE_ENTITY_VIEW@":U,  THIS-OBJECT:EntityView)
               lcSource = REPLACE (lcSource, "@BINDINGSOURCE_NUM_FIELDS@":U,    STRING (THIS-OBJECT:BindingSourceColumns:Count))

               lcSource = REPLACE (lcSource, "@DEFINE_CONTROL_VARIABLES@":U,    THIS-OBJECT:GenerateDefineControlVariables () )
               lcSource = REPLACE (lcSource, "@CONTROL_SUPPORT_VARIABLES@":U,   THIS-OBJECT:GenerateControlSupportVariables () )
               lcSource = REPLACE (lcSource, "@CONTROL_NEW@":U,                 THIS-OBJECT:GenerateControlNews ())
               lcSource = REPLACE (lcSource, "@CONTROL_BEGININIT@":U,           THIS-OBJECT:GenerateControlInit (TRUE))
               lcSource = REPLACE (lcSource, "@CONTROL_ENDINIT@":U,             THIS-OBJECT:GenerateControlInit (FALSE))
               lcSource = REPLACE (lcSource, "@BINDINGSOURCE_NEW_FIELDS@":U,    THIS-OBJECT:GenerateBindingSourceNewFields ())
               lcSource = REPLACE (lcSource, "@CONTROLS_PROPERTY_ASSIGN@":U,    THIS-OBJECT:GenerateControlsPropertyAssign ())
               lcSource = REPLACE (lcSource, "@VIEWER_PROPERTIES@":U,           THIS-OBJECT:GenerateViewerProperties ())

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
                                 "Smart Viewer Generator"{&TRAN},
                                 SUBSTITUTE ("The file ~"&1~" for Viewer ~"&2~" does already exist."{&TRAN},
                                             cOutputFile, THIS-OBJECT:ClassName)) .
            ELSE
                oReply = DialogResultEnum:DialogResultYes .

            IF NOT oReply:Equals (DialogResultEnum:DialogResultYes) THEN
                RETURN cOutputFile .
        END.

        oWriteFileEventArgs = BusinessEntityGenerator:RaiseWriteFileEvent ("SmartViewerControl":U, cOutputFile) .

        IF VALID-OBJECT (oWriteFileEventArgs) THEN DO:
            IF oWriteFileEventArgs:Cancel THEN
                RETURN cOutputFile .

            IF oWriteFileEventArgs:NewFileName > "":U THEN
                cOutputFile = oWriteFileEventArgs:NewFileName .
        END.

        COPY-LOB FROM lcSource TO FILE cOutputFile .

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
                                                               "        arrayvar0[&1] = NEW Progress.Data.ColumnPropDesc(~"&2~":U, ~"&3~":U, Progress.Data.DataType:&4).~n":U
                                                           ELSE
                                                               "        arrayvar0[&1] = new Progress.Data.ColumnPropDesc(~"&2~":U, ~"&3~":u, Progress.Data.DataType:&4).~n":U,
                                                           i,
                                                           oColumn:ColumnName,
                                                           oColumn:ColumnLabel,
                                                           oColumn:ColumnType:ToString ()) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the code block that assigns the properties for the SmartViewerControl
        Notes:
        @return The code block that assigns the properties for the SmartViewerControl
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateViewerProperties ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO.

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            IF oControl:ContainerName > "":U THEN
                ASSIGN lcCodeBlock = lcCodeBlock + SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&2:Controls:Add(THIS-OBJECT:&1).~n":U, ":Controls:Add":U),
                                                               oControl:Name, oControl:ContainerName) .
            ELSE
                ASSIGN lcCodeBlock = lcCodeBlock + SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:Controls:Add(THIS-OBJECT:&1).~n":U, ":Controls:Add":U),
                                                               oControl:Name) .
        END.

        ASSIGN lcCodeBlock = lcCodeBlock +
                             SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:Name = ~"&1~":U.~n":U, ":Name ":U),
                                         THIS-OBJECT:ClassName) +
                             SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                             "        THIS-OBJECT:Size = NEW System.Drawing.Size(&1, &2).~n":U
                                         ELSE
                                             "        this-object:Size = new System.Drawing.Size(&1, &2).~n":U,
                                         300,
                                         300) .

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code of the additional methods (event handlers)
        Notes:
        @return The source code of all additional methods (event handlers)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GetMethods ():

        DEFINE VARIABLE lcCode   AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcMethod AS LONGCHAR NO-UNDO .

        FOR EACH ttMethods:

            COPY-LOB FROM ttMethods.Code TO lcMethod .

            ASSIGN lcCode = lcCode + lcMethod .

        END.

        RETURN lcCode .

    END METHOD.

END CLASS.
