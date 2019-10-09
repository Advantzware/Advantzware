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
    File        : KendoUiBuilderViewGenerator
    Purpose     : Generator for SmartViewerControl Classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 10:05:14 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.*         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.Builder.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*       FROM PROPATH .
USING Consultingwerk.Framework.*                                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                         FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                FROM PROPATH .
USING Consultingwerk.OERA.*                                          FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.*                     FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.KendoUiBuilderViewGenerator
    INHERITS AbstractSmartViewerGenerator
    IMPLEMENTS ISmartViewerGenerator:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}

    /**
     * Purpose: Gets and sets the object with custom parameter data for the Generator
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Data AS KendoUiBuilderData NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the SmartWeb2ViewGenerator class
     * Notes:
     * @param poData The KendoUiBuilderData instance with the data for the generator
     */
    CONSTRUCTOR PUBLIC KendoUiBuilderViewGenerator (poData AS KendoUiBuilderData):
        SUPER ().

        THIS-OBJECT:BindingSourceColumns = NEW "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U ().
        THIS-OBJECT:ControlGenerators = NEW ListIControlGenerator () .
        THIS-OBJECT:Data = poData .

    END CONSTRUCTOR.

    /**
     * Purpose: Generates the source code for the SmartViewerControl
     * Notes:
     * @return The file name of the generated source
     */
    METHOD PUBLIC OVERRIDE CHARACTER Generate ():

        DEFINE VARIABLE oViewGenerator  AS ViewGenerator    NO-UNDO .
        DEFINE VARIABLE oModule         AS Module           NO-UNDO .
        DEFINE VARIABLE oGridColumns    AS CharacterList    NO-UNDO .
        DEFINE VARIABLE cBedgmFile      AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oService        AS IBusinessService NO-UNDO .

        oViewGenerator = NEW ViewGenerator (THIS-OBJECT:Data:ModuleName,
                                            THIS-OBJECT:Data:AppLocation) .

        IF oViewGenerator:ModuleExists (?, ?) THEN DO:
            oModule = oViewGenerator:LoadModule (?, ?) .
            IF oViewGenerator:HasView (oModule, THIS-OBJECT:Data:ViewName) THEN
                oViewGenerator:RemoveView (oModule, THIS-OBJECT:Data:ViewName) .
        END.
        ELSE
            oModule = NEW Module (THIS-OBJECT:Data:ModuleName) .

        oService = ServiceManager:GetBusinessService (THIS-OBJECT:Data:EntityName) .

        ASSIGN cBedgmFile = ClassHelper:ClassNameToFile (oService:GetClass():TypeName)
               cBedgmFile = SUBSTRING (cBedgmFile, 1, R-INDEX (cBedgmFile, ".":U), "CHARACTER":U) + "bedgm":U .

        IF NOT FileHelper:Exists (cBedgmFile) THEN DO:
            ASSIGN cBedgmFile = FileHelper:ShortFileName (cBedgmFile) .

            IF NOT InputPromptHelper:PromptForCharacterValue ("Search Business Entity Design"{&TRAN},
                                                              ".bedgm file"{&TRAN},
                                                              CharacterPromptTypeEnum:FileNameMustExist,
                                                              INPUT-OUTPUT cBedgmFile) THEN
                RETURN ? .

            IF NOT FileHelper:Exists (cBedgmFile) THEN
                RETURN ? .
        END.

        ASSIGN cBedgmFile = FileHelper:FindFile(cBedgmFile)  .

        DATASET dsBusinessEntity:READ-XML ("file":U, cBedgmFile, ?, ?, ?) .

        oGridColumns = NEW CharacterList () .

        {Consultingwerk/foreachABL.i ViewerFieldSpec oField IN THIS-OBJECT:Data:ViewerFieldSpecs}
            oGridColumns:Add (oField:ColumnName) .
        END.

        oViewGenerator:CreateView (oModule,
                                   DATASET dsBusinessEntity BY-REFERENCE,
                                   TRIM (SUBSTITUTE ("&1,&2":U, THIS-OBJECT:Data:EntityTable, THIS-OBJECT:Data:EntityView), ",":U),
                                   THIS-OBJECT:Data:ViewType,
                                   THIS-OBJECT:Data:ViewName,
                                   THIS-OBJECT:Data:DataProviderName,
                                   THIS-OBJECT:Data:DataSourceName,
                                   oGridColumns,
                                   THIS-OBJECT:Data:ViewerFieldSpecs) .

        oViewGenerator:SaveModule (oModule, ?, ?) .

    END METHOD .

END CLASS.
