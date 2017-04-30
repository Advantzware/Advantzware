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
    File        : SmartWeb2ViewGenerator
    Purpose     : Generator for SmartViewerControl Classes
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 10:05:14 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                    FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH.
USING Consultingwerk.Framework.Enum.*                            FROM PROPATH .
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartWeb2ViewGenerator
    INHERITS AbstractSmartViewerGenerator
    IMPLEMENTS ISmartViewerGenerator:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the output file name
        Notes:   Optionally. Some Generators may not use the Viewer package and Class
                 Name for generation.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OutputFileName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWeb2ViewGenerator class
        Notes:
        @param pcOutputFileName The output file name
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWeb2ViewGenerator (pcOutputFileName AS CHARACTER):
        SUPER ().

        THIS-OBJECT:BindingSourceColumns = NEW "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>":U ().
        THIS-OBJECT:ControlGenerators = NEW ListIControlGenerator () .

        THIS-OBJECT:OutputFileName = pcOutputFileName .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generates the source code for the SmartViewerControl
        Notes:
        @return The file name of the generated source
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE CHARACTER Generate ():

        DEFINE VARIABLE lcFields  AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE lcCode    AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cTemplate AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOptions  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cFileNAme AS CHARACTER NO-UNDO .

        cTemplate = TemplateHelper:Find ("Web/Views/web2view.template":U) .

        COPY-LOB FROM FILE cTemplate TO lcCode .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            IF TYPE-OF (oControl, ISupportsWeb2ViewGeneration) THEN
                lcFields = lcFields + CAST (oControl, ISupportsWeb2ViewGeneration):GenerateHtml () + "~n":U.

        END.

        ASSIGN lcCode = SUBSTITUTE (lcCode, lcFields) .

        COPY-LOB FROM lcCode TO FILE THIS-OBJECT:OutputFileName .

        RUN Consultingwerk/BusinessEntityDesigner/Generator/Web2View/gene4gl.p (THIS-OBJECT:OutputFileName,
                                                                                INPUT-OUTPUT cOptions,
                                                                                INPUT-OUTPUT cFileName) .

        IF cFileName > "":U THEN DO:
            COMPILE VALUE (cFileName) SAVE .
            OS-DELETE VALUE (cFileName) .
        END .

        RETURN THIS-OBJECT:OutputFileName .

    END METHOD .

END CLASS.
