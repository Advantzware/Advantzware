/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ProcedureEditorBaseForm
    Purpose     : Specialized (WinKit form) base class for the ProcedureEditorForm
                  This version excluses customers additions to EmbeddedWindowBaseForm
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 30 12:18:32 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

{Consultingwerk/products.i}

&SCOPED-DEFINE OverrideAlternativeClassName ProcedureEditorBaseForm
&SCOPED-DEFINE OverrideAlternativePackageName Consultingwerk.Studio.ProcedureEditor
&SCOPED-DEFINE OverrideAlternativeBaseClass Consultingwerk.Forms.BaseForm
&SCOPED-DEFINE OverrideEmbeddedWindowBaseFormConstructorInclude OverrideEmbeddedWindowBaseFormConstructorInclude

&IF DEFINED (WinKit) NE 0 &THEN
{Consultingwerk/WindowIntegrationKit/Forms/EmbeddedWindowBaseForm.cls}
&ELSE
CLASS Consultingwerk.Studio.ProcedureEditor.ProcedureEditorBaseForm:
END CLASS.
&ENDIF
