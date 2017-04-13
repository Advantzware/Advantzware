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
    File        : SmartBindingSourceDesigner_ShowAddFieldsChooserForm
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 24 09:58:52 CEST 2009
    Notes       : Bug 2013: Access to Dialoges in Consultingwerk.SmartComponents.Design.dll 
                  outsourced to .p file, this removes the runtime dependency on the design
                  time assembly.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.SmartComponents.Design.* FROM ASSEMBLY . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .

DEFINE INPUT  PARAMETER poBindingSource AS SmartBindingSource NO-UNDO .  
DEFINE OUTPUT PARAMETER pcColumns       AS CHARACTER          NO-UNDO .

/* ***************************  Main Block  *************************** */

pcColumns = SmartBindingSourceDesigner:ShowAddFieldsChooserForm (poBindingSource).


