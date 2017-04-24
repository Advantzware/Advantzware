/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : get-mandatory-service.i
    Purpose     : Simplify the access of mandatory services form the default
                  service container  

    Syntax      : {Consultingwerk/get-mandatory-service.i <ABL Service Type Name (Class)>} 
                  {Consultingwerk/get-mandatory-service.i Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService}

    Description : Returns a reference to a service of a give type, typically
                  an Interface type. Allows singleton like classes (framework
                  components) that are not tied to an actual class name, but
                  to an interface  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 02 14:12:07 CEST 2011
    Notes       : See Consultingwerk.Framework.ServiceContainer
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

&IF PROVERSION BEGINS "10" OR PROVERSION EQ "11.0" OR PROVERSION EQ "11.1" OR PROVERSION EQ "11.2" OR PROVERSION EQ "11.3" &THEN

    CAST (Consultingwerk.Framework.FrameworkSettings:ServiceContainer:GetMandatoryService
                    (Progress.Lang.Class:GetClass ("{1}":U)),
                     {1})
         
&ELSE

    CAST (Consultingwerk.Framework.FrameworkSettings:ServiceContainer:GetMandatoryService
                    (GET-CLASS ({1})),
                     {1})
         
&ENDIF

