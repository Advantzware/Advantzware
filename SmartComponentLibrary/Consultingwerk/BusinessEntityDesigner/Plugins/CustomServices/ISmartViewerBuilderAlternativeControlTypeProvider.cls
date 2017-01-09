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
    File        : ISmartViewerBuilderAlternativeControlTypeProvider
    Purpose     : Interface for customization classes that provide Alternative
                  Control types used by the SmartViewerBuilder
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 16 22:21:34 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Infragistics.Win.*                                             FROM ASSEMBLY .
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ISmartViewerBuilderAlternativeControlTypeProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Callback to add additional alternative Control Types to the Value List
                 of Alternative Control Types
        Notes:
        @param poOrigin The AlternativeControlTypeProviderOriginEnum describing the origin of the call
        @param poValueList The ValueList to add additional Control Types to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddCustomControlTypes (poOrigin AS AlternativeControlTypeProviderOriginEnum,
                                              poValueList AS ValueList).

END INTERFACE.
