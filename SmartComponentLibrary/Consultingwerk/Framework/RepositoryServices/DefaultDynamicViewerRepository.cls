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
    File        : DefaultDynamicViewerRepository
    Purpose     : Detaulf Repository provider for dynamic viewers
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 01 16:18:08 CEST 2012
    Notes       : Reads and writes the viewer definition from an XML file,
                  typically in the session working directory
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.RepositoryServices.DefaultDynamicViewerRepository IMPLEMENTS IDynamicViewerRepository: 

    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i &REFERENCE-ONLY=REFERENCE-ONLY}
    
    /*------------------------------------------------------------------------------
        Purpose: Loads the Layout of the dynamic viewer.                                                                      
        Notes:   The output table ttViewerDesign is defined REFERENCE-ONLY inside this 
                 class and thus should be passed by the caller BY-REFERENCE                                                                       
        @param pcSettingsKey The key to retrieve the settings for
        @param ttViewerDesign OUTPUT TABLE The table that contains the viewer design                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadSettings (pcSettingsKey AS CHARACTER, 
                                     OUTPUT TABLE FOR ttViewerDesign):
        
        FILE-INFO:FILE-NAME = SUBSTITUTE ("viewer_&1.xml":U, pcSettingsKey) . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            TEMP-TABLE ttViewerDesign:READ-XML ("FILE":U, 
                                                FILE-INFO:FULL-PATHNAME, 
                                                "EMPTY":U, 
                                                ?, 
                                                FALSE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the Layout of the dynamic viewer.                                                                        
        Notes:   The output table ttViewerDesign is defined REFERENCE-ONLY inside this 
                 class and thus should be passed by the caller BY-REFERENCE
        @param pcSettingsKey The key to store the settings for 
        @param ttViewerDesign INPUT TABLE The table that contains the viewer design to be stored                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveSettings (pcSettingsKey AS CHARACTER, 
                                     TABLE FOR ttViewerDesign):
        
        /* Mike Fechner, Consultingwerk Ltd. 10.01.2012
           Bug 2776: Unformatted WRITE-XML causes issues with READ-XML on OpenEdge 11.0 */
        TEMP-TABLE ttViewerDesign:WRITE-XML ("FILE":U,  
                                             SUBSTITUTE ("viewer_&1.xml":U, pcSettingsKey),
                                             TRUE) .

    END METHOD.

END CLASS.