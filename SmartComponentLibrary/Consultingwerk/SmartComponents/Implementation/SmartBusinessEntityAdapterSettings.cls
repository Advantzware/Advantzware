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
    File        : SmartBusinessEntityAdapterSettings
    Purpose     : Helper class for the SmartBusinessEntityAdapter class to avoid 
                  the usage of static in the hybrid type
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 03 23:04:31 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .  
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapterSettings: 

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the SmartBusinessEntityAdapterSettings class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartBusinessEntityAdapterSettings ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates the SmartBusinessEntityAdapter instance that is working
                 with the passed in IDatasetController                                                                        
        Notes:  
        @param poDatasetController The IDatasetController instance that was launched by a SmartBusinessEntityAdapter
        @return The SmartBusinessEntityAdapter that has launched the IDatasetController or the unknown value                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC SmartBusinessEntityAdapter FromDatasetController (poDatasetController AS IDatasetController):
        
        DEFINE VARIABLE oObject  AS Progress.Lang.Object       NO-UNDO .
        DEFINE VARIABLE oAdapter AS SmartBusinessEntityAdapter NO-UNDO .
        
        ASSIGN oObject = SESSION:FIRST-OBJECT . 
        
        DO WHILE VALID-OBJECT (oObject):
            IF TYPE-OF (oObject, SmartBusinessEntityAdapter) THEN DO:
                ASSIGN oAdapter = CAST (oObject, SmartBusinessEntityAdapter) .
                
                IF VALID-OBJECT (oAdapter:DatasetController) AND oAdapter:DatasetController = poDatasetController
                    THEN RETURN oAdapter .   
            END.
            
            ASSIGN oObject = oObject:NEXT-SIBLING .
        END.
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the SmartBusinessEntityAdapter that is the SmartDataSource 
                 of a SmartDataTarget
        Notes:   When the given SmartDataTarget is linked to a different adpater type
                 (e.g. SmartDatasetChildAdapter) the actual SmartBusinessEntityAdapter
                 that is the data source to this data target is retrieved
        @param poDataTarget The SmartDataTarget for which the Adapter is to be retrieve
        @return The reference to the SmartBusinessEntityAdapter being the ultimate data source
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC SmartBusinessEntityAdapter FromSmartDataTarget (poDataTarget AS ISmartDataTarget):
        
        DEFINE VARIABLE oSource AS ISmartDataSource NO-UNDO . 
        
        oSource = poDataTarget:SmartDataSource .
        
        DO WHILE VALID-OBJECT (oSource):
            
            IF TYPE-OF (oSource, SmartBusinessEntityAdapter) THEN 
                RETURN CAST (oSource, SmartBusinessEntityAdapter) .
            
            IF TYPE-OF (oSource, ISmartDataTarget) THEN 
                oSource = CAST (oSource, ISmartDataTarget):SmartDataSource . 
            ELSE 
                oSource = ? .
        END.

        RETURN ? . 

    END METHOD.

END CLASS.
