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
    File        : SampleSetBackgroundColorFilter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 06:54:02 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH . 
USING Consultingwerk.Util.*                 FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.SampleSetBackgroundColorFilter 
    IMPLEMENTS ISetBackgroundColorFilter: 

    /*------------------------------------------------------------------------------
        Purpose: Filters widget's for the WidgetHelper:SetFrameBackgroundColor routine
        Notes:   Allows customizable ignoring of widgets as well as custom 
                 handling of background colors (set color in this routine and return
                 FALSE.
                 As the ISetBackgroundColor filter is queried before the resursion to
                 nested frames is invoked this allows to filter out whole frames as 
                 well
        @param phWidget The widget for which to return if it shuld be colored or not
        @param poParameter The ISetFrameBackgroundColorParameter instance with the parameters for the SetFrameBackgroundColor routine
        @return Logical value indicating if the Widget should be color coded or not, when FALSE the widget will be ignored
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FilterWidgetForSetBackgroundColor (phWidget AS HANDLE,
                                                             poParameter AS ISetFrameBackgroundColorParameter):
        
        IF phWidget:TYPE <> WidgetTypeEnum:FillIn AND 
           phWidget:TYPE <> WidgetTypeEnum:Editor AND 
           phWidget:TYPE <> WidgetTypeEnum:ComboBox THEN 
            RETURN TRUE . 
        
        IF poParameter:TextboxColor >= 0 AND 
            (phWidget:BGCOLOR = ? OR phWidget:BGCOLOR = poParameter:TextboxColor OR phWidget:BGCOLOR = poParameter:DisabledtextColor) 
            THEN RETURN TRUE . 

        RETURN FALSE . 

    END METHOD .

END CLASS.
