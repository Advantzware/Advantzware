/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BufferDataSource
    Purpose     : Encapsulates a dynamic data-source handle
    Syntax      :
    Description : Tool to simplify the management of dynamic data-source
                  object handles in a DataAccess class.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 15 20:41:08 CEST 2016
    Notes       : It's an ABL restriction that a single buffer can only be
                  contained in a single data-source at a time, which causes
                  that implementations like
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/Switching+the+Buffer+sequence+on+a+DATA-SOURCE+in+a+DataAccess+class
                  or http://confluence.consultingwerkcloud.com/wiki/display/SCL/Filtering+in+a+Data+Access+class+on+a+table+that+is+not+part+of+the+(default)+data-source
                  typically result in the usage of dynamic buffers, dynamic
                  queries and dynamic data-source object handles
                  See http://confluence.consultingwerkcloud.com/wiki/display/SCL/New+API+for+dynamic+DATA-SOURCE+objects
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.BufferDataSource
    IMPLEMENTS IBufferDataSource
    USE-WIDGET-POOL:

    /**
     * Purpose: Returns the handle of the data-source object
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataSourceHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Return the array of dynamic buffer handles
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferHandles AS HANDLE EXTENT NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Gets and sets the array of buffer spec objects
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferSpecs AS IBufferSpec EXTENT NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns if the Buffer Data Source is initialized already
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Initialized AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PROTECTED SET.

    /**
     *  Purpose: Returns the handle of the data-source query
     *  Notes:
     */
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     */
    CONSTRUCTOR PUBLIC BufferDataSource ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 1 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 2 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 3 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 4 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 5 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     * @param pcTableName6 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER,
                                         pcTableName6 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 6 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5)
               THIS-OBJECT:BufferSpecs[6]  = NEW BufferSpec (pcTableName6) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     * @param pcTableName6 The name of the buffer table
     * @param pcTableName7 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER,
                                         pcTableName6 AS CHARACTER,
                                         pcTableName7 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 7 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5)
               THIS-OBJECT:BufferSpecs[6]  = NEW BufferSpec (pcTableName6)
               THIS-OBJECT:BufferSpecs[7]  = NEW BufferSpec (pcTableName7) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     * @param pcTableName6 The name of the buffer table
     * @param pcTableName7 The name of the buffer table
     * @param pcTableName8 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER,
                                         pcTableName6 AS CHARACTER,
                                         pcTableName7 AS CHARACTER,
                                         pcTableName8 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 8 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5)
               THIS-OBJECT:BufferSpecs[6]  = NEW BufferSpec (pcTableName6)
               THIS-OBJECT:BufferSpecs[7]  = NEW BufferSpec (pcTableName7)
               THIS-OBJECT:BufferSpecs[8]  = NEW BufferSpec (pcTableName8) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     * @param pcTableName6 The name of the buffer table
     * @param pcTableName7 The name of the buffer table
     * @param pcTableName8 The name of the buffer table
     * @param pcTableName9 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER,
                                         pcTableName6 AS CHARACTER,
                                         pcTableName7 AS CHARACTER,
                                         pcTableName8 AS CHARACTER,
                                         pcTableName9 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 9 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5)
               THIS-OBJECT:BufferSpecs[6]  = NEW BufferSpec (pcTableName6)
               THIS-OBJECT:BufferSpecs[7]  = NEW BufferSpec (pcTableName7)
               THIS-OBJECT:BufferSpecs[8]  = NEW BufferSpec (pcTableName8)
               THIS-OBJECT:BufferSpecs[9]  = NEW BufferSpec (pcTableName9) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param pcTableName1 The name of the buffer table
     * @param pcTableName2 The name of the buffer table
     * @param pcTableName3 The name of the buffer table
     * @param pcTableName4 The name of the buffer table
     * @param pcTableName5 The name of the buffer table
     * @param pcTableName6 The name of the buffer table
     * @param pcTableName7 The name of the buffer table
     * @param pcTableName8 The name of the buffer table
     * @param pcTableName9 The name of the buffer table
     * @param pcTableName10 The name of the buffer table
     */
    CONSTRUCTOR PUBLIC BufferDataSource (pcTableName1 AS CHARACTER,
                                         pcTableName2 AS CHARACTER,
                                         pcTableName3 AS CHARACTER,
                                         pcTableName4 AS CHARACTER,
                                         pcTableName5 AS CHARACTER,
                                         pcTableName6 AS CHARACTER,
                                         pcTableName7 AS CHARACTER,
                                         pcTableName8 AS CHARACTER,
                                         pcTableName9 AS CHARACTER,
                                         pcTableName10 AS CHARACTER):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 10 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = NEW BufferSpec (pcTableName1)
               THIS-OBJECT:BufferSpecs[2]  = NEW BufferSpec (pcTableName2)
               THIS-OBJECT:BufferSpecs[3]  = NEW BufferSpec (pcTableName3)
               THIS-OBJECT:BufferSpecs[4]  = NEW BufferSpec (pcTableName4)
               THIS-OBJECT:BufferSpecs[5]  = NEW BufferSpec (pcTableName5)
               THIS-OBJECT:BufferSpecs[6]  = NEW BufferSpec (pcTableName6)
               THIS-OBJECT:BufferSpecs[7]  = NEW BufferSpec (pcTableName7)
               THIS-OBJECT:BufferSpecs[8]  = NEW BufferSpec (pcTableName8)
               THIS-OBJECT:BufferSpecs[9]  = NEW BufferSpec (pcTableName9)
               THIS-OBJECT:BufferSpecs[10] = NEW BufferSpec (pcTableName10) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 1 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 2 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 3 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 4 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 5 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     * @param poBufferSpec6 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec,
                                         poBufferSpec6 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 6 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5
               THIS-OBJECT:BufferSpecs[6]  = poBufferSpec6 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     * @param poBufferSpec6 The buffer spec for the dynamic buffer
     * @param poBufferSpec7 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec,
                                         poBufferSpec6 AS IBufferSpec,
                                         poBufferSpec7 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 7 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5
               THIS-OBJECT:BufferSpecs[6]  = poBufferSpec6
               THIS-OBJECT:BufferSpecs[7]  = poBufferSpec7 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     * @param poBufferSpec6 The buffer spec for the dynamic buffer
     * @param poBufferSpec7 The buffer spec for the dynamic buffer
     * @param poBufferSpec8 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec,
                                         poBufferSpec6 AS IBufferSpec,
                                         poBufferSpec7 AS IBufferSpec,
                                         poBufferSpec8 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 8 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5
               THIS-OBJECT:BufferSpecs[6]  = poBufferSpec6
               THIS-OBJECT:BufferSpecs[7]  = poBufferSpec7
               THIS-OBJECT:BufferSpecs[8]  = poBufferSpec8 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     * @param poBufferSpec6 The buffer spec for the dynamic buffer
     * @param poBufferSpec7 The buffer spec for the dynamic buffer
     * @param poBufferSpec8 The buffer spec for the dynamic buffer
     * @param poBufferSpec9 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec,
                                         poBufferSpec6 AS IBufferSpec,
                                         poBufferSpec7 AS IBufferSpec,
                                         poBufferSpec8 AS IBufferSpec,
                                         poBufferSpec9 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 9 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5
               THIS-OBJECT:BufferSpecs[6]  = poBufferSpec6
               THIS-OBJECT:BufferSpecs[7]  = poBufferSpec7
               THIS-OBJECT:BufferSpecs[8]  = poBufferSpec8
               THIS-OBJECT:BufferSpecs[9]  = poBufferSpec9 .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferDataSource class
     * Notes:
     * @param poBufferSpec1 The buffer spec for the dynamic buffer
     * @param poBufferSpec2 The buffer spec for the dynamic buffer
     * @param poBufferSpec3 The buffer spec for the dynamic buffer
     * @param poBufferSpec4 The buffer spec for the dynamic buffer
     * @param poBufferSpec5 The buffer spec for the dynamic buffer
     * @param poBufferSpec6 The buffer spec for the dynamic buffer
     * @param poBufferSpec7 The buffer spec for the dynamic buffer
     * @param poBufferSpec8 The buffer spec for the dynamic buffer
     * @param poBufferSpec9 The buffer spec for the dynamic buffer
     * @param poBufferSpec10 The buffer spec for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferDataSource (poBufferSpec1 AS IBufferSpec,
                                         poBufferSpec2 AS IBufferSpec,
                                         poBufferSpec3 AS IBufferSpec,
                                         poBufferSpec4 AS IBufferSpec,
                                         poBufferSpec5 AS IBufferSpec,
                                         poBufferSpec6 AS IBufferSpec,
                                         poBufferSpec7 AS IBufferSpec,
                                         poBufferSpec8 AS IBufferSpec,
                                         poBufferSpec9 AS IBufferSpec,
                                         poBufferSpec10 AS IBufferSpec):
        SUPER ().

        EXTENT (THIS-OBJECT:BufferSpecs) = 10 .

        ASSIGN THIS-OBJECT:BufferSpecs[1]  = poBufferSpec1
               THIS-OBJECT:BufferSpecs[2]  = poBufferSpec2
               THIS-OBJECT:BufferSpecs[3]  = poBufferSpec3
               THIS-OBJECT:BufferSpecs[4]  = poBufferSpec4
               THIS-OBJECT:BufferSpecs[5]  = poBufferSpec5
               THIS-OBJECT:BufferSpecs[6]  = poBufferSpec6
               THIS-OBJECT:BufferSpecs[7]  = poBufferSpec7
               THIS-OBJECT:BufferSpecs[8]  = poBufferSpec8
               THIS-OBJECT:BufferSpecs[9]  = poBufferSpec9
               THIS-OBJECT:BufferSpecs[10] = poBufferSpec10 .

    END CONSTRUCTOR.

    /**
     * Purpose: Attaches the Data-Source to the ProDataset buffer object handle
     * Notes:
     * @param phProDatasetBuffer The handle of the ProDataset buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PUBLIC LOGICAL AttachDataSource (phProDatasetBuffer AS HANDLE):

        HandleAssert:ValidHandle(phProDatasetBuffer, "proProDatasetBuffer":U) .

        IF NOT THIS-OBJECT:Initialized THEN
            THIS-OBJECT:Initialize() .

        phProDatasetBuffer:ATTACH-DATA-SOURCE (THIS-OBJECT:DataSourceHandle) .

    END METHOD .

    /**
     * Purpose: Returns the handle of the data-source object
     * Notes:   The implementation is required to remove and white space characters
     *          from pcPairsList that might have been inserted to improve readability
     *          but are not supported by the DATA-SOURCE
     * @param phProDatasetBuffer The handle of the ProDataset buffer
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PUBLIC LOGICAL AttachDataSource (phProDatasetBuffer AS HANDLE,
                                            pcPairsList AS CHARACTER):

        DEFINE VARIABLE cPairsList     AS CHARACTER NO-UNDO INITIAL ? .

        HandleAssert:ValidHandle(phProDatasetBuffer, "proProDatasetBuffer":U) .

        IF NOT THIS-OBJECT:Initialized THEN
            THIS-OBJECT:Initialize() .

        IF pcPairsList > "":U THEN
            ASSIGN cPairsList = REPLACE (REPLACE (REPLACE (REPLACE (pcPairsList, " ":U, "":U), CHR(9), "":U), CHR (10), "":U), CHR(13), "":U) .

        phProDatasetBuffer:ATTACH-DATA-SOURCE (THIS-OBJECT:DataSourceHandle, cPairsList) .

    END METHOD .

    /**
     * Purpose: Cleans up all dynamic resources
     * Notes:
     */
    METHOD PROTECTED VOID Cleanup ():

        GarbageCollectorHelper:DeleteObject(THIS-OBJECT:DataSourceHandle) .
        GarbageCollectorHelper:DeleteObject(THIS-OBJECT:QueryHandle) .

        ASSIGN THIS-OBJECT:DataSourceHandle = ?
               THIS-OBJECT:QueryHandle      = ? .

        IF EXTENT (THIS-OBJECT:BufferHandles) > 0 THEN
            THIS-OBJECT:DeleteBufferHandles () .

        EXTENT (THIS-OBJECT:BufferHandles) = ? .

    END METHOD.

    /**
     * Purpose: Deletes the dynamic buffer handles
     * Notes:
     */
    METHOD PROTECTED VOID DeleteBufferHandles():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (THIS-OBJECT:BufferHandles):

            GarbageCollectorHelper:DeleteObject(THIS-OBJECT:BufferHandles[i]) .
            THIS-OBJECT:BufferHandles[i] = ? .
        END.

    END METHOD.

    /**
     * Purpose: Initializes the DATA-SOURCE object handle and supporting objects
     * Notes:   Invoked by the AttachDataSource method if not invoked before
     */
    METHOD PUBLIC VOID Initialize ():

        DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER NO-UNDO .
        DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO .

        Assert:NotNullOrZero (EXTENT (THIS-OBJECT:BufferSpecs), "EXTENT(BufferSpecs)":U) .

        THIS-OBJECT:Cleanup() .

        EXTENT (THIS-OBJECT:BufferHandles) = EXTENT (THIS-OBJECT:BufferSpecs) .

        /* No direct CREATE QUERY into THIS-OBJECT:QueryHandle due to
           proparse limitations */
        CREATE QUERY hQuery .
        ASSIGN THIS-OBJECT:QueryHandle = hQuery .

        /* Create dynamic buffers */
        DO i = 1 TO EXTENT (THIS-OBJECT:BufferSpecs):

            IF THIS-OBJECT:BufferSpecs[i]:BufferName > "":U THEN

                CREATE BUFFER hBuffer
                    FOR TABLE THIS-OBJECT:BufferSpecs[i]:TableName
                    BUFFER-NAME THIS-OBJECT:BufferSpecs[i]:BufferName .
            ELSE
                CREATE BUFFER hBuffer
                    FOR TABLE THIS-OBJECT:BufferSpecs[i]:TableName .

            ASSIGN THIS-OBJECT:BufferHandles[i] = hBuffer .

            THIS-OBJECT:QueryHandle:ADD-BUFFER (hBuffer) .
        END.

        CREATE DATA-SOURCE hDataSource .
        ASSIGN THIS-OBJECT:DataSourceHandle = hDataSource .

        ASSIGN THIS-OBJECT:DataSourceHandle:QUERY = THIS-OBJECT:QueryHandle .

    END METHOD .

    /**
     * Purpose: Returns the database field name matching a temp-table field name
     *          contained in a consumers query string (query string vs. temp-table
     *          definition)
     * Notes:   To be used in the DataAccess:SourceColumn method
     * @param pcColumn The temp table column name to return the source (database table) field name for
     * @return The database field name mapped to the passed in temp-table field
     */
    METHOD PUBLIC CHARACTER SourceColumn (pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .

        cMap = THIS-OBJECT:DataSourceHandle:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        ASSIGN iPos = LOOKUP(pcColumn, cMap) .

        IF iPos MODULO 2 = 1 THEN
            RETURN ENTRY(LOOKUP(pcColumn,cMap) + 1,cMap).
        ELSE
            RETURN "":U .

    END METHOD.

    /**
     * Purpose: Returns the default query string (USING OF) that can be used for the
     *          SourceDefaultQuery method of the DataAccess class
     * Notes:   Can not be used, when the buffers cannot be joined using OF
     * @return The query string for the source default query
     */
    METHOD PUBLIC CHARACTER SourceDefaultQuery ():

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .

        DEFINE VARIABLE cParentBuffer AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cQueryJoin    AS CHARACTER NO-UNDO .

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1":U, THIS-OBJECT:BufferHandles[1]:NAME) .

        DO i = 2 TO EXTENT (THIS-OBJECT:BufferHandles):

            IF THIS-OBJECT:BufferSpecs[i]:ParentBufferName > "":U THEN
                ASSIGN cParentBuffer = THIS-OBJECT:BufferSpecs[i]:ParentBufferName .
            ELSE
                ASSIGN cParentBuffer = THIS-OBJECT:BufferHandles[i - 1]:NAME .

            IF THIS-OBJECT:BufferSpecs[i]:QueryJoin = QueryJoinEnum:OuterJoin THEN
                ASSIGN cQueryJoin = " OUTER-JOIN":U .
            ELSE
                ASSIGN cQueryJoin = "":U /* inner-join */ .

            ASSIGN cQueryString = cQueryString + SUBSTITUTE (", EACH &2 OF &1&3":U,
                                                             cParentBuffer,
                                                             THIS-OBJECT:BufferHandles[i]:NAME,
                                                             cQueryJoin) .
        END.

        RETURN cQueryString + " INDEXED-REPOSITION":U.

    END METHOD .

    /**
     * Purpose: Destructor for the BufferDataSource class
     * Notes:
     */
    DESTRUCTOR PUBLIC BufferDataSource ():

        THIS-OBJECT:Cleanup() .

    END DESTRUCTOR.

END CLASS.
