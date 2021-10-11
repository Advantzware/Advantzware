    DEFINE TEMP-TABLE ttUserColumn
        FIELD programName        AS CHARACTER LABEL "Program"
        FIELD usrID              AS CHARACTER LABEL "User"
        FIELD colName            AS CHARACTER LABEL "Name" 
        FIELD colLabel           AS CHARACTER LABEL "Label"
        FIELD defaultColLabel    AS CHARACTER
        FIELD colVisible         AS LOGICAL   LABEL "Visible"
        FIELD colWidth           AS DECIMAL   LABEL "Width"
        FIELD colPosition        AS INTEGER   LABEL "Position"
        INDEX colPosition programName usrID colPosition 
        .
        
    DEFINE TEMP-TABLE ttDefaultUserColumn
        FIELD colName     AS CHARACTER LABEL "Name" 
        FIELD colLabel    AS CHARACTER
        FIELD colVisible  AS LOGICAL
        FIELD colWidth    AS DECIMAL
        FIELD colPosition AS INTEGER
        INDEX colName colName
        .
                