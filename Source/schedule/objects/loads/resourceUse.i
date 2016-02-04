/* resourceUse.i */

    CASE resourceUse:
      WHEN 'Include' OR WHEN 'Without' THEN
      IF NOT CAN-FIND(resourceList WHERE resourceList.resource EQ {1}) THEN
      NEXT.
      WHEN 'Exclude' THEN
      IF CAN-FIND(resourceList WHERE resourceList.resource EQ {1}) THEN
      NEXT.
    END CASE.
