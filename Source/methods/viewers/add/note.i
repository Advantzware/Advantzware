/* note.i */

display today @ note.createDate
        string(time,"hh:mm:ss am") form "x(11)" @ note.createTime
        today @ note.updateDate
        string(time,"hh:mm:ss am") form "x(11)" @ note.updateTime
        with frame {&frame-name}. 
