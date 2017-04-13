jQuery.fn.draggit = function (el, MarginLeft, MarginTop) {
    var thisdiv = this;
    var thistarget = $(el);
    var relX;
    var relY;
    var targetw = thistarget.width();
    var targeth = thistarget.height();
    var docw;
    var doch;
    var ismousedown;

    thistarget.css('position','absolute');


    thisdiv.bind('mousedown', function(e){
        var pos = $(el).offset();
        var srcX = pos.left;
        var srcY = pos.top;
        
        docw = $('body').width();
        doch = $('body').height();
        
        relX = e.pageX - MarginLeft - srcX ;
        relY = e.pageY - MarginTop - srcY;

        ismousedown = true;
    });

    $(document).bind('mousemove',function(e){ 
        if(ismousedown)
        {
            targetw = thistarget.width();
            targeth = thistarget.height();

            var maxX = docw - targetw - 10;
            var maxY = doch - targeth - 10;

            var mouseX = e.pageX;
            var mouseY = e.pageY;

            var diffX = mouseX - relX;
            var diffY = mouseY - relY;

            if(diffX < 0)   diffX = 0;
            if(diffY < 0)   diffY = 0;
            if(diffX > maxX) diffX = maxX;
            if(diffY > maxY) diffY = maxY;

            $(el).css('top', (diffY)+'px');
            $(el).css('left', (diffX)+'px');
        }
    });

    $(window).bind('mouseup', function(e){
        ismousedown = false;
    });

    return this;
};