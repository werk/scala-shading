function PanZoom(x, y, scale, onClick) {
    this.x = x;
    this.y = y;
    this.scale = scale;
    this.onClick = onClick;
    this.addClickHandler = function(handler) {
        console.log("addClickHandler");
        this.onClick = handler;
    }
}

function Zoomable(widgetElement) {
    var hammertime = new Hammer(widgetElement);

    var panZoom = new PanZoom(0, 0, 1, null);

    hammertime.get('pinch').set({enable: true});
    hammertime.on('panstart', function (ev) {
        panZoom.startX = panZoom.x;
        panZoom.startY = panZoom.y;
    });
    hammertime.on('pan', function (ev) {
        panZoom.x = panZoom.startX + ev.deltaX / panZoom.scale;
        panZoom.y = panZoom.startY + ev.deltaY / panZoom.scale;
    });
    hammertime.on('pinchstart', function (ev) {
        panZoom.startScale = panZoom.scale;
    });
    hammertime.on('pinch', function (ev) {
        panZoom.scale = panZoom.startScale - ev.deltaZ;
        if (panZoom.scale < 0.01) panZoom.scale = 0.01;
    });
    hammertime.on('tap', function (ev) {
        if(panZoom.onClick != null) {
            console.dir(panZoom);
            panZoom.onClick(ev.center.x, ev.center.y)
        }
    });
    widgetElement.addEventListener('wheel', function (ev) {
        panZoom.scale -= panZoom.scale * ev.deltaY * 0.01;
        if (panZoom.scale < 0.01) panZoom.scale = 0.01;
    });

    this.panZoom = panZoom;
}

