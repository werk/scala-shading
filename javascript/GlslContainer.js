function Vec2 (x, y) {
    this.x = x;
    this.y = y;
}

function GlslContainer(container) {
    var usedCanvas = null;
    var usedGlsl = null;

    this.load = function (code) {
        if(usedCanvas != null) {
            container.removeChild(usedCanvas)
        }
        var newCanvas =  document.createElement('canvas');
        container.appendChild(newCanvas);
        newCanvas.style.width = "100%";
        newCanvas.style.height = "100%";

        var newGlsl = Glsl({
            canvas: newCanvas,
            fragment: code,
            variables: {
                time: 0
            },
            update: function (time, delta) {
                this.set("time", time * 0.001);
            }
        });

        newGlsl.start();
        if(usedGlsl != null) usedGlsl.stop();

        usedCanvas = newCanvas;
        usedGlsl = newGlsl;

    };

}