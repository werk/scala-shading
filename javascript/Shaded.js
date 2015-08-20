"use strict";

(function() {

    var vertexShaderSource =
        "precision mediump float;\n" +
        "attribute vec2 a_position;\n" +
        "\n" +
        "void main() {\n" +
        "    gl_Position = vec4(a_position, 0, 1);\n" +
        "}\n";

    function loadShader(gl, shaderSource, shaderType) {
        var shader = gl.createShader(shaderType);
        gl.shaderSource(shader, shaderSource);
        gl.compileShader(shader);
        if(!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            var lastError = gl.getShaderInfoLog(shader);
            gl.deleteShader(shader);
            throw "Error compiling shader '" + shader + "':" + lastError;
        }
        return shader;
    }

    function Shaded(fragmentShaderSource, startTime) {
        var canvas = document.createElement('canvas');
        var gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
        if(!gl) throw "This browser does no support WebGL.";

        var program = gl.createProgram();
        var vertexShader = loadShader(gl, vertexShaderSource, gl.VERTEX_SHADER);
        var fragmentShader = loadShader(gl, fragmentShaderSource, gl.FRAGMENT_SHADER);
        gl.attachShader(program, vertexShader);
        gl.attachShader(program, fragmentShader);
        gl.linkProgram(program);
        if(!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            var lastError = gl.getProgramInfoLog(program);
            gl.deleteProgram(program);
            throw "Error in program linking: " + lastError;
            return;
        }

        gl.useProgram(program);

        var positionLocation = gl.getAttribLocation(program, "a_position");
        var resolutionLocation = gl.getUniformLocation(program, "u_resolution");
        var scaleLocation = gl.getUniformLocation(program, "u_scale");
        var offsetLocation = gl.getUniformLocation(program, "u_offset");
        var timeLocation = gl.getUniformLocation(program, "u_time");

        var buffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.enableVertexAttribArray(positionLocation);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
            -1, -1,   -1, +1,   +1, +1,
            -1, -1,   +1, +1,   +1, -1
        ]), gl.STATIC_DRAW);

        gl.uniform2f(scaleLocation, 1, 1);
        gl.uniform2f(offsetLocation, 0, 0);

        function dispose() {
            gl.deleteProgram(program);
            gl.deleteShader(vertexShader);
            gl.deleteShader(fragmentShader);
            // TODO Something else ??
        }

        function resize(width, height) {
            if(canvas.width == width && canvas.height == height) return;
            canvas.width = width;
            canvas.height = height;
            gl.viewport(0, 0, canvas.width, canvas.height);
            gl.uniform2f(resolutionLocation, canvas.width, canvas.height);
        }

        function step() {
            gl.uniform1f(timeLocation, (Date.now() - startTime) / 1000);
            gl.drawArrays(gl.TRIANGLES, 0, 6);
        }

        this.canvas = canvas;
        this.dispose = dispose;
        this.resize = resize;
        this.step = step;
    }
    window.Shaded = Shaded;
})();
