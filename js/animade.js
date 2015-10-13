Animade = function(configuration) {
    "use strict";

    var my = this;
    my.configuration = configuration;
    my.gl = null;
    my.uniforms = null;
    my.resize = resize;
    my.draw = draw;
    my.set = set;
    
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

    // Second order functions only call first order functions
    function parseFunctionsHeaders(source) {
        var variablePattern = /(?:(?:in\s+|out\s+|inout\s+)?[a-zA-Z0-9_]+(?:\s*\[[^\]]*\])*\s+[a-zA-Z0-9_]+(?:[(][^)]*[)])?(?:\s*\[[^\]]*\])*)/;
        var bodyPattern = /(?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{](?:[^{]|[{][}])*[}])*[}])*[}])*[}])*[}])*[}])*[}])*[}])*[}])*/;
        var pattern = new RegExp('(' + variablePattern.source + ')\\s*[(]\\s*(' + variablePattern.source + '?(?:\\s*[,]\\s*' + variablePattern.source + ')*)\\s*[)]\\s*[{](?:(' + bodyPattern.source + ')[}])?', 'g');
        var order = 0;

        var name = null;
        var secondOrder = {};
        var noFunctionSource = source.replace(pattern, function(full, f, ps, body) {
            if(body == null) throw 'Could not parse body of function: ' + f + '(' + ps + ')';
            if(/^\s*(in|out|inout)\b/.test(f)) throw 'Illegal in/out/inout modifier on return type: ' + f + '(' + ps + ')';
            name = /\b([a-zA-Z0-9_]+)$/g.exec(f)[1];
            if(secondOrder[name] != null) throw 'Custom functions can\'t be overloaded: ' + f + '(' + ps + ')';
            secondOrder[name] = {name: name, head: f, parameters: ps, body: body, functionParameters: ps.replace(/[^(]/g, '').length, order: order++};
            //console.log(name + ': ' + f + '(' + ps + ') {' + body + '}');
            return '// Function expanded: ' + name + '\n';
        });
        
        if(name != 'animation') throw 'The file must end with the following function: vec4 animation(vec4 position)';
        var animationFunction = secondOrder['animation'];

        //console.dir(secondOrder);
        //console.log(noFunctionSource);
        
        var firstOrder = {};
        function expand(second, functionArguments) {
            //console.dir({name: second.name, args: functionArguments});
            var suffix = functionArguments.map(function(a) { return '_' + a; }).join('') + '_';
            var key = second.name + suffix;
            var existing = firstOrder.hasOwnProperty(key) ? firstOrder[key] : null;
            if(existing != null) {
                existing.order = Math.min(existing.order, second.order - 1);
            } else {
                var parameters = [];
                var environment = {};
                var parameterPattern = new RegExp('\\s*(' + variablePattern.source + ')\\s*(?:[,]|$)', 'g');
                for(var p = parameterPattern.exec(second.parameters); p != null; p = parameterPattern.exec(second.parameters)) {
                    var functionName = /([a-zA-Z0-9_]+)\s*[(]/g.exec(p[1]);
                    if(functionName != null) {
                        environment[functionName[1]] = functionArguments.shift();
                    } else {
                        parameters.push(p[1]);
                    }
                }
                var header = second.head + suffix + '(' + parameters.join(', ') + ')';
                var body = second.body.replace(/([a-zA-Z0-9_]+)\s*[(](?:(\s*[a-zA-Z0-9_]+(?:\s*[,]\s*[a-zA-Z0-9_]+)*)\s*([,)]))?/g, function(all, name, functionArguments, end) {
                    functionArguments = functionArguments || '';
                    var target = environment.hasOwnProperty(name) ? secondOrder[environment[name]] : secondOrder[name];
                    if(target == null) return all;
                    var args = functionArguments.trim().split(/\s*[,]\s*/g);
                    var fargs = args.slice(0, target.functionParameters);
                    var vargs = args.slice(target.functionParameters).join(',');
                    fargs = fargs.map(function(arg) { 
                        return environment[arg] || arg;
                    }).filter(function(arg) {
                        return arg.length != 0;
                    });
                    
                    return expand(target, fargs) + '(' + vargs + (end == ')' ? ')' : '');
                });
                firstOrder[key] = {
                    order: second.order,
                    header: header,
                    body: body
                };
            }
            return key;
        }
        expand(animationFunction, []);
        
        //console.dir(firstOrder);
        
        var expanded = [];
        for(var y in firstOrder) if(firstOrder.hasOwnProperty(y)) {
            expanded.push(firstOrder[y]);
        }
        expanded.sort(function(a, b) { return a.order - b.order; });
        var expandedSource = expanded.map(function(x) {
            return x.header + ' {' + x.body + '}';
        }).join('\n\n');
        
        return noFunctionSource + '\n\n' + expandedSource;
    }
    var newSource = parseFunctionsHeaders(configuration.source);
    //console.log(newSource);
    
    function setupContext() {
        var gl = configuration.canvas.getContext("webgl") || configuration.canvas.getContext("experimental-webgl");
        if(!gl) throw "This browser does not support WebGL.";
        
        var source = Animade.fragmentShaderBefore + newSource + Animade.fragmentShaderAfter;

        var program = gl.createProgram();
        gl.attachShader(program, loadShader(gl, Animade.vertexShaderSource, gl.VERTEX_SHADER));
        gl.attachShader(program, loadShader(gl, source, gl.FRAGMENT_SHADER));
        gl.linkProgram(program);
        if(!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            var lastError = gl.getProgramInfoLog(program);
            throw "Error in program linking: " + lastError;
        }

        gl.useProgram(program);

        my.uniforms = {
            "u_resolution": gl.getUniformLocation(program, "u_resolution"),
            "u_scale": gl.getUniformLocation(program, "u_scale"),
            "u_offset": gl.getUniformLocation(program, "u_offset"),
            "u_time": gl.getUniformLocation(program, "u_time"),
        }

        var positionLocation = gl.getAttribLocation(program, "a_position");

        var buffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.enableVertexAttribArray(positionLocation);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
            -1, -1,   -1, +1,   +1, +1,
            -1, -1,   +1, +1,   +1, -1
        ]), gl.STATIC_DRAW);

        gl.uniform2f(my.uniforms.u_scale, 1, 1);
        gl.uniform2f(my.uniforms.u_offset, 0, 0);
        gl.uniform1f(my.uniforms.u_time, 0);
        
        my.gl = gl;
        my.source = source;
        my.program = program;
    }
    
    function set(uniforms) {
        for(var key in uniforms) if(uniforms.hasOwnProperty(key)) {
            if(!my.uniforms.hasOwnProperty(key)) {
                my.uniforms[key] = my.gl.getUniformLocation(my.program, key);
            }
            var location = my.uniforms[key];
            var vector = uniforms[key];
            if(vector.length === undefined) {
                my.gl.uniform1f(location, vector);
            } else {
                switch(vector.length) {
                    case 1: my.gl.uniform1f(location, vector[0]); break;
                    case 2: my.gl.uniform2f(location, vector[0], vector[1]); break;
                    case 3: my.gl.uniform3f(location, vector[0], vector[1], vector[2]); break;
                    case 4: my.gl.uniform4f(location, vector[0], vector[1], vector[2], vector[3]); break;
                    default: throw "Uniform must have 1-4 elements: " + key;
                }
            }
        }
    }
    
    function draw(uniforms) {
        if(uniforms != null) set(uniforms);
        my.gl.drawArrays(my.gl.TRIANGLES, 0, 6);
    }
    
    function fixViewport() {
        my.gl.viewport(0, 0, configuration.canvas.width, configuration.canvas.height);
        my.gl.uniform2f(my.uniforms.u_resolution, configuration.canvas.width, configuration.canvas.height);
    }
    
    function resize(width, height) {
        if(configuration.canvas.width == width && configuration.canvas.height == height) return;
        configuration.canvas.width = width;
        configuration.canvas.height = height;
        fixViewport();
        draw();
    }
    
    setupContext();
    fixViewport();
};

Animade.vertexShaderSource =
    "precision highp float;\n" +
    "attribute vec2 a_position;\n" +
    "\n" +
    "void main() {\n" +
    "    gl_Position = vec4(a_position, 0, 1);\n" +
    "}\n";
    
Animade.fragmentShaderBefore = 
    "precision mediump float;\n" +
    "uniform vec2 u_resolution;\n" +
    "uniform vec2 u_scale;\n" +
    "uniform vec2 u_offset;\n" +
    "uniform float u_time;\n" +
    "\n";

Animade.fragmentShaderAfter = 
    "\n" +
    "void main() {\n" +
    "    vec2 streched_position = (gl_FragCoord.xy / u_resolution) * vec2(2.0, 2.0) - vec2(1.0, 1.0);\n" +
    "    vec2 aspect = vec2(max(u_resolution.x / u_resolution.y, 1.0), max(u_resolution.y / u_resolution.x, 1.0));\n" +
    "    vec2 position = streched_position * aspect;\n" +
    "    gl_FragColor = animation_(vec4(position / u_scale - u_offset, 0.0, u_time));\n" +
    "}\n";
    
