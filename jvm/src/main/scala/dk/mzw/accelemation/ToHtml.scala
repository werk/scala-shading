package dk.mzw.accelemation

import dk.mzw.accelemation.Language._

import scala.io.Source

object ToHtml {
    def apply(animation : Animation) : String = {
        val glsl = ToGlsl(animation)
        //val before = "<html><head><title>Demo</title><style>body { margin: 0; }canvas { width: 100%; height: 100% }</style></head><body><script src=\"javascript/three.js\"></script><script id=\"fragmentShader\" type=\"x-shader/x-vertex\">\n//<![CDATA[\n"
        //val after = "//]]>\n</script><script src=\"javascript/program.js\"></script></body></html>\n"
        //before ++ glsl ++ after

        //val glslSource = Source.fromFile("javascript/glsl.js").mkString
        //val glslcontainerSource = Source.fromFile("javascript/GlslContainer.js").mkString

        s"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Demo</title>

    <style>
        html, body {
            height: 100%;
            padding: 0;
            margin: 0;
            overflow-y: hidden;
        }

    </style>

    <script id="fragment" type="shader/x-fragment">$glsl    </script>

    <script type="text/javascript">
    </script>

</head>
<body>
    <script type="text/javascript">
        if (!Glsl.supported()) alert("WebGL is not supported.");
        var loader = new GlslContainer(document.body);
        loader.load(document.getElementById("fragment").innerHTML);
    </script>
</body>
</html>
        """
    }
}
