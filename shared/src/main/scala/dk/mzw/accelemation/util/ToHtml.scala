package dk.mzw.accelemation.util

object ToHtml {

    def apply(glsl : String, title : String) : String =
        // language=html
        s"""<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>$title</title>
    <script type="text/javascript" src="https://rawgit.com/patriciogonzalezvivo/glslCanvas/master/build/GlslCanvas.js"></script>
</head>
<body style="margin: 0">
    <canvas class="glslCanvas" style="position: absolute; width: 100%; height: 100%" data-fragment="
$glsl
    "></canvas>
    <script>
        // Fixes a problem where a resize is necessary
        // TODO find a better solution
        setTimeout(function() {
            glslCanvases[0].width=1;
        });
    </script>
</body>
</html>"""

}
