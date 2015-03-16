package dk.mzw.accelemation

import dk.mzw.accelemation.Language._

object ToHtml {
    def apply(animation : Animation) : String = {
        val glsl = ToGlsl(animation)
        val before = "<html><head><title>Demo</title><style>body { margin: 0; }canvas { width: 100%; height: 100% }</style></head><body><script src=\"js/three.js\"></script><script id=\"fragmentShader\" type=\"x-shader/x-vertex\">\n//<![CDATA[\n"
        val after = "//]]>\n</script><script src=\"js/program.js\"></script></body></html>\n"
        before ++ glsl ++ after
    }
}
