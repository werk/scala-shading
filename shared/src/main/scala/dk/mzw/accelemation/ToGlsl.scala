package dk.mzw.accelemation

import dk.mzw.accelemation.Compile.SourceAndUniforms
import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object ToGlsl {

    def provided(name : String) : Animation = t => x => y => {
        Term(Call(name, List(Call("vec4",List(x.untyped, y.untyped, Constant(0), t.untyped)))))
    }
    
    def withUniforms(f : Animation, prelude : String = "") : (String, Seq[Uniform[_]]) = {
        val SourceAndUniforms(glsl, uniforms) = Compile(f, "animation", "t", "x", "y")
        val all = boilerplateUniforms(uniforms) + /*boilerplateBefore + */ prelude + glsl + boilerplateAfter
        (all, uniforms.toSeq.map(_.ref))
    }

    def apply(f : Animation, prelude : String = "") : String = withUniforms(f, prelude)._1

    private def boilerplateUniforms(uniforms : Set[UniformU]) = {
        val us = uniforms.map(u => s"uniform ${u.variableType} ${u.ref.name};").mkString("\n")
s"""
precision mediump float;
uniform vec2 u_resolution;
uniform vec2 u_scale;
uniform vec2 u_offset;
uniform float u_time;
$us
"""
    }


    private val boilerplateAfter = """
void main() {
    vec2 streched_position = (gl_FragCoord.xy / u_resolution) * vec2(2.0, 2.0) - vec2(1.0, 1.0);
    vec2 aspect = vec2(max(u_resolution.x / u_resolution.y, 1.0), max(u_resolution.y / u_resolution.x, 1.0));
    vec2 position = streched_position * aspect;
    gl_FragColor = animation(u_time, position.x, position.y);
}
"""
}


