package dk.mzw.accelemation.js

import dk.mzw.accelemation.Combinators
import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.samples.{TimeLens, Spiral, HidingDevils}

object GlobalAnimations {

    val animations = List[(String, Animation)](
        "Time Lens" -> TimeLens.apply,
        "HidingDevils" -> HidingDevils.apply,
        "Spiral" -> Spiral.apply
    )

    val effects = List[(String, Animation => Animation)](
        "Spin" -> Combinators.spin(2),
        "Circle" -> Combinators.circle(1),
        "Up" -> Combinators.translate (0, 1),
        "Left" -> Combinators.translate (1, 0)
    )

    val combinators = List[(String, Animation => Animation => Animation)](
        "Transform" -> Combinators.bendSpaceTime,
        "Add" -> Combinators.addition,
        "Subtract" -> Combinators.subtract,
        "Multiply" -> Combinators.multiply
    )

}
