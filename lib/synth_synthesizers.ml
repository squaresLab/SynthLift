open Synth_types

(** Synthesizes for an arch *)
module Mips : Synthesizer = Synth_synthesizer.Make(Synth_mips)
module Arm : Synthesizer = Synth_synthesizer.Make(Synth_arm)

(** Synthesized lifters for an arch *)
module Lifter_mips : Lifter = Synth_lifter.Make(Synth_mips)
module Lifter_arm  : Lifter = Synth_lifter.Make(Synth_arm)
