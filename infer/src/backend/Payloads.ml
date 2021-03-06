(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  { annot_map: AnnotReachabilityDomain.t option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun: BufferOverrunSummary.t option
  ; class_loads: ClassLoadsDomain.summary option
  ; cost: CostDomain.summary option
  ; crashcontext_frame: Stacktree_t.stacktree option
  ; lab_resource_leaks: ResourceLeakDomain.summary option
  ; litho: LithoDomain.t option
  ; purity: PurityDomain.summary option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; pointer_preanalysis: PointerPreanalysisDomain.t option
  ; global_preanalysis: GlobalNameType.t option
  ; semantic_summary: (SemanticSummaryDomain.t * GlobalHandler.GlobalStore.t) option
  ; siof: SiofDomain.Summary.t option
  ; starvation: StarvationDomain.summary option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.Summary.t option }

let pp pe fmt
    { annot_map
    ; biabduction
    ; buffer_overrun
    ; class_loads
    ; cost
    ; crashcontext_frame
    ; lab_resource_leaks
    ; litho
    ; purity
    ; quandary
    ; racerd
    ; pointer_preanalysis
    ; global_preanalysis
    ; semantic_summary
    ; siof
    ; starvation
    ; typestate
    ; uninit } =
  let pp_opt prefix pp fmt = function
    | Some x ->
        F.fprintf fmt "%s: %a@\n" prefix pp x
    | None ->
        ()
  in
  F.fprintf fmt "%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a%a@\n"
    (pp_opt "Biabduction" (BiabductionSummary.pp pe))
    biabduction (pp_opt "TypeState" TypeState.pp) typestate
    (pp_opt "ClassLoads" ClassLoadsDomain.pp_summary)
    class_loads
    (pp_opt "CrashContext" Crashcontext.pp_stacktree)
    crashcontext_frame
    (pp_opt "Quandary" QuandarySummary.pp)
    quandary
    (pp_opt "Siof" SiofDomain.Summary.pp)
    siof
    (pp_opt "RacerD" RacerDDomain.pp_summary)
    racerd (pp_opt "Litho" LithoDomain.pp) litho
    (pp_opt "PointerAnalysis" PointerPreanalysisDomain.pp)
    pointer_preanalysis
    (pp_opt "GlobalVarTypeAnalysis" GlobalNameType.pp)
    global_preanalysis 
    (pp_opt "SemanticSummary" GlobalHandler.whole_pp)
    semantic_summary 
    (pp_opt "BufferOverrun" BufferOverrunSummary.pp)
    buffer_overrun
    (pp_opt "AnnotationReachability" AnnotReachabilityDomain.pp)
    annot_map
    (pp_opt "Uninitialised" UninitDomain.Summary.pp)
    uninit
    (pp_opt "Cost" CostDomain.pp_summary)
    cost
    (pp_opt "Starvation" StarvationDomain.pp_summary)
    starvation
    (pp_opt "Purity" PurityDomain.pp_summary)
    purity
    (pp_opt "Resource Leaks Lab" ResourceLeakDomain.pp)
    lab_resource_leaks


let empty =
  { annot_map= None
  ; biabduction= None
  ; class_loads= None
  ; buffer_overrun= None
  ; crashcontext_frame= None
  ; cost= None
  ; litho= None
  ; purity= None
  ; quandary= None
  ; racerd= None
  ; pointer_preanalysis = None
  ; global_preanalysis = None
  ; semantic_summary= None
  ; lab_resource_leaks= None
  ; siof= None
  ; starvation= None
  ; typestate= None
  ; uninit= None }
