open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain

module LDGNode = struct

end

type node = 
    HEAD
    | Node of LogUnit.t


    

