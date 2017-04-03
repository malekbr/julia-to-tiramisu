using ParallelAccelerator

@acc function hello()
    iterations = 100
    A = Array{Int64,1}(iterations)
    @fuse begin
    A = map(x->1,A)
    A = map(x->x+1,A)
    end
    return A
end

function main()
    tic()
    B = hello()
    println("SELFTIMED ", toq())

    println(B[end])

end

main()
#=
Array{Any}((4,))
  1: Expr
    head: Symbol parfor_start
    args: Array{Any}((1,))
      1: ParallelAccelerator.ParallelIR.PIRParForStartEnd
        loopNests: Array{ParallelAccelerator.ParallelIR.PIRLoopNest}((1,))
          1: ParallelAccelerator.ParallelIR.PIRLoopNest
            indexVariable: TypedSlot
              id: Int64 8
              typ: Int64 <: Signed
            lower: Int64 1
            upper: Int64 100
            step: Int64 1
        reductions: Array{ParallelAccelerator.ParallelIR.PIRReduction}((0,))
        instruction_count_expr: Void nothing
        private_vars: Array{Union{SSAValue,SlotNumber,TypedSlot}}((0,))
    typ: Int64 <: Signed
  2: Expr
    head: Symbol call
    args: Array{Any}((3,))
      1: GlobalRef
        mod: Module Base
        name: Symbol unsafe_arrayref
      2: SlotNumber
        id: Int64 3
      3: TypedSlot
        id: Int64 8
        typ: Int64 <: Signed
    typ: Int64 <: Signed
  3: Expr
    head: Symbol call
    args: Array{Any}((4,))
      1: GlobalRef
        mod: Module Base
        name: Symbol unsafe_arrayset
      2: TypedSlot
        id: Int64 11
        typ: Array{Int64,1} <: DenseArray{Int64,1}
      3: Int64 1
      4: TypedSlot
        id: Int64 8
        typ: Int64 <: Signed
    typ: Array{Int64,1} <: DenseArray{Int64,1}
  4: Expr
    head: Symbol parfor_end
    args: Array{Any}((1,))
      1: ParallelAccelerator.ParallelIR.PIRParForStartEnd
        loopNests: Array{ParallelAccelerator.ParallelIR.PIRLoopNest}((1,))
          1: ParallelAccelerator.ParallelIR.PIRLoopNest
            indexVariable: TypedSlot
              id: Int64 8
              typ: Int64 <: Signed
            lower: Int64 1
            upper: Int64 100
            step: Int64 1
        reductions: Array{ParallelAccelerator.ParallelIR.PIRReduction}((0,))
        instruction_count_expr: Void nothing
        private_vars: Array{Union{SSAValue,SlotNumber,TypedSlot}}((0,))
    typ: Int64 <: Signed
=#

#=
Array{Any}((5,))
  1: Expr
    head: Symbol parfor_start
    args: Array{Any}((1,))
      1: ParallelAccelerator.ParallelIR.PIRParForStartEnd
        loopNests: Array{ParallelAccelerator.ParallelIR.PIRLoopNest}((1,))
          1: ParallelAccelerator.ParallelIR.PIRLoopNest
            indexVariable: TypedSlot
              id: Int64 11
              typ: Int64 <: Signed
            lower: Int64 1
            upper: Int64 100
            step: Int64 1
        reductions: Array{ParallelAccelerator.ParallelIR.PIRReduction}((0,))
        instruction_count_expr: Void nothing
        private_vars: Array{Union{SSAValue,SlotNumber,TypedSlot}}((1,))
          1: SSAValue
            id: Int64 0
    typ: Int64 <: Signed
  2: Expr
    head: Symbol call
    args: Array{Any}((3,))
      1: GlobalRef
        mod: Module Base
        name: Symbol unsafe_arrayref
      2: SlotNumber
        id: Int64 4
      3: TypedSlot
        id: Int64 11
        typ: Int64 <: Signed
    typ: Int64 <: Signed
  3: Expr
    head: Symbol =
    args: Array{Any}((2,))
      1: SSAValue
        id: Int64 0
      2: Expr
        head: Symbol call
        args: Array{Any}((3,))
          1: GlobalRef
            mod: Module Core.Intrinsics
            name: Symbol box
          2: Int64 <: Signed
          3: Expr
            head: Symbol call
            args: Array{Any}((3,))
              1: GlobalRef
                mod: Module Core.Intrinsics
                name: Symbol add_int
              2: Int64 1
              3: Int64 1
            typ: Int64 <: Signed
        typ: Int64 <: Signed
    typ: Int64 <: Signed
  4: Expr
    head: Symbol call
    args: Array{Any}((4,))
      1: GlobalRef
        mod: Module Base
        name: Symbol unsafe_arrayset
      2: TypedSlot
        id: Int64 20
        typ: Array{Int64,1} <: DenseArray{Int64,1}
      3: SSAValue
        id: Int64 0
      4: TypedSlot
        id: Int64 11
        typ: Int64 <: Signed
    typ: Array{Int64,1} <: DenseArray{Int64,1}
  5: Expr
    head: Symbol parfor_end
    args: Array{Any}((1,))
      1: ParallelAccelerator.ParallelIR.PIRParForStartEnd
        loopNests: Array{ParallelAccelerator.ParallelIR.PIRLoopNest}((1,))
          1: ParallelAccelerator.ParallelIR.PIRLoopNest
            indexVariable: TypedSlot
              id: Int64 11
              typ: Int64 <: Signed
            lower: Int64 1
            upper: Int64 100
            step: Int64 1
        reductions: Array{ParallelAccelerator.ParallelIR.PIRReduction}((0,))
        instruction_count_expr: Void nothing
        private_vars: Array{Union{SSAValue,SlotNumber,TypedSlot}}((1,))
          1: SSAValue
            id: Int64 0
    typ: Int64 <: Signed

=#
