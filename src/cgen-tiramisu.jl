#
# A Julia to Tiramisu C++ Generator
# Transformation happens in two main steps:
#
#   - First, the Julia AST from ParallelAccelerator gets transformed
#     into a Tiramisu AST of TExpr's, TBuffer's, TComputation's, etc.
#
#   - Then, each Tiramisu AST node is passed into a tiramisu_translate
#     method to generate the proper Tiramisu C++ code.
#
# Optional passes could be added in the future in between the two steps
# to perform other optimizations (eliminate redundant buffers, evaluate
# gotos, etc)
#
# tiramisu_from_root_entry is the entry point where all the magic happens
#
# Originally Created By David Vargas
#

module Tiramisu

import CompilerTools.DebugMsg
using CompilerTools.LambdaHandling
using CompilerTools.Helper
DebugMsg.init()

using ..ParallelAccelerator

importall Base

export tiramisu_from_root_entry, tcanonicalize

primitives = DataType[Int8, UInt8,
                     Int16, UInt16,
                     Int32, UInt32,
                     Int64, UInt64,
                     Float16, Float32,
                     Float64, Bool,
                     Char, Void]

jToC = Dict(
            Int8    =>  "int8_t",
            UInt8   =>  "uint8_t",
            Int16   =>  "int16_t",
            UInt16  =>  "uint16_t",
            Int32   =>  "int32_t",
            UInt32  =>  "uint32_t",
            # Int64   =>  "int64_t",
            # UInt64  =>  "uint64_t",
            Int64   =>  "int32_t",
            UInt64  =>  "uint32_t",
            Float16 =>  "float",
            Float32 =>  "float",
            Float64 =>  "double",
            Bool    =>  "bool",
            Char    =>  "char",
            Void    =>  "void"
    ) #Map from Julia types to C++ types

jToT = Dict(
            Int8    =>  "p_int8",
            UInt8   =>  "p_uint8",
            Int16   =>  "p_int16",
            UInt16  =>  "p_uint16",
            Int32   =>  "p_int32",
            UInt32  =>  "p_uint32",
            Int64   =>  "p_int32",
            UInt64  =>  "p_uint32",
            Float16 =>  "p_float32",
            Float32 =>  "p_float32",
            Float64 =>  "p_float64",
            Bool    =>  "p_boolean"
    ) #Map from Julia types to Tiramisu types

jToH = Dict(
            Int8    =>  "Halide::Int(8)",
            UInt8   =>  "Halide::UInt(8)",
            Int16   =>  "Halide::Int(16)",
            UInt16  =>  "Halide::UInt(16)",
            Int32   =>  "Halide::Int(32)",
            UInt32  =>  "Halide::UInt(32)",
            Int64   =>  "Halide::Int(32)",
            UInt64  =>  "Halide::UInt(32)",
            Float16 =>  "Halide::Float(32)",
            Float32 =>  "Halide::Float(32)",
            Float64 =>  "Halide::Float(64)",
            Bool    =>  "Halide::Boolean()"
)

Primitive = Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64,
                  Float16, Float32, Float64, Bool}


# Get type used by arch (Int32 or Int64)
IntArch = typeof(1)

# Generated slot
immutable GenSlot
    id::IntArch
end

Variable = Union{SlotNumber, SSAValue, GenSlot}

# TODO : multiple computations per variable for SlotNumber

#TODO: Edit these constants to match the proper directory of all the following
const HALIDE_INCLUDE_DIR = "/home/malek/tiramisu/Halide/include"
const HALIDE_TOOLS_DIR = "/home/malek/tiramisu/Halide/tools"
const HALIDE_LIB_DIR = "/home/malek/tiramisu/Halide/lib"
const ISL_INCLUDE_DIR = "/home/malek/tiramisu/3rdParty/isl/build/include"
const ISL_LIB_DIR = "/home/malek/tiramisu/3rdParty/isl/build/lib"
const TIRAMISU_INCLUDE_DIR = "/home/malek/tiramisu/include"
const TIRAMISU_BUILD_DIR = "/home/malek/tiramisu/build"

#The following constants are the output files created
const FILENAME = "/home/malek/generated/thello.cpp"
const FILENAME_WRAPPER = "/home/malek/generated/thello_wrapper.cpp"
const GEN = "$TIRAMISU_BUILD_DIR/thello_generator"
const OBJ = "$TIRAMISU_BUILD_DIR/thello.o"
const OBJ_WRAPPER = "$TIRAMISU_BUILD_DIR/thello_wrapper.o"
const LIB = "$TIRAMISU_BUILD_DIR/thello.so"

#Globals
function_name = ""
computationIndex = 0
computationPrefix = "S"

function make_counter()
    counter = -1
    return function()
        counter += 1
        return counter
    end
end

type Constant
    id::Int
end

constant_counter = make_counter()
new_constant() = Constant(constant_counter())

new_computation_id = make_counter()

genslot_counter = make_counter()
new_genslot() = GenSlot(genslot_counter())

@enum TBufferArgument TBOutput TBInput TBTemporary
argNames = Dict(
    TBOutput => "a_output",
    TBInput => "a_input",
    TBTemporary => "a_temporary",
)
@enum TOpStyle TOpFuncCall TOpInfix

abstract TSig

type InputSig <: TSig
    var::Variable
    pos::Int
end

type InputSizeSig <: TSig
    var::Variable
    pos::Int
end

type OutputSig <: TSig
    var::Variable
end

signature = TSig[]

abstract TExpr

type TEmptyExpr <: TExpr
end

type TValue <: TExpr
    #expr e_p0 = expr((int32_t) SIZE0);
    name::String
    val
    typ::DataType
end

#The following types are found on lower levels of a Tiramisu AST to help translate,
#modify, and optimize the Tiramisu AST
type TOp <: TExpr
    name::String
    fcn::String
    style::TOpStyle
    args::Array{TExpr, 1}
    typ::DataType
end

#The following types are what should be found on the top level of a Tiramisu AST
type TBuffer
    #buffer buf0("buf0", 2, {tiramisu::expr(10),tiramisu::expr(10)}, p_uint8, NULL, a_output, &function0);
    name
    dimensions :: Array{TExpr,1}
    typ
    argument::TBufferArgument
    is_scalar::Bool
end

type TAlloc <: TExpr
    dims::Vector{TExpr}
    typ::DataType
end



type TConstant <: TExpr
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    id::Constant
    expr :: TExpr
    typ::DataType
end

indexNames = "ijklmnop"

type ISLCondition
    var::String
    op::String
    lim::String
end

type TAccess
    computation::Vector{TExpr}
    buffer::Vector{TExpr}
end

type TComputation
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    id::Integer
    condition::TExpr
    dim::Unsigned
    expr::TExpr
    typ::DataType
    access::TAccess
    vars::Vector{Variable}
    is_input::Bool
end



type TSignature
    args
    ret
end

abstract TVar

type JVar <: TVar # Julia Variable
    id::Variable
    typ::DataType
    buffer::Union{TBuffer, Void}
    computation::Union{TComputation, Void} # Might not be there already
end

copy(x::JVar) = JVar(x.id, x.typ, x.buffer, x.computation)

type DVar <: TVar # Dummy variable TODO remove
    id::GenSlot
end


abstract TIndex

type TConstIndex <: TIndex
    val::Integer
end

type TVarIndex <: TIndex
    letter::Unsigned
end

abstract TRead <: TExpr

type TLoopVarRead <: TRead
    var::Variable
end

type TSimpleRead <: TRead
    var::TVar
end

type TIndexedRead <: TRead
    var::JVar # TODO should this be a TVar?
    indices::Vector{TExpr}
end

vars = Dict{Variable, JVar}()
inputSizeGenvars = Vector{JVar}()
readGenvars = Vector{JVar}()
genvars = Vector{JVar}()

idToBufferName(id::SSAValue) = "BSSA" * string(id.id)
idToBufferName(id::SlotNumber) = "BSN" * string(id.id)
idToBufferName(id::GenSlot) = "BGS" * string(id.id)

macro typToBufferGen(types)
    return esc(Expr(:call,
                    :Dict,
                    (:($(typ) => function(id, typ)
                        return TBuffer(idToBufferName(id),
                                       [TValue("", 1, IntArch)],
                                       $(typ),
                                       TBTemporary,
                                       true)
                    end) for typ in eval(types))...))
end

typToTBuffer = @typToBufferGen primitives


function parseNode(node::Expr)
    return exprHandlers[node.head](node)
end

function parseNode(node::Variable)
    # TODO does this always work???
    if !haskey(vars, node)
        throw(UndefVarError(Symbol(node)))
    end
    # TODO: Indexing should not be done here
    if node in loopVars
        return TLoopVarRead(node)
    end
    return TSimpleRead(vars[node])
end

function parseNode(node::Primitive)
    return TValue("", node, typeof(node))
end

function parseNode(expr::Any)
    println("Unsupported: ", typeof(expr))
end

# parsing a computation should be idem potent
function parseComputation(expr::TComputation, typ::DataType)
    println("Parsing a computation, should I be?")
    assert(expr.typ === typ)
    return expr
end

function parseComputation(expr::TExpr, typ::DataType) # only scalar for now
    if typ <: Primitive
        if isempty(condStack)
            var = new_genslot()
            push!(loopVars, var)
            dummy(var, IntArch)
            cond = (wrap(var) == wrap(0))
            access = TAccess([wrap(var)], [wrap(0)])
            indices = Variable[var]
            pop!(loopVars)
        else
            cond = condStack[end]
            access = TAccess(map(wrap, loopVars), [wrap(0)])
            indices = collect(loopVars)
        end
        comp = TComputation(
            new_computation_id(),
            cond,
            length(indices),
            expr,
            typ,
            access,
            indices,
            false # TODO : fix
        )
        schedule(comp)
        return comp
    end
end

@enum SchedTime SchedNext SchedNow SchedNever

const RootDimension = -1

SchedLevel = Int

type DelayedSchedule
    current::SchedLevel
    min::SchedLevel
    time::SchedTime
end

type OnceSchedule
    level::SchedLevel
    time::SchedTime
end

type ScheduleState
    for_level::DelayedSchedule
    fuse_level::OnceSchedule
    previous_comp::Union{TComputation, Void}
    schedule::Vector{Pair{Pair{TComputation, TComputation}, SchedLevel}}
    parallelize_level::Union{Int, Void}
    parallelize::Vector{Pair{TComputation, Int}}
end

readSchedLevel(level::SchedLevel) = level
function readSchedLevel(level::DelayedSchedule)
    result, level.min = level.min, level.current
    return result
    #
    # if level.time == SchedNext
    #     level.time = SchedNow
    #     return level.prev_level
    # elseif level.time == SchedNow
    #     return level.level
    # else
    #     # SchedNever
    #     return RootDimension
    # end
end
function readSchedLevel(level::OnceSchedule)
    if level.time == SchedNext
        level.time = SchedNow
        return RootDimension
    elseif level.time == SchedNow
        level.time = SchedNever
        return level.level
    else
        # SchedNever
        return RootDimension
    end
end


sched = ScheduleState(DelayedSchedule(RootDimension, RootDimension, SchedNever),
                      OnceSchedule(RootDimension, SchedNever),
                      nothing, [], nothing, [])

function schedule(comp::TComputation)
    level = maximum(map(readSchedLevel, [sched.for_level, sched.fuse_level]))
    if sched.previous_comp != nothing
        # Not the first computation
        push!(sched.schedule, (sched.previous_comp => comp) => level)
    end
    sched.previous_comp = comp
    if sched.parallelize_level != nothing
        push!(sched.parallelize, comp => sched.parallelize_level)
        sched.parallelize_level = nothing
    end
end

function typedOp!(op::TOp, typ::DataType)
    op.typ = typ
    return op
end

function opToISL(op::String)
    if op == "=="
        return "="
    elseif op == "&&"
        return "and"
    else
        return op
    end
end



+(args::Vararg{TExpr}) = TOp("", "+", TOpInfix, collect(args), Void)
*(args::Vararg{TExpr}) = TOp("", "*", TOpInfix, collect(args), Void)
-(args::Vararg{TExpr}) = TOp("", "-", TOpInfix, collect(args), Void)
/(args::Vararg{TExpr}) = TOp("", "/", TOpInfix, collect(args), Void)
==(args::Vararg{TExpr}) = TOp("", "==", TOpInfix, collect(args), Bool)
<=(args::Vararg{TExpr}) = TOp("", "<=", TOpInfix, collect(args), Bool)
and(args::Vararg{TExpr}) = TOp("", "&&", TOpInfix, collect(args), Bool)

function dummy(id::Variable, typ::DataType)
    return vars[id] = JVar(
        id,
        typ,
        nothing,
        nothing
    )
end

wrap(val::TExpr) = val

wrap(val::Union{Primitive, Variable}) = parseNode(val)

function wrap(val::TVar)
    if val.id in loopVars
        return TLoopVarRead(val.id)
    end
    return TSimpleRead(val)
end

function wrap(val::Any)
    # Shouldn't have nested calls
    print_with_color(:yellow, "Trying to wrap:\n")
    dump(val)
    return val
end

abstract Clause

immutable ForLoop <: Clause
    id::Int
    from::Union{Int, SlotNumber}
    to::Union{Int, SlotNumber}
    inc::Int
    var::TVar
end

clauseStack = Vector{Clause}()
condStack = Vector{TExpr}()
loopVars = Vector{Variable}()


callHandlers = Dict(
    (Core.Intrinsics, :add_int) => function(args::Vector, typ::DataType)
        return typedOp!(+(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :sub_int) => function(args::Vector, typ::DataType)
        return typedOp!(-(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :add_float) => function(args::Vector, typ::DataType)
        return typedOp!(+(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :mul_float) => function(args::Vector, typ::DataType)
        return typedOp!(*(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :div_float) => function(args::Vector, typ::DataType)
        return typedOp!(/(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :sub_float) => function(args::Vector, typ::DataType)
        return typedOp!(-(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :box) => function(args::Vector, typ::DataType)
        return parseNode(args[2])
    end,
    (Core.Intrinsics, :checked_trunc_sint) => function(args::Vector, typ::DataType)
        res = args[1](args[2]) # throw an error if invalid
        return parseNode(res)
    end,
    (ParallelAccelerator.TiramisuPrepass, :for_loop_start) => function(args::Vector, typ::DataType)
        id = args[1]
        from = args[2]
        to = args[3]
        inc = args[4]
        varid = args[6]
        var = dummy(varid, IntArch)
        push!(clauseStack, ForLoop(id, from, to, inc, var))
        push!(loopVars, varid)
        cond = <=(wrap(from) - wrap(1), wrap(var), wrap(to) - wrap(1))
        if isempty(condStack)
            push!(condStack, cond)
        else
            push!(condStack, and(condStack[end], cond))
        end
    #    sched.for_level = DelayedSchedule(length(loopVars) - 1, sched.for_level.prev_level, SchedNext)
        sched.for_level = DelayedSchedule(length(loopVars) - 1, sched.for_level.min, SchedNext)
    end,
    (ParallelAccelerator.TiramisuPrepass, :for_loop_end) => function(args::Vector, typ::DataType)
        id = args[1]
        assert(id == clauseStack[end].id)
        pop!(clauseStack)
        pop!(loopVars)
        pop!(condStack)
        # if isempty(loopVars)
        #     # sched.for_level = DelayedSchedule(RootDimension, RootDimension, SchedNever)
        #     sched.for_level = DelayedSchedule(RootDimension, RootDimension, SchedNever)
        # else
        #     sched.for_level = DelayedSchedule(length(loopVars) - 1, min(sched.for_level.prev_level, length(loopVars) - 1), SchedNext)
        # end
        current = length(loopVars) - 1
        sched.for_level = DelayedSchedule(current, min(current, sched.for_level.min), SchedNext)
    end,
    (ParallelAccelerator.API, :setindex!) => function(args::Vector, typ::DataType)
        arr = copy(vars[args[1]])
        texpr = parseNode(args[2])
        # TODO handle temporal
        # indices = Variable[]
        # added_vars = Variable[]
        # cond = condStack[end]
        # for arg in args[3:end]
        #     if isa(arg, Primitive)
        #         varid = new_genslot()
        #         var = dummy(varid, Int)
        #         cond = and(cond, wrap(var) == wrap(arg - 1))
        #         push!(indices, varid)
        #         push!(added_vars, varid)
        #         push!(loopVars, varid)
        #     else
        #         push!(indices, arg)
        #     end
        # end
        indices = collect(Variable, args[3:end])
        assert(issubset(indices, loopVars)) # TODO more thourough checking
        access = TAccess(map(wrap, loopVars), [wrap(i) for i in indices])
        comp = TComputation(
            new_computation_id(),
            # cond,
            condStack[end],
            length(loopVars),
            texpr,
            arr.typ,
            access,
            copy(loopVars),
            false # TODO fix
        )
        # for var in added_vars
        #     pop!(loopVars)
        # end
        schedule(comp)
        arr.computation = comp
        push!(used_buffers, arr.buffer.name)
        push!(genvars, arr)
    end,
    (ParallelAccelerator.API, :getindex) => function(args::Vector, typ::DataType)
        arr = vars[args[1]]
        indices = collect(Variable, args[2:end])
        assert(issubset(indices, loopVars)) # TODO more thourough checking
        return TIndexedRead(arr, map(wrap, indices))
    end,
)

function handleReturn(arg::Void, typ::DataType)
end

function handleReturn(arg::Variable, typ::DataType)
    vars[arg].buffer.argument = TBOutput
    push!(signature, OutputSig(arg))
end

function handleReturn(arg, typ::DataType)
    id = new_genslot()
    buffer = typToTBuffer[typ](id, typ)
    buffer.argument = TBOutput
    comp = parseComputation(parseNode(arg), typ)
    vars[id] = JVar(id, typ, buffer, comp)
    push!(genvars, vars[id])
    push!(signature, OutputSig(id))
end

function createEmptyComputation(typ, dims, is_scalar)
    l = length(loopVars)
    indices = Variable[new_genslot() for _ in dims]
    append!(loopVars, indices)
    map(i -> dummy(i, Int), indices)
    if is_scalar
        access = TAccess(map(wrap, indices), [wrap(0) for i in indices])
    else
        access = TAccess(map(wrap, indices), [wrap(i) for i in indices])
    end
    cond = and((<=(wrap(0), wrap(i), wrap(c) - wrap(1))
                for (i, c) in zip(indices, dims))...)
    resize!(loopVars, l)
    return TComputation(
        new_computation_id(),
        cond,
        length(dims),
        TEmptyExpr(),
        typ,
        access,
        indices,
        true,
    )
end

used_buffers = Set{String}()

function createVar(id, typ, rhs)
    parsed_rhs = parseNode(rhs)
    if isa(parsed_rhs, TAlloc)
        vars[id] = JVar(
            id,
            parsed_rhs.typ,
            TBuffer(idToBufferName(id),
                    parsed_rhs.dims,
                    parsed_rhs.typ,
                    TBTemporary,
                    false),
            createEmptyComputation(parsed_rhs.typ, parsed_rhs.dims, false),
        )
        push!(readGenvars, vars[id])
        push!(used_buffers, idToBufferName(id))
    else
        if !haskey(vars, id) # in case of reductions
            vars[id] = JVar(
                id,
                typ,
                typToTBuffer[typ](id, typ),
                createEmptyComputation(typ, TExpr[wrap(1)], true),
            )
            push!(readGenvars, vars[id])
        end
        actualComputation = parseComputation(parsed_rhs, typ)
        newVar = copy(vars[id])
        newVar.computation = actualComputation
        push!(genvars, newVar)
    end
end

function make_const(e::TExpr,
                    typ::DataType)
    C = TConstant(
        new_constant(),
        e,
        typ,
    )
    return C
end


function createInputSizeVar(n::Int, idx)
    indices = Variable[new_genslot()]
    id = new_genslot()
    map(i -> dummy(i, Int), indices)
    push!(loopVars, indices[1])
    access = TAccess(map(wrap, indices), [wrap(indices[1])])
    cond = <=(wrap(0), wrap(indices[1]), wrap(n) - wrap(1))
    b = TBuffer(idToBufferName(id),
                TExpr[wrap(n)],
                Int,
                TBInput,
                false)
    comp = TComputation(
        new_computation_id(),
        cond,
        n,
        TEmptyExpr(),
        Int,
        access,
        indices,
        true,
    )
    pop!(loopVars)

    vars[id] = JVar(
        id,
        Int,
        b,
        comp,
    )
    push!(inputSizeGenvars, vars[id])
    push!(signature, InputSizeSig(id, idx))

    bounds = TExpr[]

    for i in 0:(n-1)
        push!(bounds, TIndexedRead(vars[id], TExpr[wrap(i)]))
    end

    return bounds
end

function createVarInput(id, typ, idx)
    element_type = eltype(typ)
    n = ndims(typ)
    if n == 0
        indices = Variable[new_genslot()]
        push!(loopVars, indices[1])
        map(i -> dummy(i, Int), indices)
        access = TAccess(map(wrap, indices), [wrap(0)])
        b = typToTBuffer[typ](id, typ)
        b.argument = TBInput
        cond = wrap(indices[1]) == wrap(0)
        n = 1
        pop!(loopVars)
    else
        bounds = createInputSizeVar(n, idx)
        l = length(loopVars)
        indices = Variable[new_genslot() for _ in 1:n]
        append!(loopVars, indices)
        map(i -> dummy(i, Int), indices)
        access = TAccess(map(wrap, indices), [wrap(i) for i in indices])
        b = TBuffer(idToBufferName(id),
                    bounds,
                    element_type,
                    TBInput,
                    false)
        cond = and((<=(wrap(0), wrap(i), wrap(c) - wrap(1))
                    for (i, c) in zip(indices, bounds))...)
        resize!(loopVars, l)
    end
    comp = TComputation(
        new_computation_id(),
        cond,
        n,
        TEmptyExpr(),
        element_type,
        access,
        indices,
        true,
    )

    vars[id] = JVar(
        id,
        eltype(typ),
        b,
        comp,
    )

    push!(readGenvars, vars[id])
    push!(signature, InputSig(id, idx))
end

metaHandlers = Dict(
    :parallel => function(args)
        sched.parallelize_level = length(loopVars)
    end,

    :endparallel => function(args)
        sched.parallelize_level = nothing
    end,

    :fuse => function(args)
        # do nothing
    end,

    :endfuse => function(args)
        sched.fuse_level = OnceSchedule(Unsigned(args[1]), SchedNow)
    end,

)

exprHandlers = Dict(
    # In case of call, lookup the function in the list of supported functions
    :call => function(expr)
        called_function_id = (expr.args[1].mod, expr.args[1].name)
        return get(callHandlers,
                   called_function_id,
                   function(args) end)(expr.args[2:end], expr.typ)
    end,

    # In case of assignment, create buffer, computation
    :(=) => function(expr)
        id = expr.args[1]
        typ = expr.typ
        rhs = expr.args[2]

        if isa(rhs, Variable)
            # TODO: does this break things?
            vars[id] = vars[rhs]
        else
            createVar(id, typ, rhs)
        end
    end,

    :alloc => function(expr)
        return TAlloc([parseNode(n) for n in expr.args[2]], expr.args[1])
    end,

    :meta => function(expr)
        metaHandlers[expr.args[1]](expr.args[2:end])
    end,

    # For return, set buffer to output
    :return => function(expr)
        assert(length(expr.args) == 1) # TODO support tuples
        handleReturn(expr.args[1], expr.typ)
    end,
)

#IMPORTANT: This is the main entry point for Julia to Tiramisu Translation
#This functions performs a number of key steps:
#   -First it gets the header string which includes imports and the namespace
#   -Second it sets the name of the function, which declares the method signature,
#    setting Tiramisu default options, and declaring the function to be generated.
#   -Third it translates the body of the program using the steps outlined in the
#    beginning. This is where the majority of the work takes place
#   -Next it writes the combination of this string to a file
#   -Then the file is compiled, run, and converted to a library file
#   -Finally, the library file name is returned to be used by Julia
function tiramisu_from_root_entry(body, linfo, functionName::AbstractString)

    #Form Tiramisu String
    thdr = tiramisu_from_header(linfo)
    twrap = tiramisu_set_function(functionName)
    tbod = tiramisu_analyze_body(body, linfo)
    tc = thdr * twrap* tbod * "}\n"

    tcw = generate_wrapper()


    println(functionName)

    #Write to File
    fil = open(FILENAME,"w")
    write(fil, tc)
    close(fil)
    #println("Wrote to file")

    #Write to File
    fil = open(FILENAME_WRAPPER,"w")
    write(fil, tcw)
    close(fil)
    #println("Wrote to file")

    #Compile the Function Generator, Generate the function and link
    run(tiramisu_get_command())
    #println("Compiled Tiramisu File")
    run(tiramisu_get_gen())
    #println("Generated Function")
    run(tiramisu_get_lib_obj())
    run(tiramisu_get_lib())
    #println("Converted to .so file")
    TIRAMISU_BUILD_DIR, LIB, get_return()
end



# Converts a buffer to the corresponding type to pass to the C function
function sig_to_ccall_type(sig)
    # need to construct it in this obscure way to be equivalent to
    # the symbol equivalent of writing
    return :(Ptr{$(vars[sig.var].buffer.typ)})
end

function buffer_to_ccall_container(buffer)
    return Array(buffer.typ, [dim.val for dim in buffer.dimensions]...)
end

# TODO: handle Tuple return
function decouple_scalar(containers)
    return containers[1][1]
end

# TODO: handle Tuple return
function decouple_buffer(containers)
    return containers[1]
end

function transform_dim(d::TValue, interface, args)
    return d.val
end

function transform_dim(d::TSimpleRead, interface, args)
    sig = [s for s in interface if d.var.id == s.var][1]
    return args[sig.pos]
end

function transform_generator()
    interface = copy(signature)
    n = length([1 for s in interface if isa(s, InputSig)])
    outputs = Dict(s => vars[s.var] for s in interface if isa(s, OutputSig))
    function transform(args)
        assert(length(args) == n)
        transformed = []
        for s in interface
            if isa(s, InputSizeSig)
                push!(transformed, collect(Int32, size(args[s.pos])))
            elseif isa(s, InputSig)
                arg = args[s.pos]
                if ndims(arg) == 0
                    T = eltype(arg)
                    arg = T[arg]
                end
                push!(transformed, arg)
            else
                dims = (transform_dim(d, interface, args) for d in outputs[s].buffer.dimensions)
                push!(transformed, Array{outputs[s].buffer.typ}(dims...))
            end
        end
        return transformed
    end
end

function return_parser()
    interface = copy(signature)
    returns = []
    is_scalar = Dict(s => vars[s.var].buffer.is_scalar
                     for s in interface if isa(s, OutputSig))
    function get_return(args)
        dump(args)
        for (s, arg) in zip(interface, args)
            if isa(s, OutputSig)
                if is_scalar[s]
                    return arg[1]
                else
                    return arg
                end
            end
        end
    end
end

# returns a tuple of:
# - A tuple of types; (Ptr{Int32},)...
# - The corresponding containers in a list
function get_return()
    return (Expr(:tuple, map(sig_to_ccall_type, signature)...),
            transform_generator(),
            return_parser())
end

#Imports for Tiramisu file
function tiramisu_from_header(linfo)
    s = string("#include <isl/set.h>\n",
        "#include <isl/union_map.h>\n",
        "#include <isl/union_set.h>\n",
        "#include <isl/ast_build.h>\n",
        "#include <isl/schedule.h>\n",
        "#include <isl/schedule_node.h>\n\n",

        "#include <$(TIRAMISU_INCLUDE_DIR)/tiramisu/debug.h>\n",
        "#include <$(TIRAMISU_INCLUDE_DIR)/tiramisu/core.h>\n\n",

        "#include <stdint.h>\n",
        "#include <float.h>\n",
        "#include <limits.h>\n",
        "#include <complex>\n",
        "#include <math.h>\n",
        "#include <stdio.h>\n",
        "#include <iostream>\n",
        "#include <sstream>\n",
        "#include <vector>\n",
        "#include <string>\n",
        "#include <Halide.h>\n\n",
        "using namespace tiramisu;\n\n")
    params = [vdef for vdef in linfo.var_defs
                   if vdef.desc == 0 && vdef.name != Symbol("#self#")]
    for (i, def) in enumerate(params)
        createVarInput(SlotNumber(def.id), def.typ, i)
    end
    return s
end

#Method Signature for Tiramisu File
function tiramisu_set_function(f)
    global function_name
    global computationPrefix
    function_name = f
    computationPrefix = f*"_S"
    s = string("int main()\n{\n",
                "\tglobal::set_default_tiramisu_options();\n",
                "\tglobal::set_loop_iterator_default_data_type(tiramisu::p_int32);\n",
                "\tfunction $function_name(\"$function_name\");\n",
                "\ttiramisu::var ",
                join(("$v(\"$v\")" for v in indexNames), ", "),
                ";\n")
end

getIndex(index::TConstIndex) = string(index.val)

getIndex(index::TVarIndex) = indexNames[index.letter]

getIndices(indices::Vector{TIndex}) = join(map(getIndex, indices), ", ")

VarToString = Dict{Variable, String}

createTExpr(nthg::TEmptyExpr, varMap::VarToString) = "expr()"

createTExpr(c::TConstant, varMap::VarToString) = c.name

createTExpr(val::TValue, varMap::VarToString) = "expr(($(jToC[val.typ])) $(val.val))"
createTExpr(val::TValue, varMap::VarToString, _, __) = "expr(($(jToC[val.typ])) $(val.val))"
function createTExpr(var::TSimpleRead, varMap::VarToString)
    return "S$(var.var.computation.id)(0)"
end

function createTExpr(var::TSimpleRead, varMap::VarToString, constSet, used)
    pair = Pair{Variable, Int}(var.var.id, 0)
    name = "C$(constSet[pair].id.id)"
    push!(used, constSet[pair])
    return name
end

function createTExpr(op::TOp, varMap::VarToString)
    if op.style == TOpInfix
        return "(" * join((createTExpr(a, varMap) for a in op.args), " $(op.fcn) ") * ")"
    end
    return op.fcn * "(" * join((createTExpr(a, varMap) for a in op.args), ", ") * ")"
end

function createTExpr(op::TOp, varMap::VarToString, constSet, used)
    if op.style == TOpInfix
        return "(" * join((createTExpr(a, varMap, constSet, used) for a in op.args), " $(op.fcn) ") * ")"
    end
    return op.fcn * "(" * join((createTExpr(a, varMap, constSet, used) for a in op.args), ", ") * ")"
end

function createTExpr(var::TLoopVarRead, varMap::VarToString)
    return varMap[var.var]
end

function createTExpr(var::TIndexedRead, varMap::VarToString)
    # TODO why shouldn't this be reversed
    return ("S$(var.var.computation.id)(" *
            join((createTExpr(v, varMap) for v in var.indices), ", ") *
            ")")
end

function createTExpr(var::TIndexedRead, varMap::VarToString, constSet, used)
    assert(length(var.indices) == 1 && isa(var.indices[1], TValue))
    pair = Pair{Variable, Int}(var.var.id, var.indices[1].val)
    name = "C$(constSet[pair].id.id)"
    push!(used, constSet[pair])
    return name
end

createISLCond(val::TValue, varMap::VarToString, _, __, ___) = string(val.val)

function createISLCond(var::TLoopVarRead, varMap::VarToString, _, __, ___)
    return varMap[var.var]
end

function createISLCond(var::TSimpleRead, varMap::VarToString, constSet, createConst, usedConsts)
    pair = Pair{Variable, Int}(var.var.id, 0)
    if !haskey(constSet, pair)
        constSet[pair] = make_const(var, var.var.typ)
        push!(createConst, constSet[pair])
    end
    name = "C$(constSet[pair].id.id)"
    push!(usedConsts, name)
    return name
end


function createISLCond(var::TIndexedRead, varMap::VarToString, constSet, createConst, usedConsts)
    assert(length(var.indices) == 1 && isa(var.indices[1], TValue))
    pair = Pair{Variable, Int}(var.var.id, var.indices[1].val)
    if !haskey(constSet, pair)
        constSet[pair] = make_const(var, var.var.typ)
        push!(createConst, constSet[pair])
    end
    name = "C$(constSet[pair].id.id)"
    push!(usedConsts, name)
    return name
end

function createISLCond(op::TOp, varMap::VarToString,
                       constSet, createConst, usedConsts)
    createCond = arg -> createISLCond(arg, varMap, constSet, createConst, usedConsts)
    if op.style == TOpInfix
        return "(" * join(map(createCond, op.args), " $(opToISL(op.fcn)) ") * ")"
    end
    return opToISL(op.fcn) * "(" * join(map(createCond, op.args), ", ") * ")"
end

function getIndices(comp::TComputation)
    return join(indexNames[1:length(comp.vars)], ", ")
end

function createISL(comp::TComputation, varMap::VarToString, constSet, createConst)
    for (var, index) in zip(comp.vars, indexNames)
        varMap[var] = string(index)
    end
    usedConsts = Set{String}()
    # dump(comp)
    # dump(comp.vars)
    cond = createISLCond(comp.condition, varMap, constSet, createConst, usedConsts)
    usedConsts = join(usedConsts, ", ")
    indices = getIndices(comp)
    return "[$(usedConsts)] -> {S$(comp.id)[$indices] : $cond}"
end

function createAccess(access::Vector{TExpr}, varMap, constSet, createConst)
    return join((createISLCond(expr, varMap, constSet, createConst, Set{String}()) for expr in access), ",")
end

function createAccess(comp::TComputation, varMap::VarToString, constSet, createConst)
    for (var, index) in zip(comp.vars, indexNames)
        varMap[var] = string(index)
    end
    return (createAccess(comp.access.computation, varMap, constSet, createConst),
            createAccess(reverse(comp.access.buffer), varMap, constSet, createConst))
end

createSchedLevel(level::SchedLevel) = ((level == RootDimension) ?
                                       "tiramisu::computation::root" :
                                       indexNames[level + 1]) # Julia is 1 indexed

function get_arg_type(buffer)
    if buffer.name in used_buffers && buffer.argument == TBInput
        return argNames[TBOutput]
    end
    return argNames[buffer.argument]
end

function tiramisu_analyze_body(ast::Expr, linfo)
    # dump(ast.args)
    # throw(InterruptException())
    for expr in ast.args
        dump(expr)
        parseNode(expr)
    end
    uvars = unique(genvars)
    # computation pass
    lines = Vector{String}()
    constSet = Dict{Pair{Variable, Int}, TConstant}()

    println("Input Computations")
    for v in inputSizeGenvars
        computation = v.computation
        varMap = VarToString()
        createConst = TConstant[]
        push!(lines,
              "\ttiramisu::computation S$(computation.id)(" *
              "\"" * createISL(computation, varMap, constSet, createConst) * "\", " *
              createTExpr(computation.expr, varMap) * ", " *
              string(!computation.is_input) * ", " *
              jToT[computation.typ] * ", " *
              "&$function_name);")
    end
    # for c in consts
    #     varMap = VarToString()
    #     push!(lines,
    #           "\ttiramisu::constant $(c.name)(" *
    #           "\"$(c.name)\", " *
    #           createTExpr(c.expr, varMap) * ", " *
    #           jToT[c.typ] * ", " *
    #           "true, " *
    #           "nullptr, " *
    #           "0, " *
    #           "&$function_name);")
    # end
    println("Read Computation")
    computations = sort([var.computation for var in readGenvars], by=(c -> c.id))
    for computation in computations
        varMap = VarToString()
        createConst = TConstant[]
        # dump(computation)
        result = (
              "\ttiramisu::computation S$(computation.id)(" *
              "\"" * createISL(computation, varMap, constSet, createConst) * "\", " *
              createTExpr(computation.expr, varMap) * ", " *
              "false, " *
              jToT[computation.typ] * ", " *
              "&$function_name);")
        for c in createConst
            varMap = VarToString()
            push!(lines,
                  "\ttiramisu::constant C$(c.id.id)(" *
                  "\"C$(c.id.id)\", " *
                  createTExpr(c.expr, varMap) * ", " *
                  jToT[c.typ] * ", " *
                  "true, " *
                  "nullptr, " *
                  "0, " *
                  "&$function_name);")
        end
        push!(lines, result)
    end
    println("Computation")
    computations = sort([var.computation for var in uvars], by=(c -> c.id))
    for computation in computations
        # dump(computation)
        varMap = VarToString()
        createConst = TConstant[]
        result = (
              "\ttiramisu::computation S$(computation.id)(" *
              "\"" * createISL(computation, varMap, constSet, createConst) * "\", " *
              createTExpr(computation.expr, varMap) * ", " *
              string(!computation.is_input) * ", " *
              jToT[computation.typ] * ", " *
              "&$function_name);")
        for c in createConst
            varMap = VarToString()
            push!(lines,
                  "\ttiramisu::constant C$(c.id.id)(" *
                  "\"C$(c.id.id)\", " *
                  createTExpr(c.expr, varMap) * ", " *
                  jToT[c.typ] * ", " *
                  "true, " *
                  "nullptr, " *
                  "0, " *
                  "&$function_name);")
        end
        push!(lines, result)
    end
    println("Buffers")
    buffers_to_allocate = TBuffer[]
    buffers_to_allocate_lets = Set[]
    for var in vcat(inputSizeGenvars, readGenvars)
        varMap = VarToString()
        createConst = TConstant[]
        cIndices, bIndices = createAccess(var.computation, varMap, constSet, createConst)
        used = Set()
        push!(lines,
              "\ttiramisu::buffer $(var.buffer.name)(" *
              "\"$(var.buffer.name)\", " *
              "{" * join((createTExpr(d, varMap, constSet, used) for d in var.buffer.dimensions), ", ") * "}, " *
              jToT[var.buffer.typ] * ", " *
              get_arg_type(var.buffer) * ", " *
              "&$function_name);")
        push!(lines,
              "\tS$(var.computation.id).set_access(\"" *
              "{S$(var.computation.id)[$cIndices] -> $(var.buffer.name)[$bIndices]}" *
              "\");")
        if !isempty(used) && var.buffer.argument == TBTemporary
            push!(buffers_to_allocate, var.buffer)
            push!(buffers_to_allocate_lets, used)
        end
    end
    for var in uvars
        varMap = VarToString()
        createConst = TConstant[]
        cIndices, bIndices = createAccess(var.computation, varMap, constSet, createConst)
        push!(lines,
              "\tS$(var.computation.id).set_access(\"" *
              "{S$(var.computation.id)[$cIndices] -> $(var.buffer.name)[$bIndices]}" *
              "\");")
    end
    # Scheduling
    println("Scheduling")
    # const_sched_list = collect(values(constSet))
    # for (i, (b, cs)) in enumerate(zip(buffers_to_allocate, buffers_to_allocate_lets))
    #
    #     if i == 1
    #         push!(lines, "\ttiramisu::computation* _allocate_$(b.name) = $(b.name).allocate_at(S$(sched.schedule[1].first.first.id), tiramisu::computation::root_dimension);");
    #     else
    #         push!(lines, "\ttiramisu::computation* _allocate_$(b.name) = $(b.name).allocate_at(*_allocate_$(buffers_to_allocate[i - 1].name), tiramisu::computation::root_dimension);");
    #     end
    #     varMap = VarToString()
    #     for c in cs
    #         push!(lines, "\t_allocate_$(b.name)->add_associated_let_stmt(\"C$(c.id.id)\", $(createTExpr(c.expr, varMap)));");
    #     end
    # end
    # # push!(const_sched_list, sched.schedule[1].first.first)
    # reverse!(buffers_to_allocate)
    # for i in 1:(length(const_sched_list) - 1)
    #     (c1, c2) = (const_sched_list[i], const_sched_list[i + 1])
    #     push!(lines, "\tC$(c2.id.id).after(C$(c1.id.id), tiramisu::computation::root);")
    # end
    # if !isempty(buffers_to_allocate)
    #     cnst = const_sched_list[end]
    #     b = buffers_to_allocate[1]
    #     push!(lines, "\t_allocate_$(b.name)->after(C$(cnst.id.id), tiramisu::computation::root);")
    #     for i in 1:(length(buffers_to_allocate) - 1)
    #         (b1, b2) = (buffers_to_allocate[i], buffers_to_allocate[i + 1])
    #         push!(lines, "\t_allocate_$(b2.name)->after(*_allocate_$(b1.name), tiramisu::computation::root);")
    #     end
    #     if !isempty(sched.schedule)
    #         b = buffers_to_allocate[end]
    #         comp = sched.schedule[1].first.first
    #         push!(lines, "\tS$(comp.id).after(*_allocate_$(b.name), tiramisu::computation::root);")
    #     end
    # elseif !isempty(const_sched_list) && !isempty(sched.schedule)
    #     cnst = const_sched_list[end]
    #     comp = sched.schedule[1].first.first
    #     push!(lines, "\tS$(comp.id).after(C$(cnst.id.id), tiramisu::computation::root);")
    # end
    for ((c1, c2), level) in sched.schedule
        # level = Int(c2.dim) - level - 1
        push!(lines, "\tS$(c2.id).after(S$(c1.id), $(createSchedLevel(level)));")
    end

    for (c, level) in sched.parallelize
        # level = Int(c.dim) - level - 1
        push!(lines, "\tS$(c.id).tag_parallel_level($(createSchedLevel(level)));")
    end



    println("Interface")
    interface = [vars[s.var].buffer.name for s in signature]
    push!(lines, "\t$function_name.set_arguments({" *
                  join(map(s -> "&" * s, interface), ", ")
                  *"});")
    push!(lines, "\t$function_name.gen_time_space_domain();")
    push!(lines, "\t$function_name.gen_isl_ast();")
    push!(lines, "\t$function_name.gen_halide_stmt();")
    push!(lines, "\t$function_name.gen_halide_obj(\"$OBJ\");")
    push!(lines, "\treturn 0;")
    return join(lines, '\n')
end

function buffer_to_arg_prototype(buffer)
    return (jToC[buffer.typ] *
            # repeat("*", length(buffer.dimensions)) *
            "* $(buffer.name)")
end

getdim(dim::TValue) = dim.val
getdim(dim::TSimpleRead) = "$(dim.var.buffer.name)[0]"
function getdim(dim::TIndexedRead)
    name = dim.var.buffer.name
    indices = join(("$(i.val)" for i in dim.indices), ", ")
    return "$name[$indices]"
end

function buffer_to_halide_buffer(buffer)
    lines = String[]
    push!(lines,
          "auto *shape_$(buffer.name) = new halide_dimension_t[$(length(buffer.dimensions))];")
    for (i, dim) in enumerate(reverse(buffer.dimensions))
        push!(lines, "shape_$(buffer.name)[$(i - 1)].min = 0;")
        push!(lines, "shape_$(buffer.name)[$(i - 1)].extent = $(getdim(dim));") # TODO not always TValue
        push!(lines, "shape_$(buffer.name)[$(i - 1)].stride = 1;")
    end
    push!(lines, "Halide::Buffer<> " *
                 "$(buffer.name)_halide($(jToH[buffer.typ]), $(buffer.name), " *
                 "$(length(buffer.dimensions)), shape_$(buffer.name));")
    return join(lines, "\n")
end

function buffer_to_raw_buffer(buffer)
    return "$(buffer.name)_halide.raw_buffer()"
end

function generate_wrapper()
    # computation pass
    lines = Vector{String}()
    push!(lines, "#include \"Halide.h\"")
    push!(lines, "#include <tiramisu/utils.h>")
    push!(lines, "#include <cstdlib>")
    push!(lines, "")
    push!(lines, "int main(int, char**) {")
    push!(lines, "return 0;")
    push!(lines, "}")
    interface = [vars[s.var].buffer for s in signature]
    push!(lines, "extern \"C\" int $function_name(" * join(("halide_buffer_t *" for _ in interface), ", ") * ");")
    push!(lines, "#pragma GCC visibility push(default)")
    push!(lines, "extern \"C\" void $(function_name)_wrapper(" *
                  join(map(buffer_to_arg_prototype, interface), ", ") *
                  ") {")
    push!(lines, "#pragma GCC visibility pop")
    for buffer in interface
        push!(lines, buffer_to_halide_buffer(buffer))
    end
    push!(lines, "$function_name(" *
                  join(map(buffer_to_raw_buffer, interface), ", ") *
                  ");")
    push!(lines, "}")
    return join(lines, '\n')
end



#Returns the command to compile the written Tiramisu file
function tiramisu_get_command()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"$FILENAME","-o","$GEN","-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$TIRAMISU_BUILD_DIR", "-ltiramisu")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    Cmd(s)
end

#Returns the command to run the function generator that was compiled by the Tiramisu file
function tiramisu_get_gen()
    env = copy(ENV)
    if haskey(env, "LD_LIBRARY_PATH") && env["LD_LIBRARY_PATH"] != ""
        env["LD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    else
        env["LD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    end
    if haskey(env, "DYLD_LIBRARY_PATH") && env["DYLD_LIBRARY_PATH"] != ""
        env["DYLD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    else
        env["DYLD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    end
    s = Cmd(["$GEN"])
    setenv(s,env)
end

#Returns the command to convert the generated function from an object file to a
#dynamic shared library file. This is necessary because Julia could only use c++
#code stored in libraries as opposed to object files
function tiramisu_get_lib_obj()
    s = ["g++","-c","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L$TIRAMISU_BUILD_DIR", "-ltiramisu")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    push!(s,"$FILENAME_WRAPPER")
    push!(s,"-shared","-fPIC","-o","$OBJ_WRAPPER")
    Cmd(s)
end

#Returns the command to convert the generated function from an object file to a
#dynamic shared library file. This is necessary because Julia could only use c++
#code stored in libraries as opposed to object files
function tiramisu_get_lib()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,OBJ)
    push!(s,OBJ_WRAPPER)
    push!(s,"-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    push!(s,"-shared","-fPIC","-o","$LIB")
    Cmd(s)
end

#Canonicalizing method used to remove #'s and other undesirable chars from function name
function tcanonicalize(tok)
    replacedTokens = Set("#")
    scrubbedTokens = Set(",.({}):")
    tokenXlate = Dict(
        '*' => "star",
        '/' => "slash",
        '-' => "minus",
        '!' => "bang",
        '.' => "dot",
        '^' => "hat",
        '|' => "bar",
        '&' => "amp",
        '=' => "eq",
        '\\' => "backslash"
    )
    s = string(tok)
    s = replace(s, scrubbedTokens, "")
    s = replace(s, replacedTokens, "t")
    s = replace(s, "∇", "del")
    for (k,v) in tokenXlate
       s = replace(s, k, v)
    end
    s = replace(s, r"[^a-zA-Z0-9]", "_")
    s
end

end
