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

jToC = Dict(
            Int8    =>  "int8_t",
            UInt8   =>  "uint8_t",
            Int16   =>  "int16_t",
            UInt16  =>  "uint16_t",
            Int32   =>  "int32_t",
            UInt32  =>  "uint32_t",
            Int64   =>  "int64_t",
            UInt64  =>  "uint64_t",
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
            Int64   =>  "p_int64",
            UInt64  =>  "p_uint64",
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
            Int64   =>  "Halide::Int(64)",
            UInt64  =>  "Halide::UInt(64)",
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
const ISL_INCLUDE_DIR = "/home/malek/tiramisu/isl/build/include"
const ISL_LIB_DIR = "/home/malek/tiramisu/isl/build/lib"
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



type TConstant
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    name
    expr :: TExpr
    typ
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

type TSimpleRead <: TRead
    var::TVar
end

type TIndexedRead <: TRead
    var::JVar # TODO should this be a TVar?
    indices::Vector{Variable}
end

vars = Dict{Variable, JVar}()
genvars = Vector{JVar}()

idToBufferName(id::SSAValue) = "BSSA" * string(id.id)
idToBufferName(id::SlotNumber) = "BSN" * string(id.id)
idToBufferName(id::GenSlot) = "BGS" * string(id.id)

typToTBuffer = Dict(
    IntArch => function(id, typ)
        return TBuffer(idToBufferName(id),
                       [TValue("", 1, IntArch)],
                       IntArch,
                       TBTemporary,
                       true)
    end,
)


function parseNode(node::Expr)
    return exprHandlers[node.head](node)
end

function parseNode(node::Variable)
    # TODO does this always work???
    if !haskey(vars, node)
        throw(UndefVarError(Symbol(node)))
    end
    # TODO: Indexing should not be done here
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
            dummy(var, IntArch)
            cond = (wrap(var) == wrap(1))
            access = TAccess([wrap(var)], [wrap(0)])
            indices = Variable[var]
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
        sched(comp)
        return comp
    end
end

function sched(comp::TComputation)
    if !isempty(fuseStack)
        if haskey(fuseSet, fuseStack[end])
            push!(fuseSet[fuseStack[end]], comp)
        else
            fuseSet[fuseStack[end]] = TComputation[comp]
        end
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
-(args::Vararg{TExpr}) = TOp("", "-", TOpInfix, collect(args), Void)
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

wrap(val::TVar) = TSimpleRead(val)

function wrap(val::Any)
    # Shouldn't have nested calls
    print_with_color(:yellow, "Trying to wrap:\n")
    dump(val)
    return val
end

abstract Clause

immutable ForLoop <: Clause
    id::IntArch
    from::IntArch
    to::IntArch
    inc::IntArch
    var::TVar
end

clauseStack = Vector{Clause}()
condStack = Vector{TExpr}()
loopVars = Vector{Variable}()

callHandlers = Dict(
    (Core.Intrinsics, :add_int) => function(args::Vector, typ::DataType)
        return typedOp!(+(map(wrap, args)...), typ)
    end,
    (Core.Intrinsics, :box) => function(args::Vector, typ::DataType)
        return parseNode(args[2])
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
        cond = <=(wrap(from), wrap(var), wrap(to))
        if isempty(condStack)
            push!(condStack, cond)
        else
            push!(condStack, and(condStack[end], cond))
        end
    end,
    (ParallelAccelerator.TiramisuPrepass, :for_loop_end) => function(args::Vector, typ::DataType)
        id = args[1]
        assert(id == clauseStack[end].id)
        pop!(clauseStack)
        pop!(loopVars)
        pop!(condStack)
    end,
    (ParallelAccelerator.API, :setindex!) => function(args::Vector, typ::DataType)
        arr = vars[args[1]]
        texpr = parseNode(args[2])
        # TODO handle temporal
        indices = collect(Variable, args[3:end])
        assert(issubset(indices, loopVars)) # TODO more thourough checking
        access = TAccess(map(wrap, indices), [wrap(i) - wrap(1) for i in indices])
        comp = TComputation(
            new_computation_id(),
            condStack[end],
            length(indices),
            texpr,
            arr.typ,
            access,
            indices,
            false # TODO fix
        )
        sched(comp)
        arr.computation = comp
    end,
    (ParallelAccelerator.API, :getindex) => function(args::Vector, typ::DataType)
        arr = vars[args[1]]
        indices = collect(Variable, args[2:end])
        assert(issubset(indices, loopVars)) # TODO more thourough checking
        return TIndexedRead(arr, indices)
    end,
)

function handleReturn(arg::Variable, typ::DataType)
    vars[arg].buffer.argument = TBOutput
end

function handleReturn(arg, typ::DataType)
    id = new_genslot()
    buffer = typToTBuffer[typ](id, typ)
    buffer.argument = TBOutput
    comp = parseComputation(parseNode(arg), typ)
    vars[id] = JVar(id, typ, buffer, comp)
    push!(genvars, vars[id])
end

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
            nothing
        )
    else
        vars[id] = JVar(
            id,
            typ,
            typToTBuffer[typ](id, typ),
            parseComputation(parsed_rhs, typ),
        )
    end
    push!(genvars, vars[id])
end

fuseSet = Dict{IntArch, Vector{TComputation}}()
fuseStack = Vector{IntArch}()

metaHandlers = Dict(
    :fuse => function(args)
        push!(fuseStack, args[1])
    end,

    :endfuse => function(args)
        assert(fuseStack[end] == args[1])
        pop!(fuseStack)
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
    LIB, get_return()
end



# Converts a buffer to the corresponding type to pass to the C function
# Example : Buffer(Int32, {2, 3, 4}) becomes Ptr{Ptr{Ptr{Int32}}}
# But generate Symbol equivalent, i.e. :(Ptr{Ptr{Ptr{Int32}}})
function buffer_to_ccall_type(buffer)
    # need to construct it in this obscure way to be equivalent to
    # the symbol equivalent of writing
    return :(Ptr{$(buffer.typ)})
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

# returns a tuple of:
# - A tuple of types; (Ptr{Int32},)...
# - The corresponding containers in a list
function get_return()
    # TODO : handle input
    interface = [var.buffer for var in genvars if var.buffer.argument == TBOutput]
    # TODO: handle Tuple return
    assert(length(interface) == 1)
    decouple = interface[1].is_scalar ? decouple_scalar : decouple_buffer
    return (Expr(:tuple, map(buffer_to_ccall_type, interface)...),
            map(buffer_to_ccall_container, interface),
            decouple)
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
end

#Method Signature for Tiramisu File
function tiramisu_set_function(f)
    global function_name
    global computationPrefix
    function_name = f
    computationPrefix = f*"_S"
    s = string("int main()\n{\n",
                "\tglobal::set_default_tiramisu_options();\n",
                "\tfunction $function_name(\"$function_name\");\n",
                "\ttiramisu::var ",
                join(("$v(\"$v\")" for v in indexNames), ", "),
                ";\n")
end

getIndex(index::TConstIndex) = string(index.val)

getIndex(index::TVarIndex) = indexNames[index.letter]

getIndices(indices::Vector{TIndex}) = join(map(getIndex, indices), ", ")

VarToString = Dict{Variable, String}

createTExpr(val::TValue, varMap::VarToString) = "expr(($(jToC[val.typ])) $(val.val))"
function createTExpr(var::TSimpleRead, varMap::VarToString)
    return ("S$(var.var.computation.id)(" *
            join((varMap[v] for v in var.var.computation.vars), ", ") *
            ")")
end

function createTExpr(op::TOp, varMap::VarToString)
    if op.style == TOpInfix
        return "(" * join((createTExpr(a, varMap) for a in op.args), " $(op.fcn) ") * ")"
    end
    return op.fcn * "(" * join((createTExpr(a, varMap) for a in op.args), ", ") * ")"
end

function createTExpr(var::TIndexedRead, varMap::VarToString)
    println(varMap)
    dump(var.indices)
    return ("S$(var.var.computation.id)(" *
            join((varMap[v] for v in var.indices), ", ") *
            ")")
end

createISLCond(val::TValue, varMap::VarToString) = string(val.val)

function createISLCond(var::TSimpleRead, varMap::VarToString)
    return varMap[var.var.id]
end

function createISLCond(op::TOp, varMap::VarToString)
    createCond = arg -> createISLCond(arg, varMap)
    if op.style == TOpInfix
        return "(" * join(map(createCond, op.args), " $(opToISL(op.fcn)) ") * ")"
    end
    return opToISL(op.fcn) * "(" * join(map(createCond, op.args), ", ") * ")"
end

function getIndices(comp::TComputation)
    return join(indexNames[1:length(comp.vars)], ", ")
end

function createISL(comp::TComputation, varMap::VarToString)
    for (var, index) in zip(comp.vars, indexNames)
        varMap[var] = string(index)
    end
    cond = createISLCond(comp.condition, varMap)
    indices = getIndices(comp)
    return "{S$(comp.id)[$indices] : $cond}"
end

function createAccess(access::Vector{TExpr}, varMap)
    return join((createISLCond(expr, varMap) for expr in access), ",")
end

function createAccess(comp::TComputation, varMap::VarToString)
    for (var, index) in zip(comp.vars, indexNames)
        varMap[var] = string(index)
    end
    return (createAccess(comp.access.computation, varMap),
            createAccess(comp.access.buffer, varMap))
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

    println("Computation")
    computations = sort([var.computation for var in uvars], by=(c -> c.id))
    for computation in computations
        varMap = VarToString()
        push!(lines,
              "\ttiramisu::computation S$(computation.id)(" *
              "\"" * createISL(computation, varMap) * "\", " *
              createTExpr(computation.expr, varMap) * ", " *
              string(!computation.is_input) * ", " *
              jToT[computation.typ] * ", " *
              "&$function_name);")
    end
    println("Buffers")
    for var in uvars
        varMap = VarToString()
        cIndices, bIndices = createAccess(var.computation, varMap)
        push!(lines,
              "\ttiramisu::buffer $(var.buffer.name)(" *
              "\"$(var.buffer.name)\", " *
              string(length(var.buffer.dimensions)) * ", " *
              "{" * join((createTExpr(d, varMap) for d in var.buffer.dimensions), ", ") * "}, " *
              jToT[var.buffer.typ] * ", " *
              "NULL, " *
              argNames[var.buffer.argument] * ", " *
              "&$function_name);")
        push!(lines,
              "\tS$(var.computation.id).set_access(\"" *
              "{S$(var.computation.id)[$cIndices] -> $(var.buffer.name)[$bIndices]}" *
              "\");")
    end
    # Scheduling
    println("Scheduling")

    noAfterScheduling = Set{IntArch}()
    # TODO specify or compute fuse level
    for comps in values(fuseSet)
        for c in comps
            push!(noAfterScheduling, c.id)
        end
        fuseLevel = minimum(length(c.vars) for c in comps) - 1
        for i=1:(length(comps) - 1)
            push!(lines,
                  "\tS$(comps[i+1].id).fuse_after($fuseLevel, &S$(comps[i].id));")
        end
    end

    for i=1:(length(computations) - 1)
        if computations[i + 1].id in noAfterScheduling
            continue
        end
        push!(lines,
              "\tS$(computations[i+1].id).after(S$(computations[i].id), " *
              "computation::root_dimension);")
    end

    println("Interface")
    interface = [var.buffer.name for var in uvars if var.buffer.argument != TBTemporary]
    push!(lines, "\t$function_name.set_arguments({" *
                  join(map(s -> "&" * s, interface), ", ")
                  *"});")
    push!(lines, "\t$function_name.gen_time_space_domain();")
    push!(lines, "\t$function_name.gen_isl_ast();")
    push!(lines, "\t$function_name.gen_halide_stmt();")
    push!(lines, "\t$function_name.gen_halide_obj(\"$OBJ\");")
    push!(lines, "\treturn 0;")
    println("WHAT")
    return join(lines, '\n')
end

function buffer_to_arg_prototype(buffer)
    return (jToC[buffer.typ] *
            repeat("*", length(buffer.dimensions)) *
            " $(buffer.name)")
end

function buffer_to_halide_buffer(buffer)
    lines = String[]
    push!(lines,
          "auto *shape_$(buffer.name) = new halide_dimension_t[$(length(buffer.dimensions))];")
    for (i, dim) in enumerate(buffer.dimensions)
        push!(lines, "shape_$(buffer.name)[$(i - 1)].min = 0;")
        push!(lines, "shape_$(buffer.name)[$(i - 1)].extent = $(dim.val);") # TODO not always TValue
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
    interface = [var.buffer for var in genvars if var.buffer.argument != TBTemporary]
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
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_core.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_c.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_debug.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_utils.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide_lowering.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_from_halide.o")
    push!(s,"$FILENAME","-o","$GEN","-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
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
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_core.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_c.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_debug.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_utils.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide_lowering.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_from_halide.o")
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
    println(join(s, " "))
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
